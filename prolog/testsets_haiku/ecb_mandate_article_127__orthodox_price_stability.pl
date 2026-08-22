% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate Article 127: Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/institutional_governance
 *
 * SUMMARY:
 *   Article 127(1) TFEU establishes the ECB's mandate, listing price
 *   stability as the primary objective and employment, growth, and financial
 *   stability as secondary objectives 'without prejudice to' the primary
 *   goal. This constraint instantiates the orthodox_price_stability READING:
 *   price stability means a 2% inflation target, operationalized as the
 *   exclusive focus of monetary policy, with secondary objectives treated as
 *   non-operative unless price stability is threatened. The ECB Governing
 *   Council interprets the mandate this way; employment and climate
 *   constituencies read the same article differently
 *   (expansive_secondary_objectives and climate_incorporation readings). The
 *   kernel contest is real: the same treaty text grounds three structurally
 *   distinct constraints with different beneficiary sets, extraction
 *   profiles, and suppression mechanisms. This JSON generates only the
 *   orthodox reading as a clean ε-invariant constraint, following Rule 1
 *   (generate one reading only). The contest between readings is routed to
 *   omega variables (Rule 2), documented in kernel_context (Rule 3), and
 *   structured in cs_structure.reading_relations and axioms (Rule 4).
 *
 * KEY AGENTS:
 *   - ECB Governing Council: institutional agenda-setter, interprets mandate as exclusive 2% focus, controls rate-setting and asset purchases
 *   - Savers and creditors: beneficiaries of price stability and high real rates, concentrated power, global exit via arbitrage
 *   - Asset holders: beneficiaries of inflation credibility and capital-flow stability, concentrated power, global mobility
 *   - Employment-seekers: payers of subordinated employment objective, moderate power, constrained exit (euro-zone bound)
 *   - Climate constituencies: payers of externalized climate risk, organized but politically marginal, civilizational horizon, constrained exit
 *   - Member-state governments: institutional payers, constrained by ECB independence, forced to substitute fiscal policy for monetary stimulus
 *   - Expansive-interpretation advocates: excluded from ECB governance, trapped within euro-zone institutions, suppressed by orthodox dominance
 *   - Climate-integration advocates: excluded from mandate scope, trapped by Article 11 TFEU environmental integration being non-operational in ECB framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.71).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate Article 127: Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '9834a8ab-30aa-4ebc-8f85-af71b89f3bed').
narrative_ontology:cs_kernel_codification('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', fixed_text).
narrative_ontology:cs_authority_grounding('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', extraction).
narrative_ontology:cs_interpretation_layer_present('9834a8ab-30aa-4ebc-8f85-af71b89f3bed').
narrative_ontology:cs_reading_relation('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', secondary_objectives_non_operational, deontological).
narrative_ontology:cs_axiom('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', foundational, price_stability_exclusive_focus).
narrative_ontology:cs_axiom_status(price_stability_exclusive_focus, holdable).
narrative_ontology:cs_axiom_grounding('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', price_stability_exclusive_focus, empirically_contingent).
narrative_ontology:cs_reference_frame('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', bundesbank_price_stability_doctrine).
narrative_ontology:cs_drift_state('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', contemporary_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9834a8ab-30aa-4ebc-8f85-af71b89f3bed', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, asset_holders).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, employment_seekers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_action_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, central_banking_academic_establishment).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127(1) TFEU as conferring a mandate whose primary objective is price stability defined as 2% inflation, with secondary objectives (employment, growth, financial stability) explicitly subordinate and operative only when the primary target is not threatened. Sets interest rates, makes asset purchase decisions, and enforces this reading through governance procedures, speeches, and operational design. Justifies the exclusive focus as central bank orthodoxy and fidelity to the treaty text.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from a credible 2% inflation target because it protects the purchasing power of financial assets, savings, and fixed-income instruments. Their returns are preserved by preventing both inflation erosion and the monetary expansion that would reduce real interest rates. The orthodox reading subordinates employment and growth considerations, keeping real rates higher than an expansive reading would permit.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_creditors, beneficiary,
    powerful, generational, arbitrage, continental).

% Benefit from the ECB's commitment to price stability and high real rates: financial asset valuations, currency strength, and capital flows reflect confidence in the ECB's inflation-fighting credibility. The orthodox reading's exclusive focus on 2% signals discipline to global capital markets and supports capital inflows into euro-denominated assets.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, asset_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the cost of subordinating employment to price stability: when unemployment is high but inflation is below 2%, the orthodox reading constrains monetary stimulus that might expand labor demand. The 'without prejudice' clause in Article 127 is not operationalized, so employment remains a theoretical objective without steering capacity. They have no exit from the euro without emigration or political reorganization.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, employment_seekers, payer,
    moderate, biographical, constrained, continental).

% Bear the cost of the orthodox reading's externalization of climate risk from the mandate: the ECB's asset purchase framework does not incorporate climate-scenario analysis or climate-risk weighting into collateral frameworks, despite Article 11 TFEU requiring integration of climate policy across EU institutions. They argue the ECB's dominant position in asset markets means ignoring climate risk is a choice, not a constraint, and the orthodox reading enables that choice by treating climate as outside the mandate's scope.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_action_constituencies, payer,
    organized, civilizational, constrained, global).

% Are constrained by the ECB's exclusive price-stability reading: they cannot use monetary expansion to support employment or growth without pressuring fiscal policy, which carries higher debt costs and political constraints. The orthodox reading shifts the burden of employment and climate policy onto their budgets. They observe but cannot override ECB decisions; their exit is political and costly.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, member_state_governments, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, member_state_governments, observer).

% Would argue that Article 127's 'without prejudice' clause and the listing of secondary objectives (employment, growth, financial stability) authorize discretionary operational weight on these goals when price stability is not threatened. They are structurally excluded from ECB decision-making and their interpretation is suppressed by the orthodox reading's dominance in the institution.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, expansive_interpretation_advocates, excluded,
    organized, biographical, trapped, continental).

% Argue that Article 11 TFEU mandates climate integration into the ECB's asset purchase framework and that treating climate risk as external to the price-stability mandate is legally indefensible. They lack formal standing in ECB governance and the orthodox reading's narrow definition of the mandate scope excludes climate considerations from the institution's operational mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_integration_advocates, excluded,
    organized, civilizational, trapped, global).

% Derives intellectual authority and disciplinary coherence from the orthodox price-stability doctrine. The ECB's fidelity to a single-objective mandate validates the theoretical apparatus of modern central banking orthodoxy and provides career advancement paths for economists trained in that tradition. They benefit from the constraint's vindication of their discipline.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, central_banking_academic_establishment, beneficiary,
    organized, biographical, mobile, global).

% Has democratic legitimacy but limited formal authority over the ECB: it can hold hearings and propose amendments to the treaty, but the ECB's independence makes it operationally insulated from electoral pressure. Observes the constraint from a position of structural powerlessness, though political pressure from employment and climate constituencies flows through parliament.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_parliament, observer,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, savers_creditors).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, credible nominal anchor (2% inflation) across the euro area, solving the coordination problem of price-level expectations: member states cannot sustain independent monetary policies; a unified inflation target reduces uncertainty for wage bargaining, investment, and cross-border trade.
% TRANSFER_FUNCTION: Transfers the benefit of credible low inflation (and thus higher real returns on financial assets) to savers, creditors, and asset holders, while imposing the cost of subordinated employment and climate objectives on workers, job-seekers, climate-vulnerable constituencies, and member-state treasuries that must substitute fiscal stimulus for monetary support.
% ABSENT_VOICES: Advocates for expansive secondary objectives (employment, growth) and climate-integration readings are structurally excluded from ECB governance: they lack formal voting power and the orthodox reading's dominance in the institution suppresses their interpretations. Employment and climate constituencies have no seat at the decision table.
% DISAPPEARANCE_RATIONALE: If this exclusive-focus reading vanished and were replaced by an expansive reading, the ECB would operationalize secondary objectives: lower real interest rates would follow from employment-weighted rate-setting, asset purchase frameworks would incorporate climate risks, and the distribution of benefits and costs would shift dramatically toward workers and climate constituencies. The financial markets, currency valuations, and fiscal policy of member states would reorganize around different expectations.
% FOUNDING_PROBLEM: The Maastricht Treaty (1992) created a single currency but lacked an independent monetary authority with a clear mandate. Hyperinflation in the 1970s-80s had demonstrated the costs of politically-captured central banking; the Bundesbank's price-stability-first model was adopted as the template to prevent political pressure from eroding currency credibility and to establish a monetary anchor that would function across disparate member economies.
% FOUNDING_PROBLEM_CORROBORATION: The ECB and conservative economists attest the founding problem is still live, citing ongoing threats to currency credibility and inflation anchoring. Employment advocates and climate constituencies attest the founding problem was substantially solved by the 1990s and the exclusive focus persists as a distributional choice, not a necessity; labour economists and central bank reform advocates from outside the ECB's own institution corroborate this challenge.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (interval endpoint), capturing the ECB's operational choice to deliver concentrated benefits (high real returns for savers/creditors/asset holders) while bearing costs are borne diffusely (employment constrained, climate risks externalized, fiscal burden on governments). The series shows extraction rising from 0.55 to 0.68 over the interval, reflecting post-2008 and especially post-2015 period: zero-interest-rate policy intensified real-rate redistribution (savers and creditors bore opportunity costs; financial asset holders benefited from asset-price inflation funded by low rates). Suppression is high (0.71) because the constraint's persistence requires active exclusion of competing readings from ECB operational space: the 'without prejudice' clause is interpreted as non-operative, climate-integration proposals are resisted, and employment-weighted rate-setting is categorically rejected despite Article 127 listing these objectives. Theater_ratio is moderate (0.42): the ECB conducts genuine price-stability operations (inflation targeting, forward guidance, taper decisions), but an increasing share of institutional effort goes to defending the mandate against reinterpretation rather than executing the coordination function itself. Accessibility_collapse is high (0.78) because once the orthodox reading is adopted, alternatives become institutionally inaccessible: individual member states cannot pursue employment-first monetary policy (they have ceded that authority), the ECB's governance structure is insulated from electoral pressure, and the treaty lock-in makes reinterpretation extremely difficult. Resistance is moderate (0.62): climate and employment constituencies mount real political pressure (legislative proposals, court challenges, public advocacy), but the institution is designed to withstand that pressure and doctrinal consensus among central bankers amplifies resilience. Beneficiaries are named as a narrow, concentrated set (savers_creditors, asset_holders); victims are named as dispersed and politically marginal (employment_seekers, climate_action_constituencies). This asymmetry drives the tangled_rope classification: genuine coordination function (price stability is a real collective good), but asymmetric extraction (benefits concentrated, costs diffuse) and active enforcement (suppression of competing readings) are both necessary for the constraint to persist.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB's and savers' seats, the constraint is genuine coordination: price stability is a collective good that requires institutional commitment and treaty protections against political capture. From employment and climate constituencies' seats, the constraint is extractive: the ECB's exclusive focus on 2% is a distributional choice that privileges financial stability over labor-market inclusivity and climate risk management, justified by an orthodoxy that benefits the creditor class. The same institutional structure—the ECB's independence and single-objective focus—is interpreted by the beneficiary seats as 'necessary protection against political pressure' and by the target seats as 'structural insulation from democratic accountability.' The divergence is not measurement error; it is built into the constraint's asymmetric beneficiary/victim structure. The divergence is also the point: the engine's per-seat classification system is designed to measure exactly this kind of structural asymmetry, and a claim/metric gap (claiming rope while measuring tangled_rope extraction) is an honest acknowledgment of the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across stakeholder seats. The ECB Governing Council is near d=0.5 (symmetric: it maintains the system it designed, derives institutional prestige and autonomy from the mandate, but also bears operational constraint and political pressure). Savers/creditors sit near d=0.0 (beneficiary end: they collect the constraint's output in real interest rates and asset valuations; the constraint's persistence is entirely advantageous to them). Asset holders sit even nearer d=0.0 (they benefit from credibility and capital flows, have arbitrage exits if the constraint threatens asset valuations, and would reverse positioning only if the reading fundamentally changed). Employment-seekers sit near d=1.0 (target end: they bear subordinated-objective costs, have constrained exit from the euro-zone, and would exit employment constraints only if secondary objectives were operationalized). Climate constituencies also sit near d=1.0 (targets: they pay the cost of climate-risk externalization, have constrained exit, and benefit only if the reading shifts to climate_incorporation). The derived directionality profile explains why the constraint is tangled_rope from some seats (the ECB as coordinator of price stability, beneficiaries as free-riders on a real coordination good) and snare-like from others (employment and climate constituencies as targets of an extractive arrangement they cannot exit). The engine computes per-seat classification; this note explains why divergence is structurally real.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (currency credibility threatened by inflation, Maastricht era, 1970s-80s hyper-inflation shadow) may be obsolete or substantially resolved by now. The euro has not experienced significant inflation volatility since the financial crisis, the ECB's credibility is established, and peer central banks with dual mandates (Fed, BoE, BoJ) show no lower inflation credibility. Yet the exclusive 2% focus persists and has hardened (post-2008 and post-2015, the orthodoxy has intensified, not loosened). This is a mandatrophy signature: a constraint whose founding mandate has outlived its original function but the institutional arrangement persists due to ideological commitment and beneficiary capture. Mandatrophy_resolved is not yet declared because the empirical question omega(4) names remains open: is the founding problem live or dead? The classification remains tangled_rope either way (the constraint has coordination + extraction components regardless of whether the founding problem is obsolete). But mandatrophy would be confirmed if the founding_problem_status resolves to 'dead' while disappearance_verdict is 'world_rearranges'—a constraint whose founding function is gone but whose removal would still reorganize institutional arrangements (because beneficiaries are entrenched and have made the constraint structural).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_127_textual_ambiguity,
    'Does Article 127(1) TFEU''s phrase ''without prejudice to'' the secondary objectives authorize discretionary operational weight on those objectives when price stability is not threatened, or does it merely preserve their listing as non-binding aspirations?',
    'Formal ECB legal opinion or European Court of Justice preliminary ruling interpreting the ''without prejudice'' clause. Natural experiment from member states that propose mandatory secondary-objective weightings and observe ECB legal responses.',
    'A ''without prejudice'' ruling that authorizes discretionary secondary-objective weight would undermine the orthodox reading''s exclusive-focus claim and classify the constraint as more negotiable and less mandated than currently. The beneficiary set would expand to include employment constituencies; suppression would decline as the constraint''s necessity claim weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_127_textual_ambiguity, empirical, 'Whether Article 127''s secondary objectives are legally operational or merely aspirational.').

omega_variable(
    kernel_reading_contest_stability,
    'This constraint instantiates the orthodox_price_stability reading of the ecb_mandate_article_127 kernel. Does this reading remain institutionally dominant in the ECB''s governance, or is the kernel drifting toward the expansive_secondary_objectives or climate_incorporation readings?',
    'Monitoring ECB governance decisions, Governing Council statements, regulatory reform proposals from member states, and European Court judgments on climate integration. A series of climate-weighted collateral decisions or employment-responsive rate cuts would signal drift. Treaty amendment proposals would signal contested reading.',
    'If the kernel is drifting, this constraint''s classification may be transitional rather than stable. An expansive reading operationalization would reclassify to rope (genuine coordination of multiple objectives) or ladder the snare classification (if the orthodox reading is maintained despite explicit contradicting decisions). Climate incorporation would decompose this constraint into two separate stories with different beneficiary structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_stability, conceptual, 'Institutional stability of this reading versus sibling readings.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the measured suppression of expansive secondary-objective and climate-integration readings structural (ECB institutional design makes other readings operationally impossible) or ideological (the readings are suppressed by dominant economic doctrine even though they are technically feasible)?',
    'Post-reform suppression trajectory: if ECB governance were explicitly reformed to operationalize secondary objectives (new voting rules, mandate restatement), would suppression persist at the same level? If it declines, suppression was ideological; if it persists, it was structural.',
    'Structural suppression is harder to reverse; ideological suppression is more vulnerable to generational doctrinal shifts and political pressure. A finding that suppression is primarily ideological would weaken the constraint''s claim to naturalness and would increase the credibility of omega (1)''s resolution mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Suppression mechanism: structural barriers vs. ideological dominance.').

omega_variable(
    founding_problem_obsolescence_contest,
    'Does the founding problem (currency credibility threatened by inflation) remain live, or has it been substantially resolved by institutional design, and the exclusive price-stability focus persists as distributional choice rather than structural necessity?',
    'Comparative analysis: do other central banks (US Federal Reserve, Bank of Japan, Bank of England) with explicit multiple-objective mandates show lower inflation volatility or lower credibility than the ECB? Do euro-area inflation anchoring metrics deteriorate after secondary objectives are operationalized (if attempted)? Do financial markets treat the euro as less credible when the ECB signals employment-weight in rate-setting?',
    'If the founding problem is found obsolete but the constraint persists, this crosses the mandatrophy threshold: a constraint whose mandate has outlived its function. Classification remains tangled_rope (coordination + extraction are both present), but mandatrophy_resolved would be true, and the constraint would be a candidate for deliberate institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_contest, empirical, 'Whether the founding problem is live or obsolete.').

omega_variable(
    kernel_doctrine_genealogy,
    'This reading embodies the orthodox central-banking doctrine of the Bundesbank model (transmitted through the Maastricht Treaty design and the ECB''s founding mandate). Is this doctrine a discovered truth about monetary policy, or a constructed institutional choice that benefits particular constituencies?',
    'Historical genealogy: trace the doctrine''s origins to 1970s-80s inflation responses in Germany and the political choice to constitutionalize the Bundesbank model into EU law. Cross-national comparison: do economies with dual mandates (US, UK, Japan) show worse outcomes on inflation or currency credibility? Do they show better outcomes on employment or welfare?',
    'If the doctrine is a constructed institutional choice rather than a discovered necessity, the constraint''s classification does not change (tangled_rope remains), but the false-summit detection machinery would flag the constraint''s vindication of a particular doctrine (price_stability_primacy_doctrine) as a beneficiary rather than a natural law. This reframes the constraint as politically contingent rather than technically mandated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_doctrine_genealogy, conceptual, 'Whether the orthodox price-stability doctrine is a discovered truth or a constructed institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ecb__tr_t16, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 16, 0.42).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 24, 0.42).
narrative_ontology:measurement(ecb__tr_t28, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 28, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(ecb__be_t16, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(ecb__be_t28, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ecb__su_t16, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(ecb__su_t28, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 28, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested ecb_mandate_article_127 kernel. The sibling readings (expansive_secondary_objectives and climate_incorporation) instantiate different interpretations of the same treaty article and produce different constraint classifications, beneficiary/victim structures, and extraction profiles. All three readings share the kernel but are structurally distinct constraints. Network links enable decomposition analysis: the orthodoxy reading influences both siblings by setting the institutional baseline that would be displaced if either sibling reading were adopted. No reading forecloses another within a single coherent framework; all three coexist as live interpretive positions held by different ECB factions and external constituencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
