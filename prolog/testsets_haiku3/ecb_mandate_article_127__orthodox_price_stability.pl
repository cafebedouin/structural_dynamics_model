% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: ECB Mandate: Exclusive Price Stability Focus (Article 127 TFEU Orthodox Reading)
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   Article 127 TFEU establishes the ECB's mandate with a primary objective
 *   (price stability) and secondary objectives (employment, growth,
 *   environmental sustainability 'without prejudice' to price stability).
 *   This constraint embodies one reading of that contested text: the orthodox
 *   interpretation that subordinates secondary objectives to non-operational
 *   status, treating the primary mandate as exclusive. The ECB Governing
 *   Council operationalizes this reading through policy frameworks,
 *   collateral rules, and communication. The narrow reading benefits
 *   creditors and savers (price certainty, default risk reduction) while
 *   externalizing employment and climate risks to fiscal and environmental
 *   policy. The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope (coordination + asymmetric extraction) while the authored
 *   metrics show high suppression and rising theater (defensive codification
 *   of mandate boundaries rather than genuine functional necessity). The
 *   engine measures that structural divergence. The measurement series tracks
 *   mandate-boundary hardening from 1999 (ECB foundation, institutional
 *   consolidation phase) through 2026 (climate and employment pressures at
 *   peak, suppression stabilized at high level).
 *
 * KEY AGENTS:
 *   - ECB Governing Council: primary agenda-setter, operationalizes the orthodox reading through policy and communication frameworks
 *   - Savers/creditors and fixed-income beneficiaries: narrow but powerful beneficiary set; actively defend the orthodox reading and resist mandate expansion
 *   - Employment policy advocates and labor unions: constrained payers; argue secondary objectives should be operationalized when inflation is not threatened
 *   - Climate integration proponents: constrained payers; argue Article 11 TFEU and climate integration clause should permit active asset-purchase direction toward climate risks
 *   - Member state fiscal authorities: constrained payers; seek monetary-fiscal coordination bandwidth blocked by strict mandate subordination
 *   - European Parliament critics: excluded; lack formal leverage over mandate interpretation but can pursue legislative amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate: Exclusive Price Stability Focus (Article 127 TFEU Orthodox Reading)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '3a23ee1e-0d60-48bf-9ada-83011ee998cf').
narrative_ontology:cs_kernel_codification('3a23ee1e-0d60-48bf-9ada-83011ee998cf', fixed_text).
narrative_ontology:cs_authority_grounding('3a23ee1e-0d60-48bf-9ada-83011ee998cf', lineage).
narrative_ontology:cs_interpretation_layer_present('3a23ee1e-0d60-48bf-9ada-83011ee998cf').
narrative_ontology:cs_reading_relation('3a23ee1e-0d60-48bf-9ada-83011ee998cf', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('3a23ee1e-0d60-48bf-9ada-83011ee998cf', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('3a23ee1e-0d60-48bf-9ada-83011ee998cf', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('3a23ee1e-0d60-48bf-9ada-83011ee998cf', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_axiom('3a23ee1e-0d60-48bf-9ada-83011ee998cf', foundational, price_stability_supremacy).
narrative_ontology:cs_axiom_status(price_stability_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3a23ee1e-0d60-48bf-9ada-83011ee998cf', price_stability_supremacy, deontological).
narrative_ontology:cs_reference_frame('3a23ee1e-0d60-48bf-9ada-83011ee998cf', narrow_mandate_supremacy).
narrative_ontology:cs_drift_state('3a23ee1e-0d60-48bf-9ada-83011ee998cf', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a23ee1e-0d60-48bf-9ada-83011ee998cf', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, fixed_income_beneficiaries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, employment_policy_advocates).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_integration_proponents).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, fiscal_policy_coordinators).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_political_independence_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, price_stability_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and operationalizes Article 127 TFEU as requiring an exclusive mandate on the 2% inflation target. Sets operational frameworks, QE parameters, collateral rules, and communication strategy aligned with this reading. Justifies the narrow mandate as ensuring institutional independence from political pressure and credibility in inflation control. Produces technical guidance and policy statements that codify the orthodox interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from low, stable inflation (price certainty, real asset value preservation, borrower default risk reduction). This reading's exclusive focus on the 2% target protects their purchasing power and the real value of fixed-income claims. Their interests align structurally with the orthodox interpretation, which subordinates employment and growth considerations that might push inflation higher.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_creditors, beneficiary,
    powerful, generational, mobile, global).

% Pension funds, insurance companies, and bond-holding institutions whose liabilities and asset bases depend on low, predictable inflation. They actively support the orthodox reading and argue against mandate expansion. Gain reputational and financial benefit when the ECB commits to narrow, inflation-focused governance.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, fixed_income_beneficiaries, beneficiary,
    organized, generational, mobile, continental).

% Labor unions, social-democratic parties, member states' employment ministries, and development-focused economists argue that the ECB has secondary objectives (employment, growth) that could be weighted when price stability is not threatened. The orthodox reading subordinates these objectives to non-operational status, foreclosing institutional space for employment-sensitive monetary policy. Their policy bandwidth is constrained by the mandate interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, employment_policy_advocates, payer,
    organized, biographical, constrained, continental).

% Environmental economists, climate policy advocates, and member states' green-transition officials argue that Article 11 TFEU (environmental integration obligation) and Article 127(2)'s secondary objectives should permit active climate risk integration into asset purchases and collateral frameworks. The orthodox reading treats climate considerations as external to the mandate, subordinating them. Their climate action pathways depend on institutional flexibility the reading denies.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_integration_proponents, payer,
    moderate, generational, constrained, global).

% Member state finance ministries and EU fiscal coordination bodies argue they need monetary policy flexibility (or at least non-obstruction) to pursue employment and growth when inflation is low. The orthodox reading's strict subordination of secondary objectives creates asymmetry: the ECB can tighten aggressively for inflation control, but cannot ease for growth or employment without violating its interpreted mandate. They lack coordination bandwidth.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, fiscal_policy_coordinators, payer,
    organized, biographical, constrained, national).

% The German legal tradition of central bank independence and price stability supremacy provides intellectual and institutional authority for the orthodox reading. Bundesbank-trained officials and German-aligned governance structures reinforce the interpretation through personnel, networks, and doctrinal authority.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, bundesbank_doctrine_tradition, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(ecb_mandate_article_127__orthodox_price_stability, bundesbank_doctrine_tradition).

% MEPs from left and green parties argue the orthodox reading is inconsistent with the Treaties' environmental and social policies. They lack formal leverage over the ECB's mandate interpretation but can pursue legislative amendment, challenge the constitutionality of the reading, or push member states to coordinate monetary-fiscal policy outside ECB channels.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_critics_in_european_parliament, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, savers_creditors).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable nominal anchor (the 2% target) that coordinates private sector price-setting, wage bargaining, and long-term contract formation. A single, credible inflation target reduces uncertainty about purchasing power and allows decentralized economic coordination without constant price-level surprises.
% TRANSFER_FUNCTION: Transfers purchasing-power stability (and real asset value protection) to savers, creditors, and fixed-income beneficiaries while externalizing employment and growth objectives to fiscal policy. The narrow mandate also transfers climate-risk responsibility away from the ECB and onto governments and private-sector adaptation.
% ABSENT_VOICES: Member state labor ministries, European Parliament left and green coalitions, climate-integration economists, and non-creditor constituencies (workers, small firms, employment-sensitive sectors) would object if heard. They are structurally excluded from the ECB's governance (independent technocrats, not representative bodies) and lack formal channels to force reinterpretation.
% DISAPPEARANCE_RATIONALE: If the orthodox reading vanished and the ECB adopted an expansive or climate-integrated mandate, monetary policy would shift substantially: asset purchases would be directed toward climate transition, rates might accommodate growth/employment more flexibly, and inflation targets might shift. This would reorganize credit allocation, asset pricing, and fiscal-monetary coordination across the eurozone.
% FOUNDING_PROBLEM: High inflation and demand instability in the 1970s–80s created credibility deficits for central banks; the Bundesbank's independence and price-stability focus restored nominal anchor credibility. The ECB was designed to replicate that model at EU scale: a technocratic, independent authority insulated from political pressure to print money, with a narrow mandate to prevent the political-economy trap of persistent inflation.
% FOUNDING_PROBLEM_CORROBORATION: The ECB Governing Council and German-aligned member states attest the problem remains: inflation risks, credibility erosion, and political pressure to monetize deficits. Labor unions, climate economists, and southern European fiscal authorities attest the founding problem is substantially solved (post-2008 anchoring is stable; current inflation risks are supply-side, not demand-driven) and the narrow mandate persists as institutional self-interest rather than necessity. Independent economic analysis (IMF, OECD reports; academic consensus in heterodox economics) supports the shifted-function reading.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Base extractiveness (0.68 at interval end) is substantial because the reading transfers purchasing-power stability to a narrow creditor class while subordinating broader employment and environmental objectives to external policy domains — that distribution is asymmetric and enforced rather than negotiated. Suppression (0.72) is high because the constraint's persistence requires active institutional codification: the ECB must continually restate that secondary objectives are non-operational, defend against legislative pressure (European Parliament climate mandates, employment directives), and reject member state requests for flexibility. The orthodoxy does not persist through participant preference; it requires enforcement against legitimate alternative readings. Theater (0.28 and rising from 0.08 at founding) reflects increasing gap between the stated coordination function (nominal anchor) and the actual institutional activity (defending mandate boundaries against political pressure, managing public controversy, conducting rhetorical justification). The 27-year measurement series captures the constraint's lifecycle: 1999–2008 low suppression because the post-inflation regime was globally credible and uncontested; 2008–2015 rising suppression as fiscal crises and employment debates challenged the mandate; 2020–2026 plateau at high suppression as climate and employment pressures stabilized at persistent level (not transient crisis). The grid is authored on one shared time axis so every metric is valued at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB Governing Council and creditor seats, the constraint is genuine coordination (a narrow, credible target that enables decentralized price-setting and long-term contracting). From the employment-advocate and climate-integration seats, the same constraint operates as enforced extraction (institutional asymmetry that privileges monetary stability over legitimate secondary objectives). From the fiscal-coordination seat, it is a collective-action barrier (prevents monetary-fiscal flexibility that would benefit the eurozone as a whole when inflation is subdued). The engine computes per-seat classification from the structural data: the beneficiary seats should classify the constraint as rope or coordination-dominant; the payer seats should classify it as snare or tangled-rope with high extraction. The claimed type (tangled_rope) reflects the reading's own perspective that acknowledges both coordination (nominal anchor) and asymmetric extraction (subordination of legitimate secondary objectives).
 *
 * DIRECTIONALITY LOGIC:
 *   Savers and fixed-income beneficiaries sit at the beneficiary end of the directionality spectrum (d near 0.0): they benefit directly from price stability focus and have high exit optionality (mobile capital, globally diversified portfolios). Employment advocates, climate proponents, and fiscal coordinators sit at the target end (d near 1.0): they are constrained by the mandate interpretation, cannot exit the eurozone coordination framework, and bear the cost of subordinated objectives. The ECB Governing Council sits near the agenda-setter position: it sets the rules but is itself constrained by the Treaties and institutional path-dependence — its d is near the beneficiary end because the narrow mandate reduces political pressure and operational complexity it would face under expansive interpretation. The measurement series and beneficiary/victim declarations establish these directionalities; the engine derives effective extraction from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodox reading exhibits early-stage mandatrophy (founding problem atrophying while constraint persists). The founding problem (inflation credibility, 1970s–80s trauma) is substantially solved in the eurozone: inflation has been anchored post-1999, credibility is established, and the Bundesbank model proved durable. Yet the constraint persists, increasingly defended by institutional theater (rhetorical justifications, boundary assertions, resistance to mandate expansion) rather than functional necessity. Rising theater ratio (0.08 to 0.28) is the first indicator. The six-questions mismatch check (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags the mandatrophy hypothesis. Independent testimony (labor, climate, fiscal authorities) attests the founding problem is solved; the constraint's persistence is institutional inertia and beneficiary protection. The measurement series shows suppression rising as the founding problem evidence accumulated, rather than suppression falling (which would indicate successful coordination). The constraint did not become more functionally necessary; it became more defended. This is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_objectives_operationality_ambiguity,
    'Does ''without prejudice to the objective of price stability'' in Article 127(1) mean secondary objectives are permanently subordinate (orthodox reading) or operationizable when price stability is not threatened (expansive reading)?',
    'ECJ ruling on mandate interpretation; or definitive legislative clarification via treaty amendment. Historical drafting records do not resolve the ambiguity — both interpretations find support in negotiating position records from Maastricht and Amsterdam.',
    'If ECJ or amendment rules the clause permits operational weight on secondary objectives when inflation is low, the constraint shifts from tangled_rope (exclusivity + asymmetric extraction) to rope (genuine coordination with bounded secondary objectives). Beneficiary/victim structure inverts: employment advocates move to beneficiary; savers retain protection but do not capture exclusive benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_objectives_operationality_ambiguity, conceptual, 'Textual interpretation of Article 127(1) ''without prejudice'' clause: subordination vs. operationality of secondary objectives').

omega_variable(
    founding_problem_extinction_vs_institutional_defense,
    'Is the rising suppression required because the orthodox reading''s institutional legitimacy is genuinely under threat (founding problem atrophied, alternative readings gaining force), or because the ECB has internalized price-stability supremacy as self-evident and defends it reflexively?',
    'Post-counterfactual analysis: if the ECB adopted the expansive reading and operationalized secondary objectives, would inflation risk materially increase? Central-bank conduct studies and monetary-economics consensus on dual-mandate feasibility (US Fed, Bank of England evidence) provide external calibration.',
    'If suppression is reactive (institutional threat), the constraint remains tangled_rope with high extraction but under increasing governance pressure; if suppression is reflexive (internalized doctrine), the constraint is a piton — atrophied function defended by theatrical codification. The distinction affects remediation pathways: reactive constraints can be reformed via ECJ ruling or amendment; pitons require broader institutional delegitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_extinction_vs_institutional_defense, empirical, 'Whether rising suppression reflects actual threat to the orthodox reading or institutional theater defending atrophied mandate necessity').

omega_variable(
    climate_integration_externality_assignment,
    'Are climate risks truly external to the ECB''s price-stability mandate (orthodox reading), or is the assignment of climate-transition responsibility to fiscal/environmental policy a constructed externality that protects the narrow mandate at the cost of systemic financial risk?',
    'Central-bank stress testing and climate-financial analysis: if climate risks materialize as inflation or financial instability in ECB''s operating domain, externalization proves untenable. Alternatively: ECB explicit climate-risk integration (NGFS frameworks, prudential regulation) reveals the ''externality'' was always false and institutional choice, not structural necessity.',
    'If climate risks are material to price stability (orthodox incorporation view), the constraint becomes climate-integrated tangled_rope (coordination + extraction, but under revised beneficiary set including green-transition actors). If externalization is sustainable, the constraint remains pure extraction onto climate advocates but proves stable. Intermediate: climate integration happens via secondary channels (prudential frameworks, collateral revisions) while the mandate interpretation formally stays orthodox, producing hybrid theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_integration_externality_assignment, empirical, 'Whether climate-risk externalization is structurally defensible or constructed to protect the narrow mandate').

omega_variable(
    kernel_reading_contention_structure,
    'Is the three-reading contest (orthodox, expansive, climate-incorporated) a genuine structural ambiguity in the text, or a distributional struggle where institutional actors claim different readings to protect their distributional interests (savers prefer orthodox; labor prefers expansive; environmentalists prefer climate-incorporated)?',
    'Textual archaeology and drafting-intent analysis; comparative institutional readings (how ECB, national central banks, ECJ, and European Parliament parse Article 127); distributional analysis of who benefits under each reading.',
    'If the contest is structural ambiguity, all three readings are legitimate and the choice is political (which authority governs interpretation). If the contest is distributional struggle, the readings are post-hoc rationalizations and the constraint''s stability depends on whoever controls the ECB''s institutional agenda. The first view suggests the constraint is amenable to stable resolution via clarification; the second suggests it will oscillate with institutional power shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention_structure, conceptual, 'Whether the three-reading contest is structural ambiguity or distributional struggle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_127_ortho_tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.08).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t1999, observed).
narrative_ontology:measurement(ecb_127_ortho_tr_t2008, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2008, 0.12).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t2008, observed).
narrative_ontology:measurement(ecb_127_ortho_tr_t2015, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t2015, observed).
narrative_ontology:measurement(ecb_127_ortho_tr_t2020, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2020, 0.25).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t2020, observed).
narrative_ontology:measurement(ecb_127_ortho_tr_t2024, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t2024, observed).
narrative_ontology:measurement(ecb_127_ortho_tr_t2026, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(ecb_127_ortho_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ecb_127_ortho_be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.45).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t1999, observed).
narrative_ontology:measurement(ecb_127_ortho_be_t2008, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t2008, observed).
narrative_ontology:measurement(ecb_127_ortho_be_t2015, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t2015, observed).
narrative_ontology:measurement(ecb_127_ortho_be_t2020, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t2020, observed).
narrative_ontology:measurement(ecb_127_ortho_be_t2024, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t2024, observed).
narrative_ontology:measurement(ecb_127_ortho_be_t2026, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(ecb_127_ortho_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb_127_ortho_su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t1999, observed).
narrative_ontology:measurement(ecb_127_ortho_su_t2008, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t2008, observed).
narrative_ontology:measurement(ecb_127_ortho_su_t2015, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t2015, observed).
narrative_ontology:measurement(ecb_127_ortho_su_t2020, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t2020, observed).
narrative_ontology:measurement(ecb_127_ortho_su_t2024, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t2024, observed).
narrative_ontology:measurement(ecb_127_ortho_su_t2026, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(ecb_127_ortho_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.1).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% Article 127 TFEU is a contested kernel instantiated in three structurally distinct constraint stories: (1) orthodox_price_stability (this story) — narrow beneficiary set (creditors), high suppression, exclusive mandate interpretation; (2) expansive_secondary_objectives — broader beneficiary set (includes employment advocates), operationalizable secondary objectives, balanced discretion; (3) climate_incorporation — environmental risk integration, Article 11 TFEU mandatory environmental consideration. All three share the same kernel text (Article 127 TFEU) but differ in ε (extractiveness for different seats), beneficiary/victim assignment, suppression mechanisms, and type classification. The three readings are linked by network.affects_constraints so comparative analysis can track how one reading's institutional dominance suppresses the others. Each story is authored with independent ε (the standing arrangement under contest assessed by that reading's own lights), not hedged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
