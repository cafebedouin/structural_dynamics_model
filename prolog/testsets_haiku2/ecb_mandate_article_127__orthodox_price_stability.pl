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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   Article 127 of the Treaty on the Functioning of the European Union
 *   establishes the ECB's mandate with a primary objective (price stability)
 *   and secondary objectives (support for other EU policies without prejudice
 *   to price stability). The orthodox price stability reading interprets
 *   'without prejudice' as a hard constraint: secondary objectives remain
 *   subordinate and non-operational unless price stability is already
 *   secured. This reading benefits creditors and savers by prioritizing
 *   inflation control and externalizes employment and climate risks. The
 *   constraint is a tangled rope because it coordinates a price-stability
 *   expectation (genuine coordination good) while simultaneously extracting
 *   costs from the employment-constrained and climate-vulnerable through
 *   suppression of alternative policy frames. The measurement series traces
 *   rising extractiveness (baseline concerns about employment and climate
 *   compound with time) and rising theater ratio (ECB engagement with
 *   secondary objectives grows increasingly ceremonial, as consultation
 *   without operational weight).
 *
 * KEY AGENTS:
 *   - ECB Governing Council: institutional agenda-setter; sets and enforces the orthodox interpretation through policy statements and operational decisions
 *   - Creditor savers: powerful beneficiaries; gain from inflation control and interest-rate stability
 *   - Unemployment-constrained workers: organized payers; bear costs of restrictive bias in monetary policy
 *   - Climate-risk-externalized future generations: powerless, civilization-horizon payers; bear externalized climate transition costs
 *   - Expansionist member states: excluded; structurally unable to operationalize growth/employment objectives through ECB coordination
 *   - Legal constitutionalists: observer seat; assess Treaty interpretation and legitimacy of the reading
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
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'b843d039-6f1f-4259-9b94-e4b7d5494115').
narrative_ontology:cs_kernel_codification('b843d039-6f1f-4259-9b94-e4b7d5494115', fixed_text).
narrative_ontology:cs_authority_grounding('b843d039-6f1f-4259-9b94-e4b7d5494115', extraction).
narrative_ontology:cs_interpretation_layer_present('b843d039-6f1f-4259-9b94-e4b7d5494115').
narrative_ontology:cs_reading_relation('b843d039-6f1f-4259-9b94-e4b7d5494115', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('b843d039-6f1f-4259-9b94-e4b7d5494115', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('b843d039-6f1f-4259-9b94-e4b7d5494115', foundational, secondary_objectives_dormant_without_explicit_securitization).
narrative_ontology:cs_axiom_status(secondary_objectives_dormant_without_explicit_securitization, holdable).
narrative_ontology:cs_axiom_grounding('b843d039-6f1f-4259-9b94-e4b7d5494115', secondary_objectives_dormant_without_explicit_securitization, deontological).
narrative_ontology:cs_axiom('b843d039-6f1f-4259-9b94-e4b7d5494115', secondary, central_bank_independence_requires_narrow_mandate).
narrative_ontology:cs_axiom_status(central_bank_independence_requires_narrow_mandate, holdable).
narrative_ontology:cs_axiom_grounding('b843d039-6f1f-4259-9b94-e4b7d5494115', central_bank_independence_requires_narrow_mandate, instrumental).
narrative_ontology:cs_reference_frame('b843d039-6f1f-4259-9b94-e4b7d5494115', price_stability_primacy).
narrative_ontology:cs_drift_state('b843d039-6f1f-4259-9b94-e4b7d5494115', contemporary_climate_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b843d039-6f1f-4259-9b94-e4b7d5494115', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditor_savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, currency_stability_beneficiaries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployment_constrained).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_risk_externalized).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, price_stability_primacy_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_independence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and operationalizes Article 127. Frames the 2% inflation target as the binding legal mandate and treats employment/growth/climate as secondary considerations that operate 'without prejudice' to price stability — meaning they are deferred unless price stability is already secured. Sets monetary policy instruments and conducts asset purchases. Justifies the narrow reading as fidelity to the Treaty text and defense of central bank independence from political pressure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from low and stable inflation, which preserves purchasing power of savings and fixed-income investments. The orthodox reading prioritizes their interests by keeping inflation near 2% target rather than tolerating higher inflation to support employment or investment in climate transitions. Can arbitrage currency and asset movements across jurisdictions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, creditor_savers, beneficiary,
    powerful, biographical, arbitrage, global).

% Workers and job-seekers in the eurozone. The orthodox reading subordinates employment objectives, meaning monetary policy will not be deployed to support job creation if doing so would risk overshooting the 2% inflation target. Economic slack persists longer than under an expansive interpretation. Their exit options are limited by eurozone labor immobility and fiscal policy being controlled by member states, not the ECB.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployment_constrained, payer,
    organized, biographical, trapped, continental).

% Future generations and climate-impacted populations. The orthodox reading excludes climate risk from the ECB's mandate operationalization, treating it as an externality for fiscal authorities to manage. Monetary policy does not systematically incorporate climate transition risk into asset purchase decisions or collateral frameworks, externalizing climate costs outside the central bank's responsibility structure. Exit is impossible; representation is absent from policy-making.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_risk_externalized, payer,
    powerless, civilizational, trapped, global).

% National governments in the eurozone seeking ECB support for growth and employment objectives. They are structurally excluded from setting ECB policy (by design, to ensure independence) but their concerns would push toward the expansive reading. They can lobby and appeal for mandate reinterpretation, but the orthodox reading strips them of fiscal-monetary coordination levers.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, expansionist_member_states, excluded,
    organized, biographical, constrained, continental).

% EU bodies (Commission, Parliament) responsible for climate targets under Article 11 TFEU. They cannot compel the ECB to operationalize climate risk as a monetary-policy objective; the orthodox reading treats climate policy as external to the central bank's remit. They can advocate for mandate revision but cannot override the current Treaty interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_policy_coordinating_institutions, excluded,
    institutional, generational, constrained, continental).

% Constitutional law scholars and EU Court of Justice analysts who assess whether the orthodox reading is textually justified. They occupy an analytical seat: observing whether the 'without prejudice' language truly subordinates secondary objectives or whether it permits operational weight when price stability is not under threat. Their interpretive authority influences the credibility of whichever reading prevails.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, legal_constitutionalists, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, creditor_savers).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable nominal anchor (2% inflation target) that coordinates private-sector expectations and economic contracting across the eurozone. A single inflation target reduces uncertainty for cross-border commerce and investment.
% TRANSFER_FUNCTION: Moves purchasing power from wage-earners and job-seekers (who face tighter labor markets under restrictive policy) to creditors and savers (whose fixed-income returns preserve value). Also transfers climate risks from institutional balance sheets to future generations and climate-vulnerable populations.
% ABSENT_VOICES: Unemployed persons and climate-impacted parties have no seat at the ECB Governing Council and no binding representation in mandate debates. Member state fiscal authorities are structurally excluded by independence-by-design. Sibling reading advocates (expansionist and climate-incorporation voices) are present in EU discourse but non-operational in ECB policy formation.
% DISAPPEARANCE_RATIONALE: If the orthodox mandate reading vanished and were replaced by expansive or climate-inclusive readings, ECB monetary policy would operationalize employment and climate objectives more actively. Asset purchases would weight climate transition support; interest rates would tolerate higher inflation to reduce unemployment. The eurozone labor market, investment patterns, and climate transition pathways would reorganize. Member state fiscal authorities would gain implicit coordination leverage with the central bank.
% FOUNDING_PROBLEM: Inflation in the 1970s–80s eroded savings, discouraged investment, and destabilized currency unions. Central bank independence was established to insulate monetary policy from political pressure for short-term expansion at the cost of long-term price instability.
% FOUNDING_PROBLEM_CORROBORATION: Central bankers and creditor-class economists attest the founding problem is live and require eternal vigilance against inflation. Labor economists and climate scientists attest the founding problem was solved in most developed economies by the 1990s and that the orthodox reading now subordinates live contemporary problems (unemployment persistence, climate risk accumulation) to a solved historical one. Legislative testimony from the European Parliament and member state fiscal authorities supports the 'solved problem' reading; ECB leadership supports the 'eternal vigilance' reading.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.68) because the constraint systematically privileges creditor interests over employment and climate. The beneficiary set is narrow (savers/creditors); the cost-bearer set is broad and diffuse (the unemployed, future generations). Suppression is high (0.71) because alternative interpretations of Article 127 are not merely disfavored but actively ruled out by ECB institutional practice — the 'without prejudice' language is interpreted as a prohibition on operational weight for secondary objectives, not a permission. Theater ratio is moderate (0.42) because the ECB does engage in climate-risk consultations and employment-impact reviews, but these exercises do not change policy outputs; they are performative compliance with the letter of secondary-objective requirements while maintaining the orthodox spirit. The measurement series shows extractiveness and suppression rising gradually through the interval (as climate risks accumulate and unemployment effects compound) while theater ratio plateaus (the ceremonial engagement settles into steady-state performance). Accessibility_collapse is high (0.78) because once the orthodox reading is established institutional practice, alternative readings become legally and politically inaccessible to most actors — the ECB's interpretation is self-enforcing through precedent. Resistance is moderate (0.62) because member states, the European Parliament, and climate advocates all mount real resistance to the orthodox reading, but the ECB's independence (by design) insulates it from that resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB Governing Council's seat, the orthodox reading is faithful execution of the Treaty text and essential defense of central bank independence from political capture. From the unemployment-constrained seat, it appears as subordination of real economic hardship to an inflation target that is already met. From the climate-risk-externalized seat, it is institutional negligence of a civilizational problem. The engine computes these divergences from the structural data: different power levels (institutional vs. organized/powerless), different exit options (constrained vs. trapped), and opposed beneficiary/victim classifications all produce different directionality values and per-seat type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor savers have low d (near 0.2) because they are full beneficiaries with exit via arbitrage — the constraint subsidizes their position. Unemployment-constrained workers have high d (near 0.85) because they are victims with trapped exit — they cannot arbitrage out of the eurozone labor market and bear the full cost of restrictive policy bias. Climate-risk-externalized parties have d near 0.9 (full targets with zero exit) because they are entirely passive victims with no voice in the constraint's operation. The ECB Governing Council itself has d near 0.5 (symmetric, if we view them as operators of the constraint rather than beneficiaries of it — their 'benefit' is institutional autonomy, their 'cost' is the delegation of mandate interpretation without direct political mandate). This symmetric directionality is the engine's way of marking the agenda-setter role; agenda-setters appear neither as pure beneficiaries nor pure targets. The exclusion of expansionist member states gives them an ambiguous d because they are barred from the constraint's operation entirely — they are not beneficiaries or victims within it, but excluded parties who would be high-d payers if they were inside.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodox reading exhibits early-stage mandatrophy signals: the founding problem (inflation stability) has been substantially solved in the eurozone for 25+ years, yet the constraint's operation is intensifying (extractiveness rising, suppression holding steady). The theater ratio is telling: the ECB's expanding engagement with secondary objectives without operational effect is a classic mandatrophy symptom. However, this is NOT yet full piton status because the constraint retains its original function (anchoring inflation expectations) and creditor beneficiaries still collect substantial rents. The classification as tangled_rope (not piton) reflects that the constraint continues to coordinate a real good (price stability) while extracting asymmetric costs. If extractiveness continues rising and theater ratio continues plateauing over the next 15-year interval, a reclassification to piton would be warranted. The R5 founding_problem_status='contested' is the diagnostic fulcrum: the benefiting parties (ECB, creditors, stability advocates) assert the founding problem is eternal; the cost-bearing parties assert it is solved and the constraint now extracts without genuine function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_indeterminacy_without_prejudice,
    'Does ''without prejudice to price stability'' grammatically mean (a) secondary objectives remain dormant unless price stability is already secured, or (b) secondary objectives are live operational goals so long as they do not contradict price stability?',
    'Textual analysis by external constitutional scholars (not ECB-appointed) comparing EU language usage across treaties; linguistic analysis of original language versions (German, French, Italian) for intent signals. EU Court of Justice preliminary ruling on mandate interpretation.',
    'Interpretation (a) supports the orthodox reading and justifies suppression of secondary objectives; interpretation (b) supports the expansive reading and opens operational space for employment/climate weight. This is a conceptual/textual ambiguity, not empirically resolvable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_indeterminacy_without_prejudice, conceptual, 'Core semantic ambiguity of the Treaty language itself.').

omega_variable(
    independence_vs_legitimacy_tradeoff,
    'How much operational weight on secondary objectives can the ECB adopt before its policy independence is undermined by member state influence, versus how much suppression of secondary objectives undermines democratic legitimacy?',
    'Empirical: track member state pressure for mandate expansion and monitor whether ECB autonomy erodes as secondary objectives become operative. Comparative: study other central banks (Fed dual mandate, Bank of England) for evidence of independence loss under broader mandates.',
    'If independence is found robust under secondary-objective weight (as Fed evidence suggests), the orthodox reading''s independence defense becomes less compelling; if independence is fragile, the orthodox reading is vindicated. This shapes the legitimacy of both readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_vs_legitimacy_tradeoff, empirical, 'Whether central bank independence requires mandate narrowness.').

omega_variable(
    climate_risk_materiality_for_price_stability,
    'Is climate transition risk material to price stability itself, such that the ECB''s primary mandate REQUIRES considering climate in asset purchases and collateral frameworks?',
    'Empirical: climate scenario analysis showing inflation or financial-stability risks from unmanaged climate transition; stress testing and asset-price modeling incorporating climate pathways. Assessment whether climate risk is endogenous to price stability or external to it.',
    'If climate risk is found material to primary price-stability objective, the climate-incorporation reading gains structural ground and the orthodox reading''s externalization of climate becomes untenable. If climate is found external to price stability, the readings remain structurally distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_risk_materiality_for_price_stability, empirical, 'Whether climate transition risk is internal or external to the price-stability mandate.').

omega_variable(
    suppression_mechanism_institutional_vs_internalized,
    'Is the suppression of secondary objectives maintained by institutional structures (ECB governance rules, member state contracts) or by internalized beliefs among ECB staff about proper central banking?',
    'Organizational ethnography: survey ECB staff attitudes toward secondary objectives and compare stated reasons (institutional rules vs. professional dogma). Track whether governance rule changes (e.g., explicit secondary-objective directives from member states) alter practice.',
    'If suppression is primarily institutional, mandate revision via treaty amendment could alter the constraint rapidly. If suppression is internalized (staff believe secondary objectives are improper), revision would face resistance even with new rules. Affects the cost of fixing the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_internalized, empirical, 'Whether suppression is structural or cognitive.').

omega_variable(
    kernel_reading_kernel_contest_this_is_a_kernel_reading,
    'This constraint is ONE reading of a contested kernel (Article 127 TFEU). The kernel is fixed; the readings differ in how they interpret ''without prejudice'' and which subsequent obligations bind the ECB. Are the three readings (orthodox, expansive, climate) exhaustive, or are other structurally distinct readings possible?',
    'Systematic review of all major ECB policy statements, EU institutional positions, and legal scholarship to identify whether any reading exists that is not captured by these three or their logical combinations.',
    'If the three readings are exhaustive, the kernel contest is bounded and futures can be modeled across three branches. If other readings exist, the contestation space is larger and unpredictability is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_kernel_contest_this_is_a_kernel_reading, conceptual, 'Completeness of the kernel reading set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(ecb__tr_t0, observed).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(ecb__tr_t5, observed).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(ecb__tr_t10, observed).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t15, observed).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t20, observed).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(ecb__be_t0, observed).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(ecb__be_t5, observed).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(ecb__be_t10, observed).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(ecb__be_t15, observed).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ecb__be_t20, observed).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ecb__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(ecb__su_t0, observed).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(ecb__su_t5, observed).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(ecb__su_t10, observed).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ecb__su_t15, observed).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ecb__su_t20, observed).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(ecb__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel: ECB Mandate Article 127 TFEU. All three readings share the same kernel text but interpret the 'without prejudice' clause and the scope of secondary objectives differently. The orthodox reading (this story) interprets secondary objectives as dormant unless price stability is already secured, narrowing beneficiaries to creditors/savers and externalizing employment and climate risks. The expansive reading permits operational weight on secondary objectives when price stability is not under threat, broadening coordination function. The climate reading integrates Article 11 TFEU and treats climate risk as an operational secondary objective. All three are live readings held by different institutional constituencies. This story models the orthodox reading's structure, metrics, and beneficiary/victim configuration independently; the sibling stories model the others independently. Network links enable contamination analysis: if the orthodox reading's legitimacy erodes (e.g., via climate risk materialization), pressure propagates to siblings. Constraint family identity: ecb_mandate_article_127.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
