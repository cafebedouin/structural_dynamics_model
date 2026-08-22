% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary — Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the SOVEREIGNTY PRIMACY READING of the
 *   contested NAFTA jurisdictional boundary kernel. The reading asserts that
 *   trade agreement text (the kernel) operates as a coordination mechanism
 *   for market access, NOT as a supreme law overriding domestic regulatory
 *   authority. Under this reading, states retain full jurisdiction to set
 *   labor, environmental, and health standards within their territory; trade
 *   obligations constrain HOW those standards are applied (non-discriminatory
 *   manner, no disguised protectionism) but not WHETHER they can be set. The
 *   constraint is low-extraction (0.18), low-suppression (0.12), and
 *   minimal-theater (0.08) under this reading because the binding structural
 *   relationship is coordination with side constraints, not extraction. The
 *   same treaty text, read under the CAPITAL SUPREMACY READING (a sibling
 *   constraint), would instantiate a much higher-extraction constraint in
 *   which trade panels gain authority to strike down domestic standards as
 *   violations of capital-mobility obligations. This story instantiates only
 *   the sovereignty-primacy reading; the capital-supremacy reading is a
 *   separate constraint authored under a different file.
 *
 * KEY AGENTS:
 *   - domestic_regulatory_agencies: Institutional agenda-setter (analytical spatial scope, set the regulatory standards within their territory)
 *   - state_legislatures: Institutional beneficiary (preserve statutory authority to enact protections)
 *   - labor_unions: Organized beneficiary (constrained exit, advocate for protections through domestic process)
 *   - environmental_ngos: Organized beneficiary (constrained exit, advocate for environmental standards domestically)
 *   - multinational_enterprises: Powerful payer (mobile exit, bear compliance costs across jurisdictions)
 *   - trading_partners: Institutional excluded (would prefer capital-supremacy reading)
 *   - trade_dispute_panels: Institutional observer (analytical seat, interpret the boundary)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '6a21daef-29da-44b8-a68f-70af1735bae8').
narrative_ontology:cs_kernel_codification('6a21daef-29da-44b8-a68f-70af1735bae8', fixed_text).
narrative_ontology:cs_authority_grounding('6a21daef-29da-44b8-a68f-70af1735bae8', lineage).
narrative_ontology:cs_interpretation_layer_present('6a21daef-29da-44b8-a68f-70af1735bae8').
narrative_ontology:cs_reading_relation('6a21daef-29da-44b8-a68f-70af1735bae8', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6a21daef-29da-44b8-a68f-70af1735bae8', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('6a21daef-29da-44b8-a68f-70af1735bae8', foundational, regulatory_authority_domestic_retained).
narrative_ontology:cs_axiom_status(regulatory_authority_domestic_retained, holdable).
narrative_ontology:cs_axiom_grounding('6a21daef-29da-44b8-a68f-70af1735bae8', regulatory_authority_domestic_retained, deontological).
narrative_ontology:cs_axiom('6a21daef-29da-44b8-a68f-70af1735bae8', foundational, trade_obligations_constrain_application_not_legitimacy).
narrative_ontology:cs_axiom_status(trade_obligations_constrain_application_not_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6a21daef-29da-44b8-a68f-70af1735bae8', trade_obligations_constrain_application_not_legitimacy, conventional).
narrative_ontology:cs_reference_frame('6a21daef-29da-44b8-a68f-70af1735bae8', domestic_regulatory_sovereignty_preserved).
narrative_ontology:cs_drift_state('6a21daef-29da-44b8-a68f-70af1735bae8', contemporary_trade_politics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a21daef-29da-44b8-a68f-70af1735bae8', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_ngos).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full statutory authority to set labor, environmental, and health standards within their territory. Under this reading, trade agreement obligations create compliance costs and market-access constraints, but do not override or preempt domestic regulatory jurisdiction. Agencies defend standards through domestic legal processes; treaty text frames these efforts as compatible with trade obligations, not subordinate to them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Retain the statutory power to enact labor, environmental, and health protections without treaty override. Legislation stands on its own jurisdictional footing; compliance with trade obligations is a constraint on how those standards are enforced (non-discriminatory application, no disguised protectionism), not on whether they can be set at all. The boundary reading protects legislative supremacy in the domestic regulatory domain.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_legislatures, beneficiary,
    institutional, generational, analytical, national).

% Advocate for robust labor standards and enforcement. Under this reading, domestic labor law is not subordinated to treaty capital-mobility provisions; unions can pursue stronger standards legislatively. Their power is constrained by the need to avoid facially discriminatory design, but the reading preserves the right to demand protection through democratic process.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions, beneficiary,
    organized, generational, constrained, national).

% Work to establish environmental protections through domestic legislation and enforcement. This reading affirms that environmental standards are not trumped by trade obligations; they coexist with trade agreements on equal legal footing. Extraction costs are the market-access limitations and compliance complexity, not jurisdictional override.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_ngos, beneficiary,
    organized, generational, constrained, national).

% Face differential labor, environmental, and health compliance obligations across jurisdictions. Under this reading, they cannot challenge domestic regulatory standards via trade dispute as illegal per se; instead, they must comply or absorb cost. Exit options (relocating production, shifting supply chains) remain available but are market choices, not legal entitlements. The reading limits extraction to voluntary compliance costs, not to regulatory harmonization pressure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_enterprises, payer,
    powerful, biographical, mobile, global).

% Would prefer that the reading of trade agreements emphasized capital mobility and regulatory harmonization as treaty obligations. Under this reading, their ability to challenge domestic standards through trade dispute is limited to discrimination claims, not to challenging the standard's validity on its merits. They remain bound by the agreement but lack the supremacy lever the capital-supremacy reading would grant them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trading_partners, excluded,
    institutional, generational, trapped, global).

% Interpret and apply the trade agreement text. Under this reading, their authority is limited to evaluating whether a regulation is applied in a discriminatory manner or is a disguised trade restriction; they lack authority to strike down a standard as violating capital-mobility or harmonization obligations. Their interpretive scope is narrower than under the capital-supremacy reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, observer,
    institutional, generational, analytical, global).

% Authored the text that both the sovereignty-primacy and capital-supremacy readings claim to interpret. The text itself is ambiguous on the jurisdictional boundary; different readings extract different meanings from the same words. Under this reading, the drafters' intent is read as preserving domestic regulatory authority; under the capital-supremacy reading, the intent is read as subordinating it. The kernel is the text; the readings are the contest.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, treaty_drafters, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, treaty_drafters, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for mutually recognized market access across trading partners: traders in one jurisdiction know they can reach markets in others without total regulatory transformation; regulatory agencies know they must apply standards in a non-discriminatory manner; investment can flow across borders under predictable (though not harmonized) rules. The coordination solves the collective-action problem of bilateral market fragmentation without requiring harmonization.
% TRANSFER_FUNCTION: Moves compliance-cost burdens to multinational enterprises (who must meet multiple jurisdictional standards) and to domestic regulators (who must justify their standards as non-discriminatory rather than capturing them as domestic policy tools). The treaty does not, under this reading, transfer wealth from one seat to another; instead, it constrains how domestic authority can be exercised. The flow is constraint on exercise, not extraction.
% ABSENT_VOICES: Capital-mobility advocates and multinational enterprises that profit from regulatory arbitrage are not excluded from the conversation (they sit as payers), but they argue for a different reading. The sovereignty-primacy reading is contested by those who would prefer the capital-supremacy reading, which would grant trade panels supremacy over domestic standards. That contestation is live in current trade politics; the reading reflects one pole of an ongoing dispute.
% DISAPPEARANCE_RATIONALE: If this reading vanished (i.e., if the capital-supremacy reading became the binding interpretation), multinational enterprises would gain the ability to challenge domestic labor and environmental standards through trade dispute as violations of capital-mobility obligations. Domestic regulatory agencies would face a new enforcement burden: not just applying standards evenly, but defending their standards' legitimacy against external challenge. Labor unions and environmental advocates would lose a legal protection — the right to demand standards without proving economic necessity. The world would not rearrange itself overnight, but the locus of regulatory authority would shift from domestic legislatures to trade panels. Conversely, if a stricter sovereignty reading prevailed (subordinating trade agreements to domestic law entirely), trading partners would lose market-access guarantees. The contested verdict reflects genuine structural disagreement about what would follow from dominance of either reading.
% FOUNDING_PROBLEM: Cross-border commerce created disputes between trading partners: merchants in one jurisdiction wanted predictable access to others' markets without conforming to radically different standards; regulators in each jurisdiction wanted to maintain domestic authority to protect labor, environment, and health. The agreement was built to solve the collective-action problem of bilateral coordination without requiring sovereignty surrender.
% FOUNDING_PROBLEM_CORROBORATION: Trade negotiators from the era (1990s NAFTA negotiation) attested both aims: market access and sovereignty preservation. Subsequent dispute record shows traders arguing the founding problem now demands regulatory harmonization (capital-supremacy reading), while labor and environmental advocates argue it demands regulatory space preservation (sovereignty-primacy and embedded-liberalism readings). The historical record does not close the contest; multiple coherent readings coexist.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the sovereignty-primacy reading, extractiveness is LOW (0.18 end-of-interval) because the constraint does not extract from the payers (multinational enterprises); it merely imposes compliance costs that are voluntary market-participation costs rather than extractive rents. The enterprises can choose to comply with standards, absorb the cost and exit markets where standards are onerous, or litigate on grounds of discrimination — but they cannot challenge the standard's legitimacy via trade dispute. Suppression is MINIMAL (0.12) because the constraint does not require sustained coercion: it operates through the normal jurisdictional separation of powers, not through active enforcement of a rule against resistance. Theater is MINIMAL (0.08) because the constraint's function (coordinate market access while preserving regulatory space) is genuine and operationalized; the rule does what it claims to do. The measurement trajectory is slightly rising (extractiveness from 0.12 to 0.18 over 35 years) because multinational enterprises increasingly operate in higher-standard jurisdictions and bear mounting compliance costs, but the rise is slow and plateaus, suggesting the constraint stabilizes at this low-extraction level. This reading claims ROPE type: genuine coordination of market access with side constraints on how that coordination is achieved, no asymmetric extraction, no systematic victimhood.
 *
 * PERSPECTIVAL GAP:
 *   The multinational-enterprise seat and the domestic-regulatory-agency seat should compute differently. From the enterprise perspective, the constraint limits market access (they cannot operate in high-cost jurisdictions without high compliance expense); from the regulatory perspective, the constraint ENABLES authority (they can set standards, provided they apply them non-discriminatorily). The engine computes directionality from beneficiary/victim data: enterprises are listed as payers (bearing compliance costs), agencies as beneficiaries (retaining jurisdiction). The divergence in computed type — if the enterprise seat computes to snare while the regulatory seat computes to rope — reflects genuine structural asymmetry: one seat experiences extraction (constrained mobility), the other experiences empowerment (retained authority). This is not a defect; it is the core signal the per-seat classification produces.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational enterprises are PAYERS under this reading: they bear compliance-cost burdens (listed in victims array per schema, though the reading claims low extraction). Their directionality is moderately HIGH (d ~ 0.55–0.65) because they face real constraints on where and how they operate, and they have constrained exit (cannot simply ignore standards in major markets). Domestic regulatory agencies are BENEFICIARIES: they retain jurisdictional authority and can deploy it (listed in beneficiaries array). Their directionality is LOW (d ~ 0.15–0.25) because the constraint empowers rather than constrains them. Labor unions and environmental NGOs are also BENEFICIARIES (constrained exit, organized power): they can advocate for stronger standards domestically. Their directionality is MODERATE (d ~ 0.35–0.45): they benefit from the jurisdictional preservation but face resistance from capital-mobility advocates. This reading does not list trading partners as victims because under the sovereignty-primacy reading they retain the right to regulate; they are excluded (would object if present) not victimized. The directionality logic supports a ROPE classification from the agency seat (low extraction, genuine coordination) and a mixed/TANGLED-ROPE classification from the enterprise seat (side constraints that impose real costs, but not pure extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-border commerce coordination without sovereignty surrender) remains LIVE under this reading. The constraint continues to solve that problem: it establishes market-access rules while preserving regulatory authority. There is no mandatrophy — the arrangement has not outlived its function. The contestation is not between a live function and a dead one, but between TWO READINGS of what the function means. The capital-supremacy reading would argue mandatrophy HAS occurred (the founding problem is now solved, so regulatory subordination should follow); the sovereignty-primacy reading argues the founding problem is still live (markets still require coordination, sovereignty still needs preservation). This reading's claim of ROPE type rests on the function remaining live and the coordination remaining genuine. If the capital-supremacy reading empirically dominated (trade panels regularly striking down domestic standards), the sovereignty-primacy reading's classification would drift toward PITON (theater mounting as the constraint persists despite its stated function being overridden by another reading). But as of the measurement interval, the reading remains internally coherent: low extraction, minimal suppression, genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_ambiguity_jurisdictional_boundary,
    'Does the NAFTA text (Article 104, regulatory provisions) logically entail supremacy of trade obligations over domestic standards, or does it permit domestic regulatory authority to coexist with trade obligations?',
    'Textual analysis comparing the regulatory language to explicit supremacy clauses in other treaties; historical negotiation records; subsequent case law from dispute panels under this and other trade agreements.',
    'If the text logically entails supremacy, the capital-supremacy reading is the correct interpretation and this sovereignty-primacy reading misreads the kernel. If the text is ambiguous or compatible with coexistence, both readings are defensible interpretations of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_ambiguity_jurisdictional_boundary, conceptual, 'Textual meaning of the treaty''s jurisdictional boundary.').

omega_variable(
    extraction_framing_under_sovereignty_reading,
    'Are compliance costs borne by multinational enterprises properly characterized as extraction (under this sovereignty-primacy reading), or are they market-participation costs that do not constitute extraction?',
    'Comparative analysis of exit options and profitability data: if enterprises earn positive returns even after compliance costs in all markets, the cost is a market constraint, not extraction. If compliance costs in certain jurisdictions exceed enterprise profitability, the constraint extracts surplus.',
    'If compliance costs are pure market constraints, extractiveness remains LOW (0.18) and the rope classification holds. If they exceed profitability, extractiveness rises to MODERATE (0.35–0.50) and the classification drifts toward tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_framing_under_sovereignty_reading, empirical, 'Whether compliance-cost burden constitutes extraction or market participation.').

omega_variable(
    liveliness_of_founding_problem,
    'Is the founding problem (coordination without sovereignty surrender) genuinely live, or has the problem been partially solved by other mechanisms (bilateral trade agreements, WTO disciplines, investor-state dispute settlement) that make the NAFTA jurisdictional boundary less salient?',
    'Comparison of dispute-settlement frequency and outcomes over time; analysis of whether states continue to defend new regulatory standards as compatible with trade obligations; measurement of multinational reliance on the NAFTA''s market-access rules versus alternative mechanisms.',
    'If the founding problem is losing salience, the constraint''s function is atrophying and mandatrophy risk rises (classification could drift from rope toward piton). If the problem remains live and actively generating dispute, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liveliness_of_founding_problem, empirical, 'Whether the constraint''s original coordination function remains vital or is being superseded by other institutional mechanisms.').

omega_variable(
    sovereignty_primacy_vs_capital_supremacy_contest,
    'What empirical or structural observation would decisively favor the sovereignty-primacy reading over the capital-supremacy reading, or vice versa?',
    'Track the composition and rulings of NAFTA/USMCA dispute panels; observe whether panels strike down domestic standards as trade violations, or limit challenges to discrimination claims; monitor trade negotiation priorities and legislative action in signatory states.',
    'If dispute panels consistently strike down domestic standards, the capital-supremacy reading is the operative constraint despite this story''s claim. The sovereignty-primacy reading would then be a rhetorical frame, not a structural reality, and this story would need reclassification to piton (theater masking capital supremacy). If panels respect domestic regulatory authority, the sovereignty-primacy reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primacy_vs_capital_supremacy_contest, empirical, 'Which reading of the jurisdictional boundary kernel is operationally dominant in dispute resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(naft_tr_t5, observed).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(naft_tr_t10, observed).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(naft_tr_t15, observed).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(naft_tr_t20, observed).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(naft_tr_t25, observed).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(naft_tr_t30, observed).
narrative_ontology:measurement(naft_tr_t35, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 35, 0.08).
narrative_ontology:measurement_basis(naft_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(naft_be_t5, observed).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement_basis(naft_be_t10, observed).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(naft_be_t15, observed).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(naft_be_t20, observed).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(naft_be_t25, observed).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(naft_be_t30, observed).
narrative_ontology:measurement(naft_be_t35, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 35, 0.18).
narrative_ontology:measurement_basis(naft_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(naft_su_t5, observed).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(naft_su_t10, observed).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(naft_su_t15, observed).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(naft_su_t20, observed).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(naft_su_t25, observed).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(naft_su_t30, observed).
narrative_ontology:measurement(naft_su_t35, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 35, 0.12).
narrative_ontology:measurement_basis(naft_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% The NAFTA jurisdictional boundary kernel admits multiple readings that instantiate structurally distinct constraints. This story (sovereignty_primacy_reading) asserts that trade obligations are subordinate to domestic regulatory authority; the capital_supremacy_reading asserts the opposite; the embedded_liberalism_reading claims a balanced coexistence. All three stories interpret the same treaty text (the kernel) but extract different constraints from it. The readings coexist as live positions in contemporary trade politics. Authors decomposing this kernel should author one constraint per reading, with each reading's own metrics, stakeholders, and type claim. The network.affects_constraints array links all three readings, signaling that they are interdependent interpretations of a single kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
