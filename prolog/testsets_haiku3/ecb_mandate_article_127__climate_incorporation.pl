% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Article 127 TFEU Climate Risk Integration Mandate
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint instantiates the climate-incorporation reading of the
 *   contested Article 127 TFEU ECB mandate kernel. The ECB's monetary
 *   operations have gradually integrated climate transition risk into
 *   collateral frameworks, asset purchase criteria, and stress-testing models
 *   under the interpretation that Article 127's environmental integration
 *   clause (read together with Article 11 TFEU) permits—or requires—climate
 *   risk pricing as a financial-stability concern. This reading benefits
 *   climate-aligned institutions and beneficiaries while extracting from
 *   fossil-fuel-reliant borrowers through collateral haircuts and refinancing
 *   cost increases. The reading is actively defended by ECB technical staff
 *   and supported by EU climate law and ECJ jurisprudence, but is contested
 *   by orthodox monetary economists and some member states who read the
 *   mandate as price-stability-exclusive. The constraint operates as tangled
 *   rope: genuine coordination function (pricing transition risk uniformly
 *   across eurozone assets, aligning monetary and climate policy), asymmetric
 *   extraction (fossil fuel exposure penalized, transition aligned
 *   beneficiaries supported), and active enforcement (portfolio tilting,
 *   haircut adjustments, collateral eligibility review).
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda-setter (institutional/analytical), defines climate incorporation scope and enforces via collateral frameworks
 *   - fossil_fuel_reliant_borrowers: victims (powerful/constrained), face collateral haircuts and refinancing cost increases
 *   - climate_transition_beneficiaries: primary beneficiaries (organized/mobile), gain from favorable collateral treatment and asset purchase support
 *   - member_state_governments: dual-positioned (institutional/constrained), benefit from climate alignment but face extraction on fossil-heavy sovereign debt
 *   - conventional_collateral_providers: dual victims/beneficiaries (moderate/constrained), experience haircuts on legacy collateral but gain on transition-aligned holdings
 *   - orthodox_ecb_constituencies: excluded (institutional/trapped), their mandatary objections are absorbed as legitimate concern rather than treated as mandatary disagreement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.59).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Article 127 TFEU Climate Risk Integration Mandate").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '5e4cc5ff-12aa-4e92-bc84-113e7fd1b303').
narrative_ontology:cs_kernel_codification('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', formalized).
narrative_ontology:cs_authority_grounding('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', lineage).
narrative_ontology:cs_interpretation_layer_present('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303').
narrative_ontology:cs_reading_relation('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', foundational, climate_transition_risk_is_material_financial_risk).
narrative_ontology:cs_axiom_status(climate_transition_risk_is_material_financial_risk, holdable).
narrative_ontology:cs_axiom_grounding('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', climate_transition_risk_is_material_financial_risk, empirically_contingent).
narrative_ontology:cs_axiom('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', foundational, article_11_tfeu_environmental_integration_binding_on_ecb).
narrative_ontology:cs_axiom_status(article_11_tfeu_environmental_integration_binding_on_ecb, holdable).
narrative_ontology:cs_axiom_grounding('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', article_11_tfeu_environmental_integration_binding_on_ecb, deontological).
narrative_ontology:cs_reference_frame('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', environmental_integration_expansive_mandate).
narrative_ontology:cs_drift_state('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', contemporary_climate_crisis_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5e4cc5ff-12aa-4e92-bc84-113e7fd1b303', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_beneficiaries).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_alignment).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_reliant_borrowers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, conventional_collateral_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, conventional_collateral_providers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, member_state_governments).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, member_state_governments).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, environmental_integration_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, central_bank_climate_mandate_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU and decides whether climate risk constitutes a material monetary policy consideration triggering integration requirements. Sets collateral valuation frameworks, haircut schedules, and asset purchase criteria. Justifies climate incorporation as managing financial stability risks and supporting EU policy coherence. Bears the controversy around mandate scope.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Face higher collateral haircuts, higher refinancing costs, and reduced eligibility for ECB asset purchases when their balance sheets carry climate transition risk (carbon liabilities, stranded assets, sector exposure). Their exit options are constrained by EU jurisdiction and the ECB's role as monopoly lender of last resort. They argue climate haircuts exceed financial-risk justification and constitute industrial policy masked as monetary policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_reliant_borrowers, payer,
    powerful, biographical, constrained, continental).

% Include EU climate policy advocates, renewable energy developers, green finance intermediaries, and member-state governments with climate commitments. Benefit from ECB collateral framework that prices climate risk, making transition-aligned borrowers cheaper to refinance and transition-exposed borrowers more expensive. Their leverage rises as climate policy consensus tightens; they can exit by exiting the monetary union if ECB does not align.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_beneficiaries, beneficiary,
    organized, generational, mobile, continental).

% Banks, insurers, and other financial institutions that hold legacy collateral bundles (sovereign debt, mortgages, corporate bonds with embedded climate exposure). They face haircuts on their collateral holdings but also see their transition-aligned assets gain value. The net position varies by institution; poorly positioned institutions face collateral crises if haircuts widen.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, conventional_collateral_providers, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, conventional_collateral_providers, beneficiary).

% Are bound by EU climate law (Green Deal, taxonomy regulations) and benefit from ECB alignment with climate objectives (cheaper refinancing for green bonds, higher costs for high-emitting sectors). Member states with fossil fuel-dependent economies face extraction via higher sovereign borrowing costs if their collateral or debt profile carries climate risk. Their exit option is political resistance in EU governance forums.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, member_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, member_state_governments, payer).

% Economists, policymakers, and governing council members who read Article 127 as exclusive price-stability mandate and view climate incorporation as mandate creep would argue for narrower interpretation. They are structurally excluded from the climate-incorporation reading's beneficiary set and face institutional pressure to accept the expanded mandate. Their objections are absorbed in governance as 'legitimate prudential concern' rather than treated as mandatary disagreement.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_ecb_constituencies, excluded,
    institutional, biographical, trapped, continental).

% The EU's climate policy objectives (consolidated in the Green Deal, taxonomy, carbon border adjustment) are vindicated by ECB integration because central bank policy now reinforces rather than contradicts EU law. This is not a party that collects rents; it is a policy framework that gains enforcement through monetary operations.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_institutions_and_member_states_climate_policy, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(ecb_mandate_article_127__climate_incorporation, eu_institutions_and_member_states_climate_policy).

% Global asset managers, pension funds, and hedge funds observe the ECB's collateral framework for signal about European financial risk. Climate haircuts change the relative pricing of eurozone assets and redirect capital flows. They assess whether the ECB's climate incorporation is credible financial risk management or industrial policy; their capital allocation responds accordingly.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, international_financial_market_participants, observer,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, climate_transition_beneficiaries).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns ECB monetary operations with EU climate policy objectives and manages transition-related financial stability risks through unified collateral valuation. Solves the coherence problem: previously, the ECB operated in monetary-stability frame while EU member states and institutions committed to climate transition, creating contradictory asset price signals. Climate-inclusive collateral haircuts price transition risk consistently across the eurozone financial system.
% TRANSFER_FUNCTION: Moves the burden of transition risk pricing from the EU climate-policy constituency (who benefit from faster transition) to fossil-fuel-reliant and transition-exposed borrowers (whose collateral loses value and refinancing costs rise). The constraint redistributes the cost of transition timing from those with long-term climate commitments to those with near-term emissions dependencies.
% ABSENT_VOICES: Fossil fuel sector representatives and member states with high carbon exposure are named in the constraint structure. Excluded voices include economic schools that read Article 127 as price-stability-exclusive mandate and argue climate incorporation exceeds the ECB's legal authority. These objectors are present in some ECB decisions and central bank forums but are structurally excluded from the climate-incorporation reading's justification frame.
% DISAPPEARANCE_RATIONALE: If the mandate requirement disappeared — if the ECB reverted to price-stability-only operations and ceased climate risk integration in collateral frameworks — eurozone asset prices would reorient immediately: fossil fuel collateral would regain value, transition-aligned assets would lose pricing support, and member states' climate-policy implementation would decouple from monetary operations. The constraint actively channels capital toward transition, so removing it would trigger capital reallocation and slow transition implementation across the eurozone.
% FOUNDING_PROBLEM: By ~2019–2022, the ECB faced a coherence crisis: EU member states and institutions were legally binding themselves to climate transition via the Green Deal and related instruments, but the central bank's monetary operations (collateral frameworks, asset purchases, refinancing terms) remained climate-neutral, pricing climate and non-climate risk identically. This created contradictory price signals: the EU's own institutions were financing transition while the ECB priced as though transition risk did not exist. The founding problem is: how can the ECB maintain financial stability and respect EU law coherence if it ignores material risks that EU policy itself recognizes?
% FOUNDING_PROBLEM_CORROBORATION: ECB technical staff and climate-aware governing council members attest the coherence problem is real and material. The European Court of Justice, EU Parliament, and Commission argue climate risk is material to financial stability and therefore within ECB scope. Orthodox monetary economists (outside the ECB but influential in central banking circles) and some ECB members contest the founding problem, arguing climate risk is a policy lever, not a pre-existing financial fact that the ECB merely discovered. The German constitutional court's (failed) challenge to ECB climate-inclusive bond purchases shows the dispute remains live.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint operates via non-transparent portfolio tilting and collateral valuation that redistributes refinancing costs without explicit levying or tariff mechanisms—it is harder to articulate and contest than explicit taxation, making extraction less visible and thus more persistent. Suppression is substantial (0.59) because the constraint's operation depends on suppressing alternative collateral frameworks or mandate interpretations that would price climate risk differently or not at all; the suppression operates via technical standards, ECB governing council consensus, and central bank communication that normalize climate incorporation as inevitable financial risk management rather than framing it as mandatary discretion. Theater is moderate (0.42) because genuine financial-stability risk exists (stranded asset risk, transition pathway risk, credit concentration risk in carbon-intensive sectors are real), but a growing share of the ECB's climate-related operations (portfolio tilting, discriminatory haircuts) serve climate policy aims that extend beyond financial stability's narrower frame. The measurement series tracks extraction rising as climate incorporation deepens (t=0 to t=25, extraction rises 0.48→0.68) and suppression requirement rising as orthodox constituencies push back and the ECB must work harder to maintain consensus on the expanded mandate. Resistance remains high (0.74–0.76) because fossil fuel sector, orthodox economists, and some member states actively contest the interpretation; the constraint persists despite substantial opposition, not because opposition is weak.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB's institutional seat, the constraint is genuine coordination of monetary policy with EU law coherence and financial-stability imperatives. From the orthodox mandate-constrained seat, the same operations constitute mandate creep and illegitimate industrial policy. From the fossil-fuel-reliant borrower's seat, the constraint is extraction via collateral discrimination. From the climate-beneficiary seat, the constraint is finally operationalizing a policy commitment that legal and policy frameworks established but monetary operations contradicted. The engine should compute these as substantially different per-seat types: the agenda-setter computes rope or tangled-rope; the constrained fossil-fuel seat computes snare; the beneficiary seat computes rope. The authored metrics and structural data (beneficiaries, victims, enforcement mechanism, suppression via portfolio tilting) establish these asymmetries without the narrative claiming the type—that divergence is the measurement the corpus is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   ECB governing council directionality trends toward 0.0 (beneficiary): they set the agenda, sustain the framework, and derive enhanced institutional authority from expanded mandate interpretation. Fossil-fuel-reliant borrowers trend toward 1.0 (target): they face collateral discrimination, higher refinancing costs, constrained exit (they cannot leave eurozone banking without massive portfolio restructuring). Climate-transition beneficiaries trend toward 0.0 (beneficiary): they collect pricing support, preferential collateral treatment, and capital flow redirection toward green investment. Member states with high carbon exposure occupy 0.6–0.7 (substantial target): they face higher sovereign refinancing costs but retain exit through political voice (EU governance resistance). Orthodox constituencies occupy the 0.8–0.9 range (target): they are excluded from the beneficiary set and bear institutional pressure to accept the mandate expansion. The directionality derivation flows directly from beneficiary/victim declarations and exit options: beneficiaries (climate-aligned, EU policy constituency) are mobile within EU frame and gain preferential treatment → low d; victims (fossil-fuel-exposed) are constrained within eurozone → high d; excluded orthodox actors (institutional/trapped in ECB governance) experience structural opposition without exit → high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coherence between EU climate law and ECB operations) is contested in status but not in existence: even orthodox mandate-interpreters concede that climate risk is material to financial stability; they only dispute whether the ECB's legal basis extends to pricing it. This prevents simple mandatrophy certification. However, two omega variables probe whether the founding problem has shifted: (1) whether the original coherence problem is solved by climate integration or merely displaced (the constraint coordinates ECB and climate policy but may fragment eurozone financial system along climate-exposure lines), and (2) whether the ECB is genuinely pricing transition risk as financial stability (narrow, technical) or has operationalized climate policy goals (broad, political). If the latter, the mandate has drifted from coherence-solving to policy-implementation, which would be mandatrophy—the founding problem (monetary-climate incoherence) is solved, but the constraint persists for a different reason (climate policy acceleration). The measurement series shows theater rising slowly (0.25→0.42), which is diagnostic of mandatrophy drift but not conclusive—some rise in theater-ratio is expected as technical climate risk incorporation becomes normalized and routine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financial_stability_vs_climate_policy_boundary,
    'Is the ECB genuinely pricing climate transition risk as a financial-stability matter (narrow, technical claim about credit and market risk), or has climate incorporation operationalized climate policy goals that extend beyond financial stability into industrial policy?',
    'Comparative analysis of ECB collateral haircuts and asset purchase criteria: if haircuts correlate with transition-risk financial metrics (stranded asset probability, credit-rating downgrade risk, default correlation), the pricing is financial-stability grounded; if they correlate with EU climate taxonomy classifications or emissions intensity independent of financial-risk metrics, the pricing is policy-driven. Expert review of ECB governing council meeting minutes and technical papers to assess reasoning.',
    'If primarily financial-stability grounded, the constraint is defensible as mandate-compliant risk management and the measured extraction is the price of accurate risk pricing. If primarily policy-driven, the constraint is mandate creep (mandatrophy) and the measured extraction is industrial policy, not monetary operations—classification could shift from tangled_rope (genuine coordination + extraction) toward snare (extraction with coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_stability_vs_climate_policy_boundary, empirical, 'Whether climate incorporation is financial-risk pricing or climate-policy operationalization.').

omega_variable(
    mandate_interpretation_foreclosure,
    'Does the climate-incorporation reading foreclose the orthodox price-stability reading within a single institutional framework (the ECB charter), or do they coexist as live competing interpretations?',
    'Textual analysis of Article 127 TFEU and ECB statute: if environmental integration and price-stability clauses are logically compatible (both can be true in a single mandate), the readings coexist; if one clause logically precludes the other, one reading forecloses the other. ECJ jurisprudence on mandate scope (Weiss, OMT cases, pending climate cases) will supply authoritative resolution.',
    'If the readings coexist, the constraint persists as contested institutional authority, and the measured extraction (0.68) reflects distributional conflict, not settled policy. If climate incorporation forecloses price-stability exclusivity, the constraint transitions toward settled law and the theater-ratio should stabilize or decline as contestation decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_foreclosure, conceptual, 'Whether the climate-incorporation reading logically forecloses the orthodox mandate reading or whether both remain defensible.').

omega_variable(
    eurozone_financial_fragmentation_risk,
    'Does climate-inclusive collateral framework increase financial fragmentation within the eurozone by penalizing member states with high carbon exposure (e.g., Poland, Germany''s industrial base), or does it distribute transition risk evenly as a genuine financial-stability measure?',
    'Time-series analysis of eurozone sovereign spreads and refinancing costs, decomposed by member-state carbon exposure and fossil fuel dependency: if climate-heavy member states experience disproportionate refinancing cost increases beyond what their credit risk metrics justify, the constraint is fragmenting. Central bank stress-test data and cross-border capital flow analysis.',
    'If fragmentation is substantial, the constraint poses systemic risk to eurozone stability and may eventually trigger mandatary override (price stability takes precedence); if fragmentation is modest and within normal financial differentiation, the constraint is stable and the measured extraction (0.68) reflects legitimate risk differentiation. High fragmentation risk could eventually reclassify the constraint as containing seeds of its own collapse (piton-trajectory or time-limited snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurozone_financial_fragmentation_risk, empirical, 'Whether climate-inclusive collateral framework fragments the eurozone or distributes transition risk evenly.').

omega_variable(
    fossil_fuel_sector_exit_velocity,
    'How quickly will fossil-fuel-reliant borrowers (coal, oil companies, heavy industry) exit eurozone refinancing markets in response to collateral haircuts, or will they absorb the costs and remain?',
    'Tracking of fossil-fuel-sector debt issuance patterns, borrowing-cost trends, and credit-migration patterns (switching to non-EU funding, deleveraging, bankruptcy): rapid exit suggests high suppression and strong resistance; slow exit suggests low suppression or acceptance of the extraction. Capital-flow data and credit registry analysis.',
    'Rapid exit would shift the constraint from extraction mechanism toward financial-market reorientation (fossil fuel borrowers voluntarily exit eurozone frame), reducing measured extraction and suppression. Slow exit suggests high acceptance or high exit barriers, sustaining measured extraction. Exit velocity informs whether the constraint is stable or whether financial-system adaptation will eventually unwind it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_fuel_sector_exit_velocity, empirical, 'How quickly fossil-fuel-reliant borrowers adjust to collateral discrimination imposed by climate incorporation.').

omega_variable(
    committer_frame_reading_coexistence,
    'This constraint instantiates the climate-incorporation reading. The sibling readings (orthodox_price_stability, expansive_secondary_objectives) are other constraints in the same kernel. Is the coexistence of these readings within the ECB''s current institutional practice stable, or are structural forces pushing toward foreclosure of one reading by another?',
    'ECB governing council voting patterns and policy consensus tracking: are orthodox mandate-interpreters being gradually outvoted and sidelined (indicating emergent foreclosure), or do they retain veto points and interpretive influence? Trajectory of ECB legal argumentation in European court proceedings and policy papers. Assess whether the kernel itself is stable or eroding.',
    'If the climate-incorporation reading is strengthening relative to orthodox alternatives, the constraint''s persistence is secure and extraction may deepen over time. If orthodox resistance is stiffening or gaining institutional ground, the constraint could face eventual reversal or dilution. Foreclosure is a slow institutional process; this omega tracks whether it is underway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_coexistence, conceptual, 'Whether the climate-incorporation reading and orthodox price-stability reading coexist stably or whether structural forces are driving foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_climate_tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ecb_climate_tr_t0, observed).
narrative_ontology:measurement(ecb_climate_tr_t5, ecb_mandate_article_127__climate_incorporation, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(ecb_climate_tr_t5, observed).
narrative_ontology:measurement(ecb_climate_tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(ecb_climate_tr_t10, observed).
narrative_ontology:measurement(ecb_climate_tr_t15, ecb_mandate_article_127__climate_incorporation, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ecb_climate_tr_t15, observed).
narrative_ontology:measurement(ecb_climate_tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ecb_climate_tr_t20, observed).
narrative_ontology:measurement(ecb_climate_tr_t25, ecb_mandate_article_127__climate_incorporation, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(ecb_climate_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ecb_climate_be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ecb_climate_be_t0, observed).
narrative_ontology:measurement(ecb_climate_be_t5, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(ecb_climate_be_t5, observed).
narrative_ontology:measurement(ecb_climate_be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(ecb_climate_be_t10, observed).
narrative_ontology:measurement(ecb_climate_be_t15, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ecb_climate_be_t15, observed).
narrative_ontology:measurement(ecb_climate_be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ecb_climate_be_t20, observed).
narrative_ontology:measurement(ecb_climate_be_t25, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ecb_climate_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ecb_climate_su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(ecb_climate_su_t0, observed).
narrative_ontology:measurement(ecb_climate_su_t5, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(ecb_climate_su_t5, observed).
narrative_ontology:measurement(ecb_climate_su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(ecb_climate_su_t10, observed).
narrative_ontology:measurement(ecb_climate_su_t15, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(ecb_climate_su_t15, observed).
narrative_ontology:measurement(ecb_climate_su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(ecb_climate_su_t20, observed).
narrative_ontology:measurement(ecb_climate_su_t25, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(ecb_climate_su_t25, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(ecb_climate_grid_01, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(ecb_climate_grid_02, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(class), 25, 0.79).
narrative_ontology:measurement(ecb_climate_grid_03, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(ecb_climate_grid_04, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(individual), 25, 0.71).
narrative_ontology:measurement(ecb_climate_grid_05, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(ecb_climate_grid_06, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(organizational), 25, 0.68).
narrative_ontology:measurement(ecb_climate_grid_07, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(ecb_climate_grid_08, ecb_mandate_article_127__climate_incorporation, accessibility_collapse(structural), 25, 0.76).
narrative_ontology:measurement(ecb_climate_grid_09, ecb_mandate_article_127__climate_incorporation, resistance(class), 0, 0.75).
narrative_ontology:measurement(ecb_climate_grid_10, ecb_mandate_article_127__climate_incorporation, resistance(class), 25, 0.78).
narrative_ontology:measurement(ecb_climate_grid_11, ecb_mandate_article_127__climate_incorporation, resistance(individual), 0, 0.73).
narrative_ontology:measurement(ecb_climate_grid_12, ecb_mandate_article_127__climate_incorporation, resistance(individual), 25, 0.76).
narrative_ontology:measurement(ecb_climate_grid_13, ecb_mandate_article_127__climate_incorporation, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(ecb_climate_grid_14, ecb_mandate_article_127__climate_incorporation, resistance(organizational), 25, 0.74).
narrative_ontology:measurement(ecb_climate_grid_15, ecb_mandate_article_127__climate_incorporation, resistance(structural), 0, 0.68).
narrative_ontology:measurement(ecb_climate_grid_16, ecb_mandate_article_127__climate_incorporation, resistance(structural), 25, 0.71).
narrative_ontology:measurement(ecb_climate_grid_17, ecb_mandate_article_127__climate_incorporation, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(ecb_climate_grid_18, ecb_mandate_article_127__climate_incorporation, stakes_inflation(class), 25, 0.71).
narrative_ontology:measurement(ecb_climate_grid_19, ecb_mandate_article_127__climate_incorporation, stakes_inflation(individual), 0, 0.5).
narrative_ontology:measurement(ecb_climate_grid_20, ecb_mandate_article_127__climate_incorporation, stakes_inflation(individual), 25, 0.59).
narrative_ontology:measurement(ecb_climate_grid_21, ecb_mandate_article_127__climate_incorporation, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(ecb_climate_grid_22, ecb_mandate_article_127__climate_incorporation, stakes_inflation(organizational), 25, 0.62).
narrative_ontology:measurement(ecb_climate_grid_23, ecb_mandate_article_127__climate_incorporation, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(ecb_climate_grid_24, ecb_mandate_article_127__climate_incorporation, stakes_inflation(structural), 25, 0.68).
narrative_ontology:measurement(ecb_climate_grid_25, ecb_mandate_article_127__climate_incorporation, suppression(class), 0, 0.45).
narrative_ontology:measurement(ecb_climate_grid_26, ecb_mandate_article_127__climate_incorporation, suppression(class), 25, 0.64).
narrative_ontology:measurement(ecb_climate_grid_27, ecb_mandate_article_127__climate_incorporation, suppression(individual), 0, 0.42).
narrative_ontology:measurement(ecb_climate_grid_28, ecb_mandate_article_127__climate_incorporation, suppression(individual), 25, 0.55).
narrative_ontology:measurement(ecb_climate_grid_29, ecb_mandate_article_127__climate_incorporation, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(ecb_climate_grid_30, ecb_mandate_article_127__climate_incorporation, suppression(organizational), 25, 0.58).
narrative_ontology:measurement(ecb_climate_grid_31, ecb_mandate_article_127__climate_incorporation, suppression(structural), 0, 0.4).
narrative_ontology:measurement(ecb_climate_grid_32, ecb_mandate_article_127__climate_incorporation, suppression(structural), 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_taxonomy_collateral_standards).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eurozone_sovereign_debt_pricing_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested ECB Article 127 TFEU mandate kernel. The climate-incorporation reading operationalizes climate risk pricing as mandated by environmental integration clauses and financial-stability considerations. Sibling readings (orthodox_price_stability and expansive_secondary_objectives) instantiate competing interpretations of the same kernel with different structural consequences. All three are live positions held by different ECB constituencies; none forecloses the others within current institutional practice. They form a constraint family linked by kernel identity: affects_constraints edges point to the competing readings and to downstream constraints that operationalize each reading (green taxonomy standards implement climate incorporation; sovereign pricing dynamics emerge from all three readings in interaction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
