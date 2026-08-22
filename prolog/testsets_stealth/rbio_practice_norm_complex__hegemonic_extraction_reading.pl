% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order as Frozen Hegemonic Project (Hegemonic Extraction Reading)
 *   domain: international relations / international law / political economy
 *
 * SUMMARY:
 *   Under the hegemonic extraction reading, the rules-based international
 *   order operates as a multilateral coordination apparatus whose amendment
 *   routes are formally open and practically closed: Charter amendment and
 *   Security Council expansion fail at the P5 veto, IFI governance
 *   realignment fails at creditor supermajorities, and institutional
 *   path-dependency closes what the formal rules leave open. Enforcement is
 *   selective — intervention, sanctions, and program conditionality track the
 *   enforcement coalition's interests rather than any rule-consistent trigger
 *   — and crisis lending converts fiscal distress into policy control. The
 *   epsilon referent is the standing arrangement itself, as this reading
 *   assesses it: genuine coordination functions (trade rules, dispute
 *   settlement, crisis liquidity) run through the same structures that
 *   transfer wealth and policy autonomy to transatlantic creditors and P5
 *   governance-holders. Claim and metrics are authored independently: this
 *   reading claims tangled_rope — coordination plus asymmetric extraction
 *   under active enforcement — while the metrics describe the extraction's
 *   magnitude and the theater of its legitimating rhetoric.
 *
 * KEY AGENTS:
 *   - us_and_european_capital: primary beneficiary (institutional/arbitrage) — collects debt service, opened markets, and dollar privileges without administering enforcement
 *   - p5_permanent_members: agenda-setter and positional beneficiary (institutional/arbitrage) — hold the veto that freezes amendment; authorize enforcement selectively
 *   - ifi_management: administering agenda-setter (institutional/constrained) — designs and certifies adjustment programs; answers to creditor supermajorities
 *   - global_south_states: primary payer (moderate/constrained) — bear conditionality and selective enforcement; reform demands fail at the veto gate
 *   - structural_adjustment_populations: deepest payer (powerless/trapped) — absorb austerity with no vote and no migration exit
 *   - emerging_powers_coalition: partially symmetric payer (organized/mobile) — comply selectively while financing counter-institutions
 *   - global_south_civil_society: excluded voice (powerless/trapped) — would contest program design; holds no seat
 *   - international_law_scholars: analytical observer — audit the selectivity record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order as Frozen Hegemonic Project (Hegemonic Extraction Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international relations / international law / political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '01b37b75-ffa1-48cc-8288-e89765c28aff').
narrative_ontology:cs_kernel_codification('01b37b75-ffa1-48cc-8288-e89765c28aff', fixed_text).
narrative_ontology:cs_authority_grounding('01b37b75-ffa1-48cc-8288-e89765c28aff', extraction).
narrative_ontology:cs_interpretation_layer_present('01b37b75-ffa1-48cc-8288-e89765c28aff').
narrative_ontology:cs_reading_relation('01b37b75-ffa1-48cc-8288-e89765c28aff', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('01b37b75-ffa1-48cc-8288-e89765c28aff', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('01b37b75-ffa1-48cc-8288-e89765c28aff', foundational, conditionality_is_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('01b37b75-ffa1-48cc-8288-e89765c28aff', conditionality_is_coerced_contract, empirically_contingent).
narrative_ontology:cs_axiom('01b37b75-ffa1-48cc-8288-e89765c28aff', foundational, enforcement_selectivity_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('01b37b75-ffa1-48cc-8288-e89765c28aff', enforcement_selectivity_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_axiom('01b37b75-ffa1-48cc-8288-e89765c28aff', secondary, unauthorized_intervention_serves_extraction).
narrative_ontology:cs_axiom_status(unauthorized_intervention_serves_extraction, holdable).
narrative_ontology:cs_axiom_grounding('01b37b75-ffa1-48cc-8288-e89765c28aff', unauthorized_intervention_serves_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('01b37b75-ffa1-48cc-8288-e89765c28aff', decolonized_sovereign_equality).
narrative_ontology:cs_drift_state('01b37b75-ffa1-48cc-8288-e89765c28aff', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('01b37b75-ffa1-48cc-8288-e89765c28aff', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, ifi_management).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, emerging_powers_coalition).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, security_council_primacy_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, creditor_supermajority_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transatlantic creditors, portfolio investors, and export firms whose claims are enforced through IFI programs, investor-state arbitration, and sanctions regimes. They collect debt service, newly opened markets, and dollar-centred financing privileges; they do not administer the enforcement machinery day to day and can reallocate capital across jurisdictions when terms turn unfavourable.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Hold Charter vetoes that make the order formally revisable but practically frozen; authorize or block enforcement coalitions case by case; the US and European members additionally hold over-weighted IFI voting shares and leadership conventions. They administer the arrangement and collect positional rents from its frozen state: no amendment passes without their consent, so no amendment threatens their privileges.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, beneficiary).

% IMF and World Bank boards and staff who design adjustment programs, certify compliance, and control the disbursement tap. Careers, institutional budgets, and the program pipeline depend on the apparatus continuing; they answer to creditor supermajorities rather than to program-country populations.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, ifi_management, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, ifi_management, beneficiary).

% Formally equal treaty members who acceded, mostly as colonies or newly independent states, to institutions they did not design at parity. They bear conditionality, asymmetric dispute outcomes, and selective intervention, while their collective reform demands — Council expansion, quota realignment — fail at the veto gate. Exit means forgoing reserve access, market access, and crisis lending; counter-institution building is possible but slow and costly.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    moderate, generational, constrained, global).

% Households, workers, and patients in program countries who absorb austerity — cuts to health, education, food and fuel subsidies, public employment — under budgets their governments accept under creditor pressure. They hold no vote in any IFI board, and the migration exit is gated by the visa regimes of the same creditor states.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, biographical, trapped, global).

% Large under-represented economies (BRICS and allies) whose governance shares lag their economic weight. They comply selectively, accumulate reserves as self-insurance against program conditionality, and finance parallel institutions — development banks, swap networks, settlement channels. Their exit capacity is partial and growing: they still draw on the order's market access and crisis facilities while building alternatives.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, emerging_powers_coalition, payer,
    organized, generational, mobile, global).

% Debt-justice movements, program-country labor unions, and health advocates who would contest program design and Council composition but hold no seat in IFI decision-making or Charter amendment; consultation is advisory at best, and their objections surface only as protest outside the negotiating rooms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_civil_society, excluded,
    powerless, generational, trapped, global).

% International lawyers and TWAIL historians who document the gap between the Charter's formal equality and enforcement practice. They take no enforcement action and collect no rents; their record-keeping is the main external audit of the arrangement's selectivity.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The order coordinates interstate trade, finance, and security intercourse under common rules: market-access commitments, a dispute-settlement apparatus, reserve and crisis-lending facilities, and a standing council for collective security decisions — problems no single state can solve alone.
% TRANSFER_FUNCTION: Moves wealth and policy autonomy from Global South states and their populations to transatlantic creditors and P5 governance-holders: debt service under conditionality, asymmetric market-access terms, reserve-currency and seigniorage privileges, and veto-protected institutional control.
% ABSENT_VOICES: Program-country populations and Global South civil society hold no IFI votes; debtor governments negotiate under creditor supermajority rules; would-be Council reformers sit outside the P5 amendment gate. The unanimity behind 'rules-based' language arises partly because these seats were never in the room where the rules froze.
% DISAPPEARANCE_RATIONALE: Trade, finance, and security intercourse would not stop, but the P5 veto rents, creditor supermajorities, conditionality pipelines, and dollar-centred privileges are arrangements rather than natural facts: their overnight disappearance would force immediate renegotiation of governance shares, program terms, and enforcement authority, and the counter-institutions now under construction would become the default coordination layer.
% FOUNDING_PROBLEM: The interwar collapse — depression, competitive devaluations, preferential trade blocs, and two world wars — created the demand for standing multilateral management of trade, finance, and collective security, designed at Bretton Woods and San Francisco before most of today's member states held seats.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 1944-45 negotiating record itself (most of today's members were absent or colonial), G77 and NIEO declarations attesting that the founding design excluded the Global South, and TWAIL historiography of the order's construction. The order's defenders attest the founding problem is live (crisis management is still needed); Global South coalitions attest that the operative core now defends privilege. The dispute is documented, not self-asserted by beneficiaries alone.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: conditionality transfers, asymmetric terms, and veto rents are large and patterned, though the order also delivers real crisis liquidity and market access that damps epsilon below snare range. Suppression 0.72 is authored as a raw structural property — veto gates, creditor supermajority rules, sanctions capacity, selective authorization — and is not scaled by scope; extractiveness is the engine-scaled quantity. Theater 0.55: 'rules-based' and 'revisable' rhetoric performs universality while amendment routes are closed, so roughly half the order's legitimating activity defends the frozen structure rather than coordinating. Accessibility collapse 0.45: alternatives (default, non-alignment, BRICS institutions) remain visible and partially usable, so the constraint does not fully collapse the option space. Resistance 0.6: the Non-Aligned Movement, G77/NIEO demands, debt repudiations, and BRICS counter-institution building constitute sustained and partially effective resistance — the coalition-power check for structurally weaker seats is live here. Claimed type tangled_rope: a genuine coordination function and asymmetric extraction run through the same structures, held together by active enforcement. Measurements share one seven-point grid (1945-2025) across all three tracked metrics; the 2005 suppression dip marks the Iraq-era cost of overt coercion, not relaxation of the underlying structure; the 2020-2025 extraction plateau tracks counter-institutional exit eroding the extractive margin at the margin while rhetoric intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (P5 members, IFI management) experience the arrangement as the coordination they administer and the revision they legitimately gate; the payer seats (Global South states, adjustment populations) experience the same structures as extraction they cannot amend; capital experiences it as security for claims. From the P5 seat the veto is constitutional prudence; from the program-country seat the identical veto is the mechanism that froze the constitution against them. The engine computes these per-seat divergences from power, exit, and directionality; this file does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   us_and_european_capital sits near the full-beneficiary end: declared beneficiary, collects the transfer, arbitrage-grade exit. p5_permanent_members derive near-beneficiary d: they administer, collect veto rents, and face no amendment threat; their beneficiary role is structural, not incidental. global_south_states derive high target d as declared victims with constrained exit and moderate power. structural_adjustment_populations sit nearest the full-target end: declared victims, trapped exit, powerless. ifi_management is an administering agenda-setter with constrained exit, partially captured toward the structure it runs. emerging_powers_coalition carries the story's one directionality override (organized, d 0.58): their declared payer role would derive a high target d, but their large trade-order benefits and mobile counter-institution exit place them nearer symmetric — the override corrects the derivation for this seat. global_south_civil_society derives high d as excluded victims but holds no enforcement lever; the observer seat is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar-collapse management — is contested rather than dead: crisis lending and trade coordination remain live functions, which is why the misclassification risk runs both ways. Reading the order as a pure snare erases the coordination Global South states genuinely consume; reading it as a rope (the liberal sibling's move) erases the extraction the same structures perform. The tangled_rope claim holds both: the mandate has partially atrophied into privilege defense — theater_ratio rising 0.22 to 0.55 tracks legitimating activity outpacing coordinating activity — but the coordination core has not fully died, so the arrangement is not yet a piton and not a pure snare. The status=contested by verdict=world_rearranges combination is coherent: arrangements demonstrably depend on the order, and the parties dispute whether what depends on it is the founding problem or the rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the hegemonic_extraction_reading of the rbio_practice_norm_complex kernel — what would each sibling reading change structurally, and where exactly does the disagreement sit?',
    'No dataset resolves a framing choice; resolution is explicit authorial commitment. The liberal_institutional_reading would re-author the same standing arrangement with low epsilon (selectivity as a capacity problem, revisability as live consent), keeping the referent fixed; the sovereignty_maximalist_reading would relocate the victim set to every state subjected to externally-authorized intervention and dissolve the capital-beneficiary seat into a sovereignty-violation frame.',
    'Under the liberal sibling the arrangement computes toward rope (coordination-dominant, low extraction); under the sovereignty sibling it computes with a different victim structure and no creditor-capture seat. The disagreement is located at two structural elements: whether enforcement selectivity evidences intent or capacity, and whether the consent baseline is state sovereignty or popular/class welfare. Epsilon here is reading-indexed over a fixed referent and stable within this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: one RBIO practice complex, three readings with different epsilon, beneficiary sets, and consent baselines.').

omega_variable(
    selectivity_intent_vs_emergence,
    'Does enforcement selectivity reveal coordinated extractive intent (this reading''s claim), or is it an emergent byproduct of capacity asymmetries and alliance politics without a coordinating intent?',
    'Process-trace enforcement decisions across the interval: do interventions, sanctions, and program terms track extractive opportunity (resources, markets, strategic assets, creditor exposure) more than need or legal merit? Archival record of P5 and IFI deliberations is the primary evidence base.',
    'If emergent, epsilon falls toward the coordination-cost range and the type drifts toward rope with capture symptoms; if patterned intent is established, the snare boundary is approached and victim-seat directionality rises toward full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_intent_vs_emergence, empirical, 'Whether enforcement selectivity is intent-revealing or emergent from capacity asymmetry.').

omega_variable(
    formal_revisability_is_real,
    'Is the order genuinely un-amendable in practice (P5 veto plus institutional path-dependency closing formal revision), or do live amendment routes exist that this reading discounts?',
    'Audit actual amendment attempts 1945-2025 (Council expansion packages, IMF quota realignments, Charter amendment conferences) and locate each failure point: veto, ratification threshold, or agenda control.',
    'If live routes exist, the frozen-structure claim weakens, theater_ratio should fall, and the constraint reverts to an ordinary contested tangled_rope; if every route fails at the P5 gate, the freeze is confirmed and the revisability rhetoric is itself the theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_revisability_is_real, empirical, 'Whether formal revisability is practically closed or merely costly.').

omega_variable(
    capital_vs_state_rents,
    'Are the primary extraction receipts private transatlantic capital (debt service, opened markets, dollar privileges) or the P5 states themselves (positional veto rents, seigniorage, institutional control)?',
    'Follow the flows: program debt-service incidence between official and private claimholders, dollar reserve and seigniorage incidence, and the distribution of IFI-derived privileges between state treasuries and private creditors.',
    'If capital is the capturer, gain_flow stays with us_and_european_capital and the extraction is creditor-shaped; if state rents dominate, the p5_permanent_members seat is the capturer and the reading''s class framing needs a state-rent correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_vs_state_rents, empirical, 'Which seat captures the extraction: private transatlantic capital or P5 state rents.').

omega_variable(
    kernel_codification_framing,
    'Is the kernel the fixed treaty text (UN Charter plus the Bretton Woods Articles of Agreement), or the distributed ''rules-based order'' rhetorical practice that selectively invokes that text?',
    'Conceptual choice documented here rather than data-resolved: the fixed-text framing treats the Charter as the adjudicating kernel with drift absorbed by Security Council and IFI-board interpretation; the alternative framing treats the kernel as the informal discourse, which is under-specified and has no single adjudicator.',
    'Under the alternative framing, kernel_codification moves from fixed_text to distributed and authority_grounding toward practice/distributed, changing where drift registers (interpretation layer versus the discourse itself) without changing this reading''s epsilon or beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'CS-framing under-determination: fixed treaty text versus distributed rhetorical kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t1945, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t1960, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1975, 0.34).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t1975, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t1990, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2005, 0.52).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t2005, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t2020, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t2020, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_tr_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(rbio_hegemonic_reading_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t1945, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t1960, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1975, 0.63).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t1975, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1990, 0.74).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t1990, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t2005, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t2020, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t2020, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_be_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_hegemonic_reading_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t1945, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t1960, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t1975, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t1990, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t2005, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t2005, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t2020, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t2020, observed).
narrative_ontology:measurement(rbio_hegemonic_reading_su_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(rbio_hegemonic_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the rules-based international order' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the rbio_practice_norm_complex kernel. This file is the hegemonic_extraction_reading (epsilon 0.78; beneficiaries transatlantic capital and P5 governance-holders; victims Global South states and adjustment populations). The liberal_institutional_reading authors the same standing arrangement with low epsilon (selectivity as capacity problem); the sovereignty_maximalist_reading authors a different victim set (all externally-authorized intervention targets) with no capital-beneficiary seat. The upstream liberal reading is the one cited as evidence for the order's legitimacy; this reading and the sovereignty reading are downstream critiques of it. Each file carries its own stable epsilon; they are linked here as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
