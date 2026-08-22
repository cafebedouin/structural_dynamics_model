% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism: Provincial Residual Sovereignty Within Federal Structure
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   Confederation (1867) was framed and ratified as a compact among sovereign
 *   provinces, each retaining residual powers over property, civil rights,
 *   and natural resources under s.92 of the Constitution Act 1867. The
 *   compact_federalism reading instantiates the claim that provinces entered
 *   voluntarily, retain the capacity to negotiate exit (Quebec sovereignty
 *   movements, separation referenda), and that federal authority — though
 *   legitimately established for enumerated purposes — remains conditional on
 *   provincial consent and must negotiate rather than impose on core
 *   provincial interests. This reading contests the federal-subordination
 *   framing and exists in tension with the resource-sovereignty-primacy
 *   reading that treats s.92A as granting absolute provincial control over
 *   natural resources. The constraint measures how extraction accumulates as
 *   the compact's function shifts from coordination of markets to enforcement
 *   of national standards (climate policy, tariff binding, equalization) that
 *   increasingly override provincial choice.
 *
 * KEY AGENTS:
 *   - participating_provinces: Original confederating provinces (Ontario, Quebec, Nova Scotia, New Brunswick) claiming residual sovereignty and veto over boundary changes
 *   - federal_authority: Central government claiming enumerated powers and paramountcy in conflict zones
 *   - provincial_political_leadership: Individual premiers using sovereignty framing for political capital and negotiating leverage
 *   - resource_dependent_provinces: Alberta, Saskatchewan, Newfoundland & Labrador constrained by federal climate policy despite s.92A claims
 *   - excluded_territories: Yukon, NWT, Nunavut with limited self-government and no role in compact negotiation
 *   - constitutional_jurists: Supreme Court interpreting the boundary between federal and provincial authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.58).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism: Provincial Residual Sovereignty Within Federal Structure").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '86fb3fd5-e74a-4218-a4e8-ff14027361fc').
narrative_ontology:cs_kernel_codification('86fb3fd5-e74a-4218-a4e8-ff14027361fc', fixed_text).
narrative_ontology:cs_authority_grounding('86fb3fd5-e74a-4218-a4e8-ff14027361fc', extraction).
narrative_ontology:cs_interpretation_layer_present('86fb3fd5-e74a-4218-a4e8-ff14027361fc').
narrative_ontology:cs_reading_relation('86fb3fd5-e74a-4218-a4e8-ff14027361fc', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('86fb3fd5-e74a-4218-a4e8-ff14027361fc', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('86fb3fd5-e74a-4218-a4e8-ff14027361fc', foundational, confederation_consensual_compact).
narrative_ontology:cs_axiom_status(confederation_consensual_compact, holdable).
narrative_ontology:cs_axiom_grounding('86fb3fd5-e74a-4218-a4e8-ff14027361fc', confederation_consensual_compact, conventional).
narrative_ontology:cs_axiom('86fb3fd5-e74a-4218-a4e8-ff14027361fc', foundational, provincial_residual_sovereignty).
narrative_ontology:cs_axiom_status(provincial_residual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('86fb3fd5-e74a-4218-a4e8-ff14027361fc', provincial_residual_sovereignty, conventional).
narrative_ontology:cs_axiom('86fb3fd5-e74a-4218-a4e8-ff14027361fc', secondary, exit_negotiable_not_forbidden).
narrative_ontology:cs_axiom_status(exit_negotiable_not_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('86fb3fd5-e74a-4218-a4e8-ff14027361fc', exit_negotiable_not_forbidden, conventional).
narrative_ontology:cs_reference_frame('86fb3fd5-e74a-4218-a4e8-ff14027361fc', original_confederation_1867_compact).
narrative_ontology:cs_drift_state('86fb3fd5-e74a-4218-a4e8-ff14027361fc', contemporary_federal_enforcement_infrastructure_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('86fb3fd5-e74a-4218-a4e8-ff14027361fc', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, participating_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_political_leadership).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, excluded_territories).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_extraction_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Original confederating provinces (Ontario, Quebec, Nova Scotia, New Brunswick) retain constitutional powers over property, civil rights, local matters, and natural resources within their territories. They coordinate through federal structures while maintaining capacity to negotiate equalization formulas, climate policy exemptions, and exit terms. Leadership frames this as preserved sovereignty; they resist federal overreach on resource extraction, environmental standards, and social policy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, participating_provinces, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, participating_provinces, agenda_setter).

% Administers the federal constitutional order, manages intergovernmental transfers, negotiates national standards on climate and taxation. Claims enumerated powers under s.91 Constitution Act 1867 plus federal paramountcy in conflict zones. Interprets the compact as granting it authority to set binding national policy within its jurisdiction; frames federal authority as conditional on provincial participation in good faith.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Individual provincial premiers and their governments extract political capital from asserting provincial autonomy, demanding exemptions, blocking federal initiatives. They benefit from the ambiguity: can claim sovereignty to their electorates while retaining access to federal transfer payments and coordinated markets. Exit threat (separation threats, withholding cooperation) is a routine negotiating tool.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_political_leadership, beneficiary,
    powerful, biographical, mobile, regional).

% Provinces dependent on fossil fuel extraction or hydropower (Alberta, Saskatchewan, Newfoundland & Labrador) are constrained by federal climate policy, carbon pricing regimes, and investment restrictions while federal authority claims paramountcy over interprovincial trade. They argue s.92A grants them absolute resource sovereignty but face federal overrule on emissions and market access. Their options are litigation, withholding resources, or capitulation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_dependent_provinces, payer,
    moderate, generational, trapped, regional).

% Yukon, Northwest Territories, Nunavut have limited self-government; they cannot participate as provinces in the compact federalism framework and have no constitutional route to exit or renegotiate. They are subject to federal policy and provincial resource extraction decisions made without their direct consent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, excluded_territories, payer,
    powerless, biographical, trapped, regional).

% Oil, gas, and mining corporations operating in resource provinces benefit from provincial authority over extraction (s.92A) which they leverage against federal environmental regulation. They extract rents from the ambiguity between federal climate authority and provincial resource control, using separation threats and provincial governments as shields against federal standards.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_extraction_sector, beneficiary,
    powerful, biographical, mobile, global).

% Climate action advocates, labor organizations, and equity coalitions that span provincial boundaries have limited voice in federalism negotiations; they are excluded from the compact's direct parties. They would argue for binding national standards but lack the veto power of provinces in the current framework.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, citizen_coalitions_cross_provincial, excluded,
    moderate, biographical, constrained, national).

% Supreme Court and appellate judiciary interpret the boundary between federal and provincial authority through constitutional cases. They operate as a referee in disputes over paramountcy but do not directly set policy. Their rulings can shift the effective boundary without changing the written text.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_jurists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, federal_authority).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a continental market with unified currency, common tariff against external trade, shared defense and foreign policy, and interprovincial trade without tariff. Solves the fragmentation problem that would re-emerge if confederation dissolved — tariff wars, duplicate regulation, capital flight, military vulnerability to external pressure.
% TRANSFER_FUNCTION: Moves tax revenue from high-income provinces (Ontario) and resource-rich provinces (Alberta) to lower-income provinces through equalization transfers. Also moves regulatory authority over climate, emissions, trade policy FROM provincial capitals TO federal level, which resource provinces experience as extraction of their resource rents and extraction of their autonomous authority over land use and extraction policy.
% ABSENT_VOICES: Excluded territories (Yukon, NWT, Nunavut, former Rupert's Land) have no voice in the original compact and no constitutional route to negotiate exit or renegotiate terms. Indigenous nations historically sovereign before confederation are structurally absent. Cross-provincial civil society coalitions (climate, labor, equity) have no veto role in federation negotiations — they are excluded from the compact's party set and thus have no credible exit threat to enforce their preferences.
% DISAPPEARANCE_RATIONALE: If confederation dissolved overnight, thirteen separate jurisdictions would emerge with no unified tariff, currency, or defense. Continental market integration would fragment into regional trading blocs; the Canadian dollar would disappear or fragment; defense capacity would splinter. Decades of renegotiation would follow to reconstruct even partial integration — if it occurred at all. The market, financial, and security arrangements that confederation enables would require wholesale reconstruction.
% FOUNDING_PROBLEM: Post-Confederation (1867-1900s): Four separate British North American colonies faced fragmentation pressure — duplicate government costs, tariff wars between colonies, U.S. trade pressure, no unified military or diplomatic voice. Confederation solved this by creating a single market, single tariff, unified currency, and common defense. Quebec required protection for its civil law and Catholic education. The compact balanced these by giving federal authority over trade/defense and provincial authority over local matters and property.
% FOUNDING_PROBLEM_CORROBORATION: Historical attestation from confederation debates (MacDonald, Cartier, George-Brown papers) confirms the founding problem. Contemporary attestation from federal authorities argues the problem remains live — internal market fragmentation would re-emerge if confederation dissolved. Contemporary contestation from resource-dependent provinces argues the founding problem is solved and the constraint now persists as rent extraction. Constitutional scholars (Courchene, Hogg, Woolf, Arend) outside the benefiting parties attest to the problem's historical reality but diverge on whether it remains live in 2026. This omega documents the ambiguity: whether the constraint still solves the problem it was built for, or whether the problem is solved and the constraint now extracts beyond its foundational purpose.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits substantial and rising extractiveness (0.62 at interval end, up from 0.35 in 1867) because the compact's function has shifted from enabling inter-provincial coordination (low-extraction) to enforcing national standards (climate policy, taxation, trade rules) that increasingly override provincial preference, particularly for resource-dependent provinces. Suppression requirement has risen from 0.25 to 0.58 because maintaining federal paramountcy over climate and environmental policy requires active enforcement against provincial defiance (Alberta's legal battles, Saskatchewan's carbon-tax challenges, Quebec's refusal of federal invasiveness). Theater ratio is moderate (0.42) because the compact still contains genuine coordination (unified market, currency union) but this function is increasingly theatrical — the constraint persists primarily through enforcement of federal standards, not coordination benefit. Accessibility_collapse is lower at interval end (0.52 in 2026 vs 0.65 in 1867) because alternatives have become more salient: explicit separation movements, asymmetric federalism proposals, renegotiation of fiscal arrangements. Resistance is high (0.71) because provincial leaders, resource sectors, and independence movements actively challenge federal overreach. The leveled coercion grid shows structural suppression rising from 0.32 to 0.62 as federal enforcement infrastructure hardened, while organizational-level suppression (provincial governments) rose from 0.18 to 0.55 as premiers face federal pressure; individual-level suppression (citizens) remained low (0.12 to 0.35) because most Canadians experience federalism as background structure, not direct coercion.
 *
 * PERSPECTIVAL GAP:
 *   The federal agenda-setter and participating provinces perceive this constraint differently because they sit in opposed positions. Federal authority sees the compact as granting it legitimacy to establish binding national standards within its enumerated powers (s.91) and to override provincial preferences when federal interests (climate, trade, defense) are at stake. Resource-dependent provinces see the same structure as extractive overreach — they claim s.92A grants them sovereignty over their resources and view federal carbon pricing as a violation of the compact's original terms. Provincial leaders frame this as negotiation leverage to extract concessions (exemptions, transfers); federal authority frames the same demands as bad faith. The engine should compute federal-seat classification as rope-leaning (sees genuine market coordination), resource-province-seat as snare-leaning (experiences extraction without coordination benefit), and participating-province-seat as tangled-rope-middle (genuine coordination + asymmetric extraction both present). The claim (tangled_rope) reflects the structural reality: coordination function is real, extraction is asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality vectors are stark: participating provinces (Ontario, Quebec) and federal authority sit near d=0.5 (symmetric: they benefit from the unified market and coordinate within it, but also bear the suppression costs of federal paramountcy). Resource-dependent provinces sit near d=0.9 (full targets): they receive no coordination benefit proportional to their extraction cost (they would prefer open resource markets, unrestricted extraction, and provincial pricing power), yet they are trapped in the federal system by geography and economic integration. Provincial political leadership sits near d=0.3 (slight beneficiary): they extract political capital from sovereignty framing without bearing the full cost of exit. Excluded territories sit near d=1.0 (pure targets): no benefit, no choice, no voice. The compact's enforcement machinery (court system, fiscal transfers, regulatory power) is distributed such that beneficiaries (federal authority, large provinces, resource-extraction sectors) have high power and mobile exit, while targets (resource-dependent provinces, excluded territories) have moderate-to-zero power and trapped exit. This structural asymmetry drives the rising extractiveness: as the constraint's function shifted from coordination to enforcement, beneficiaries invested in suppression infrastructure (legal frameworks, intergovernmental dispute mechanisms, transfer-payment conditions) that targets cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented colonial trade, duplicate defense costs) was substantially solved by 1945 — unified market, common defense, tariff coordination were genuine achievements. Yet the constraint persists and has become increasingly extractive. The Mandatrophy Resolution framework suggests the constraint is now in the snare-adjacent cell: its original coordination function is stable but has become a platform for federal and provincial incumbent extraction. Federal authority maintains the compact to preserve its paramountcy and transfer-payment authority; resource provinces maintain nominal confederation while threatening exit to extract exemptions and negotiating leverage; provincial leaders maintain it for political capital without paying the full cost of exit. The theater ratio (0.42) reflects this: ceremony and legal theater (constitutional rounds, intergovernmental conferences, court rulings) consume a large share of effort, while the actual coordination function (currency, market integration, tariff policy) runs on autopilot. The mandatrophy verdict is live but contested — federal authority claims the compact still solves vital coordination problems; provinces increasingly claim it is a historical relic they exit whenever feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_subordination_reading,
    'Is the Canadian constitution best read as a compact among sovereign provinces that retain residual authority, or as a supreme law that subordinates all provincial authority to federal sovereignty?',
    'This is a conceptual question grounded in competing traditions of constitutional interpretation. Resolution would require acceptance of one reading over the other by sufficient political authority (constitutional amendment, clear Supreme Court precedent, widespread political consensus). The empirical signature of resolution would be: if compact_federalism prevails, provinces exercise demonstrated exit capacity (successful separation or asymmetric renegotiation); if subordination prevails, exit threats fail and federal authority asserts supreme force.',
    'High. If compact_federalism prevails (currently contested), the constraint''s extractiveness would decline as provincial exit becomes credible — federal authority would be forced to negotiate rather than impose. If subordination prevails, extractiveness would rise further as federal authority is confirmed supreme and exit threats are revealed as empty theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compact_vs_subordination_reading, conceptual, 'Whether confederation is a compact or a supreme legal hierarchy.').

omega_variable(
    resource_sovereignty_scope,
    'Does s.92A grant provinces absolute sovereignty over natural resources, or is that sovereignty subject to federal paramountcy on interprovincial trade and emissions?',
    'Appellate case law interpreting s.92A in conflict with federal climate and trade authority. The Alberta Reference (2021) and successor cases will determine whether provinces can impose resource taxes/restrictions that federal authority claims violate interprovincial trade freedom. If Supreme Court upholds provincial resource sovereignty against federal challenge, the compact_federalism reading gains empirical support; if it subordinates s.92A to federal climate authority, resource-sovereignty-primacy reading is foreclosed.',
    'High. Resource-dependent provinces'' directionality vector (d-value) would shift downward (toward beneficiary, away from target) if s.92A is confirmed absolute; would shift upward (toward full target) if federal paramountcy is confirmed. This directly affects the constraint''s classification from resource-province seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_scope, empirical, 'Whether s.92A grants subordinate or supreme provincial resource authority.').

omega_variable(
    exit_capacity_under_duress,
    'Can provinces credibly exit the confederation under duress (economic coercion, federal overreach) or is the threat of exit purely theatrical?',
    'Empirical: a successful exit (successful referendum, formal negotiation leading to sovereignty) would demonstrate capacity; repeated failed exit attempts (Quebec referenda defeated, separation movements unable to mobilize majority support) would demonstrate theater. The measure is whether exit requires federal permission (subordination reading prevails) or federal negotiation (compact reading holds). Post-2026 data: if a province successfully negotiates asymmetric arrangements (resource policy exemptions, fiscal transfer renegotiation, distinct society status) by making exit credible, the compact reading''s foundation is confirmed. If all exit attempts are suppressed or fail politically, the reading becomes increasingly incoherent.',
    'High. Exit capacity directly determines the effective directionality of resource-dependent provinces: if exit is credible, d-values shift toward beneficiary (exit option is real, hence less trapped); if exit is pure theater, d-values shift toward target (identity-locked or trapped exit). This affects the classification of the constraint from every provincial seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_under_duress, empirical, 'Whether provincial exit is structurally available or theatrical.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.58 at interval end) structural (federal enforcement capacity) or internalized (provincial leaders have accepted federal authority as legitimate)?',
    'Post-exit observation: if a province were to exit or achieve substantial renegotiation, would suppression persist (indicating it was internalized — the province had internalized federal authority as legitimate) or disappear (indicating it was structural — federal force was the active restraint). Measurement via defiance experiments: provinces that openly violate federal norms (Alberta on carbon pricing, Quebec on federal invasiveness) show structural suppression; provinces that comply while expressing private resistance show partially internalized suppression.',
    'Medium. If suppression is substantially internalized (d=0.5+), the constraint could shift from snare-adjacent to rope if beneficiaries'' coordination benefits were re-established. If suppression is structural (active federal enforcement), the snare reading is confirmed and internalization is not blocking exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is enforcement capacity or internalized legitimacy.').

omega_variable(
    coordinate_vs_extract_function_separation,
    'Can the genuine coordination benefits of the compact (unified market, common currency, tariff union) be separated from the extractive federal overreach (climate standards, equalization conditions, interprovincial trade restrictions)?',
    'Counterfactual: if a province renegotiated to retain market access and currency union while opting out of federal climate policy and equalization, would the market coordination persist? If yes, the functions are separable and extraction is pure overhead; if no, the functions are bundled and federal authority''s enforcement is structural, not extractive overreach.',
    'High. If separable, the compact reading is vindicated — provinces could negotiate à la carte participation, shifting the constraint toward rope (genuine coordination, negotiable terms). If inseparable, the constraint is snare-flavored because beneficiaries can force targets to accept extraction as the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordinate_vs_extract_function_separation, conceptual, 'Whether coordination and federal extraction can be operationally separated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.18).
narrative_ontology:measurement(prov_tr_t1945, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.32).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(prov_tr_t2026, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.35).
narrative_ontology:measurement(prov_be_t1945, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1945, 0.48).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.52).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(prov_be_t2026, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.25).
narrative_ontology:measurement(prov_su_t1945, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1945, 0.38).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement(prov_su_t2026, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2026, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1867, tn=2026
narrative_ontology:measurement(prov_grid_01, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(class), 1867, 0.42).
narrative_ontology:measurement(prov_grid_02, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(class), 2026, 0.38).
narrative_ontology:measurement(prov_grid_03, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(individual), 1867, 0.35).
narrative_ontology:measurement(prov_grid_04, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(individual), 2026, 0.28).
narrative_ontology:measurement(prov_grid_05, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(organizational), 1867, 0.55).
narrative_ontology:measurement(prov_grid_06, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(organizational), 2026, 0.48).
narrative_ontology:measurement(prov_grid_07, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(structural), 1867, 0.65).
narrative_ontology:measurement(prov_grid_08, provincial_sovereignty_boundary__compact_federalism, accessibility_collapse(structural), 2026, 0.52).
narrative_ontology:measurement(prov_grid_09, provincial_sovereignty_boundary__compact_federalism, resistance(class), 1867, 0.32).
narrative_ontology:measurement(prov_grid_10, provincial_sovereignty_boundary__compact_federalism, resistance(class), 2026, 0.65).
narrative_ontology:measurement(prov_grid_11, provincial_sovereignty_boundary__compact_federalism, resistance(individual), 1867, 0.18).
narrative_ontology:measurement(prov_grid_12, provincial_sovereignty_boundary__compact_federalism, resistance(individual), 2026, 0.58).
narrative_ontology:measurement(prov_grid_13, provincial_sovereignty_boundary__compact_federalism, resistance(organizational), 1867, 0.28).
narrative_ontology:measurement(prov_grid_14, provincial_sovereignty_boundary__compact_federalism, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(prov_grid_15, provincial_sovereignty_boundary__compact_federalism, resistance(structural), 1867, 0.35).
narrative_ontology:measurement(prov_grid_16, provincial_sovereignty_boundary__compact_federalism, resistance(structural), 2026, 0.72).
narrative_ontology:measurement(prov_grid_17, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(class), 1867, 0.42).
narrative_ontology:measurement(prov_grid_18, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(class), 2026, 0.55).
narrative_ontology:measurement(prov_grid_19, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(individual), 1867, 0.28).
narrative_ontology:measurement(prov_grid_20, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(individual), 2026, 0.42).
narrative_ontology:measurement(prov_grid_21, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(organizational), 1867, 0.58).
narrative_ontology:measurement(prov_grid_22, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(prov_grid_23, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(structural), 1867, 0.72).
narrative_ontology:measurement(prov_grid_24, provincial_sovereignty_boundary__compact_federalism, stakes_inflation(structural), 2026, 0.81).
narrative_ontology:measurement(prov_grid_25, provincial_sovereignty_boundary__compact_federalism, suppression(class), 1867, 0.22).
narrative_ontology:measurement(prov_grid_26, provincial_sovereignty_boundary__compact_federalism, suppression(class), 2026, 0.48).
narrative_ontology:measurement(prov_grid_27, provincial_sovereignty_boundary__compact_federalism, suppression(individual), 1867, 0.12).
narrative_ontology:measurement(prov_grid_28, provincial_sovereignty_boundary__compact_federalism, suppression(individual), 2026, 0.35).
narrative_ontology:measurement(prov_grid_29, provincial_sovereignty_boundary__compact_federalism, suppression(organizational), 1867, 0.18).
narrative_ontology:measurement(prov_grid_30, provincial_sovereignty_boundary__compact_federalism, suppression(organizational), 2026, 0.55).
narrative_ontology:measurement(prov_grid_31, provincial_sovereignty_boundary__compact_federalism, suppression(structural), 1867, 0.32).
narrative_ontology:measurement(prov_grid_32, provincial_sovereignty_boundary__compact_federalism, suppression(structural), 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, canadian_climate_policy_federal_paramountcy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, equalization_transfer_asymmetry).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three constraint stories, each instantiating a different reading of the boundary between federal enumerated power and provincial residual sovereignty. This story (compact_federalism) treats Confederation as a consensual compact among provinces that retain negotiable exit capacity. Sibling story constitutional_subordination treats provinces as federal creatures with no inherent sovereignty. Sibling story resource_sovereignty_primacy treats s.92A as granting absolute provincial resource sovereignty that forecloses federal climate authority. Each reading produces a different ε (extractiveness from provincial seats), different beneficiary structures, and different classification. They are linked via affects_constraints to enable contamination analysis: if compact_federalism's foundational axiom (provincial_consent_binding) is foreclosed by constitutional amendment or Supreme Court ruling, the classification shifts downstream to subordination-reading constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, organized, 0.55).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
