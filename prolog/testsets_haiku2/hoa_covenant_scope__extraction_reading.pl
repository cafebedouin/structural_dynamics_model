% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Extraction Mechanism via Fine Proliferation
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint instantiates the EXTRACTION READING of the HOA covenant
 *   kernel. The covenant exists ostensibly to maintain property standards and
 *   coordinate shared infrastructure. Under this reading, the constraint's
 *   actual structural function has shifted: it operates as a revenue
 *   generation mechanism for boards, management companies, and attorneys
 *   through fine proliferation, selective enforcement against vulnerable
 *   homeowners, expedited lien processes, and attorney fee extraction. The
 *   founding coordination problem (value preservation, externality
 *   resolution) is substantially solved by market incentives and housing
 *   codes; what persists is the rent-seeking structure. Board members and
 *   management firms benefit disproportionately; financially vulnerable
 *   homeowners and renters bear concentrated costs. This is NOT a claim about
 *   the constraint's legitimacy or desirability—it is a structural reading of
 *   its actual operation.
 *
 * KEY AGENTS:
 *   - board_members: agenda-setting beneficiary; controls enforcement priorities and fine levels; receives selective leniency and assessment reduction
 *   - property_management_firms: organized beneficiary; compensated per violation; profit motive drives aggressive enforcement escalation
 *   - legal_counsel_contracted: organized beneficiary; billable hours increase with litigation; advises on fine escalation pathways
 *   - financially_vulnerable_homeowners: powerless victim; subject to selective enforcement; exit blocked by liens and sale complications
 *   - renters_via_pass_through: powerless victim; no vote; costs passed through; subject to enforcement via landlord proxies
 *   - affluent_homeowners: powerful beneficiary; rarely fined; benefit from enforcement against others; easy exit option
 *   - state_regulators: institutional observer; limited intervention authority; reactive to complaints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.68).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.76).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Extraction Mechanism via Fine Proliferation").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '4ed32740-df16-4294-a6ec-e5352bbd9fed').
narrative_ontology:cs_kernel_codification('4ed32740-df16-4294-a6ec-e5352bbd9fed', fixed_text).
narrative_ontology:cs_authority_grounding('4ed32740-df16-4294-a6ec-e5352bbd9fed', extraction).
narrative_ontology:cs_interpretation_layer_present('4ed32740-df16-4294-a6ec-e5352bbd9fed').
narrative_ontology:cs_reading_relation('4ed32740-df16-4294-a6ec-e5352bbd9fed', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ed32740-df16-4294-a6ec-e5352bbd9fed', hoa_covenant_scope__behavioral_control_reading, influences).
narrative_ontology:cs_axiom('4ed32740-df16-4294-a6ec-e5352bbd9fed', foundational, fine_revenue_decoupled_from_coordination_cost).
narrative_ontology:cs_axiom_status(fine_revenue_decoupled_from_coordination_cost, holdable).
narrative_ontology:cs_axiom_grounding('4ed32740-df16-4294-a6ec-e5352bbd9fed', fine_revenue_decoupled_from_coordination_cost, empirically_contingent).
narrative_ontology:cs_axiom('4ed32740-df16-4294-a6ec-e5352bbd9fed', foundational, enforcement_targets_powerless_residents_selectively).
narrative_ontology:cs_axiom_status(enforcement_targets_powerless_residents_selectively, holdable).
narrative_ontology:cs_axiom_grounding('4ed32740-df16-4294-a6ec-e5352bbd9fed', enforcement_targets_powerless_residents_selectively, empirically_contingent).
narrative_ontology:cs_reference_frame('4ed32740-df16-4294-a6ec-e5352bbd9fed', covenant_as_coordination_mechanism).
narrative_ontology:cs_drift_state('4ed32740-df16-4294-a6ec-e5352bbd9fed', contemporary_financialized_hoa_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ed32740-df16-4294-a6ec-e5352bbd9fed', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel_contracted).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, moderate_income_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, affluent_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, moderate_income_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set enforcement priorities and fine levels. Vote to adopt new covenant restrictions and increase assessment caps. Receive reduced assessment obligations on their own properties and favorable treatment in enforcement decisions. Many serve multiple terms and accumulate de facto control over which properties face scrutiny. Exit is possible but reputationally costly in tight-knit communities.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, beneficiary).

% Contracted by boards to enforce covenants and generate fine revenue. Receive compensation per violation processed, incentivizing violation detection and aggressive enforcement. Profits rise with fine volume and collection velocity. Can shift to other HOAs if a single community attempts cost reduction.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, arbitrage, regional).

% Hired to draft new restrictive covenants, prosecute lien actions, and represent the association in disputes with homeowners. Billable hours increase with litigation volume and procedural complexity. Often advises on fine escalation and expedited enforcement pathways. Can seek other HOA clients.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel_contracted, beneficiary,
    organized, biographical, arbitrage, regional).

% Subject to selective enforcement of covenant violations, often minor infractions (paint color, landscaping, flag display). Cannot easily exit: home sale requires disclosure of violations and liens; refinancing is blocked by unpaid fines; relocation is costly. Face escalating fines, attorney fees added to balances, and expedited lien foreclosure if unable to pay. No meaningful representation in enforcement decisions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, trapped, local).

% Bear increased HOA costs passed through by landlord-owners. Have no vote in board decisions or enforcement policy. Can exit via lease termination but must find alternative rental in market where HOA-governed properties dominate. Enforcement violations attributed to tenants (noise, guest parking, landscaping) result in owner fines that landlords often pass back via lease violations or non-renewal.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Have sufficient resources to comply with most covenant requirements and to contest fines if motivated, but lack the time and legal resources of wealthy residents. Receive selective enforcement: less scrutinized than vulnerable homeowners, but still subject to surprise enforcement campaigns. Benefit modestly from property value maintenance but bear the cost of assessment increases and fine structure expansion. Exit requires sale at potentially reduced value if community is labeled as high-enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, moderate_income_homeowners, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, moderate_income_homeowners, beneficiary).

% Rarely fined; violations are overlooked or resolved via informal negotiation. Benefit from property value maintenance driven by strict enforcement against others. Often serve on the board or maintain social proximity to board members. Can exit easily via sale; often own multiple properties. Receive reduced assessment obligations through board discretion.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, affluent_homeowners, beneficiary,
    powerful, generational, mobile, local).

% Receive complaints about HOA practices but have limited statutory authority to intervene in covenant enforcement or fine structures in most jurisdictions. Can investigate predatory lien practices and discriminatory enforcement but remedies are slow and reactive. Monitor legislative proposals to cap fines or require transparency.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_regulators, observer,
    institutional, generational, analytical, national).

% Homeowners who would advocate for reduced enforcement, lower fines, or community oversight mechanisms are excluded from decision-making by board control of the agenda and voting structure. Recall mechanisms are weak or absent in many jurisdictions; reform requires majority consensus that enforcement beneficiaries are incentivized to prevent.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, would_be_alternative_governance, excluded,
    moderate, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains aesthetic uniformity and property upkeep to preserve neighborhood property values; coordinates shared infrastructure maintenance responsibilities and resolves externalities from neglected properties affecting neighbors.
% TRANSFER_FUNCTION: Moves capital from property owners and renters (via assessment and fines) to board members (via reduced assessments and selective enforcement favoritism), property management firms (via violation-based compensation), and legal counsel (via enforcement litigation and covenant drafting).
% ABSENT_VOICES: Renters have no vote and minimal representation; financially vulnerable homeowners cannot afford to contest enforcement; alternative governance models (community recall, transparent enforcement standards, fine caps) are excluded by board agenda control.
% DISAPPEARANCE_RATIONALE: If covenant enforcement and fine mechanisms disappeared, board members would lose a consolidation tool; property management and legal fees would collapse; vulnerable homeowners would face lower extraction pressure; property value maintenance would depend on market incentives rather than coercive aesthetics—the neighborhood's economic structure would reorganize around voluntary coordination rather than enforced compliance.
% FOUNDING_PROBLEM: Early suburban HOA covenants were established to maintain property values and resolve externalities from neglected properties; shared infrastructure (pools, roads, common areas) required collective maintenance funding and dispute resolution.
% FOUNDING_PROBLEM_CORROBORATION: State housing authorities and independent economic analysis confirm that property maintenance and value preservation are substantially solved through market incentives and basic housing codes in most jurisdictions; the founding coordination problem is no longer live. Board members and management firms dispute this, citing ongoing maintenance costs, but do not deny that fine revenue now exceeds documented maintenance expenses in many HOAs (peer-reviewed housing studies, state audit data from communities with transparent accounting).
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness reaches 0.68 because the constraint systematically transfers capital from vulnerable residents to beneficiary groups through mechanisms decoupled from documented service costs. Suppression is high (0.76) because enforcement depends on active coercion—threat of liens, foreclosure, and legal consequences—and vulnerable homeowners cannot easily exit or contest fines. Theater ratio is elevated (0.62) because enforcement is increasingly performative: the stated maintenance justification is contradicted by enforcement patterns that target minor infractions on vulnerable properties while ignoring violations by board-connected residents. Measurement series show extraction rising over 25 years as fine structures proliferate, new covenant provisions are adopted, and management company compensation models reward violation detection. Theater ratio rises because enforcement intensity increases (more patrols, more violation notices) while documented maintenance needs remain flat—enforcement apparatus outgrows its stated function. The shared time grid ensures every metric is authored at every examined point; no backfill or interpolation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (vulnerable homeowners, renters) compute as snare or high-extraction tangled_rope from their own structural position because coercion is their lived experience and alternatives collapse (exit is blocked). The agenda-setter seat (board) computes as rope or moderate-extraction coordination because they author the rule as cooperation and experience it as such (their costs are reduced, their exit is easy, their enforcement authority is legitimate). The management firm seats compute as moderate-extraction rope because they have arbitrage exit and genuine service provision (they do perform maintenance work), even though the violation-compensation model biases enforcement. This divergence is the measurement the engine takes: when the same constraint computes as snare from the payer seat and rope from the agenda-setter seat, that divergence flags the extraction reading as structurally real, not merely a matter of interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Board members are beneficiaries (collect selective assessment reductions, favorable enforcement) with powerful status and mobile exit (can relocate or step down), placing d near 0.1. Management firms are beneficiaries (billable violation revenue) with organized status and arbitrage exit (can shift to other HOAs), placing d near 0.2. Legal counsel are beneficiaries (litigation hours, drafting contracts) with organized status and arbitrage exit, placing d near 0.2. Financially vulnerable homeowners are victims (bear fine extraction) with powerless status and trapped exit (home sale blocked by liens, refinancing blocked by balances), placing d near 0.95. Renters are victims (cost pass-through, no vote) with powerless status and constrained exit (rental market dominated by HOAs), placing d near 0.90. Affluent homeowners are beneficiaries despite powerful status and mobile exit because they receive selective enforcement leniency (d near 0.2). The directionality curve is steep: extraction amplifies toward powerless trapped targets and dampens toward powerful or arbitrage-capable beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows clear mandatrophy: the founding problem (property value preservation, externality resolution) is substantially dead—housing codes and market incentives handle these functions adequately. The constraint persists because the beneficiary structure (board authority, management compensation, legal fees) has accumulated its own survival mechanisms independent of the founding function. Fine structures proliferate, new covenant provisions are added, enforcement intensity increases—yet documented maintenance needs remain flat and property values track market trends, not enforcement activity. The theater ratio rising to 0.62 indicates performative enforcement: the apparatus exists to maintain itself, not to solve the founding coordination problem. The mandatrophy verdict prevents misclassifying this as rope (genuine coordination) when the structural data show it is a rent-seeking mechanism riding on the legitimate coordination it no longer performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fine_revenue_vs_maintenance_cost,
    'Does documented annual fine revenue exceed documented annual shared maintenance costs, and if so, by what margin?',
    'Mandatory transparent accounting: state regulation requiring HOAs to publish annual fine revenue, maintenance expenditures, and reserve allocations. Peer-reviewed housing studies comparing revenue-to-cost ratios across HOA populations.',
    'If fine revenue materially exceeds documented maintenance costs, the extraction reading is corroborated—fines have decoupled from their coordination justification. If revenue tracks costs, the coordination reading gains support and extraction is more partial/incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fine_revenue_vs_maintenance_cost, empirical, 'Whether fine volume exceeds coordination-function cost.').

omega_variable(
    selective_enforcement_evidence,
    'Is enforcement of covenant violations applied uniformly across resident groups, or are vulnerable/powerless residents disproportionately fined for violations board-connected residents commit without penalty?',
    'Analysis of HOA violation records and enforcement decisions stratified by resident income, board affiliation, and legal representation. Audit studies: planted violations (identical rule breaches) at properties with different resident demographics and tracked enforcement response.',
    'Evidence of selective enforcement corroborates the extraction reading and establishes targeting of powerless residents. Uniform enforcement supports the coordination reading and suggests discrimination claims would be harder to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_evidence, empirical, 'Whether enforcement patterns target vulnerable residents disproportionately.').

omega_variable(
    exit_cost_asymmetry,
    'What is the actual cost and time required for a financially vulnerable homeowner to exit an HOA (sale with liens, refinancing blockage, legal clearance) relative to a board member or affluent resident?',
    'Comparison of sale timelines, lender requirements, and legal clearance costs for properties with HOA enforcement histories vs. clean records. Interviews with real estate agents and title companies about exit barriers.',
    'If exit costs are materially higher for vulnerable residents (liens blocking refinance, sale price reduction from enforcement history, legal fees to clear violations), the directionality asymmetry is confirmed—powerless status combined with trapped exit amplifies extractiveness. If exit costs are uniform, the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_asymmetry, empirical, 'Whether exit cost diverges by resident wealth and enforcement history.').

omega_variable(
    kernel_reading_boundaries,
    'Is the extraction reading structurally distinct from the behavioral_control_reading, or do they describe the same enforcement mechanism from different normative angles?',
    'Comparative analysis of enforcement patterns: the behavioral_control reading emphasizes aesthetic conformity (selective enforcement of appearance-based violations), while the extraction reading emphasizes revenue (selective enforcement of high-fine violations). Do enforcement campaigns target appearance or high-fee violations disproportionately?',
    'If enforcement disproportionately targets high-fee violations (especially against vulnerable residents), the extraction and behavioral-control readings are distinct constraints with different ε values. If enforcement treats appearance and high-fee violations equivalently, the readings describe a single constraint from different narrative angles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundaries, conceptual, 'Whether the extraction and behavioral-control readings are structurally distinct or narratively alternative framings of the same mechanism.').

omega_variable(
    suppression_internalization,
    'To what extent is the measured suppression (0.76) structural (external barriers: liens, foreclosure risk, legal costs) versus internalized (residents accept enforcement as legitimate despite cost)?',
    'Post-exit trajectory: do vulnerable homeowners who successfully exit HOAs report reduced anxiety and financial stress, or do they retain internalized deference to covenant authority? Surveys of lapsed HOA members vs. current members; qualitative interviews about psychological burden of enforcement.',
    'If suppression is largely internalized (residents comply willingly despite cost extraction), the constraint may carry deeper behavioral lock-in than structural coercion alone suggests, raising extraction severity. If suppression is purely structural and breaks cleanly after exit, extraction can be modeled as straightforward capital transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.59).
narrative_ontology:measurement_basis(hoa__tr_t15, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__extraction_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(hoa__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(hoa__be_t15, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__extraction_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(hoa__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(hoa__su_t15, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__extraction_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(hoa__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, residential_property_lien_foreclosure_acceleration).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel instantiates three structurally distinct constraints: coordination_reading (Rope) emphasizes externality resolution and shared infrastructure maintenance; behavioral_control_reading (Tangled Rope) emphasizes aesthetic uniformity enforcement as property value maximization; extraction_reading (Tangled Rope, this story) emphasizes revenue generation and board power consolidation via fine proliferation and selective enforcement. All three share the same legal text and formal HOA governance structure but attribute different causal mechanisms and beneficiary structures to the same standing arrangement. Extraction and behavioral-control readings coexist across different HOA populations: enforcement campaigns vary by community in whether they prioritize appearance violations (behavioral-control mode) or high-fee violations (extraction mode). The readings influence each other: aggressive fine escalation (extraction mode) requires the behavioral justification (appearance conformity) to maintain legitimacy—the two readings are structurally coupled even as they compete for narrative authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
