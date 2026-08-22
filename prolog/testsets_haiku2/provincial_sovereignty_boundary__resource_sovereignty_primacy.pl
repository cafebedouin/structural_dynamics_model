% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty and Territorial Integrity (Resource-Sovereignty-Primacy Reading)
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   This constraint is the resource-sovereignty-primacy reading of the
 *   provincial sovereignty boundary kernel—a contested interpretation of
 *   Canadian federalism codified in s.92A of the Constitution Act 1982. Under
 *   this reading, provincial ownership of natural resources grounds absolute
 *   provincial sovereignty over resource development; federal climate policy,
 *   carbon pricing, and interprovincial coordination mandates are reframed as
 *   illegitimate federal extraction from provincial jurisdiction. The reading
 *   is held by provincial governments and resource industries; it is disputed
 *   by federal climate authorities, Indigenous nations, and constitutional
 *   scholars who read s.92A as qualified by federal treaty and environmental
 *   powers. The constraint described here is the standing arrangement under
 *   contest: the provincial resource governance structure as experienced and
 *   justified by the resource-sovereignty-primacy reading.
 *
 * KEY AGENTS:
 *   - provincial_governments: Agenda-setters and primary beneficiaries; assert absolute resource sovereignty and resist federal climate/fiscal intervention
 *   - federal_government: Payer in the reading's framing; constrained from implementing national climate policy; treated as extracting from provincial jurisdiction
 *   - provincial_resource_industries: Beneficiaries; operate under stable provincial tenure and low federal regulatory risk (relative to other readings)
 *   - interprovincial_climate_coordination: Payer; the collective need for coordinated emissions reduction is framed as a prisoner's dilemma the reading refuses to solve
 *   - Indigenous_resource_claimants: Victims; their prior resource claims are subordinated to provincial sovereignty under this reading
 *   - international_climate_partners: Excluded; bear the cost of uncoordinated provincial resource expansion through global emissions but have no enforcement standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.71).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty and Territorial Integrity (Resource-Sovereignty-Primacy Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political/constitutional/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '58de7da7-6657-4f0d-9be8-6eb039a50098').
narrative_ontology:cs_kernel_codification('58de7da7-6657-4f0d-9be8-6eb039a50098', fixed_text).
narrative_ontology:cs_authority_grounding('58de7da7-6657-4f0d-9be8-6eb039a50098', lineage).
narrative_ontology:cs_interpretation_layer_present('58de7da7-6657-4f0d-9be8-6eb039a50098').
narrative_ontology:cs_reading_relation('58de7da7-6657-4f0d-9be8-6eb039a50098', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('58de7da7-6657-4f0d-9be8-6eb039a50098', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('58de7da7-6657-4f0d-9be8-6eb039a50098', foundational, resource_ownership_grounds_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('58de7da7-6657-4f0d-9be8-6eb039a50098', resource_ownership_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('58de7da7-6657-4f0d-9be8-6eb039a50098', secondary, provincial_unilateral_exit_right).
narrative_ontology:cs_axiom_status(provincial_unilateral_exit_right, holdable).
narrative_ontology:cs_axiom_grounding('58de7da7-6657-4f0d-9be8-6eb039a50098', provincial_unilateral_exit_right, deontological).
narrative_ontology:cs_reference_frame('58de7da7-6657-4f0d-9be8-6eb039a50098', provincial_resource_autonomy_baseline).
narrative_ontology:cs_drift_state('58de7da7-6657-4f0d-9be8-6eb039a50098', contemporary_climate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('58de7da7-6657-4f0d-9be8-6eb039a50098', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_regulatory_authority).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_climate_coordination).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_regulatory_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold constitutional title to natural resources within their boundaries. Interpret s.92A as grounding absolute provincial sovereignty and treat federal climate, carbon pricing, and emissions regulation as illegitimate interference in internal affairs. Their institutional identity is fused with resource sovereignty—they cannot exit the claim without ceasing to be sovereign provinces in their own understanding. They set the rules for resource development, capture the rents, and defend against federal encroachment through constitutional argument and political resistance.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, beneficiary).

% Claims authority over interprovincial and international environmental commitments via POGG and treaty power. Attempts to implement carbon pricing, emissions standards, and climate coordination. Under the resource-sovereignty-primacy reading, these federal initiatives are reframed as illegitimate extraction from provincial jurisdiction. Federal authority is constrained by provincial sovereignty claims and cannot unilaterally impose climate policy. Their exit would require constitutional amendment or Supreme Court victory reinterpreting s.92A to subordinate provincial ownership to federal environmental authority.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, excluded).

% Oil, gas, forestry, mining, uranium firms. Licensed and regulated by provinces under the assumption of stable provincial tenure grounded in s.92A. The resource-sovereignty-primacy reading insulates them from federal carbon pricing, interprovincial resource-sharing mandates, and climate-driven production limits. They benefit from the reading's assertion that federal climate intervention is illegitimate extraction. Their exit options are relatively high (capital can move to other provinces or countries), but the reading reduces perceived regulatory risk in the home province.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries, beneficiary,
    organized, biographical, mobile, national).

% Indigenous nations with Aboriginal and treaty rights to resources on unceded or treaty lands. The resource-sovereignty-primacy reading treats provincial resource ownership as absolute and superseding Indigenous rights. Indigenous communities seeking resource consent-based governance find their claims structurally subordinated to provincial title. Their exit is identity-locked: they cannot leave their territories without abandoning their nations. The reading actively suppresses Indigenous sovereignty claims and requires them to negotiate from weakness with provincial governments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_claimants, payer,
    powerless, generational, identity_locked, national).

% The collective need for coordinated emissions reductions across provinces to meet international climate commitments and manage transboundary environmental impacts. The resource-sovereignty-primacy reading treats interprovincial coordination as a prisoner's dilemma with no solution: each province has incentive to expand resource extraction unilaterally if others do not constrain themselves. Federal coordination mechanisms (carbon pricing, emissions standards) are the tools to escape the dilemma, but the reading reframes them as federal extraction. The coordination mechanism becomes impossible within the reading's logic.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_climate_coordination, payer,
    analytical, generational, trapped, national).

% Environment Canada, Natural Resources Canada, and other federal agencies tasked with implementing climate policy and environmental standards. The resource-sovereignty-primacy reading constrains their authority: provinces do not allow federal environmental assessment of major resource projects, do not implement federal emissions standards, and do not accept federal veto over resource development. Their ability to regulate is substantially limited by the reading's assertion of provincial sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_regulatory_agencies, excluded).

% Other nations bound by Paris Agreement and bilateral trade agreements with Canada. They have no standing to enforce emissions reductions or resource management within Canada's borders but bear the cost of uncoordinated Canadian resource expansion through global emissions. The resource-sovereignty-primacy reading creates uncertainty about Canada's ability to honor climate and trade commitments. They are excluded from the conversation on resource governance and have only post-hoc leverage (sanctions, carbon tariffs, trade retaliation).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, international_climate_partners, excluded,
    institutional, generational, constrained, global).

% Constitutional lawyers, political theorists, and federalism scholars who analyze whether s.92A ownership is absolute or conditional on federal powers. They serve as the reading's external validators or critics and produce the interpretive authority that legitimates (or challenges) the sovereignty claim within academic and judicial discourse. Their output feeds directly into Supreme Court reasoning and public debate.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, canadian_constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates resource development rights territorially: provinces authorize resource extraction within their boundaries, manage environmental review and licensing, and capture rents. Solves the problem of overlapping claims to the same resource base (provincial, Indigenous, federal climate interests) by placing a single provincial authority in control of allocation decisions.
% TRANSFER_FUNCTION: Moves resource rents (oil, gas, forest products, minerals) from extraction firms to provincial treasuries. Moves regulatory authority from federal environmental agencies to provincial resource ministries. Moves decision-making power over major resource projects from federal and Indigenous governance bodies to provincial legislatures. In the reading's framing, federal climate policy and carbon pricing constitute illegitimate extraction from provincial jurisdiction—the federal government collects carbon tax and regulatory compliance costs from provinces and resource firms without provincial consent.
% ABSENT_VOICES: Indigenous nations with prior and continuing resource claims are structurally excluded from resource governance (their consent is not required for resource development); federal climate authorities and environmental agencies are excluded from resource-allocation decisions (provinces do not allow federal veto or mandatory environmental standards); international climate partners are excluded from enforcement mechanisms (they cannot compel emissions reductions from Canada). All three groups would object to the absolute provincialism of the reading but are kept out by its assertion of provincial title as superseding their claims.
% DISAPPEARANCE_RATIONALE: If the resource-sovereignty-primacy reading collapsed—if provinces were required to subordinate resource decisions to federal climate authority, or if Indigenous consent became mandatory, or if interprovincial coordination agreements were binding—resource governance would reorganize entirely: provincial resource planning would face federal climate constraints; Indigenous nations would have veto power over major projects; federal carbon pricing would no longer face provincial constitutional objection; interprovincial coordination on emissions would become a federal obligation, not a provincial choice. The provincial revenue stream, regulatory autonomy, and fossil fuel expansion trajectory would face material constraints.
% FOUNDING_PROBLEM: The 1982 patriation of the Constitution codified provincial ownership of natural resources (s.92A) to preserve provincial control over resource development and taxation—a concern rooted in earlier federal-provincial conflicts over resource taxation and regulation, particularly in the West (Alberta oil and gas, uranium, forestry). The founding problem was asymmetric federal power to tax and regulate resources away from provincial benefit, threatening provincial fiscal autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments and resource-industry associations attest the problem is live: federal carbon pricing, emissions regulation, and climate policy constitute new federal overreach into resource matters and threaten provincial revenue and autonomy. Federal climate authorities, environmental agencies, Indigenous organizations, and international climate partners attest the founding problem has been superseded by a new problem (inadequate emissions constraints and climate action). Constitutional scholars attest both versions are live claims with no clear judicial consensus; Supreme Court has not definitively ruled on whether s.92A limits federal climate authority.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.48 → 0.68) because the reading reframes federal climate and fiscal policy as illegitimate extraction from provincial jurisdiction, while providing no mechanism for interprovincial coordination or Indigenous consent in resource governance. The extraction is asymmetric: provinces collect the coordination benefit (stable resource tenure, revenue capture) and federal/international/Indigenous interests pay the cost (constrained climate action, uncoordinated emissions, excluded voice). Suppression is similarly high (0.58 → 0.71) and rising because maintaining the sovereignty claim requires actively suppressing: (1) federal climate authority claims, (2) Indigenous resource-consent requirements, (3) interprovincial coordination agreements, and (4) treaty obligations that limit provincial unilateralism. Theater ratio rises and plateaus (0.22 → 0.42 at t=25, then stable) as the reading's legitimacy increasingly depends on constitutional theater—repeated assertions of provincial sovereignty, appeals to text and history—rather than functional governance of resource externalities. The rise stabilizes at t=25 because the reading reaches its maximum viable theater level: beyond that point, further claims to absolute sovereignty become obviously inconsistent with observable federal coordination and Indigenous litigation. Accessibility collapse is moderate (0.62): alternatives to provincial resource control do exist (federal coordination, Indigenous co-management, interprovincial agreements) and remain intellectually available, but the reading's control of provincial executive authority and constitutional interpretation makes them politically costly to exit toward. Resistance is high (0.73) and stable: federal agencies, Indigenous nations, climate advocates, and international partners mount continuous resistance to the reading's sovereignty claim, but the reading's entrenchment in provincial institutional identity makes it durable against this pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial government seat, this is genuine coordination: provinces allocate resources through licensing, capture rents, and maintain institutional autonomy—a real problem solved (avoiding federal overreach into resource matters). From the federal seat, this is pure extraction masked as coordination: provinces use the sovereignty claim to evade climate commitments, carbon pricing, and environmental standards, shifting the cost of uncoordinated emissions onto federal authorities and international partners. From the Indigenous seat, this is snare: provincial resource ownership excludes Indigenous consent and sovereignty, and the reading's assertion of provincial absolutism makes Indigenous exit from subordination structurally impossible. The engine should compute Tangled_Rope for the provincial seat (real coordination + asymmetric extraction of federal authority), Snare for the federal seat (no coordination benefit, constrained authority), and Snare for the Indigenous seat (no coordination benefit, active suppression of rights). The perspective gap is structural and derives from different beneficiary/victim positions, not from disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments sit at the beneficiary end of the directionality spectrum (d ≈ 0.1): they benefit from the resource sovereignty claim, have high institutional power, can exit federal constraints by asserting sovereignty (identity-locked but directional), and operate at the institutional level where the reading originates. Federal authorities sit at the target end (d ≈ 0.85): the reading reframes their climate and fiscal authority as illegitimate extraction from provincial jurisdiction; they are institutionally powerful but constrained from implementing national policy; their exit would require constitutional amendment or litigation victory. Indigenous claimants sit at the target end (d ≈ 0.9): they are powerless relative to provincial governments, their resource claims are subordinated by the reading, their exit options are identity-locked (they cannot leave their territories), and the reading actively suppresses their sovereignty claims. Interprovincial climate coordination sits at the analytical end (d ≈ 0.5): it is not an agent but a collective problem whose solution is blocked by the reading; the constraint's operation makes the coordination impossible. Federal and provincial governments compute different types from the same constraint because their directionality differs: the federal seat experiences it as snare (constrained by provincial sovereignty claims, target of provincial extraction), while the provincial seat experiences it as rope (genuine coordination of resource allocation, albeit excluding Indigenous parties and creating negative externalities).
 *
 * MANDATROPHY ANALYSIS:
 *   The resource-sovereignty-primacy reading shows signs of mandatrophy resolved: the founding problem (federal overreach into resource taxation) was live in 1982 but is contested/dead by 2020+. Federal climate policy is now the dominant concern for federal authorities and international partners, not resource taxation per se. The reading's justification (protecting provincial revenue from federal extraction) remains institutionally ensconced, but the problem it was built to solve has shifted. The constraint persists through institutional inertia and identity fusion—provinces have built their fiscal models and political institutions around resource control, making exit psychologically costly even if the original threat has diminished. Theater ratio rising (0.22 → 0.42) documents this drift: more of the enforcement energy goes into repeating the sovereignty claim than into solving genuine resource-allocation problems. The reading's persistence depends on suppressing awareness of mandatrophy: if provinces and the public acknowledged that the founding problem is dead but the constraint persists as institutional theater, the sovereignty claim would face legitimacy pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s92a_absolute_vs_conditional_ownership,
    'Does s.92A grant provinces absolute sovereignty over resources, or conditional ownership subject to federal treaty and environmental powers?',
    'Supreme Court of Canada ruling on whether federal climate policy and carbon pricing impair provincial ownership rights, or whether provincial ownership is qualified by federal POGG and treaty-making power.',
    'If absolute ownership is upheld, the resource-sovereignty-primacy reading stands and federal climate authority is constrained; if conditional ownership is upheld, the reading shifts to constitutional subordination (provinces must coordinate with federal authority) and extracted gains move to the federal seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(s92a_absolute_vs_conditional_ownership, empirical, 'Whether provincial resource ownership is absolute or conditional on federal environmental/treaty authority.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (federal overreach into resource taxation) still live, or has it been replaced by a new problem (inadequate federal/interprovincial climate coordination)?',
    'Historical analysis of federal-provincial resource disputes since 1982; constitutional scholars'' consensus on the primary federalism problem in contemporary resource governance; public opinion surveys on whether provincial autonomy or climate action is the priority.',
    'If the founding problem is dead, the constraint shows mandatrophy and is liable to reclassification as Piton (institutional theater maintaining a solution to a solved problem). If it is still live, the constraint''s Tangled Rope classification is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem (federal overreach) persists or has been superseded by climate coordination needs.').

omega_variable(
    indigenous_sovereignty_subordination_mechanism,
    'Is Indigenous resource subordination a necessary feature of the resource-sovereignty-primacy reading, or could the reading accommodate Indigenous co-governance and consent requirements?',
    'Constitutional amendment recognizing Indigenous sovereignty over resources; Supreme Court ruling on whether s.92A is consistent with s.35 Aboriginal rights; legislative co-management agreements between provinces and Indigenous nations that vest genuine veto power.',
    'If Indigenous sovereignty is separable from the reading, the reading''s extraction against Indigenous claimants could be reduced while preserving provincial resource control. If inseparable, the reading''s cost to Indigenous nations is structural and cannot be negotiated away within the reading''s own logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_subordination_mechanism, conceptual, 'Whether the reading''s resource sovereignty claim necessarily subordinates Indigenous sovereignty or could be modified to accommodate it.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of federal climate authority structural (legal constraints from s.92A interpretation) or internalized (provinces have come to believe their sovereignty is absolute and cannot imagine alternative federal roles)?',
    'Qualitative analysis of provincial legislative discourse and legal briefs: do provinces cite structural constitutional limits or ideological commitment to sovereignty? Post-constitutional-amendment thought experiments: would provinces accept federal climate authority if s.92A were amended to clarify federal jurisdiction?',
    'If suppression is mainly structural, removing the constitutional ambiguity (amendment clarifying federal authority) would substantially reduce it. If suppression is internalized through institutional identity fusion, the suppression would persist even after constitutional clarification because provinces have fused their identities with resource sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, conceptual, 'Whether suppression of federal authority is a legal constraint or an internalized belief rooted in institutional identity.').

omega_variable(
    externality_cost_distribution,
    'What is the total cost of uncoordinated provincial resource extraction (greenhouse gas emissions, transboundary pollution, climate damages) relative to the benefit captured by provinces (resource rents, fiscal autonomy)?',
    'Environmental economics analysis comparing provincial resource revenue to global/national climate and environmental damages attributable to Canadian resource expansion; cost-benefit analysis of coordinated vs. uncoordinated extraction pathways.',
    'If external costs exceed provincial benefits, the constraint is net-extractive across the full stakeholder set (provinces extract from the rest of the world and future generations). If costs are smaller than benefits, the coordination trade-off is more balanced. Either way, the reading''s framing of federal climate policy as extraction obscures these costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_cost_distribution, empirical, 'Whether uncoordinated resource extraction creates larger external costs than provincial benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.22).
narrative_ontology:measurement(prov_tr_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 5, 0.26).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 10, 0.31).
narrative_ontology:measurement(prov_tr_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 15, 0.36).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 20, 0.4).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 25, 0.42).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 30, 0.42).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prov_be_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(prov_be_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(prov_su_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(prov_su_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, canadian_federalism_climate_policy_coordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_sovereignty_claims).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_emissions_trading).

% DUAL FORMULATION NOTE:
% This story (resource_sovereignty_primacy) is one reading of the contested kernel provincial_sovereignty_boundary. Sibling stories (constitutional_subordination, compact_federalism) instantiate alternative readings of the same constitutional text (s.92A, the distribution of powers between provincial and federal governments). Each reading assigns different extractiveness, beneficiary/victim structures, and classifications based on whose interpretation of the text is treated as correct. This constraint family reveals how a single text can ground structurally different constraints depending on whose reading is authoritative. The ε value is stable within each reading (what the reading is about does not change), but the parties, the costs, and the beneficiaries differ radically across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
