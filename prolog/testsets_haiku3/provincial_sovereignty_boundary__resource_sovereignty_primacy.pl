% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy Reading (s.92A Constitution Act 1982)
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   Section 92A of the Constitution Act 1982 explicitly grants provinces
 *   ownership and authority over non-renewable resources within their
 *   territories. One constitutional reading — resource-sovereignty-primacy —
 *   interprets this grant as grounding absolute provincial sovereignty:
 *   provinces can set extraction rates, pricing, and export policy
 *   unilaterally, and federal climate policy (carbon pricing, emissions
 *   limits) that constrains resource development constitutes illegitimate
 *   extraction of provincial authority. This reading is contested by two
 *   siblings: compact_federalism (which emphasizes negotiated exit and
 *   residual provincial authority) and constitutional_subordination (which
 *   subordinates provincial resource claims to federal enumerated powers over
 *   environment, trade, and taxation). The resource-sovereignty-primacy
 *   reading claims s.92A text is clear and should prevail in case of
 *   collision with federal authority. This constraint story is ONE reading of
 *   the contested kernel; it is not neutral among the three.
 *
 * KEY AGENTS:
 *   - provincial_governments: Institutional agenda-setters interpreting s.92A as grounding sovereignty and using it to reject federal climate policy
 *   - federal_government: Institutional payer attempting to enforce carbon pricing and climate coordination; constrained by provincial constitutional claims
 *   - federal_climate_policy_constituencies: Organized payer bearing the cost of delayed climate action due to provincial sovereignty claims
 *   - interprovincial_resource_commons_users: Powerless trapped payer (citizens and ecosystems affected by extraction in other provinces)
 *   - resource_extraction_industries: Powerful beneficiary with mobile arbitrage exit; benefits from provincial rejection of federal regulation
 *   - other_provinces: Excluded institutional actors who would benefit from federal coordination but lack standing in resource jurisdiction
 *   - supreme_court_of_canada: Analytical observer and arbiter of the constitutional reading itself
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
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy Reading (s.92A Constitution Act 1982)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political/constitutional/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '7690f7ad-32b9-4dc9-a211-dda7a38a991b').
narrative_ontology:cs_kernel_codification('7690f7ad-32b9-4dc9-a211-dda7a38a991b', fixed_text).
narrative_ontology:cs_authority_grounding('7690f7ad-32b9-4dc9-a211-dda7a38a991b', lineage).
narrative_ontology:cs_interpretation_layer_present('7690f7ad-32b9-4dc9-a211-dda7a38a991b').
narrative_ontology:cs_reading_relation('7690f7ad-32b9-4dc9-a211-dda7a38a991b', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('7690f7ad-32b9-4dc9-a211-dda7a38a991b', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('7690f7ad-32b9-4dc9-a211-dda7a38a991b', foundational, section_92a_text_is_dispositive).
narrative_ontology:cs_axiom_status(section_92a_text_is_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('7690f7ad-32b9-4dc9-a211-dda7a38a991b', section_92a_text_is_dispositive, conventional).
narrative_ontology:cs_axiom('7690f7ad-32b9-4dc9-a211-dda7a38a991b', foundational, resource_control_grounds_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_control_grounds_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7690f7ad-32b9-4dc9-a211-dda7a38a991b', resource_control_grounds_territorial_sovereignty, deontological).
narrative_ontology:cs_reference_frame('7690f7ad-32b9-4dc9-a211-dda7a38a991b', provincial_resource_ownership_primacy).
narrative_ontology:cs_drift_state('7690f7ad-32b9-4dc9-a211-dda7a38a991b', climate_urgency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7690f7ad-32b9-4dc9-a211-dda7a38a991b', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_resource_commons_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_constitutional_primacy).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_control_equals_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret s.92A as grounding absolute provincial sovereignty over resource extraction, pricing, and export within their territories. Set resource policy unilaterally, treat federal climate and fiscal measures as extractive intrusions on their authority. Justify this reading by pointing to the constitutional text's explicit grant of ownership and control over non-renewable resources. Collect rents and regulatory authority directly from resource development.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Attempts to enforce carbon pricing, interprovincial resource-sharing standards, and fiscal transfers tied to environmental compliance. Frames these as legitimate federal coordination of a shared atmosphere and equalization obligations. Constrained by the constitutional text (s.92A) and political leverage of resource-rich provinces; cannot override provincial ownership claims without triggering exit threats or constitutional amendment.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Bear the cost of delayed or uncoordinated climate action when provinces deploy s.92A to block federal emissions policy. They lack constitutional standing to contest provincial resource authority and depend on federal advocacy to constrain it; their exit option is relocation to jurisdictions with stricter climate policy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies, payer,
    organized, biographical, constrained, national).

% Citizens and ecosystems in downstream provinces affected by resource extraction and environmental degradation in upstream provinces. Have no jurisdictional standing to contest extraction decisions made under s.92A by the resource-controlling province. Trapped by geographic dependence on shared watersheds, air, and ecological systems; exit is possible only through relocation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_resource_commons_users, payer,
    powerless, biographical, trapped, national).

% Benefit directly from provinces wielding s.92A to exclude federal climate regulation and maintain competitive extraction costs. Their mobility (they can develop elsewhere if a province tightens regulation) gives them arbitrage exit relative to the provincial constraint, but their scale and political leverage in resource-dependent economies gives them de facto co-agenda-setter power alongside provincial governments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_industries, beneficiary,
    powerful, biographical, mobile, global).

% Resource-scarce or environmentally vulnerable provinces would benefit from stronger federal coordination of resource externalities and climate policy. Excluded from the resource-sovereignty decision-making that shapes the constraint itself; their voice appears only in federal forums, not in the provincial councils where resource policy is set. Constrained by the constitutional primacy this reading grants to the resource-owning province.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, excluded,
    institutional, generational, constrained, national).

% Interprets the constitutional meaning of s.92A and s.91 (federal enumerated powers). Has upheld both provincial resource ownership and federal climate authority, creating formal ambiguity about which prevails in collision cases. Seat for adjudicating the constitutional reading itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates jurisdiction over resource policy within a federal system: provinces get exclusive ownership and control over non-renewable resources located within their borders; this solves the problem of determining WHO decides extraction rates, pricing, and export policy across a multicultural federation with asymmetric resource endowments.
% TRANSFER_FUNCTION: Transfers effective authority over climate policy and interprovincial environmental externalities from federal coordination mechanisms (carbon pricing, water-quality standards, emissions targets) to unilateral provincial veto through resource-sovereignty claims. The constraint moves regulatory power and economic rents from federal constituencies and non-resource provinces to resource-rich provincial governments and extraction industries.
% ABSENT_VOICES: Citizens in resource-scarce provinces and downstream ecosystems affected by extraction externalities are excluded from the resource-sovereignty decision. They would argue for federal coordination of shared atmospheric and hydrological systems, but their voice carries no weight in the provincial resource-governance forums where this reading is enacted. Federal climate advocates are marginalized because s.92A, under this reading, places resource decisions beyond federal reach.
% DISAPPEARANCE_RATIONALE: If s.92A were repealed or reinterpreted to subordinate provincial resource control to federal climate and fiscal authority, resource policy would reorganize around federal coordination mechanisms (carbon pricing, unified emissions trading, interprovincial water-sharing agreements). Extraction rates would fall in jurisdictions that chose federal compliance over provincial sovereignty; federal fiscal transfers would be recalibrated to reward environmental compliance; interprovincial coordination on shared resources would become enforceable rather than negotiated. The architecture of resource governance would shift from unilateral provincial authority to federal coordination with provincial consent.
% FOUNDING_PROBLEM: The 1867 Constitution Act and subsequent amendments up to 1982 left ambiguous whether provinces retained 'essential sovereignty' over resources within their territories or whether federal authority over trade, taxation, and national concerns (environment, climate) could override provincial resource claims. The 1982 addition of s.92A(1)(c) — explicit provincial ownership of non-renewable resources — was intended to resolve this ambiguity in the provincial favour by grounding resource control in constitutional text rather than residual prerogative.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments and resource-extraction advocates attest the founding problem is LIVE and s.92A's text is clear: federal climate policy that restricts extraction is illegitimate federal overreach into provincial jurisdiction. Federal climate advocates and non-resource provinces attest the problem is REFRAMED, not solved: s.92A clarified ownership but left jurisdiction over IMPACTS (climate, interprovincial externalities) ambiguous, and s.91 grants federal power over those impacts. Constitutional scholars (e.g., Hogg's Constitutional Law of Canada) document both readings as live within the constitutional tradition; courts have upheld both provincial resource authority AND federal climate power without resolving the collision case. No outside corroboration settles the dispute — it is the dispute itself.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.42 (at 1982 when s.92A was new and its implications untested) to 0.68 (at 2026 after decades of provincial deployment against federal climate policy). This trajectory reflects the reading's increasing use as a jurisdictional shield: early years saw cautious invocation; by the mid-2000s (climate urgency rise), provinces weaponized s.92A to block federal carbon policy, making the extraction visible and asymmetric. Theater rises from 0.15 to 0.42 because provinces increasingly invoke resource-sovereignty language (invoking heritage, identity, prosperity) while using it tactically to preserve extraction industries. Suppression rises from 0.48 to 0.71 because federal climate alternatives are increasingly foreclosed by the constitutional claim: once s.92A is deployed, federal policy faces not just political opposition but constitutional-veto logic. Accessibility of alternatives (for federal climate constituencies) collapses as provincial sovereignty claims harden — the 'legitimate' pathway for federal action narrows. Resistance from federal and climate constituencies remains substantial (0.58) throughout because the constraint is contested, not accepted as natural law, but resistance is structurally suppressed by the constitutional reading itself.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial seat, this is not extraction but RECOVERY of legitimate authority: s.92A is read as correcting earlier federal overreach, and the refusal to subordinate resource decisions to federal climate policy is seen as constitutional fidelity. From the federal seat and the climate-policy-constituency seats, the same structure is experienced as asymmetric extraction: provinces use constitutional text to dodge legitimate climate coordination, and the federal government is powerless to enforce it without constitutional amendment. From the Supreme Court analytical seat, both readings are plausible within the constitutional order, creating formal ambiguity that functions as provincial veto. The engine will compute a per-seat classification: the provincial-government seat may compute closer to rope (genuine coordination problem solved), while the federal-government and climate-constituency seats compute closer to snare (asymmetric extraction with veto, no genuine coordination benefit accruing to them). This divergence is the measurement the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments are the structural beneficiaries: they collect regulatory authority and rents from resource development; federal climate policy that would constrain extraction is framed as illegitimate intrusion. Direction d ≈ 0.15 (beneficiary seat). Federal government and climate constituencies are the structural targets: they bear the cost of delayed climate action; their exit option is constrained (federal government must operate within the constitutional order; climate constituencies must accept whatever policy emerges from federal-provincial negotiation). Direction d ≈ 0.80 (target seats). Resource-extraction industries are co-beneficiaries: they benefit from the provincial veto on federal regulation, though their power is wielded through provincial proxies. Other provinces are excluded from the resource-sovereignty decision but would benefit from federal coordination: they are not targets of extraction but are unable to enforce their preference. The directionality gradient is not overridden; it emerges from the structural asymmetry in who controls the resource-sovereignty claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine ambiguity in the 1867 and 1980 constitutional text about whether resource control lived with the provinces or the federation. S.92A (1982) was added to clarify: provinces own non-renewable resources and exercise authority over extraction. That clarification solved a real coordination problem — it reduced uncertainty about who decides resource policy. However, the extraction measured here is NOT the clarity provision itself; it is the DEPLOYMENT of s.92A as a veto on federal climate policy, a problem that did not exist in 1982 (climate urgency was not yet a policy battleground). The founding problem remains live (ambiguity about collision cases: when resource sovereignty meets federal climate authority, who prevails?), but the constraint's operation has drifted from clarifying provincial authority toward using that authority to block federal environmental coordination. This is a textbook case of a coordination mechanism being repurposed as an extraction veto. The mandatrophy signal comes from the rising extraction and theater metrics: the constraint is being maintained theatrically (via sovereignty rhetoric) to preserve extraction (de facto veto on federal policy) whose original justification (clarifying resource ownership) no longer requires it. A genuine mountain would have flat or declining metrics; the rising trajectory signals institutional inertia and tactical deployment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_ownership_vs_impact_jurisdiction,
    'Does s.92A''s grant of provincial resource OWNERSHIP carry with it exclusive jurisdiction over the IMPACTS of resource extraction (climate, pollution, interprovincial externalities)? Or is impact jurisdiction separable, leaving federal authority intact?',
    'Supreme Court ruling in a case where federal climate policy directly constrains provincial extraction (e.g., federal carbon tax applied to provincial resource rents; federal water-quality standard conflicting with provincial extraction practice).',
    'If ownership includes impact jurisdiction, this reading''s classification as tangled_rope holds: coordination (resource allocation) + asymmetric extraction (federal climate policy as federal overreach on provincial authority). If impact jurisdiction is separable, this reading becomes closer to snare: pure federal power denial with no genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_ownership_vs_impact_jurisdiction, conceptual, 'Whether s.92A ownership encompasses jurisdiction over resource extraction impacts or only title to the resource itself.').

omega_variable(
    absolute_vs_constitutional_sovereignty,
    'Does ''sovereignty'' in the resource-sovereignty-primacy claim mean ABSOLUTE unilateral authority (no federal intervention, unilateral exit right), or does it mean PRIMACY within the federal constitutional order (provincial authority unless explicitly federal)? The text of s.92A says provinces have authority over non-renewable resources; it does not say ''immune from federal law''.',
    'Textual analysis of s.92A alongside s.91 (federal trade, taxation, environment powers) and the Supremacy Clause (Constitution Act 1982, s.52: the Constitution is supreme law). Precedent from cases deciding conflicts (e.g., whether federal environmental assessment can block provincial extraction).',
    'If the reading claims ABSOLUTE sovereignty, it is foreclosing the constitutional_subordination and compact_federalism readings (because those preserve federal-provincial coexistence). If it claims PRIMACY, it merely influences those readings (they debate the scope of primacy, not its existence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_constitutional_sovereignty, conceptual, 'Whether resource-sovereignty-primacy implies absolute unilateral provincial authority or constitutional primacy within a federal order.').

omega_variable(
    exit_legitimacy_under_duress,
    'If a province believes federal climate policy is extractive overreach on its s.92A authority, does s.92A ground a unilateral right to exit the federation? Or does exit require federal consent (constitutional amendment)? The compact_federalism reading claims exit is negotiable under duress; this reading frames exit differently.',
    'Constitutional amendment or Supreme Court ruling on whether unilateral provincial exit is a logical consequence of resource-sovereignty-primacy. Historical evidence from Confederation negotiations about the status of the provinces as ''compact'' parties (if they were, they retain exit rights; if they were constituent parts of a new federal nation, they do not).',
    'If unilateral exit is a logical consequence of this reading, the reading forecloses compact_federalism (both claim exit, but by different authority). If exit requires negotiation or federal consent, the readings coexist (both acknowledge exit possibility but under different conditions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_legitimacy_under_duress, conceptual, 'Whether resource-sovereignty-primacy logically entails unilateral provincial exit rights or whether exit remains negotiated.').

omega_variable(
    suppression_of_federal_coordination,
    'Is the measured suppression (0.71) a property of the resource-sovereignty-primacy reading itself (the constitutional text suppresses federal alternatives), or is it a symptom of how the reading is deployed politically (provinces invoke s.92A to block federal policy that would otherwise be viable)? Structural vs. performative suppression.',
    'Post-exit analysis: if a province formally abandoned the s.92A claim, would federal climate policy face different political or legal barriers? Or has the s.92A reading become internalized such that federal policy is seen as illegitimate regardless of whether it is invoked in litigation?',
    'If suppression is structural (the text itself), the constraint is harder to unwind (requires constitutional amendment). If suppression is performative/political (the deployment, not the text), the constraint could be reframed or negotiated without amendment. This affects the fixing_cost classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_federal_coordination, empirical, 'Whether suppression in this constraint is a structural property of s.92A or a political deployment of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.15).
narrative_ontology:measurement_basis(prov_tr_t1982, projected).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(prov_tr_t1995, observed).
narrative_ontology:measurement(prov_tr_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(prov_tr_t2005, observed).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.37).
narrative_ontology:measurement_basis(prov_tr_t2015, observed).
narrative_ontology:measurement(prov_tr_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2022, 0.41).
narrative_ontology:measurement_basis(prov_tr_t2022, observed).
narrative_ontology:measurement(prov_tr_t2026, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(prov_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.42).
narrative_ontology:measurement_basis(prov_be_t1982, projected).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement_basis(prov_be_t1995, observed).
narrative_ontology:measurement(prov_be_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement_basis(prov_be_t2005, observed).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement_basis(prov_be_t2015, observed).
narrative_ontology:measurement(prov_be_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement_basis(prov_be_t2022, observed).
narrative_ontology:measurement(prov_be_t2026, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(prov_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.48).
narrative_ontology:measurement_basis(prov_su_t1982, projected).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(prov_su_t1995, observed).
narrative_ontology:measurement(prov_su_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement_basis(prov_su_t2005, observed).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(prov_su_t2015, observed).
narrative_ontology:measurement(prov_su_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement_basis(prov_su_t2022, observed).
narrative_ontology:measurement(prov_su_t2026, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(prov_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_enforcement).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_water_governance).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, carbon_pricing_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the provincial-sovereignty-boundary kernel. The sibling constraints (constitutional_subordination, compact_federalism) are linked via the same kernel but instantiate different authority structures. All three stories share a common referent (the constitutional allocation of resource authority) but author different ε values and beneficiary/victim structures depending on the reading's premises about whether s.92A text or negotiated federalism governs the scope of provincial authority. The three-way reading_relations in cs_structure capture the structural logic: resource_sovereignty_primacy claims to foreclose the other two within a single legal framework (text-based reasoning), while those two claim to coexist (historical/negotiation-based reasoning). The ε divergence reflects that disagreement: if text is dispositive, ε is moderate (coordination + some asymmetry); if negotiation is the frame, ε may be lower (more genuine bargaining) or higher (more power-driven extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
