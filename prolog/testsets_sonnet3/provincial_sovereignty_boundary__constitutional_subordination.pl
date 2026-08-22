% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Subordination to Federal Constitutional Supremacy
 *   domain: political/constitutional/resource_governance
 *
 * SUMMARY:
 *   Since Confederation, and hardened by the 1982 patriation and the 1998
 *   Secession Reference, the dominant constitutional doctrine in Canada
 *   treats provinces as entities whose powers derive entirely from the
 *   constitution rather than from antecedent sovereign compact. This grounds
 *   federal authority to run equalization transfers, impose national climate
 *   policy under POGG and criminal-law heads, and treat unilateral provincial
 *   secession as void ab initio absent a constitutional-amendment-track
 *   negotiated exit. Resource-producing provinces bear disproportionate
 *   fiscal and regulatory cost under this framework while resource-receiving
 *   provinces and national policy constituencies benefit from its
 *   legitimation of federal reach.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter (institutional/analytical) — administers subordination doctrine, sets equalization and climate policy
 *   - resource_producing_provinces: payer (powerful/constrained) — bears fiscal and regulatory cost of the settlement
 *   - equalization_receiving_provinces: beneficiary (moderate/constrained) — net recipient of transfers legitimized by the doctrine
 *   - separatist_political_movements: payer (organized/trapped) — exit option foreclosed as a matter of positive constitutional law
 *   - constitutional_courts: observer/agenda_setter (institutional/analytical) — both interprets and actively constitutes the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.52).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.61).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Subordination to Federal Constitutional Supremacy").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/constitutional/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'ce700381-16df-410a-a8bd-f8402c5b3a24').
narrative_ontology:cs_kernel_codification('ce700381-16df-410a-a8bd-f8402c5b3a24', formalized).
narrative_ontology:cs_authority_grounding('ce700381-16df-410a-a8bd-f8402c5b3a24', lineage).
narrative_ontology:cs_interpretation_layer_present('ce700381-16df-410a-a8bd-f8402c5b3a24').
narrative_ontology:cs_reading_relation('ce700381-16df-410a-a8bd-f8402c5b3a24', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('ce700381-16df-410a-a8bd-f8402c5b3a24', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('ce700381-16df-410a-a8bd-f8402c5b3a24', foundational, provinces_lack_inherent_sovereignty).
narrative_ontology:cs_axiom_status(provinces_lack_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ce700381-16df-410a-a8bd-f8402c5b3a24', provinces_lack_inherent_sovereignty, conventional).
narrative_ontology:cs_axiom('ce700381-16df-410a-a8bd-f8402c5b3a24', foundational, unilateral_secession_is_constitutional_nullity).
narrative_ontology:cs_axiom_status(unilateral_secession_is_constitutional_nullity, holdable).
narrative_ontology:cs_axiom_grounding('ce700381-16df-410a-a8bd-f8402c5b3a24', unilateral_secession_is_constitutional_nullity, conventional).
narrative_ontology:cs_axiom('ce700381-16df-410a-a8bd-f8402c5b3a24', secondary, federal_pogg_authority_extends_to_national_climate_policy).
narrative_ontology:cs_axiom_status(federal_pogg_authority_extends_to_national_climate_policy, holdable).
narrative_ontology:cs_axiom_grounding('ce700381-16df-410a-a8bd-f8402c5b3a24', federal_pogg_authority_extends_to_national_climate_policy, instrumental).
narrative_ontology:cs_reference_frame('ce700381-16df-410a-a8bd-f8402c5b3a24', confederation_as_constitutional_creation).
narrative_ontology:cs_drift_state('ce700381-16df-410a-a8bd-f8402c5b3a24', post_1982_patriation_and_secession_reference, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ce700381-16df-410a-a8bd-f8402c5b3a24', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_producing_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_political_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy_advocates).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, peace_order_and_good_government_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional supremacy under the patriation settlement and Supreme Court reference doctrine; sets equalization formulas, national climate policy, and interprets any provincial exit attempt as requiring federal constitutional amendment consent. Administers the enforcement machinery (courts, transfer conditionality, reference cases) that treats provinces as constitutional creatures rather than sovereign parties.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Generate resource revenue subject to equalization transfers and federal climate/emissions policy that constrains extraction pace and export infrastructure. Their s.92A resource ownership is real but bounded by federal trade-and-commerce and environmental jurisdiction; exit or unilateral policy divergence is met with reference-case litigation and transfer leverage. They bear the fiscal cost of a settlement they did not individually ratify.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_producing_provinces, payer,
    powerful, generational, constrained, regional).

% Receive federal equalization transfers funded substantially by resource-producing provinces' fiscal capacity; benefit from the constitutional subordination frame because it legitimizes federal redistribution as ordinary governance rather than extraction between sovereign parties.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces, beneficiary,
    moderate, generational, constrained, regional).

% Organize around provincial self-determination but face a constitutional order that treats unilateral secession as a nullity absent federal-constitutional-amendment-track consent (per the Secession Reference). Their exit option does not exist as a matter of positive law; any path runs through the very federal process they seek to leave.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_political_movements, payer,
    organized, biographical, trapped, regional).

% Rely on federal peace-order-and-good-government and criminal-law jurisdiction to impose national emissions standards and carbon pricing that would be unachievable if provinces held resource-sovereignty veto power. Benefit directly from the subordination reading's legitimation of federal climate authority.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_constituencies, beneficiary,
    organized, civilizational, mobile, national).

% Argue provinces should control resource-derived fiscal capacity without equalization clawback or federal conditionality. Their compact-federalism-adjacent claims are not cognizable within the subordination framework's own doctrinal terms; they are litigants and dissenters, not co-authors of the settlement.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy_advocates, excluded,
    moderate, generational, constrained, regional).

% Adjudicate reference cases (Secession Reference, resource and environmental jurisdiction disputes) and in doing so both interpret and actively constitute the subordination doctrine — an observer seat that also sets binding precedent enforcing the boundary it describes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, diffuse).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable locus of constitutional authority that prevents unilateral fragmentation, enables federation-wide redistribution (equalization) and coordinated national policy (climate, trade, defense) that no province could unilaterally produce or credibly commit to alone.
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-producing provinces to equalization-receiving provinces via federal transfer formulas, and moves policy discretion over resource extraction pace and climate regulation from provincial legislatures to federal jurisdiction, justified by constitutional subordination rather than negotiated compact.
% ABSENT_VOICES: Separatist movements and resource-sovereignty advocates argue the 1867/1982 settlement was imposed rather than freely compacted and that provinces retain inherent or residual sovereignty; they are litigants before federal courts rather than co-equal parties to the interpretive question, and their compact-federalism reading is treated by this reading's own courts as doctrinally foreclosed rather than debated on equal footing.
% DISAPPEARANCE_RATIONALE: If constitutional subordination were replaced overnight by compact federalism or resource-sovereignty primacy, equalization transfers would require renegotiated consent from resource-producing provinces, federal climate policy would lose its jurisdictional basis over resource extraction, and separatist exit would become a negotiable political question rather than a constitutional nullity — the federation's fiscal and regulatory architecture would require wholesale renegotiation.
% FOUNDING_PROBLEM: The 1867 Confederation and 1982 patriation settlements needed to establish a stable, judicially enforceable answer to 'what happens when a province and the federal government disagree' that did not require renegotiating the union's existence every time a dispute arose.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court of Canada (Reference re Secession of Quebec, 1998) and successive federal governments attest the subordination framework as settled doctrine solving an ongoing coordination problem. Resource-producing provincial governments, independent constitutional scholars outside the federal apparatus, and separatist movements attest the founding problem was distributional and colonial-administrative in origin (imposing a settlement on unequal parties) and that the doctrine now functions primarily to entrench federal redistributive and regulatory reach rather than resolve a live coordination failure.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is moderate rather than extreme: real coordination value exists in stable federation-wide fiscal transfers and coherent national policy, but a genuine cost is imposed on resource-producing provinces who did not individually ratify the 1982 settlement and whose exit option is foreclosed rather than negotiated. Suppression (0.61) exceeds extraction because the doctrine's persistence depends on active judicial enforcement (reference cases) foreclosing the compact-federalism and resource-sovereignty alternatives as a matter of law, not merely as a matter of political preference. Theater ratio (0.28) is modest — courts do genuine adjudicative work — but has risen as the doctrine increasingly performs legitimation work for redistributive and regulatory reach beyond its original coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and equalization/climate-policy beneficiaries sit near the beneficiary end: the subordination frame directly legitimizes their fiscal and regulatory reach. Resource-producing provinces and separatist movements sit near the target end: their exit and fiscal-autonomy claims are foreclosed rather than negotiated, and their constrained/trapped exit options amplify effective extraction relative to a mobile actor. Constitutional courts occupy a genuinely dual position — nominally analytical observers, but their reference-case rulings are the enforcement mechanism that constitutes the boundary, which is why the secondary_role of agenda_setter is warranted rather than a pure observer classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unilateral fragmentation and enabling federation-wide coordination absent perpetual renegotiation — remains partially live (federations do need stable default rules), which is why founding_problem_status is authored as contested rather than dead. But the doctrine's growth from a coordination default into an affirmative vindication of federal redistributive and regulatory reach (equalization formula expansion, POGG-grounded climate policy) suggests function has been layered onto the original coordination floor rather than merely preserved. This is precisely the tangled_rope signature: a genuine coordination function (stable constitutional order) coexists with asymmetric extraction (resource-producing provinces funding transfers and absorbing regulatory constraint they did not individually consent to) under active enforcement (reference-case litigation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_provincial_sovereignty,
    'Is the constitutional_subordination reading the structurally correct account of the 1867/1982 settlement, or do the compact_federalism and resource_sovereignty_primacy readings better capture the founding intent and current s.92A resource-ownership provisions?',
    'This is not empirically resolvable within a single constitutional order — it depends on which historical and doctrinal sources (Confederation debates, patriation negotiation record, s.92A text, Secession Reference reasoning) are given interpretive priority, and different constitutional traditions and provincial governments make different choices. The compact_federalism sibling reading treats provinces as antecedent sovereign parties with negotiable exit; the resource_sovereignty_primacy sibling treats s.92A resource ownership as grounding absolute territorial sovereignty over resource policy. Each is authored as its own separate constraint story with its own ε and stakeholder structure.',
    'If compact_federalism were the operative reading, exit and fiscal-autonomy claims would be legitimate negotiating positions rather than constitutional nullities, and the extraction this story documents would be reframed as coercive imposition on sovereign parties (raising ε substantially). If resource_sovereignty_primacy were operative, federal climate and equalization authority over resource-producing provinces would itself be the extractive structure. This story holds the subordination reading fixed and does not average across siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_provincial_sovereignty, conceptual, 'Which of three sibling readings of the provincial_sovereignty_boundary kernel is structurally correct; this story fixes the constitutional_subordination reading.').

omega_variable(
    supreme_court_neutrality_vs_construction,
    'Is the Supreme Court''s role in the Secession Reference and related jurisprudence a neutral discovery of pre-existing constitutional meaning, or an active construction of the subordination doctrine that could have gone otherwise?',
    'Comparative constitutional analysis of how courts in other federations (Spain re Catalonia, the pre-1861 US) resolved analogous secession and resource-sovereignty questions, and closer reading of dissents and academic critique of the Secession Reference''s reasoning.',
    'If constructed rather than discovered, the doctrine''s claim to settle the founding problem as a matter of law rather than politics is weaker, and its persistence depends more heavily on continued judicial and federal enforcement will than on inherent constitutional logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supreme_court_neutrality_vs_construction, conceptual, 'Whether the doctrine reflects discovered constitutional meaning or judicial construction.').

omega_variable(
    equalization_extraction_magnitude,
    'What share of resource-producing provinces'' fiscal transfer to the federation under equalization reflects genuine federation-coordination cost versus asymmetric extraction enabled by the subordination doctrine''s foreclosure of exit and renegotiation?',
    'Comparative fiscal-federalism analysis against federations with negotiated (rather than constitutionally-imposed) transfer formulas, and counterfactual modeling of transfer levels under a compact-federalism renegotiation regime.',
    'A high extraction share would support reclassifying this constraint closer to snare; a low share would support a rope-leaning classification with the subordination doctrine functioning mainly as coordination infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equalization_extraction_magnitude, empirical, 'How much of the fiscal transfer is coordination cost versus extraction enabled by foreclosed exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1867, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1867, 0.1).
narrative_ontology:measurement(prov_tr_t1940, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1940, 0.14).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1867, 0.3).
narrative_ontology:measurement(prov_be_t1940, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1940, 0.35).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.42).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1998, 0.46).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1867, 0.4).
narrative_ontology:measurement(prov_su_t1940, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1940, 0.45).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.55).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the provincial_sovereignty_boundary kernel. constitutional_subordination (this file) treats provinces as constitutionally-derived entities with no inherent sovereignty and legitimates federal equalization and climate authority; compact_federalism treats the federation as a compact among sovereign provinces retaining residual sovereignty and negotiable exit; resource_sovereignty_primacy treats s.92A resource ownership as grounding absolute provincial sovereignty over resource policy. The three stories share a beneficiary/victim structure that inverts across readings — federal_government and equalization-receiving provinces are beneficiaries here but would be recast as extraction targets under resource_sovereignty_primacy. Each maintains its own stable ε; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
