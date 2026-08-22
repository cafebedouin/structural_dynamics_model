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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Subordination to Federal Constitutional Authority
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   This story authors the constitutional-subordination reading of a
 *   contested federalism kernel: provinces exist as creatures of the federal
 *   constitution, possess no inherent sovereignty prior to or independent of
 *   it, and any exit from confederation requires federal consent mediated
 *   through constitutional processes (the amending formula, the Clarity Act
 *   framework). Under this reading, federal equalization transfers and
 *   national climate policy are legitimate exercises of paramount federal
 *   authority, and unilateral provincial secession is a constitutional
 *   nullity rather than a live legal option. This is one of three sibling
 *   readings of the same underlying kernel (provincial_sovereignty_boundary);
 *   the compact-federalism reading and the resource-sovereignty-primacy
 *   reading are separate constraint stories with their own ε values,
 *   beneficiaries, and victims — they are not blended into this one.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter and adjudicating authority over the scope of provincial powers
 *   - resource_producing_provinces: bear the transfer and policy-override costs of federal paramountcy
 *   - separatist_movements: structurally foreclosed from unilateral exit by the same constitutional order they seek to leave
 *   - equalization_receiving_provinces: net beneficiaries of the redistributive channel this reading legitimizes
 *   - constitutional_courts: analytical/adjudicating observer seat
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
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Subordination to Federal Constitutional Authority").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/constitutional/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '5f7f6ba5-e98d-4984-bdff-103e19489be8').
narrative_ontology:cs_kernel_codification('5f7f6ba5-e98d-4984-bdff-103e19489be8', fixed_text).
narrative_ontology:cs_authority_grounding('5f7f6ba5-e98d-4984-bdff-103e19489be8', lineage).
narrative_ontology:cs_interpretation_layer_present('5f7f6ba5-e98d-4984-bdff-103e19489be8').
narrative_ontology:cs_reading_relation('5f7f6ba5-e98d-4984-bdff-103e19489be8', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('5f7f6ba5-e98d-4984-bdff-103e19489be8', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('5f7f6ba5-e98d-4984-bdff-103e19489be8', foundational, provinces_possess_no_inherent_sovereignty).
narrative_ontology:cs_axiom_status(provinces_possess_no_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5f7f6ba5-e98d-4984-bdff-103e19489be8', provinces_possess_no_inherent_sovereignty, conventional).
narrative_ontology:cs_axiom('5f7f6ba5-e98d-4984-bdff-103e19489be8', foundational, unilateral_secession_constitutionally_null).
narrative_ontology:cs_axiom_status(unilateral_secession_constitutionally_null, holdable).
narrative_ontology:cs_axiom_grounding('5f7f6ba5-e98d-4984-bdff-103e19489be8', unilateral_secession_constitutionally_null, conventional).
narrative_ontology:cs_axiom('5f7f6ba5-e98d-4984-bdff-103e19489be8', secondary, federal_paramountcy_over_national_concern_matters).
narrative_ontology:cs_axiom_status(federal_paramountcy_over_national_concern_matters, holdable).
narrative_ontology:cs_axiom_grounding('5f7f6ba5-e98d-4984-bdff-103e19489be8', federal_paramountcy_over_national_concern_matters, instrumental).
narrative_ontology:cs_reference_frame('5f7f6ba5-e98d-4984-bdff-103e19489be8', confederation_as_indivisible_union).
narrative_ontology:cs_drift_state('5f7f6ba5-e98d-4984-bdff-103e19489be8', post_secession_reference_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f7f6ba5-e98d-4984-bdff-103e19489be8', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_producing_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, peace_order_good_government_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional authority (via the courts and the amending formula) to define what provinces are and are not permitted to do, including whether a province may leave confederation. Administers equalization transfers and asserts jurisdiction over interprovincial and international matters (trade, climate, pipelines) that provinces argue intrude on their domains. Its exit from this arrangement is not applicable — it is the framework itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Own natural resources under s.92A but find federal climate and pipeline policy, along with equalization formulas that do not fully credit resource revenue costs, constraining how those resources may be developed and taxed. Cannot secede without amendment-level federal and provincial consent (per the Secession Reference and Clarity Act), so their leverage is confined to negotiation, litigation, and periodic threats of a referendum that federal authority can slow-walk or reinterpret.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_producing_provinces, payer,
    powerful, generational, constrained, regional).

% Seek unilateral or negotiated exit from confederation. The Supreme Court's Secession Reference and the federal Clarity Act require a clear referendum question and clear majority as adjudicated by Parliament before negotiation even begins, and any resulting exit still requires constitutional amendment consent from federal and other provincial actors. Their exit from the constraint itself is foreclosed by the constraint's own terms — the only route out runs through the body they are trying to leave.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, payer,
    organized, biographical, trapped, regional).

% Receive equalization transfers funded substantially by federal revenues drawn disproportionately from wealthier, resource-producing provinces. Have every incentive to affirm that provinces lack inherent sovereignty and that the federal government's redistributive and regulatory authority is legitimate, since that authority is the channel through which the transfers flow.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces, beneficiary,
    moderate, generational, constrained, regional).

% The practical scope for any province to set its own resource, environmental, and fiscal policy without federal override — named here for completeness as the abstract good being constrained, not as an actor with agency of its own.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(provincial_sovereignty_boundary__constitutional_subordination, provincial_fiscal_autonomy).

% Benefit from a constitutional reading under which federal climate policy (carbon pricing, emissions targets) applies uniformly across provinces under peace-order-and-good-government or national-concern doctrine, rather than being blocked piecemeal by resource-producing provinces invoking resource sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_constituencies, beneficiary,
    organized, generational, mobile, national).

% Adjudicate disputes over the boundary between federal and provincial authority (the Secession Reference, the Greenhouse Gas Pollution Pricing Act reference, s.92A disputes). Their rulings do not resolve the underlying kernel contest but do determine, case by case, which reading of provincial sovereignty controls in a given dispute.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, diffuse).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single constitutional order lets a federation with sharply unequal resource endowments run one currency, one internal market, one system of interprovincial transfers, and one external voice on matters like trade and climate — without each province needing to separately negotiate its economic and environmental relations with every other government on earth.
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-rich provinces to equalization-receiving provinces via federal transfers, and moves regulatory authority over resource development, pipelines, and emissions from provincial to federal hands wherever federal authority is asserted to be paramount; in exchange, exit and unilateral policy divergence are foreclosed as legitimate options for any single province.
% ABSENT_VOICES: Indigenous nations whose own sovereignty claims predate and are not resolved by the federal-provincial division of powers are largely absent from this framing entirely; a province's own internal minorities (who may prefer the federal reading even where their provincial government does not) are also not separately represented in this two-level dispute.
% DISAPPEARANCE_RATIONALE: If federal constitutional supremacy over provinces dissolved overnight, resource-producing provinces would face no legal barrier to withholding equalization-funding revenue, unilaterally setting emissions and resource policy, or pursuing exit without federal-consent gating — the equalization system, national carbon pricing regime, and the unity of the federation itself would need to be renegotiated from first principles.
% FOUNDING_PROBLEM: At Confederation and through subsequent constitutional patriation (1867, 1982), the founding problem was establishing a stable, indivisible union capable of common defense, an internal common market, and a coherent international personality, rather than a loose league of sovereign states that could defect or fragment under external or internal pressure.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court of Canada, in the 1998 Secession Reference, affirmed from an institutionally independent seat that unilateral secession has no basis in domestic or international law and that any exit requires negotiation grounded in constitutional principles — corroboration from outside the federal government's own political interest. Resource-producing provinces and separatist movements counter that this 'indivisible union' framing was never fully consented to by all constituent provinces and that its persistence serves federal and equalization-receiving interests more than any settled founding consensus.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.52) rather than extreme: this reading genuinely solves a real coordination problem (a functioning internal market, common defense, coherent external relations) and is not pure rent extraction, but the transfer of fiscal capacity and regulatory authority from resource-rich to resource-poor and federally-aligned constituencies is real and asymmetric, which is why tangled_rope rather than rope or mountain is the structurally accurate claim. Suppression (0.61) reflects that exit is not merely discouraged but constitutionally routed through federal gatekeeping (Secession Reference, Clarity Act) — a resource-producing province or separatist movement cannot simply walk away; the path out runs through the body being exited. Theater ratio (0.28) is modest and rising slowly, reflecting some increase in symbolic reaffirmation of unity (post-1995 referendum, Clarity Act) alongside genuine functional coordination continuing throughout.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government sits at the beneficiary/agenda-setter end: it defines the boundary of provincial authority and is the addressee of any request to change it. Equalization-receiving provinces and national climate constituencies are secondary beneficiaries — they benefit from the federal paramountcy this reading legitimizes without administering it themselves. Resource-producing provinces and separatist movements sit at the target end: they bear the fiscal and policy costs of federal paramountcy and are structurally blocked from unilateral exit, with separatist movements at 'trapped' rather than merely 'constrained' because the exit the constraint denies IS their entire objective.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing fragmentation of a union with weak internal cohesion at Confederation — is genuinely contested as live vs. dead: the federal government and equalization-receiving provinces treat it as permanently live (any loosening risks unraveling), while resource-producing provinces and separatists argue the union is now stable enough on other grounds (economic integration, shared currency, defense arrangements) that the sovereignty-subordination doctrine persists more as a control mechanism than a solution to an active fragmentation risk. This is exactly the tangled_rope signature: a real coordination function (union stability, internal market) coexists with an enforcement mechanism (constitutional gatekeeping of exit) that now serves asymmetric extraction as much as the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_origin_ambiguity,
    'Did Confederation create provinces as subordinate constitutional creatures, or was it a compact among pre-existing sovereign colonial entities that retained residual sovereignty?',
    'Historical-legal analysis of the British North America Act''s drafting record, colonial assemblies'' understanding of what they were consenting to in 1867, and subsequent judicial treatment (Persons Case, patriation reference, Secession Reference) as evidence of which founding narrative has prevailed in practice.',
    'If the compact reading is historically dominant, the constitutional_subordination reading is itself a later federal consolidation of authority rather than a neutral description of original design — which would reclassify this reading''s coordination claim as partly retrofitted legitimation for accumulated federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_origin_ambiguity, conceptual, 'Whether provincial subordination was the founding design or a later federal consolidation.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings (constitutional_subordination, compact_federalism, resource_sovereignty_primacy) locate their disagreement — is it about historical fact (what was agreed in 1867/1982), about legal doctrine (what the courts have since held), or about normative legitimacy (what SHOULD govern regardless of history)?',
    'Decompose each reading''s core axiom into its historical, doctrinal, and normative components and test which component actually drives the classification difference in each sibling story.',
    'If the disagreement is purely doctrinal (what courts have held), this reading is on strong ground since the Secession Reference is settled law. If it is normative (what should be true regardless of settled doctrine), the sibling readings remain live political positions that no court ruling resolves — which is the coexists_with relation authored below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the axis of disagreement among the three kernel readings.').

omega_variable(
    equalization_beneficiary_capture,
    'Do equalization-receiving provinces genuinely believe in the constitutional-subordination reading as a matter of principle, or do they hold it instrumentally because it is the reading that produces transfers to them?',
    'Examine whether equalization-receiving provinces'' constitutional arguments shift when equalization formula changes threaten to reduce their receipts, or hold constant regardless of fiscal self-interest.',
    'If instrumental, the beneficiary classification here is confirmed as capturing a real material interest rather than a disinterested constitutional judgment, strengthening the tangled_rope reading over a pure rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equalization_beneficiary_capture, empirical, 'Whether equalization recipients'' constitutional position is principled or self-interested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1867, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1867, 0.1).
narrative_ontology:measurement(prov_tr_t1930, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1930, 0.14).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1867, 0.35).
narrative_ontology:measurement(prov_be_t1930, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1930, 0.4).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.44).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1998, 0.47).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1867, 0.4).
narrative_ontology:measurement(prov_su_t1930, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1930, 0.44).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraint stories decomposing the natural-language concept 'provincial sovereignty in Canadian federalism' per the ε-invariance principle. constitutional_subordination (this story) authors moderate extraction (0.52) reflecting a genuine but asymmetric coordination arrangement. compact_federalism would author lower extraction closer to a rope (provinces as consenting co-founders with negotiable exit). resource_sovereignty_primacy would author extraction from the opposite direction (federal overreach into constitutionally-owned resources as the extractive act, with resource-producing provinces as beneficiaries of their own claimed sovereignty rather than victims). Each reading has a distinct ε, distinct beneficiary/victim sets, and a distinct claimed_type — they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
