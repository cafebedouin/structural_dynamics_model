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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Resource Sovereignty Primacy Reading of Provincial Ownership (s.92A)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   provincial_sovereignty_boundary kernel: that s.92A's grant of provincial
 *   ownership over natural resources grounds a form of territorial
 *   sovereignty that renders federal climate and fiscal policy touching those
 *   resources structurally illegitimate, and that treats unilateral exit from
 *   federal coordination frameworks as a constitutional entitlement flowing
 *   from resource control. The reading has hardened since the 1980s from a
 *   jurisdictional claim into an increasingly totalizing sovereignty
 *   doctrine, especially visible in provincial 'sovereignty act' legislation
 *   and litigation resisting federal carbon pricing and impact-assessment
 *   regimes. Two sibling readings of the same kernel —
 *   constitutional_subordination (provinces as creatures of the federal
 *   constitution with no inherent sovereignty) and compact_federalism
 *   (provinces as parties to a negotiable compact with residual sovereignty)
 *   — are NOT part of this story; they are separate constraints with their
 *   own ε and stakeholder sets, linked here only structurally.
 *
 * KEY AGENTS:
 *   - resource_exporting_provincial_governments: agenda_setter (institutional/arbitrage) — administers the sovereignty claim and collects royalty revenue
 *   - federal_climate_policy_apparatus: primary target (institutional/constrained) — bears the cost of the sovereignty claim's expansion into federal jurisdiction
 *   - indigenous_nations_within_provincial_boundaries: excluded party (powerless/trapped) — title claims subordinated by the same doctrine, absent from the federal-provincial dispute
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates but does not resolve the underlying kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.4).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Resource Sovereignty Primacy Reading of Provincial Ownership (s.92A)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '505a746c-a4bd-4f51-b0cd-4d394380b2ac').
narrative_ontology:cs_kernel_codification('505a746c-a4bd-4f51-b0cd-4d394380b2ac', fixed_text).
narrative_ontology:cs_authority_grounding('505a746c-a4bd-4f51-b0cd-4d394380b2ac', lineage).
narrative_ontology:cs_interpretation_layer_present('505a746c-a4bd-4f51-b0cd-4d394380b2ac').
narrative_ontology:cs_reading_relation('505a746c-a4bd-4f51-b0cd-4d394380b2ac', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('505a746c-a4bd-4f51-b0cd-4d394380b2ac', provincial_sovereignty_boundary__compact_federalism, influences).
narrative_ontology:cs_axiom('505a746c-a4bd-4f51-b0cd-4d394380b2ac', foundational, resource_ownership_grounds_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_grounds_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('505a746c-a4bd-4f51-b0cd-4d394380b2ac', resource_ownership_grounds_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('505a746c-a4bd-4f51-b0cd-4d394380b2ac', secondary, federal_policy_touching_provincial_resources_is_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(federal_policy_touching_provincial_resources_is_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('505a746c-a4bd-4f51-b0cd-4d394380b2ac', federal_policy_touching_provincial_resources_is_presumptively_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('505a746c-a4bd-4f51-b0cd-4d394380b2ac', id_1982_patriation_settlement_jurisdictional_grant).
narrative_ontology:cs_drift_state('505a746c-a4bd-4f51-b0cd-4d394380b2ac', post_greenhouse_gas_reference_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('505a746c-a4bd-4f51-b0cd-4d394380b2ac', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_exporting_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_royalty_treasuries).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_extraction_industry).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_apparatus).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_within_provincial_boundaries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_carbon_budget_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers royalty regimes, permitting, and export infrastructure for oil, gas, potash, and mineral resources under s.92A, and reads that ownership clause as license to treat federal carbon pricing, emissions caps, and equalization formulas as extraterritorial incursions on sovereign territory rather than valid federal policy. Collects royalty revenue directly and uses the sovereignty framing to resist federal conditions attached to transfers or infrastructure approval.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_exporting_provincial_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives royalty and lease revenue that funds provincial budgets without equivalent federal claw-back once the sovereignty framing is accepted; benefits directly from any weakening of federal fiscal or environmental conditionality attached to resource development.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_royalty_treasuries, beneficiary,
    institutional, generational, arbitrage, regional).

% Operates under provincial permitting regimes that the sovereignty reading insulates from federal emissions and impact-assessment overlays; benefits from reduced regulatory layering and from the political cover the sovereignty claim provides against federal intervention, while retaining capital mobility across jurisdictions if provincial terms sour.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_extraction_industry, beneficiary,
    organized, biographical, mobile, national).

% Attempts to impose national carbon pricing, emissions targets, and interprovincial pipeline conditions; under this reading, every such measure touching resource development is recast as an illegitimate incursion on provincial territorial sovereignty, forcing the federal government into litigation, reference-case defense, or negotiated retreat to preserve any national climate framework.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_apparatus, payer,
    institutional, generational, constrained, national).

% Bears cross-border emissions, watershed contamination, and market-price externalities generated by resource-exporting provinces' development decisions, without a comparable sovereignty claim of their own to resist the costs; cannot exit the airshed, watershed, or national market they share with the exporting province.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities, payer,
    moderate, biographical, trapped, national).

% Hold treaty and unceded-title claims to the same lands and resources s.92A assigns to provincial ownership; this reading treats provincial sovereignty as prior and absolute, which forecloses or subordinates Indigenous jurisdictional claims to the same territory without their consent, and they are not party to the federal-provincial sovereignty dispute despite bearing its consequences most directly.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_within_provincial_boundaries, excluded,
    powerless, generational, trapped, regional).

% A non-actor policy good — the capacity to plan and meet a national emissions trajectory — that degrades whenever any single province's resource sector is treated as sovereign territory exempt from coordinated federal constraint; listed for completeness, not as a party with agency.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_carbon_budget_coherence, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_carbon_budget_coherence).

% Adjudicates division-of-powers references (e.g. greenhouse gas pricing references) between the sovereignty reading and the subordination reading, producing binding but incomplete resolutions that neither side treats as final and that leave the underlying kernel contest open.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_royalty_treasuries).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provincial control over resource development permits fast, locally-informed decisions about extraction, land use, and royalty-setting without requiring unanimous federal-provincial agreement on every project; s.92A genuinely solves a real coordination problem of who decides on resource development within provincial boundaries.
% TRANSFER_FUNCTION: This reading converts a jurisdictional allocation (who administers resource development) into a sovereignty claim that moves the cost of federal climate coordination and interprovincial externality management onto downstream provinces, the federal treasury's coordination capacity, and Indigenous nations whose title claims are treated as subordinate to provincial ownership.
% ABSENT_VOICES: Indigenous nations holding treaty and unceded title to the same resources are structurally outside the federal-provincial sovereignty dispute; downstream provinces bearing emissions and market externalities have no comparable sovereignty claim to invoke in response and are typically absent from the constitutional argument entirely, appearing only as affected third parties in litigation, if at all.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy reading were abandoned overnight in favor of the subordination reading, federal carbon pricing and impact-assessment conditions would apply to resource development without a sovereignty veto, provincial treasuries would face real federal clawback risk on royalty-linked benefits, and Indigenous title claims would no longer be structurally subordinated to a provincial ownership doctrine that currently forecloses them — a substantial reallocation of who controls resource revenue and its externalities.
% FOUNDING_PROBLEM: s.92A (1982) was enacted to resolve a genuine 1970s-80s dispute over provincial jurisdiction to manage, tax, and export non-renewable resources and electricity, after federal pricing and export-tax interventions during the energy crisis provoked a constitutional crisis over resource control, particularly in Alberta and Saskatchewan.
% FOUNDING_PROBLEM_CORROBORATION: Federal constitutional scholars and the Supreme Court's greenhouse gas pricing reference majority attest that s.92A grants proprietary and management rights over resources within provincial boundaries but does not grant immunity from valid federal legislation of general application (including carbon pricing) — a reading from outside the resource-exporting provinces' own legal apparatus that the sovereignty-primacy claim substantially overstates the clause's founding purpose.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.28 (1982, a genuine jurisdictional settlement) to 0.62 (2024, active resistance to national carbon pricing and impact-assessment regimes) as the reading has been mobilized to contest an expanding range of federal policy instruments. Suppression is moderate (0.4) rather than high: the reading operates mostly through litigation, legislative resolutions, and political mobilization rather than direct coercion, though provincial 'sovereignty act' legislation increases enforcement infrastructure over time. Theater ratio (0.3) reflects that some of the sovereignty rhetoric is genuinely functional (defending real provincial fiscal interests) and some is performative signaling to domestic electorates. Resistance is high (0.7) because federal courts, downstream provinces, and Indigenous nations actively contest the doctrine's expansion — this is not a settled arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the resource-exporting provincial government's seat, s.92A read this way is a coordination mountain — a fixed constitutional fact establishing who controls resources within provincial boundaries, non-negotiable and pre-political. From the federal climate apparatus's seat, the same reading computes as active extraction: a jurisdictional grant has been inflated into an immunity claim that blocks legitimate exercises of federal power of general application, upheld by ongoing litigation and political resistance rather than by settled constitutional consensus. The gap is the story: the engine should show a claimed-mountain-like framing from the agenda-setter seat diverging sharply from a tangled-rope or snare-like computation from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-exporting provincial governments and their treasuries sit at the beneficiary end: they collect royalty revenue and gain a legal shield against federal conditionality. The domestic extraction industry benefits similarly through reduced regulatory overlay, though its mobility gives it more exit than the treasuries have. The federal climate apparatus and downstream provinces sit at the target end: the federal government bears the burden of litigating around the sovereignty claim to preserve any national policy, and downstream provinces bear externalities without a comparable sovereignty defense. Indigenous nations are the most acute victims structurally — their trapped exit options and powerless standing combine with a doctrine that treats their title claims as subordinate to provincial ownership, a cost this reading imposes as a side effect of asserting sovereignty rather than as its stated target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 1970s-80s federal export-tax and pricing interventions provoking a genuine crisis over who controls resource revenue — was substantially resolved by s.92A itself in 1982. The sovereignty-primacy reading's continued and expanding invocation against a different problem (2010s-2020s federal climate policy) suggests the doctrine has been redirected from its founding function toward a new extraction target; the founding_problem_status is marked contested rather than dead because resource-exporting provinces continue to assert live jurisdictional threats (equalization formula grievances, pipeline approval delays) alongside the newer climate-policy target, making the genealogy genuinely disputed rather than a clean case of function-drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s92a_scope_ambiguity,
    'Does s.92A''s grant of provincial ownership and management of natural resources extend to immunity from valid federal legislation of general application (like carbon pricing), or is it limited to proprietary and administrative control within a still-subordinate constitutional order?',
    'Supreme Court division-of-powers jurisprudence, particularly the greenhouse gas pricing references and future litigation over federal impact-assessment and emissions-cap legislation touching resource sectors.',
    'If courts confirm the immunity reading, this constraint''s structural position strengthens toward a settled mountain-like jurisdictional fact; if courts consistently reject it (as the 2021 GGPPA reference majority did), the reading is revealed as a contested and substantially extractive doctrine riding on a much narrower original grant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(s92a_scope_ambiguity, conceptual, 'Whether s.92A grounds sovereignty-as-immunity or only proprietary jurisdiction within a subordinate constitutional order.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the resource_sovereignty_primacy reading the dominant lived framework in resource-exporting provinces, or is it a mobilization strategy deployed selectively against federal climate policy while compact_federalism or a pragmatic subordination reading governs day-to-day intergovernmental relations on other files?',
    'Comparative analysis of provincial government rhetoric and legal argument across policy domains (equalization, healthcare transfers, pipeline approvals, carbon pricing) to determine whether the sovereignty framing is invoked consistently or only when resource revenue is at stake.',
    'If the sovereignty framing is selectively deployed only against climate policy, the constraint''s extraction is more concentrated and instrumental than a general constitutional philosophy would suggest, which would raise the assessed theater_ratio; if it is a consistently held general framework, the extraction is more diffuse and the doctrine more structurally entrenched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the sovereignty reading is a general governing philosophy or an issue-specific mobilization tool.').

omega_variable(
    indigenous_title_subordination_ambiguity,
    'Does the provincial ownership doctrine under s.92A, as read here, structurally require the subordination of unresolved Indigenous title and treaty claims to the same lands, or is that subordination a separable political choice rather than a logical entailment of the sovereignty-primacy reading?',
    'Comparative analysis of court rulings (e.g. Tsilhqot''in, Haida Nation) addressing the relationship between provincial resource ownership and unextinguished Aboriginal title, and provincial government practice regarding free, prior, and informed consent in resource permitting.',
    'If subordination is logically entailed by the sovereignty-primacy reading, the reading''s victim set formally includes Indigenous nations as a structural matter, not an incidental one, strengthening the tangled_rope classification; if separable, the harm to Indigenous nations is better modeled as a distinct, compounding constraint rather than intrinsic to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_title_subordination_ambiguity, conceptual, 'Whether Indigenous title subordination is intrinsic to or separable from the resource-sovereignty-primacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1992, 0.14).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(prov_tr_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.28).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2002, 0.38).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement(prov_be_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1992, 0.22).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2002, 0.25).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement(prov_su_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the provincial_sovereignty_boundary kernel. constitutional_subordination reads provinces as creatures of the federal constitution with no inherent sovereignty (exit requires federal consent); compact_federalism reads Confederation as a compact among sovereign provinces with negotiable exit under duress. All three share the same underlying s.92A text and jurisdictional dispute but author distinct ε, beneficiary/victim structures, and classifications because they instantiate structurally different legitimacy claims, not different measurements of the same claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
