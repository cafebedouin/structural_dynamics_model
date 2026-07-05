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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Resource Sovereignty Primacy Reading of s.92A (Provincial Absolute Sovereignty Claim)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   provincial_sovereignty_boundary kernel: the claim that s.92A's grant of
 *   provincial ownership over natural resources grounds not merely
 *   jurisdictional authority but absolute territorial sovereignty, such that
 *   federal climate and fiscal policy touching resource development is
 *   illegitimate extraction and unilateral exit from Confederation is a live
 *   constitutional right flowing from that sovereignty. This is distinct from
 *   the compact_federalism reading (provinces retain residual sovereignty
 *   from a founding compact, exit negotiable) and the
 *   constitutional_subordination reading (provinces are creatures of the
 *   federal constitution with no inherent sovereignty). The three readings
 *   are separate constraints with separate ε values, linked via
 *   network.affects_constraints — this file does not average across them or
 *   describe the contest internally; it generates only the primacy reading's
 *   own structure.
 *
 * KEY AGENTS:
 *   - resource_exporting_provincial_governments: agenda_setter (institutional/arbitrage) — deploys the sovereignty framing to resist federal climate/fiscal policy
 *   - domestic_extractive_industry: beneficiary (powerful/mobile) — benefits from weakened federal oversight, has more exit than the province itself
 *   - federal_climate_policy_constituencies: payer (organized/constrained) — bears the cost of undermined national climate coordination
 *   - downstream_provinces_bearing_externalities: payer (moderate/trapped) — bears pollution and fiscal externalities with no voice under the primacy framing
 *   - indigenous_nations_with_unresolved_title: excluded (powerless/trapped) — title claims foreclosed by the absolutist ownership framing before adjudication
 *   - federal_government: payer/agenda_setter (institutional/constrained) — loses coordinating authority while remaining nominally responsible for it
 *   - constitutional_scholars_and_courts: observer (analytical) — adjudicate the boundary between resource jurisdiction and sovereignty claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.58).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.42).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Resource Sovereignty Primacy Reading of s.92A (Provincial Absolute Sovereignty Claim)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '1c017759-2641-4dce-8c4b-14e568c94b1b').
narrative_ontology:cs_kernel_codification('1c017759-2641-4dce-8c4b-14e568c94b1b', fixed_text).
narrative_ontology:cs_authority_grounding('1c017759-2641-4dce-8c4b-14e568c94b1b', lineage).
narrative_ontology:cs_interpretation_layer_present('1c017759-2641-4dce-8c4b-14e568c94b1b').
narrative_ontology:cs_reading_relation('1c017759-2641-4dce-8c4b-14e568c94b1b', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('1c017759-2641-4dce-8c4b-14e568c94b1b', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('1c017759-2641-4dce-8c4b-14e568c94b1b', foundational, resource_ownership_constitutes_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_constitutes_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1c017759-2641-4dce-8c4b-14e568c94b1b', resource_ownership_constitutes_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('1c017759-2641-4dce-8c4b-14e568c94b1b', secondary, federal_climate_fiscal_policy_is_extraction_absent_provincial_consent).
narrative_ontology:cs_axiom_status(federal_climate_fiscal_policy_is_extraction_absent_provincial_consent, holdable).
narrative_ontology:cs_axiom_grounding('1c017759-2641-4dce-8c4b-14e568c94b1b', federal_climate_fiscal_policy_is_extraction_absent_provincial_consent, instrumental).
narrative_ontology:cs_reference_frame('1c017759-2641-4dce-8c4b-14e568c94b1b', settled_resource_jurisdiction_1982).
narrative_ontology:cs_drift_state('1c017759-2641-4dce-8c4b-14e568c94b1b', contemporary_climate_federalism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1c017759-2641-4dce-8c4b-14e568c94b1b', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_exporting_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_extractive_industry).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_unresolved_title).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_ownership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes s.92A ownership of natural resources to assert that resource development decisions are an exercise of sovereign, not merely delegated, jurisdiction. Uses this reading to resist federal carbon pricing, environmental assessment triggers, and equalization formulas it characterizes as extraction of provincial wealth. Sets the political agenda for referenda and legal challenges built on the sovereignty framing; captures royalty revenue and political capital from the claim.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_exporting_provincial_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits directly when the sovereignty framing succeeds in blocking or delaying federal climate and emissions policy that would raise compliance costs. Funds advocacy and litigation supporting the primacy reading; can relocate capital across jurisdictions if the provincial regulatory environment becomes unfavorable, giving it more exit than the province itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_extractive_industry, beneficiary,
    powerful, biographical, mobile, continental).

% Bears the cost when national climate targets are undermined by provincial assertions that federal policy is illegitimate interference with sovereign resource jurisdiction. Cannot exit the shared atmospheric and fiscal consequences of delayed federal action regardless of which province asserts the primacy claim; has no comparable constitutional lever to counter it directly.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies, payer,
    organized, generational, constrained, national).

% Receive the pollution, market volatility, and equalization pressure generated by resource-exporting provinces' policy choices without having any say in those choices, because the sovereignty-primacy reading treats resource decisions as purely internal to the owning province. Cannot compel consultation or cost-sharing without triggering the exact federal intervention the reading declares illegitimate.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities, payer,
    moderate, biographical, trapped, national).

% Hold unresolved or overlapping title and treaty claims to the same lands and resources the provincial sovereignty claim treats as unambiguously provincial property. The primacy reading forecloses their standing before it is even litigated, since s.92A ownership is asserted as absolute rather than as subject to prior Aboriginal title; they are not party to the provincial-federal contest but bear its consequences directly.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_unresolved_title, excluded,
    powerless, civilizational, trapped, regional).

% Loses policy-making authority over emissions, interprovincial pipelines, and fiscal transfers to the extent the primacy reading gains legal or political traction, while remaining constitutionally responsible for national coordination it can no longer fully exercise. Also acts as an agenda-setter in the counter-direction, litigating and legislating to preserve federal jurisdiction — making this seat structurally contested rather than purely victimized.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, agenda_setter).

% Adjudicate and comment on the scope of s.92A, distinguishing province-owned-resource-management (settled law) from province-as-sovereign-territorial-entity (contested extension). Their rulings and scholarship are the primary mechanism by which the primacy reading's overreach, if any, becomes visible.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_exporting_provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: s.92A genuinely coordinates one real thing: it settles which order of government administers Crown lands, mines, and forestry resources within a province, avoiding duplicate or conflicting resource-management regimes between federal and provincial administrations.
% TRANSFER_FUNCTION: The primacy reading moves political and fiscal authority away from federal climate and equalization mechanisms and concentrates it in resource-exporting provincial governments and the industries that benefit from lighter federal oversight; it also moves standing away from Indigenous nations whose title claims predate and are not extinguished by provincial resource ownership.
% ABSENT_VOICES: Indigenous nations with unresolved title are structurally outside the provincial-federal contest the sovereignty-primacy reading stages, yet the reading's absolutist framing of ownership directly forecloses their claims without adjudicating them. Downstream provinces bearing pollution and market externalities have no seat in a framing that treats resource decisions as purely internal to the owning jurisdiction.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy reading were abandoned tomorrow (replaced by, e.g., constitutional_subordination), resource-exporting provinces would lose their strongest available argument against federal climate conditionality and equalization formulas; litigation strategy, referendum campaigns, and industry advocacy currently built on the sovereignty framing would need to be reconstructed on narrower jurisdictional grounds. Federal policy space would expand immediately.
% FOUNDING_PROBLEM: s.92A itself was added in 1982 to resolve genuine uncertainty and interprovincial friction over provincial taxation and management authority over non-renewable resources, electrical energy, and forestry, after cases like CIGOL had narrowed provincial resource powers. The underlying problem was clarifying resource jurisdiction, not establishing territorial sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and the drafting record (federal-provincial negotiations preceding the 1982 patriation) attest that s.92A was a jurisdictional clarification over resource management and indirect taxation, not a sovereignty declaration. This corroboration comes from outside the resource-exporting provinces and extractive industry that now benefit from the expanded sovereignty reading; the provinces asserting primacy are themselves the interested party disputing this narrower genealogy.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the primacy reading, while riding on the genuine coordination function of settled resource-jurisdiction law, extends that settled function into a much larger claim (territorial sovereignty, illegitimacy of federal fiscal/climate policy, unilateral exit) that transfers real authority and cost away from federal constituencies, downstream provinces, and Indigenous claimants. Suppression (0.42) is moderate: the claim is advanced through litigation, referenda, and political mobilization rather than direct coercion, but it does function to foreclose Indigenous title claims and downstream-province voice without their participation. Theater ratio (0.28) is moderate-low: real royalty revenue and real policy divergence are at stake, not pure performance, though referendum campaigns built on the sovereignty claim contain a rising performative component. Resistance (0.72) is high because federal governments, constitutional scholars, and Indigenous nations actively contest the sovereignty extension in courts and public discourse — this is a live, contested reading, not a settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-exporting provincial governments and the extractive industry sit near the beneficiary end: the sovereignty-primacy framing is the mechanism by which they resist externally imposed costs (carbon pricing, equalization) and it directly serves their institutional and commercial interests. Federal climate constituencies and downstream provinces sit near the target end: they bear diffuse, uninternalized costs and have no reciprocal sovereignty claim to invoke. Indigenous nations sit at the most extreme target end despite not being a nominal party to the provincial-federal contest, because the absolutist ownership claim structurally forecloses their standing — this is the clearest case in the story of a directionality override candidate, since the derivation from beneficiary/victim declarations alone would treat them as merely 'excluded' rather than actively foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clarifying resource-management jurisdiction after CIGOL-era uncertainty) is largely resolved as a jurisdictional matter — the world has settled which order of government administers resource royalties and management day to day. What persists and has grown is the much larger sovereignty claim layered on top, used against founding-problem-unrelated targets (federal climate policy, equalization, Indigenous title). This is a mandatrophy pattern: an old, resolved coordination problem is invoked to legitimate a new extractive claim that was never litigated as such. Classifying this reading as tangled_rope rather than snare preserves the fact that SOME genuine coordination (avoiding dual resource administration) remains real, while flagging that the primacy extension is where the extraction lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_ownership_vs_territorial_sovereignty_conflation,
    'Does s.92A''s grant of provincial ownership and management authority over natural resources logically entail territorial sovereignty in the sense of a right to resist federal law and to exit Confederation unilaterally, or is this a rhetorical extension of a narrower property/jurisdiction right?',
    'Comparative constitutional analysis of the 1982 drafting record and subsequent jurisprudence (e.g., interprovincial trade and environmental assessment cases) testing whether courts have ever treated s.92A ownership as displacing federal jurisdiction over matters with extraprovincial effects (climate, interprovincial pipelines).',
    'If courts and the drafting record decisively reject the sovereignty extension, this reading is a false summit dressed as settled constitutional law rather than a live contested reading — reclassification toward snare becomes more defensible. If courts have genuinely left the sovereignty question open, tangled_rope with contested status remains the more accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_ownership_vs_territorial_sovereignty_conflation, conceptual, 'Whether resource ownership under s.92A structurally entails territorial sovereignty, or whether this is an extractive rhetorical extension of settled jurisdictional law.').

omega_variable(
    indigenous_title_priority_ambiguity,
    'Is provincial resource ownership under s.92A subject to prior, unextinguished Aboriginal title, or does the primacy reading''s assertion of absolute provincial ownership implicitly (and prematurely) foreclose title claims that remain constitutionally and legally unresolved?',
    'Track outcomes of pending and future title litigation (post-Tsilhqot''in framework applications) in resource-rich provinces asserting the sovereignty-primacy reading; if courts consistently find s.92A ownership subordinate to unresolved title, the primacy reading''s foreclosure of Indigenous standing is legally incorrect, not merely contested.',
    'Resolution in favor of title priority would substantially increase the measured suppression/extraction attributable to this reading, since it would confirm the reading operates by foreclosing a legally superior claim rather than merely competing with a co-equal one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_title_priority_ambiguity, empirical, 'Whether the primacy reading''s ownership claim is legally subordinate to unresolved Aboriginal title, making its exclusion of Indigenous nations a structural error rather than a contested framing choice.').

omega_variable(
    unilateral_exit_right_naturalness,
    'Is the claimed unilateral constitutional right to exit Confederation, grounded in resource sovereignty, a genuine (if currently unexercised) constitutional entitlement, or is it a constructed political claim with no support in patriation-era constitutional text or subsequent jurisprudence (cf. the Secession Reference''s negotiated-exit framework)?',
    'Compare this reading''s unilateral-exit claim against the Supreme Court''s 1998 Secession Reference, which required negotiation rather than recognizing unilateral secession even where a clear referendum majority exists.',
    'If unilateral exit has no jurisprudential support, the ''illegitimate extraction'' framing of federal policy under this reading loses its strongest structural claim, and the reading''s extractiveness score should be understood as resting on political mobilization rather than settled constitutional entitlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_exit_right_naturalness, empirical, 'Whether the sovereignty-primacy reading''s unilateral exit claim has jurisprudential grounding or is a constructed political assertion inconsistent with existing secession jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(prov_tr_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(prov_tr_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.22).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement(prov_be_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(prov_be_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(prov_su_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(prov_su_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__compact_federalism).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the provincial_sovereignty_boundary kernel. resource_sovereignty_primacy (this file) claims the strongest sovereignty extension, grounded specifically in s.92A resource ownership; constitutional_subordination claims provinces have no inherent sovereignty and treats federal authority as unqualified; compact_federalism claims residual sovereignty from a founding interprovincial compact with negotiable exit. The three readings are not measurement variants of one constraint — they have different beneficiary/victim structures, different ε trajectories, and different legal genealogies. resource_sovereignty_primacy forecloses constitutional_subordination directly (a province cannot simultaneously be sovereign-by-resource-ownership and a creature of the federal constitution with no inherent sovereignty in the same legal framework) while coexisting with compact_federalism (both readings assert some form of provincial sovereignty, differing mainly on its textual source and the conditions for exit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
