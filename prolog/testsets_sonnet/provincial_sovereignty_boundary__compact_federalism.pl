% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Compact Federalism Reading of the Provincial Sovereignty Boundary
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story instantiates the compact_federalism reading of the
 *   provincial_sovereignty_boundary kernel: Confederation is understood as a
 *   treaty-like compact among pre-existing sovereign provinces that retained
 *   residual sovereignty rather than surrendering it to a newly created
 *   federal sovereign. Under this reading, federal authority operates
 *   conditionally on provincial consent, equalization is a negotiated fiscal
 *   arrangement rather than a constitutional entitlement, provinces may
 *   override national climate policy within their jurisdiction, and secession
 *   requires negotiation rather than federal permission. This is a distinct
 *   constraint from constitutional_subordination (which treats provinces as
 *   constitutional creatures with no inherent sovereignty) and from
 *   resource_sovereignty_primacy (which grounds sovereignty specifically in
 *   s.92A resource ownership rather than in the founding compact itself) —
 *   those are separate stories with separate epsilon values, linked here only
 *   by kernel membership.
 *
 * KEY AGENTS:
 *   - resource_exporting_provinces: Primary beneficiary (powerful/arbitrage) — uses compact framing as negotiating leverage over equalization and climate policy
 *   - have_not_provinces: Primary payer (moderate/constrained) — depends on equalization transfers that become negotiable rather than guaranteed under this reading
 *   - federal_government: Agenda-setter constrained by the reading (institutional/constrained) — retains nominal supremacy but must govern through negotiated intergovernmental agreement
 *   - indigenous_nations_outside_the_compact: Excluded party (powerless/trapped) — has no seat in a framework organized entirely around the province-federal bargain
 *   - courts_and_constitutional_scholars: Analytical observer — adjudicates individual disputes without resolving the underlying kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.42).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.38).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Reading of the Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, 'f17d8442-be4b-423f-9294-fa7e38d16cc4').
narrative_ontology:cs_kernel_codification('f17d8442-be4b-423f-9294-fa7e38d16cc4', distributed).
narrative_ontology:cs_authority_grounding('f17d8442-be4b-423f-9294-fa7e38d16cc4', distributed).
narrative_ontology:cs_reading_relation('f17d8442-be4b-423f-9294-fa7e38d16cc4', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('f17d8442-be4b-423f-9294-fa7e38d16cc4', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('f17d8442-be4b-423f-9294-fa7e38d16cc4', foundational, provinces_are_pre_existing_sovereigns).
narrative_ontology:cs_axiom_status(provinces_are_pre_existing_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('f17d8442-be4b-423f-9294-fa7e38d16cc4', provinces_are_pre_existing_sovereigns, conventional).
narrative_ontology:cs_axiom('f17d8442-be4b-423f-9294-fa7e38d16cc4', secondary, federal_authority_is_consent_conditional).
narrative_ontology:cs_axiom_status(federal_authority_is_consent_conditional, holdable).
narrative_ontology:cs_axiom_grounding('f17d8442-be4b-423f-9294-fa7e38d16cc4', federal_authority_is_consent_conditional, conventional).
narrative_ontology:cs_reference_frame('f17d8442-be4b-423f-9294-fa7e38d16cc4', colonial_confederation_treaty_bargain).
narrative_ontology:cs_drift_state('f17d8442-be4b-423f-9294-fa7e38d16cc4', post_1982_patriation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f17d8442-be4b-423f-9294-fa7e38d16cc4', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments_generally).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, have_not_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_climate_policy_coherence).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_outside_the_compact).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, provincial_residual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, confederal_compact_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold s.92A resource ownership and treat the compact reading as license to override federal climate and equalization policy that would tax or constrain resource extraction. Use the negotiated-exit premise as leverage in intergovernmental bargaining — threatening non-cooperation rather than actual secession, which they do not need to pursue because the compact framing already gives them a veto-adjacent position at the negotiating table.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces, agenda_setter).

% Depend on federal equalization transfers, which the compact reading treats as negotiable rather than a constitutional entitlement. When resource-rich provinces successfully resist equalization formula changes or contribution levels by invoking sovereign-compact logic, the fiscal transfer these provinces rely on becomes a bargaining chip rather than a settled obligation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, have_not_provinces, payer,
    moderate, biographical, constrained, national).

% A national carbon framework or emissions target cannot bind uniformly if any province can invoke residual sovereignty to override it provincially. The coherence of the policy as a national instrument is the thing that erodes each time a province successfully asserts an override; it has no seat of its own to negotiate from.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_climate_policy_coherence, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(provincial_sovereignty_boundary__compact_federalism, federal_climate_policy_coherence).

% Must negotiate with provinces as quasi-sovereign counterparties rather than administer them as constitutional subordinates. Retains formal constitutional supremacy on paper but exercises it only through negotiated intergovernmental agreements, conferences, and side payments — every unilateral federal move risks being read as a breach of the compact and triggering provincial non-cooperation or litigation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, payer).

% Were never parties to the provincial compact and are not seated at federal-provincial negotiating tables, yet resource decisions made under provincial sovereignty claims (s.92A extraction rights) directly affect treaty and unceded territory. The compact_federalism reading, by grounding legitimacy in the province-federal bargain, structurally has no slot for a third sovereignty claim predating both.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_outside_the_compact, excluded,
    powerless, generational, trapped, regional).

% Adjudicate reference cases and produce competing doctrinal accounts of whether Confederation was a compact of pre-existing sovereigns or a constitutional creation. Their rulings do not settle the kernel dispute so much as arbitrate individual disputes while leaving the underlying reading contested.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, courts_and_constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables provinces with divergent resource endowments, economic structures, and political cultures to remain within a single federation by treating federal authority as conditional on provincial consent rather than imposed — avoiding secession by making exit a negotiated, not forced, event, and by allowing provincial override on contentious files like climate policy.
% TRANSFER_FUNCTION: Moves bargaining leverage from the federal government and have-not provinces toward resource-rich provinces: equalization obligations, national climate targets, and federal fiscal transfers all become subject to provincial consent rather than settled entitlement, shifting fiscal and regulatory outcomes toward whichever provinces can credibly threaten non-cooperation.
% ABSENT_VOICES: Indigenous nations whose territorial and resource sovereignty claims predate and are structurally independent of both provincial and federal sovereignty are not seated in the province-federal bargain the compact reading organizes around; their objection — that neither province nor federal government has clean title to negotiate resource sovereignty over unceded land — has no slot in this reading's framework.
% DISAPPEARANCE_RATIONALE: If the compact reading were abandoned in favor of strict constitutional subordination, federal climate policy could be imposed uniformly without provincial override, equalization would revert to a constitutional entitlement rather than a negotiated item, and resource-rich provinces would lose their negotiating leverage overnight — the entire architecture of executive federalism (first ministers' conferences, intergovernmental side-agreements, opt-out provisions) that this reading legitimizes would lose its foundation.
% FOUNDING_PROBLEM: At Confederation, several colonies with distinct economies, legal traditions, and degrees of reluctance about union needed a framework that would secure their participation without them fearing absorption into a unitary state that could override their local interests unilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Resource-exporting provinces and provincial governments generally attest the founding problem remains live — citing ongoing regional economic divergence and the risk of federal overreach on resource and climate files. Federal constitutional scholars and courts (an outside, non-beneficiary corroborating source) are split: some treat the compact theory as historically contestable revisionism serving present-day provincial resource interests rather than a settled account of 1867; the founding-era historical record itself is genuinely ambiguous, which is precisely what sustains the kernel contest.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).
:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects the reading's real but partial redistribution of leverage toward resource-rich provinces at the expense of have-not provinces and national policy coherence — moderate rather than severe because the coordination function (avoiding secession, accommodating regional diversity) is genuine and not merely cover. Suppression (0.38) is comparatively low: the compact reading does not primarily operate through coercion of dissenters but through structuring which claims are treated as legitimate starting points in negotiation — its force is normative and institutional rather than coercive. Theater ratio (0.31) captures that a meaningful share of intergovernmental conference activity is now performative reaffirmation of provincial standing rather than substantive bargaining, and this share has grown as the reading became institutionalized. The measurement series shows gradual intensification from 1867 through 1982 (Constitution Act, s.92A entrenchment) to the present, tracking the reading's increasing use as leverage in equalization and climate disputes rather than a sharp break.
 *
 * PERSPECTIVAL GAP:
 *   From the resource-exporting-province seat, this reading describes genuine constitutional principle — the honest historical account of how a reluctant federation came together. From the have-not-province or federal-coherence seat, the same reading operates as an extraction mechanism: a doctrine invoked selectively when it favors resisting redistribution or national standards, rarely invoked to justify accepting federal authority. The engine should register this divergence as the tangled_rope signature — real coordination function (avoiding a fractured or coerced federation) coexisting with asymmetric extraction (leverage concentrated in resource-rich provinces) sustained by active enforcement (repeated invocation in litigation and intergovernmental negotiation).
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-exporting provinces sit near the beneficiary end: the compact reading converts their existing resource wealth into negotiating leverage over federal policy, and their exit options are effectively arbitrage-grade (credible non-cooperation threats without needing to actually secede). Have-not provinces sit toward the target end: their fiscal security depends on equalization remaining a settled entitlement, which this reading destabilizes into something negotiable. The federal government is agenda-setter in form but structurally constrained in practice — it sets the formal agenda but cannot execute policy against a resistant province without triggering the compact's central premise that consent is required. Indigenous nations are excluded entirely from directionality computation on the province-federal axis because the reading's framework has no seat for a sovereignty claim that precedes and does not derive from the founding compact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing participation of reluctant, economically divergent colonies without unitary absorption — was substantially resolved by the mid-20th century as federal institutions matured and provincial identities stabilized within the federation. Its persistence as an active, intensifying doctrine (rather than a settled historical fact) into 2025, especially around resource and climate policy, suggests the founding problem's status is now 'contested' rather than cleanly 'dead': it is invoked with renewed intensity precisely where it serves resource-rich provinces' present-day fiscal and regulatory interests, which is the mandatrophy signature — an arrangement whose justifying problem has receded but whose invocation has not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_theory_historical_accuracy,
    'Is the compact theory of Confederation an accurate historical account of the 1867 founding, or a retrospective doctrinal construction serving present-day provincial resource interests?',
    'Historical review of the confederation debates, colonial office correspondence, and contemporaneous legal opinion from the 1860s-1870s to establish whether founding participants themselves understood the arrangement as a compact of sovereigns versus a unitary constitutional creation.',
    'If the compact reading is historically accurate, this constraint functions closer to a genuine (if contested) coordination mechanism reflecting founding intent. If it is a later doctrinal construction, the reading functions closer to a legitimating narrative retrofitted onto present-day extraction of leverage by resource-rich provinces — pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compact_theory_historical_accuracy, empirical, 'Whether compact_federalism reflects genuine founding-era understanding or retrospective construction.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does compact_federalism currently dominate practical intergovernmental negotiation over constitutional_subordination and resource_sovereignty_primacy, and does that dominance track legal merit or provincial bargaining power?',
    'Track which reading is invoked by which actors in which disputes over time (equalization negotiations, climate policy disputes, resource royalty disputes) and correlate invocation patterns with provincial fiscal capacity and bargaining leverage rather than with doctrinal consistency.',
    'If reading selection tracks bargaining power rather than legal merit, the kernel contest itself is partly endogenous to the extraction this reading produces — provinces select whichever reading currently serves their negotiating position, which would deepen rather than resolve the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the choice among sibling readings is driven by legal reasoning or by the bargaining power of the parties invoking each reading.').

omega_variable(
    indigenous_sovereignty_exclusion_resolvability,
    'Can the compact_federalism framework be extended to seat indigenous nations as a third sovereignty claim, or does the two-party (province/federal) structure of the compact narrative structurally preclude this without becoming a different kernel reading entirely?',
    'Examine whether existing tripartite negotiation forums (e.g. modern treaty processes, self-government agreements) that include indigenous nations as parties operate under compact_federalism logic or under a distinct framework not captured by any of the three declared kernel readings.',
    'If the exclusion is structurally irreducible within compact_federalism, this constraint''s coordination claim is narrower than it presents — it coordinates a two-party bargain while excluding a necessary third party from land and resource decisions, which strengthens the case that indigenous_nations_outside_the_compact should be treated as excluded rather than merely underrepresented.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_exclusion_resolvability, conceptual, 'Whether the province-federal compact structure can accommodate indigenous sovereignty claims or excludes them by design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.15).
narrative_ontology:measurement(prov_tr_t1949, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1949, 0.18).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.22).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.2).
narrative_ontology:measurement(prov_be_t1949, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1949, 0.24).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.3).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.25).
narrative_ontology:measurement(prov_su_t1949, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1949, 0.28).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.33).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the provincial_sovereignty_boundary kernel. constitutional_subordination instantiates the opposing premise (provinces as constitutional creatures, no inherent sovereignty) and stands in a forecloses relationship to this reading. resource_sovereignty_primacy instantiates a narrower, resource-specific grounding for provincial power that typically coexists with and is deployed alongside this reading by the same provincial actors. Each reading carries its own epsilon, beneficiary/victim structure, and classification per the epsilon-invariance principle; they are linked here for contamination and coupling analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
