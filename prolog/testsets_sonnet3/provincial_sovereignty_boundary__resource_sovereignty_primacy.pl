% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Resource Sovereignty Primacy Reading of Provincial Ownership (s.92A)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story instantiates ONE contested reading of the provincial
 *   sovereignty boundary kernel: that provincial ownership of natural
 *   resources under s.92A of the Constitution Act 1982 grounds a form of
 *   absolute territorial sovereignty, such that resource control is
 *   equivalent to statehood, federal climate and fiscal policy touching
 *   resources is illegitimate extraction, and unilateral exit from the
 *   federation is a constitutional right flowing from that sovereignty. This
 *   is not the only reading of the same constitutional text — sibling
 *   readings (compact_federalism, constitutional_subordination) are separate
 *   constraint stories with their own ε and stakeholder structures. This
 *   story authors ONLY the sovereignty-primacy reading, cleanly, without
 *   averaging across readings.
 *
 * KEY AGENTS:
 *   - resource_producing_provincial_governments: agenda_setter/beneficiary (institutional/arbitrage) — administers resource jurisdiction and deploys sovereignty framing against federal policy
 *   - domestic_resource_extraction_industry: beneficiary (organized/mobile) — gains regulatory insulation from the sovereignty framing
 *   - federal_government: payer/excluded (institutional/constrained) — loses practical policy capacity under the framing
 *   - downstream_provinces_bearing_externalities: payer (moderate/trapped) — absorbs external costs with no seat at the table
 *   - indigenous_nations_with_overlapping_territorial_claims: excluded (powerless/trapped) — prior title claims erased by the two-settler-government framing
 *   - constitutional_courts: observer (institutional/analytical) — adjudicates but cannot definitively settle the political contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.58).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.42).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Resource Sovereignty Primacy Reading of Provincial Ownership (s.92A)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '4792df87-7a0d-412f-bd8e-9d73b0072198').
narrative_ontology:cs_kernel_codification('4792df87-7a0d-412f-bd8e-9d73b0072198', fixed_text).
narrative_ontology:cs_authority_grounding('4792df87-7a0d-412f-bd8e-9d73b0072198', lineage).
narrative_ontology:cs_interpretation_layer_present('4792df87-7a0d-412f-bd8e-9d73b0072198').
narrative_ontology:cs_reading_relation('4792df87-7a0d-412f-bd8e-9d73b0072198', provincial_sovereignty_boundary__compact_federalism, influences).
narrative_ontology:cs_reading_relation('4792df87-7a0d-412f-bd8e-9d73b0072198', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_axiom('4792df87-7a0d-412f-bd8e-9d73b0072198', foundational, resource_ownership_constitutes_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_constitutes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4792df87-7a0d-412f-bd8e-9d73b0072198', resource_ownership_constitutes_sovereignty, conventional).
narrative_ontology:cs_axiom('4792df87-7a0d-412f-bd8e-9d73b0072198', secondary, unilateral_exit_flows_from_resource_control).
narrative_ontology:cs_axiom_status(unilateral_exit_flows_from_resource_control, holdable).
narrative_ontology:cs_axiom_grounding('4792df87-7a0d-412f-bd8e-9d73b0072198', unilateral_exit_flows_from_resource_control, conventional).
narrative_ontology:cs_reference_frame('4792df87-7a0d-412f-bd8e-9d73b0072198', resource_ownership_as_statehood_attribute).
narrative_ontology:cs_drift_state('4792df87-7a0d-412f-bd8e-9d73b0072198', post_2015_climate_federalism_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('4792df87-7a0d-412f-bd8e-9d73b0072198', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_resource_extraction_industry).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_overlapping_territorial_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers resource leases, royalties, and development permitting under s.92A, and asserts that this ownership power constitutes a form of sovereignty coextensive with statehood. Uses this reading to resist federal carbon pricing, emissions caps, and equalization formulas as illegitimate incursions on a sovereign domain, and periodically invokes it in threats of unilateral secession or non-compliance with national fiscal frameworks. Retains substantial revenue and political leverage under this framing that a narrower property-rights reading would not provide.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments, beneficiary).

% Benefits directly when provincial resource authority is read as sovereign and therefore insulated from federal environmental or climate regulation. Lobbies for the sovereignty framing because it forecloses a class of federal intervention that a mere jurisdictional-division reading would still permit. Can relocate capital across jurisdictions if the framing weakens, giving it real exit even as it argues for the strongest version of provincial claims.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, domestic_resource_extraction_industry, beneficiary,
    organized, biographical, mobile, continental).

% Holds constitutional authority over interprovincial trade, treaty implementation, and (contested) national climate commitments, but under this reading any federal instrument that touches resource development is recast as extraction from a sovereign province rather than ordinary federal jurisdiction. Bears the political and fiscal cost of policy paralysis and litigation, and cannot use its formal constitutional levers without being framed as an aggressor against sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, excluded).

% Absorb the environmental, market, and equalization consequences of upstream resource decisions made unilaterally under the sovereignty framing, with no seat in the resource-owning province's permitting process. Cannot exit the federation to escape these externalities and have no comparable sovereignty claim of their own to invoke in response.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, downstream_provinces_bearing_externalities, payer,
    moderate, generational, trapped, national).

% Hold pre-existing and often unextinguished title and treaty rights over the same lands s.92A assigns to provincial ownership. The sovereignty-primacy reading treats provincial resource ownership as absolute and prior, which structurally erases or subordinates Indigenous sovereignty claims that predate the provinces themselves. Their objections are litigated case by case but the sovereignty framing itself is rarely put to them as a question.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_with_overlapping_territorial_claims, excluded,
    powerless, civilizational, trapped, regional).

% Voters and organizations seeking a coordinated national emissions framework find that any such coordination is contested at the threshold as an assault on provincial sovereignty rather than debated on its regulatory merits, so the framing itself forecloses the policy conversation before it starts.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_constituencies, payer,
    moderate, civilizational, constrained, national).

% Adjudicates disputes between the provincial ownership reading and federal jurisdictional claims, drawing on precedent that has generally treated s.92A as a division-of-powers provision rather than a sovereignty grant. Its rulings shape which reading of the kernel carries practical legal weight, though it cannot resolve the underlying political contest definitively.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provincial ownership of resources under s.92A does solve a real coordination problem: it assigns clear administrative jurisdiction over resource development, leasing, and royalties to a single government closest to the resource, avoiding overlapping federal-provincial permitting chaos.
% TRANSFER_FUNCTION: The sovereignty-primacy reading moves the practical capacity to set national climate and fiscal policy away from federal institutions and downstream/externality-bearing provinces, and toward resource-producing provincial governments and the industries operating within them; it also moves recognition and priority away from Indigenous nations whose title claims predate provincial ownership.
% ABSENT_VOICES: Indigenous nations with overlapping or prior title are structurally outside the provincial-vs-federal sovereignty contest as staged; the framing treats the dispute as a two-party contest between orders of settler-colonial government, which is itself an act of exclusion this reading does not surface. Downstream provinces bearing externalities also have no formal voice in the resource-owning province's decisions.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy reading were abandoned in favor of a narrower jurisdictional reading, federal climate and fiscal instruments touching resources would no longer face a threshold sovereignty objection, national coordination on emissions and equalization would become politically and legally easier, and unilateral secession threats grounded in resource ownership would lose their constitutional anchor.
% FOUNDING_PROBLEM: The 1982 patriation settlement needed to resolve long-standing Western Canadian grievances about federal control over resource pricing and export (notably post-1970s energy conflicts), by giving provinces clear, unambiguous jurisdiction over their own resources.
% FOUNDING_PROBLEM_CORROBORATION: Resource-producing provincial governments and industry attest the sovereignty reading is a live and necessary bulwark against federal overreach. Independent constitutional scholars, federal officials, and several Supreme Court rulings (interpreting s.92A as an ordinary division-of-powers amendment, not a sovereignty grant) attest that the founding problem was administrative jurisdiction over resource management, not a grant of statehood-level sovereignty, and that the sovereignty framing is a later political extension beyond the drafters' intent.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the sovereignty-primacy reading, as a political-legal instrument, does real extractive work: it forecloses federal climate/fiscal coordination and Indigenous title recognition by recasting them as sovereignty violations, transferring practical policy capacity to resource-producing provinces and their industries. Suppression (0.42) is moderate rather than high because courts have generally not endorsed the strong sovereignty reading, leaving real (if costly) avenues for federal action and Indigenous litigation — the suppression is more rhetorical/political than fully coercive. Theater ratio (0.3) captures that a genuine coordination function (clear administrative jurisdiction over resource permitting) persists underneath the sovereignty rhetoric, but an increasing share of invocation is performative — used in political messaging and negotiating leverage rather than in actual administration of resources. Accessibility collapse (0.4) is moderate: alternative readings of s.92A remain legally live and are actively litigated, so alternatives have not collapsed as they would for a genuine natural-law constraint. Resistance (0.72) is high because federal governments, downstream provinces, climate constituencies, and Indigenous nations all actively contest this reading in courts, legislatures, and public discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the resource-producing province's seat, this is straightforward exercise of a constitutionally granted property right elevated (correctly, in their view) to sovereign status — a rope, even a mountain-adjacent natural entitlement. From the federal government's and downstream provinces' seats, the same textual provision is being read expansively to achieve a political-extraction outcome — coordination on paper, extraction in operation — hence the tangled_rope claim from the authoring seat, which is expected to diverge from the agenda-setter's own self-perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-producing provincial governments and their aligned extraction industry sit near the beneficiary end: the sovereignty framing directly expands their effective jurisdiction and insulates them from federal cost-imposition. The federal government, downstream provinces, and climate constituencies sit near the target end: their formal constitutional levers are recast as illegitimate under this reading, and they bear the coordination costs of climate policy paralysis without a comparable sovereignty claim to invoke back. Indigenous nations are the deepest target: their prior title is not merely burdened but conceptually erased by a framing that treats resource sovereignty as flowing from provincial statehood rather than acknowledging older claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear jurisdiction over resource administration, resolving 1970s-era federal-provincial energy conflicts) is largely live in its narrow form — provinces do need administrative clarity over resource development. But the sovereignty-primacy EXTENSION of that founding problem — treating resource ownership as equivalent to statehood-level sovereignty with a unilateral exit right — was never corroborated by the drafters' intent or by subsequent jurisprudence; it is a later political overlay on a settled administrative arrangement. This is exactly the mandatrophy risk the classification should catch: mislabeling a narrow, resolved coordination function (permitting jurisdiction) as an unresolved sovereignty question that licenses ongoing extraction and secession threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s92a_scope_ambiguity,
    'Does s.92A''s grant of provincial resource ownership constitute a genuine sovereignty attribute (statehood-adjacent), or is it an ordinary division-of-powers provision assigning administrative jurisdiction, later reinterpreted expansively for political leverage?',
    'Constitutional drafting history, Supreme Court jurisprudence on the scope of s.92A relative to federal trade-and-commerce and treaty powers, and comparative analysis of how similar resource-ownership clauses function in other federations without triggering sovereignty claims.',
    'If s.92A is genuinely a narrow administrative grant, the sovereignty-primacy reading is a constructed extraction dressed as constitutional necessity (supporting the tangled_rope/possible snare-adjacent classification). If courts eventually recognize an expansive sovereignty reading, the constraint moves toward genuine coordination with strong exit protections for the province.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(s92a_scope_ambiguity, conceptual, 'Whether s.92A textually supports sovereignty-primacy or only administrative jurisdiction — the core interpretive fork this reading depends on.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the disagreement between this reading and its siblings (compact_federalism, constitutional_subordination) live — in the text of s.92A itself, in the theory of confederation''s founding (compact vs. constitutional creation), or in a downstream political strategy that uses whichever reading currently serves resource-producing provinces'' interests?',
    'Track whether resource-producing provinces consistently invoke sovereignty-primacy or switch fluidly between this reading and compact_federalism depending on which serves the immediate political goal (e.g., equalization disputes vs. climate policy disputes) — instrumental switching would indicate the disagreement is strategic rather than doctrinal.',
    'If provinces switch readings opportunistically, this substantially raises confidence that resource_sovereignty_primacy functions as a tool of extraction rather than a sincerely held constitutional theory, supporting a higher effective extraction reading than the text alone would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame ambiguity: whether the reading choice tracks doctrine or political convenience.').

omega_variable(
    indigenous_title_priority_unresolved,
    'Does provincial resource ownership under s.92A operate subject to, or in erasure of, unextinguished Indigenous title and treaty rights over the same territory?',
    'Ongoing litigation (e.g., title and consultation case law) testing whether s.92A ownership is legally prior to or subordinate to Aboriginal title claims not extinguished by treaty.',
    'If courts affirm Indigenous title as prior and unextinguished, the sovereignty-primacy reading''s claim to ''absolute'' provincial sovereignty is structurally false even on its own terms, sharply raising the extraction and suppression the reading imposes on Indigenous nations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_title_priority_unresolved, empirical, 'Whether provincial resource sovereignty is legally absolute or subordinate to prior Indigenous title.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(prov_tr_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(prov_tr_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.28).
narrative_ontology:measurement(prov_be_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(prov_be_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(prov_be_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(prov_su_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1990, 0.24).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(prov_su_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2010, 0.34).
narrative_ontology:measurement(prov_su_t2018, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the provincial_sovereignty_boundary kernel. compact_federalism and constitutional_subordination are separate constraint files with their own ε, beneficiaries, and stakeholder structures, representing structurally different claims about what a province IS under confederation. All three should carry mutual network links; this file links outward to both siblings per the kernel decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
