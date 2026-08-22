% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundary Practice as Locally Renegotiated Coordination Norm (Localized Practice Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   In thousands of local communities across the subcontinent, birth-group
 *   (jati) boundaries organize marriage pooling, mutual aid, ritual
 *   cooperation, and occupational transmission. On this reading — the
 *   localized_practice_reading of the jati_practice_norm kernel — those
 *   boundaries are not fixed by scripture and not frozen by any state
 *   apparatus: they are maintained by local practice, renegotiated case by
 *   case (a feast rank adjusted here, a marriage circle widened there), and
 *   they proliferate — the category count has grown into the thousands
 *   precisely because splits, merges, and rank claims continuously multiply
 *   the boundaries rather than eroding them. Enforcement is weak: councils
 *   cajole more than they coerce, and modern courts have stripped
 *   caste-council penalties of legal force. The costs that remain fall on
 *   specific seats — suitors whose chosen marriages cross a line, women
 *   narrowed by hypergamy rules, lower-ranked groups who owe deference —
 *   while the coordination benefits accrue broadly to members. This file is
 *   one member of a three-story constraint family; the
 *   orthodox_textual_reading and the colonial_census_reading instantiate the
 *   same kernel with their own epsilon, enforcement seats, and victim sets,
 *   linked through network.affects_constraints. Assumption note: the 120-unit
 *   interval is read as roughly 1900 to the present, and all epsilon values
 *   here are authored only over this reading's referent — the standing
 *   arrangement of locally practiced, locally renegotiated jati boundary
 *   maintenance — assessed by this reading's own lights.
 *
 * KEY AGENTS:
 *   - jati_council_elders: agenda-setter (organized/identity_locked, local) — presides over boundary adjudication and marriage recognition; collects adjudication standing and ritual precedence
 *   - jati_members: primary beneficiary (moderate/constrained, regional) — draws marriage pooling, mutual aid, and ceremonial cooperation from boundary closure
 *   - hereditary_occupational_specialists: beneficiary (moderate/constrained, regional) — skill transmission and reputation certification run through jati networks
 *   - locally_dominant_jatis: top beneficiary (powerful/constrained, regional) — receives the deference and service flow at collective ceremonies
 *   - cross_boundary_marriage_suitors: primary target (powerless/trapped, local) — preferred marriages blocked by the boundary; sometimes released by fission
 *   - women_under_hypergamy_rules: target (powerless/trapped, regional) — bear the asymmetric costs of endogamy
 *   - lower_status_local_jatis: dual-positioned target (organized/constrained, regional) — pay deference upward while drawing coordination benefits from the same structure
 *   - jati_exit_communities: excluded voice (moderate/mobile, regional) — demonstrate exit survivability; structurally absent from deliberation
 *   - social_anthropologists: analytical observer (analytical/analytical, global) — document renegotiation and proliferation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.3).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.25).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary Practice as Locally Renegotiated Coordination Norm (Localized Practice Reading)").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, 'b5a2f83c-69c1-4511-a916-40c26105cbb2').
narrative_ontology:cs_kernel_codification('b5a2f83c-69c1-4511-a916-40c26105cbb2', distributed).
narrative_ontology:cs_authority_grounding('b5a2f83c-69c1-4511-a916-40c26105cbb2', practice).
narrative_ontology:cs_interpretation_layer_present('b5a2f83c-69c1-4511-a916-40c26105cbb2').
narrative_ontology:cs_reading_relation('b5a2f83c-69c1-4511-a916-40c26105cbb2', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5a2f83c-69c1-4511-a916-40c26105cbb2', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('b5a2f83c-69c1-4511-a916-40c26105cbb2', foundational, jati_boundaries_negotiated_in_local_practice).
narrative_ontology:cs_axiom_status(jati_boundaries_negotiated_in_local_practice, holdable).
narrative_ontology:cs_axiom_grounding('b5a2f83c-69c1-4511-a916-40c26105cbb2', jati_boundaries_negotiated_in_local_practice, empirically_contingent).
narrative_ontology:cs_axiom('b5a2f83c-69c1-4511-a916-40c26105cbb2', secondary, scriptural_varna_as_legitimating_resource_not_generative_source).
narrative_ontology:cs_axiom_status(scriptural_varna_as_legitimating_resource_not_generative_source, holdable).
narrative_ontology:cs_axiom_grounding('b5a2f83c-69c1-4511-a916-40c26105cbb2', scriptural_varna_as_legitimating_resource_not_generative_source, empirically_contingent).
narrative_ontology:cs_reference_frame('b5a2f83c-69c1-4511-a916-40c26105cbb2', locally_negotiated_boundary_practice).
narrative_ontology:cs_drift_state('b5a2f83c-69c1-4511-a916-40c26105cbb2', post_census_post_reservation_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b5a2f83c-69c1-4511-a916-40c26105cbb2', '2026-08-10T14:30:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, jati_members).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, hereditary_occupational_specialists).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, locally_dominant_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, cross_boundary_marriage_suitors).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, women_under_hypergamy_rules).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, lower_status_local_jatis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, lower_status_local_jatis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preside over the village- and region-level assemblies that hear boundary questions: whether a proposed marriage falls inside the recognized circle, whether a family's conduct has crossed a line, whether a sub-group's claim to separate status should be ratified. Their standing — the seat at the front of the assembly, the first portion at collective feasts — flows from holding this adjudication role, a role their families have held across generations. Their authority exists only inside this arrangement; stepping out of it means becoming an ordinary elder with no assembly to preside over.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_council_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Marry within the circle their birth group defines, contribute to collective funds at life-cycle ceremonies, draw on the group's credit and labor-sharing in crises, and call on the same network for work, tenants, and matchmakers. The boundary keeps the marriageable set and the obligation network legible at low cognitive cost. Leaving is possible — urban relatives do it — but means losing the aid circle, the marriage introductions, and the ceremonial community all at once, so most stay and renegotiate at the margins instead.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_members, beneficiary,
    moderate, biographical, constrained, regional).

% Weaver, potter, carpenter, and goldsmith lineages whose trade travels with the birth group: apprenticeship runs parent to child inside the group, skill claims are certified by group reputation, and marriage alliances double as business partnerships. The boundary shields their market niche from outside entrants; it also ties their household's livelihood to the group's collective standing.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, hereditary_occupational_specialists, beneficiary,
    moderate, generational, constrained, regional).

% The top-ranked landholding groups in a local cluster: they receive the first portions and the elevated seating at collective feasts, are served by lower-ranked groups at life-cycle ceremonies, and their marriage circles set the standard others measure themselves against. The deference they receive is a flow, not a stock — it must be continuously re-performed and can be contested by rank claims from below — which is why they invest heavily in ceremony, in council standing, and in the marriage placements of their children.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, locally_dominant_jatis, beneficiary,
    powerful, generational, constrained, regional).

% Young people whose chosen partner sits outside the circle their families recognize. Within the local arrangement there is no approved path to the marriage they want: families withhold consent, assemblies refuse recognition, and the couple faces severance from kin, land, and ceremony. The arrangement's release valve is fission — a faction sometimes secedes, forms a new circle that admits such marriages, and the boundary is renegotiated rather than the couple coerced — but until a split happens, the suitor's desired option is simply closed while their family, property, and whole social world remain inside.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, cross_boundary_marriage_suitors, payer,
    powerless, biographical, trapped, local).

% Marry equal or upward, never down: the rule narrows a woman's acceptable partner set below her brother's, since his downward marriages are sometimes tolerated while hers are not. Her family's standing rides on her marriage compliance, and a cross-circle relationship damages her name more than his. She holds no seat in the assemblies that decide these questions; her position is set by the marriage others negotiate on her behalf, and her life chances follow it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, women_under_hypergamy_rules, payer,
    powerless, biographical, trapped, regional).

% Occupy the lower rungs of the local feast-and-service order: they serve at collective ceremonies, receive the seating and food distribution that marks their rank, and owe deference to the groups above them. They are at the same time full members of the arrangement — their own marriage circles, aid funds, and ceremonies run on the same boundary logic — and they continuously negotiate their position, adopting the practices of higher groups and petitioning for better feast placement. Their costs and their benefits flow through the same structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, lower_status_local_jatis, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, lower_status_local_jatis, beneficiary).

% Kin and neighbors who left: migrated to cities, married across the old lines, or converted, and built lives in which the birth group's boundary organizes nothing. Their existence is the standing demonstration that exit is survivable. In the villages they came from they have no standing to speak — departure is exactly what strips their standing — so the deliberations that renew the boundary proceed without hearing from the people who tested it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_exit_communities, excluded,
    moderate, biographical, mobile, regional).

% Fieldworkers who document marriage circles, feast hierarchies, and boundary disputes across regions and decades. Their inventories of categories — now numbering in the thousands and still multiplying — and their case studies of splits, rank claims, and rule adjustments are the evidentiary base for reading the boundary system as continuously negotiated practice rather than fixed order.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, social_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, locally_dominant_jatis).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the alliance and closure problems of a heterogeneous local society without any central registry: endogamous boundaries define a legible marriageable set; jati membership organizes mutual aid and credit circles, life-cycle ceremony cooperation, occupational apprenticeship with skill certification by reputation, and local dispute adjudication. Boundary maintenance is the membership mechanism that keeps those pools closed enough to function while remaining adjustable case by case.
% TRANSFER_FUNCTION: Moves marriage alliances within the boundary (marriageability is exchanged only across recognized lines), moves deference and service upward at collective ceremonies from lower-ranked to higher-ranked local jatis, and moves mutual aid, credit, and ceremonial obligation within each circle. The costs of the movement fall on those whose preferences or positions cross the lines.
% ABSENT_VOICES: Women affected by boundary decisions are rarely seated in the male councils that adjudicate them; they are present as subjects of negotiation, not as deliberators. Those who exited — urban migrants, converts, cross-circle couples — are structurally disqualified from local deliberation precisely because leaving is what strips their standing to speak, so the forums that renew the boundary never hear the strongest testimony that exit is survivable. Lower-ranked jatis attend rank negotiations but seldom set their terms.
% DISAPPEARANCE_RATIONALE: If the boundaries dissolved overnight, marriage pooling would need a replacement legible set (personal networks, class endogamy, registered choice), aid and credit circles would re-form around other closures — kin, neighborhood, profession — and the local ceremonial exchange grid and its rank order would lose their organizing frame. The reading predicts the rearrangement would be largely substitutive rather than catastrophic, because the arrangement performs coordination functions rather than expressing a natural order; but the rearrangement would be real and would take a generation.
% FOUNDING_PROBLEM: Pre-modern local societies needed to solve alliance and trust problems at scale without central registries or contract enforcement: whom may my child marry, whose skill claims can I trust, to whom can I extend credit in a crisis, who owes what at a life-cycle ceremony. Locally administered birth-group boundaries were the solution that emerged, adjustable by the very communities that used them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: a century of ethnography independent of the councils documents marriage circles, feast hierarchies, and aid obligations operating as described; demographic and survey series show endogamy persisting at high rates even where formal enforcement has lapsed; historical scholarship on pre-colonial occupational and ritual organization attests the alliance and certification functions. The councils also assert the problem's liveness, but the load-bearing corroboration is external — field records, census and survey series, and economic histories of caste-based occupational networks.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.30 at interval end) because, on this reading, most governed members are net beneficiaries: the boundary buys a legible marriageable set, a closed mutual-aid circle, and certified skill reputation, and the paying seats are minorities within a broadly mutual structure. Suppression is authored low (0.25) and is a raw structural property — it is not scaled by power or scope; the engine scales only extractiveness. The category count multiplying into the thousands is the reading's central evidence of weak enforcement: a coercively held boundary erodes under fission pressure, whereas these boundaries multiply. Theater stays low (0.22): purity claims and feast-rank ritual are real performances but subordinate to functioning marriage arrangement, aid pooling, and dispute adjudication. Accessibility_collapse is moderate (0.40): workable alternatives persist — cross-circle marriage is rising with urbanization, occupational mobility increasingly bypasses jati lines, and exit communities demonstrate survivable outsides. Resistance (0.35) is real but episodic: love marriages, rank-revolt claims from below, and quiet exit rather than sustained confrontation. The three measurement series share one time grid (t = 0, 20, 40, 60, 80, 100, 120). Base_extractiveness drifts gently down as courts stripped caste-council penalties of legal force and urban mobility rose. Suppression_requirement falls along the same enforcement-decay trajectory — this series is authored because enforcement-capacity change is the dynamic this story traces. Theater_ratio rises slowly as ritual performance outlasts some functions, without approaching piton territory.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the council elders' position the boundary order is a stewardable, negotiable thing they administer. From the cross-boundary suitor's and the hypergamy-constrained woman's position the same order is a closure with no approved path around it — and with powerless power and trapped exit, those seats should compute far more extractive than the aggregate epsilon of 0.30 suggests. Lower-ranked jatis sit genuinely dual: they pay deference up and draw coordination benefits from the same structure, so their computed position should hover near symmetric. Exit communities, already outside, experience the arrangement as optional. The divergence between the aggregate rope profile and the trapped seats' computed extraction is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: jati_members and hereditary_occupational_specialists draw the coordination benefits directly (low d); locally_dominant_jatis additionally receive the deference flow, sitting nearest the pure-beneficiary end. Targets: cross_boundary_marriage_suitors and women_under_hypergamy_rules bear the closure's costs with powerless power and trapped exit (high d); lower_status_local_jatis bear deference costs but hold a secondary beneficiary position — their own marriage circles, aid funds, and ceremonies run on the same boundary logic — so their true d sits near symmetric despite appearing in the victims array. No directionality_overrides are authored: the override handle is power_atom-keyed, and the one mixed seat (lower_status_local_jatis, organized) shares its power atom with the elder council, so a downward override would wrongly drag the elders toward symmetry; the secondary_role declaration carries the duality instead. Spatial scopes are local-to-regional, keeping verification cheap and limiting the scope-side amplification of effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — alliance pooling, aid closure, skill certification at local scale — are live, so no mandatrophy is declared, and the scaffold framing would misread a steady-state coordination arrangement as transitional. The rope claim guards against the opposite error as well: it prevents importing the orthodox reading's pollution-enforcement extraction or the census reading's administrative reification into this arrangement's epsilon. The residual risks run both ways, and the omegas carry them: fission-as-enforcement would mean the low observable suppression hides a harder mechanism, while rank-asymmetric benefit capture would mean the coordination benefits pool upward — either would push the computed classification toward tangled_rope. Theater stays low and the function is real, so there is no piton drift signal; nothing here is vestigially maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the localized_practice_reading of the jati_practice_norm kernel. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'No dataset resolves a framing choice; resolution is comparative authoring — the orthodox_textual_reading and colonial_census_reading are separate constraint stories whose epsilon, enforcement seats, and victim sets can be compared against this one.',
    'Under the orthodox_textual_reading the same boundary system is scripturally mandated with pollution enforcement — epsilon and suppression rise sharply and the victim set expands to all deviants. Under the colonial_census_reading the enforcement seat moves from local councils to state enumeration — suppression becomes administrative and the agenda-setter seat relocates. The rope classification holds only within this reading''s framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a contested three-reading kernel; siblings are separate constraints, and the disagreement sits in the source and maintenance mechanism of boundary rigidity.').

omega_variable(
    fission_as_enforcement_ambiguity,
    'Is the proliferation of jati categories (3000+ and multiplying) evidence of weak enforcement, or of enforcement operating through fission — deviant factions expelled into new endogamous groups rather than coerced back into line?',
    'Trace documented boundary-split cases: did parent jatis attempt coercive restoration (boycott, penalty, violence) before fission, or did they ratify the split? Measure coercion-attempt rates across split events.',
    'If fission is the enforcement mechanism, observable proliferation coexists with high effective suppression and the constraint computes closer to tangled_rope; the authored low suppression would be an artifact of measuring the wrong mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fission_as_enforcement_ambiguity, empirical, 'Whether proliferation indicates weak enforcement or fission-based enforcement.').

omega_variable(
    coordination_benefit_asymmetry_by_rank,
    'Do the coordination benefits (marriage pooling, mutual aid, ritual cooperation) accrue symmetrically across local jati rank, or do upper-ranked groups and council elders capture a disproportionate share while lower-ranked groups bear the deference costs?',
    'Network analysis of aid flows, marriage exchange, and ceremony expenditure by rank position within a set of local jati clusters.',
    'Symmetric accrual supports the rope classification; capture by upper ranks through the same structure would push toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_asymmetry_by_rank, empirical, 'Whether the coordination benefit pool is rank-symmetric or captured upward.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the residual force that holds the boundaries (social pressure, family severance threats, council censure) primarily structural or internalized — do members who exit carry boundary-normative attitudes with them?',
    'Post-exit attitude trajectories among urban migrants and converts: does boundary-compliance sentiment decay with distance from the enforcement community, or persist?',
    'If internalized, the falling suppression_requirement series overstates enforcement decay — the arrangement''s hold weakens structurally while persisting cognitively, and any drift toward piton-like theatrical maintenance would be masked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized mechanism behind the residual boundary-holding force.').

omega_variable(
    hypergamy_asymmetry_separability,
    'Is the cost asymmetry borne by women under hypergamy rules intrinsic to the jati boundary system, or a separable patriarchal overlay that local renegotiation could remove without dissolving the boundaries?',
    'Compare boundary stability and coordination function across jatis with symmetric versus hypergamous marriage rules; test whether symmetric-rule jatis maintain equally stable circles and aid networks.',
    'If separable, the women''s high-extraction seat indicts a co-traveling gender arrangement rather than the boundary system itself, and the constraint family should decompose into linked but distinct stories (jati boundaries; gender hierarchy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypergamy_asymmetry_separability, empirical, 'Whether the gendered cost asymmetry is intrinsic to the boundary system or a separable overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_loc_practice_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t0, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t20, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t40, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t60, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t80, jati_practice_norm__localized_practice_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t80, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.21).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t100, observed).
narrative_ontology:measurement(jati_loc_practice_tr_t120, jati_practice_norm__localized_practice_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement_basis(jati_loc_practice_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(jati_loc_practice_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(jati_loc_practice_be_t0, observed).
narrative_ontology:measurement(jati_loc_practice_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(jati_loc_practice_be_t20, observed).
narrative_ontology:measurement(jati_loc_practice_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(jati_loc_practice_be_t40, observed).
narrative_ontology:measurement(jati_loc_practice_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement_basis(jati_loc_practice_be_t60, observed).
narrative_ontology:measurement(jati_loc_practice_be_t80, jati_practice_norm__localized_practice_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement_basis(jati_loc_practice_be_t80, observed).
narrative_ontology:measurement(jati_loc_practice_be_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement_basis(jati_loc_practice_be_t100, observed).
narrative_ontology:measurement(jati_loc_practice_be_t120, jati_practice_norm__localized_practice_reading, base_extractiveness, 120, 0.3).
narrative_ontology:measurement_basis(jati_loc_practice_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_loc_practice_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(jati_loc_practice_su_t0, observed).
narrative_ontology:measurement(jati_loc_practice_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(jati_loc_practice_su_t20, observed).
narrative_ontology:measurement(jati_loc_practice_su_t40, jati_practice_norm__localized_practice_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(jati_loc_practice_su_t40, observed).
narrative_ontology:measurement(jati_loc_practice_su_t60, jati_practice_norm__localized_practice_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(jati_loc_practice_su_t60, observed).
narrative_ontology:measurement(jati_loc_practice_su_t80, jati_practice_norm__localized_practice_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement_basis(jati_loc_practice_su_t80, observed).
narrative_ontology:measurement(jati_loc_practice_su_t100, jati_practice_norm__localized_practice_reading, suppression_requirement, 100, 0.26).
narrative_ontology:measurement_basis(jati_loc_practice_su_t100, observed).
narrative_ontology:measurement(jati_loc_practice_su_t120, jati_practice_norm__localized_practice_reading, suppression_requirement, 120, 0.25).
narrative_ontology:measurement_basis(jati_loc_practice_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% Constraint family (three stories): the colloquial label 'the caste system' conflates three structurally distinct claims — scripturally fixed varna hierarchy with pollution enforcement (jati_practice_norm__orthodox_textual_reading), locally negotiated and proliferating practice boundaries (this file), and administratively reified census categories (jati_practice_norm__colonial_census_reading). Each is authored as a separate epsilon-invariant constraint and linked here. Epsilon differs across the family because the referent arrangements differ: the boundary-maintenance mechanism — and therefore who enforces, who pays, and how extractive the arrangement is — is exactly what the readings dispute, so a single story with a measurement parameter would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
