% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy Reading (s.92A as Absolute Territorial Dominion)
 *   domain: political economy/federalism/resource governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   provincial_sovereignty_boundary: the resource_sovereignty_primacy
 *   reading, which holds that provincial ownership of natural resources under
 *   s.92A of the Constitution Act 1982 grounds absolute territorial
 *   sovereignty, that federal climate and fiscal policy touching resources is
 *   therefore illegitimate extraction from sovereign property, and that
 *   unilateral exit is a constitutional right. The epsilon referent is the
 *   standing arrangement under contest - provincial ownership embedded in a
 *   federal framework of carbon pricing, equalization, and infrastructure
 *   gatekeeping - assessed by this reading's own lights, under which that
 *   arrangement is heavily extractive. Sibling readings
 *   (constitutional_subordination, compact_federalism) share the referent and
 *   author different epsilons; they are other files, not part of this one.
 *   The claimed type and the metrics are independent authored facts: I claim
 *   tangled_rope because the arrangement genuinely coordinates a continental
 *   union while asymmetrically loading its costs, and I author high
 *   extraction because that is what the arrangement looks like from this
 *   reading's seat - the engine computes per-seat classifications from the
 *   structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - - federal_government: agenda-setter and collector (institutional/arbitrage) - sets carbon price, equalization formula, assessment gates; absorbs no comparable reciprocal constraint
 *   - - resource_producing_provincial_governments: primary target (powerful/trapped) - owns the estate, bears the overlay, cannot lawfully exit
 *   - - equalization_recipient_provinces: secondary beneficiary (moderate/constrained) - receive formula transfers scaled to resource revenues
 *   - - energy_sector_workforces: concentrated payers (organized/constrained) - bear adjustment costs of federal timelines
 *   - - resource_dependent_municipalities: diffuse payers (powerless/trapped) - absorb shocks with least buffering
 *   - - indigenous_nations: excluded seat (organized/trapped) - hold qualifying claims to the same estate, outside the two-party frame
 *   - - supreme_court_of_canada: analytical observer (institutional/analytical) - defines what each seat may lawfully attempt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.78).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.6).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy Reading (s.92A as Absolute Territorial Dominion)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political economy/federalism/resource governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '09fd36ff-f69e-4eb2-aae5-69f30886d55e').
narrative_ontology:cs_kernel_codification('09fd36ff-f69e-4eb2-aae5-69f30886d55e', fixed_text).
narrative_ontology:cs_authority_grounding('09fd36ff-f69e-4eb2-aae5-69f30886d55e', lineage).
narrative_ontology:cs_interpretation_layer_present('09fd36ff-f69e-4eb2-aae5-69f30886d55e').
narrative_ontology:cs_reading_relation('09fd36ff-f69e-4eb2-aae5-69f30886d55e', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('09fd36ff-f69e-4eb2-aae5-69f30886d55e', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('09fd36ff-f69e-4eb2-aae5-69f30886d55e', foundational, resource_ownership_grounds_absolute_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_grounds_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('09fd36ff-f69e-4eb2-aae5-69f30886d55e', resource_ownership_grounds_absolute_sovereignty, deontological).
narrative_ontology:cs_axiom('09fd36ff-f69e-4eb2-aae5-69f30886d55e', secondary, federal_resource_policy_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(federal_resource_policy_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('09fd36ff-f69e-4eb2-aae5-69f30886d55e', federal_resource_policy_is_illegitimate_extraction, deontological).
narrative_ontology:cs_reference_frame('09fd36ff-f69e-4eb2-aae5-69f30886d55e', provincial_proprietary_sovereignty).
narrative_ontology:cs_drift_state('09fd36ff-f69e-4eb2-aae5-69f30886d55e', post_ggppa_reference_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('09fd36ff-f69e-4eb2-aae5-69f30886d55e', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, energy_sector_workforces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_dependent_municipalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the nationwide carbon-price backstop, the equalization formula, and the impact-assessment regime that gates export infrastructure crossing provincial lines. Collects carbon-pricing revenue and returns portions through household rebates and program spending. Can shift between tax, regulation, and spending instruments and can tolerate provincial non-cooperation in ways provinces cannot reciprocate, but cannot abandon the fiscal union without dissolving its own revenue base.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, beneficiary).

% Receive unconditional annual transfers scaled to a fiscal-capacity measure that counts provincial resource revenues. Their program spending tracks the formula's continuity, and they have limited leverage over its design beyond first-ministers conferences. Several are simultaneously contributors on other federal lines, which complicates any simple debtor-creditor picture.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces, beneficiary,
    moderate, biographical, constrained, regional).

% Own and administer the natural-resource estate within their borders under s.92A, collect royalties, and fund the bulk of resource development. At the same time they are bound by federal carbon pricing they did not consent to, an equalization measure that counts their resource revenues, and federal assessment gates on the pipelines and ports their products need. The 1998 Secession Reference forecloses lawful unilateral exit; their remaining levers are litigation, provincial statute such as sovereignty acts, withholding from federal programs, and political mobilization.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments, payer,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_producing_provincial_governments, agenda_setter).

% Work in construction, operations, and services tied to project approvals and carbon-cost pass-through. Skills are sector- and region-specific, so federal decarbonization timelines land on them as retraining, relocation, or unemployment. Unions and industry associations give them voice, but the decisions that determine their employment are made in capitals where they hold few seats.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, energy_sector_workforces, payer,
    organized, immediate, constrained, regional).

% Single-industry towns whose tax bases, service levels, and population tracks rise and fall with resource activity and with the approval status of specific projects. They have no meaningful exit: housing equity and community ties are sunk in place. They absorb both commodity busts and policy shocks with the least buffering of any seat.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_dependent_municipalities, payer,
    powerless, biographical, trapped, local).

% Hold constitutionally protected Aboriginal and treaty rights and outstanding title claims that intersect the same resource estates the province owns and the federation regulates. Duty-to-consult jurisprudence gives them procedural voice rather than consent rights over much of the activity in dispute. They are largely outside the two-party frame in which this boundary is argued, though their claims qualify what either level of government may do on the land.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations, excluded,
    organized, generational, trapped, regional).

% Adjudicates division-of-powers, carbon-pricing, and secession questions. Its 1998 Secession Reference and its 2021 upholdance of the federal greenhouse-gas pricing act are the load-bearing rulings for the current boundary. It collects nothing and pays nothing; its outputs define what each other seat may lawfully attempt.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement secures provincial ownership of natural resources so that development investment has stable tenure, while assigning to the federation the problems no province can solve alone: interprovincial and export trade, macroeconomic stabilization, a nationwide emissions commitment, and fiscal insurance across asymmetric regional shocks.
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-rich provinces into federal revenue and out again toward lower-capacity provinces through a formula that counts resource revenues; and moves development discretion from provincial owners to federal regulators through carbon pricing and project assessment, at the price of provincial compliance the owners did not individually bargain for.
% ABSENT_VOICES: Indigenous nations holding s.35 rights and title claims are structurally outside the argument: the dispute proceeds as a two-party quarrel between province and federation over an estate whose ownership both assume, while the parties whose legal orders first occupied the land hold procedural rather than consent-standing. Future generations bearing climate costs likewise have no seat. Their absence lets each side claim the mantle of the injured owner.
% DISAPPEARANCE_RATIONALE: If the boundary arrangement vanished overnight, the fiscal union would lose its insurance mechanism, exporters would lose their assessment rail, provinces would lose tenure security for development capital, and every climate commitment would lose its enforcing instrument; investment, migration, and intergovernmental bargaining would all reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Reconcile regional control of natural resources with national economic union: after the 1980 National Energy Program crisis, resource provinces demanded entrenched ownership as the price of patriation, and s.92A was the 1982 settlement that entrenched ownership while leaving federal trade, taxation, and emergency powers intact.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court of Canada's references on secession and on federal carbon pricing document the unresolved tension from a seat outside the benefiting parties; the Rowell-Sirois Royal Commission and subsequent constitutional scholarship trace the original problem and confirm it was never settled, only managed; provincial fairness-panel testimony records the grievance in the resource provinces' own institutional voice. No party to the dispute claims the founding compromise achieved finality.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.78 at interval end) because, from this reading's seat, the federal overlay takes value from provincial-owned assets without the owners' consent: carbon pricing on the exercise of ownership, an equalization measure that converts resource wealth into transfers elsewhere, and assessment gates on the export routes the assets require. Suppression (0.60) is a raw structural property, unscaled by power or scope: it reflects federal legislative supremacy in enumerated fields plus judicial enforcement against unwilling provinces, tempered by the provinces' real countervailing powers. Theater (0.32) captures consultation and national-unity ritual that the reading experiences as performance layered over decided outcomes. Accessibility collapse is moderate (0.45): intra-system alternatives persist - litigation, provincial statute, coalition politics, interprovincial deal-making - but the exit alternative itself is collapsed, which is precisely the reading's core complaint. Resistance is high (0.72): constitutional references, a provincial sovereignty act, an equalization referendum, and repeated interprovincial pipeline attempts. The temporal series share one grid; extraction ratchets upward through Kyoto, the pan-Canadian framework, and the 2019 imposition of the backstop on refusing provinces, with a brief post-1982 dip when s.92A itself was the concession.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute differently from identical facts. From the federal seat the arrangement is a fiscal union it administers and a shared atmosphere it is obliged to govern; from the resource-province seat the same instruments are levies on sovereign property enforced by courts the owners did not persuade. Recipient provinces experience the formula as insurance, not extraction. Among nominally equal resource provinces, exit quality differs: one holds a decades-old offshore accord resetting its fiscal terms, another sells hydro on long-term contracts, a third has neither - same power atom, different effective constraint. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the federal government sits nearest the beneficiary end (it collects and redirects), recipient provinces somewhat above it (they receive flows but do not run the machinery). Victim declarations map to high directionality: resource provinces, with trapped exit, sit nearest the full-target end - the trap is doing heavy work, since a mobile owner could arbitrage jurisdictions and the reading's whole grievance is that this one cannot. Workforces and municipalities inherit high directionality through dependence on the same estate. Indigenous nations are declared excluded rather than victim or beneficiary: within this reading's frame they are outside the derivation entirely, which is itself the finding - the reading's unanimity is manufactured by a frame that never admitted them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconcile regional ownership with national union after the NEP crisis - is live, not dead: the same collision recurs in every carbon-pricing and pipeline cycle, so no mandatrophy is declared and none should be inferred from the persistence of conflict. The classification discipline cuts both ways here. Calling the arrangement a snare would erase the genuine coordination it performs (tenure security, fiscal insurance, a single climate instrument) and would validate the reading's most maximalist claims; calling it a rope would erase the asymmetric loading its payers document and would dismiss a grievance that courts keep partially vindicating. Tangled rope keeps both halves visible: the coordination is real, the extraction is real, and the enforcement is what holds the combination together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the provincial_sovereignty_boundary kernel; would the sibling readings (constitutional_subordination, compact_federalism) produce a different epsilon, victim set, and type for the same referent?',
    'Author the sibling stories against the identical referent (the provincial-federal resource authority boundary) and compare computed per-seat classifications; the disagreement is located in the ground of provincial authority (creature-of-constitution vs. founding-compact residue vs. proprietary dominion under s.92A).',
    'Under constitutional_subordination the same arrangement computes as low-extraction legitimate hierarchy; under compact_federalism as intermediate reneged-agreement; the entire classification of Canadian resource governance hinges on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: reading-indexed classification over a shared kernel referent.').

omega_variable(
    s92a_textual_scope_of_absoluteness,
    'Does s.92A''s own text support the absolute-sovereignty reading, given that its later subsections expressly preserve federal indirect taxation of resource products, federal law-making over interprovincial and export trade in resources, and federal paramountcy to the extent of inconsistency?',
    'Textual and doctrinal analysis of s.92A(2)-(6) alongside the drafting history of the 1982 patriation package; the absolutist premise must explain away the section''s built-in federal carve-outs.',
    'If the text itself embeds federal priority, the reading''s foundational premise is a normative overlay rather than a description of enacted law, weakening its claim to constrain federal climate and fiscal policy at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(s92a_textual_scope_of_absoluteness, empirical, 'Whether the cited constitutional text actually carries the absolutist weight the reading places on it.').

omega_variable(
    indigenous_title_qualification,
    'Do s.35 Aboriginal and treaty rights, and outstanding Aboriginal title claims, qualify provincial ownership such that the territory is not unencumbered provincial dominion?',
    'Ongoing title litigation, modern treaty implementation, and recognition-of-rights legislation; several claims directly overlap the resource estates the reading treats as absolutely provincial.',
    'If Indigenous title qualifies or supersedes provincial ownership, the absolutist premise fails on its own terms (the owner is not the sole owner) and the victim set of any operative version of this doctrine changes fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_title_qualification, conceptual, 'Unresolved third-party encumbrance on the ownership that grounds the reading.').

omega_variable(
    unilateral_exit_legal_status,
    'Is unilateral secession a constitutional right flowing from proprietary dominion, as this reading asserts, or is lawful exit conditioned on negotiation and a clear majority on a clear question, as the 1998 Secession Reference holds?',
    'Constitutional amendment, a negotiated secession precedent, or a future court revisiting the Reference; no current instrument resolves it in the reading''s favor.',
    'If exit is negotiable-only, the reading''s remedy for perceived extraction is foreclosed within the legal order, pushing its adherents toward extra-constitutional pressure and raising the effective stakes of every federal-provincial resource dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_exit_legal_status, conceptual, 'Status of the exit right the reading treats as foundational.').

omega_variable(
    net_fiscal_incidence_dispute,
    'Are resource-producing provinces actually net fiscal losers under the union once federal spending in their territory, stabilization transfers, and shared-debt benefits are counted, or only under the narrow equalization-line accounting the reading uses?',
    'Independent net-fiscal-balance studies using consistent methodology across provinces and across commodity cycles.',
    'If resource provinces are roughly net-neutral over a cycle, measured extraction falls sharply and the reading''s grievance reduces to distributional timing; if they are persistent net contributors, the high epsilon authored here is corroborated from outside the movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_fiscal_incidence_dispute, empirical, 'Empirical magnitude of the transfer the reading characterizes as confiscatory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psb_resource_sovereignty_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.14).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t1982, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1990, 0.17).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t1990, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t1998, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1998, 0.21).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t1998, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2005, 0.24).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t2005, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.27).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t2015, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2019, 0.29).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t2019, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2022, 0.31).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t2022, observed).
narrative_ontology:measurement(psb_resource_sovereignty_tr_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(psb_resource_sovereignty_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(psb_resource_sovereignty_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t1982, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t1990, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t1998, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t1998, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t2005, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t2015, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t2019, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2022, 0.74).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t2022, observed).
narrative_ontology:measurement(psb_resource_sovereignty_be_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(psb_resource_sovereignty_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(psb_resource_sovereignty_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t1982, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t1990, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t1998, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1998, 0.46).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t1998, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2005, 0.49).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t2005, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t2015, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t2019, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t2022, observed).
narrative_ontology:measurement(psb_resource_sovereignty_su_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(psb_resource_sovereignty_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'provincial rights over resources' decomposes into three structurally distinct readings of one kernel, linked here per the epsilon-invariance principle. constitutional_subordination is the upstream, judicially established baseline (its premises are the ones the Supreme Court has repeatedly affirmed) and influences the operating environment of both downstream readings; compact_federalism and resource_sovereignty_primacy are downstream, more contested, and politically allied though grounded differently. This story links to both siblings; each sibling file should link back and to the others so the family has no orphans.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
