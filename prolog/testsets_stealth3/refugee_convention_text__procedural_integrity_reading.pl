% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This file instantiates the procedural-integrity reading of the 1951
 *   Refugee Convention / 1967 Protocol: the treaty binds states to give every
 *   asylum claimant a fair individualized determination — registration,
 *   hearing, reasons, appeal, and protection against return throughout —
 *   while leaving the substantive protection threshold deliberately flexible.
 *   On this reading the operative guarantee is the process itself: states may
 *   narrow who qualifies, but may not abolish the hearing, and the
 *   correctness of outcomes is secondary to the integrity of the procedure
 *   that produced them. The standing arrangement this story measures is the
 *   regime as actually operated: functioning adjudication in many
 *   jurisdictions alongside a documented and expanding record of pushbacks,
 *   offshore handling without equivalent guarantees, and expedited tracks
 *   without counsel. The colloquial label 'the Refugee Convention' decomposes
 *   into structurally distinct readings with different victim sets; this file
 *   authors only the procedural-integrity instantiation (see
 *   network.dual_formulation_note). KEY AGENTS (by structural relationship):
 *   - claimants_denied_procedural_access: Primary target (powerless/trapped)
 *   — bears the arrangement's downside with no hearing and no alternative
 *   forum - claimants_in_individualized_procedure: Protected seat
 *   (powerless/trapped) — receives the safeguard the reading exists to secure
 *   - frontline_host_states: Cost-concentrated party (organized/constrained)
 *   — absorbs arrivals by geography with partial reimbursement -
 *   destination_states: Dual-positioned party (institutional/arbitrage) —
 *   funds the machinery, gains removal legitimacy, shifts handling abroad -
 *   unhcr_supervisory_machinery: Administrator (institutional/constrained) —
 *   supervises, guides, and documents without compulsion power -
 *   domestic_asylum_courts: Institutional beneficiary
 *   (institutional/identity_locked) — their function is constituted by the
 *   guarantee - immigration_legal_aid_sector: Practice beneficiary
 *   (organized/mobile) — the procedural rights generate their caseload -
 *   refugee_led_organizations: Excluded voice (powerless/trapped) — lived
 *   experience with no formal seat - migration_policy_researchers: Analytical
 *   observer (analytical/analytical) — tracks variance and procedural quality
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'a92a1311-2758-4870-b018-bb8d744c345a').
narrative_ontology:cs_kernel_codification('a92a1311-2758-4870-b018-bb8d744c345a', fixed_text).
narrative_ontology:cs_authority_grounding('a92a1311-2758-4870-b018-bb8d744c345a', lineage).
narrative_ontology:cs_interpretation_layer_present('a92a1311-2758-4870-b018-bb8d744c345a').
narrative_ontology:cs_reading_relation('a92a1311-2758-4870-b018-bb8d744c345a', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('a92a1311-2758-4870-b018-bb8d744c345a', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('a92a1311-2758-4870-b018-bb8d744c345a', foundational, process_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(process_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('a92a1311-2758-4870-b018-bb8d744c345a', process_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('a92a1311-2758-4870-b018-bb8d744c345a', secondary, outcome_subordinate_to_procedure).
narrative_ontology:cs_axiom_status(outcome_subordinate_to_procedure, holdable).
narrative_ontology:cs_axiom_grounding('a92a1311-2758-4870-b018-bb8d744c345a', outcome_subordinate_to_procedure, deontological).
narrative_ontology:cs_reference_frame('a92a1311-2758-4870-b018-bb8d744c345a', fair_individualized_determination_baseline).
narrative_ontology:cs_drift_state('a92a1311-2758-4870-b018-bb8d744c345a', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a92a1311-2758-4870-b018-bb8d744c345a', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, claimants_in_individualized_procedure).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, destination_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, domestic_asylum_courts).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, immigration_legal_aid_sector).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_machinery).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, claimants_denied_procedural_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, frontline_host_states).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_assessment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Filed a claim inside a working determination system. Receives registration, a hearing with interpretation, written reasons, and at least one appeal layer; many are ultimately refused and returned, but the hearing and its record are what they receive. Cannot choose forum or adjudicator; leaving the territory before decision forfeits the claim, and after refusal the usual destination is the country they fled.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, claimants_in_individualized_procedure, beneficiary,
    powerless, immediate, trapped, global).

% Intercepted at land or sea borders, pushed back, held in offshore or transit processing, or routed into expedited tracks without counsel or competent interpretation. No individual hearing of the claim occurs; they bear return, detention, or prolonged legal invisibility. Every neighboring door closes in sequence, so there is nowhere to take the claim instead.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, claimants_denied_procedural_access, payer,
    powerless, immediate, trapped, global).

% Geography delivers arrivals to them first regardless of fiscal capacity. They run registration, reception, and hosting at scale with partial international reimbursement, and their schools, labor markets, and border services absorb the difference. Tightening standards draws supervisory criticism; sustaining standards beyond capacity feeds domestic political backlash. Exiting would mean denouncing the treaty or closing borders to trade and neighbors alike.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, frontline_host_states, payer,
    organized, generational, constrained, regional).

% Wealthy states of intended destination. They finance the supervisory machinery, accept comparatively small shares of the hosted population relative to capacity, and obtain a defensible record for returning unsuccessful claimants because a recognized procedure ran somewhere. They can relocate handling through funding partnerships and designation schemes, and their own commitments are largely self-defined and revisable in practice.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, destination_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, destination_states, agenda_setter).

% The treaty's designated supervisor. Issues interpretive guidance, convenes the executive committee, runs status determination under mandate in dozens of states lacking national systems, and publishes documentation of state practice. Its budget, field presence, and mandate scope all depend on the regime continuing; it catalogs violations it has no power to compel anyone to remedy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_machinery, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr_supervisory_machinery, beneficiary).

% National tribunals and courts that review determinations and build the case law giving the treaty domestic force. Their dockets, doctrines, and professional authority expanded with the procedural guarantee; the adjudicative function has become what these institutions are. Retiring the function would require redefining the institutions themselves.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, domestic_asylum_courts, beneficiary,
    institutional, biographical, identity_locked, national).

% Lawyers, NGOs, and funded advice providers whose daily practice is representing claimants through the procedure. The procedural rights generate the caseload; funding formulas follow it; expertise accumulates in this channel and transfers only partially to adjacent migration work.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, immigration_legal_aid_sector, beneficiary,
    organized, biographical, mobile, continental).

% Associations led by former refugees and claimants with lived experience of the procedures. They hold no formal seat in the executive committee, in treaty supervision, or in compact governance. They would press for procedures designed with participants rather than for them, and for the denied-access experience to count as evidence in the record.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_led_organizations, excluded,
    powerless, generational, trapped, global).

% Academic and statistical observers tracking grant-rate variance, procedural-quality indicators, and pushback documentation across jurisdictions. They publish findings and testify; they hold no enforcement power, and their seat is analytical.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, migration_policy_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, destination_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of assigning responsibility for displaced persons: individualized determination produces portable, mutually recognizable statuses, so each state can rely on the legibility of another's procedure rather than on its mercy, and no single state faces the arrival burden alone without a rule for passing claims on.
% TRANSFER_FUNCTION: Moves adjudicative burden and hosting responsibility toward states of first contact; moves removal legitimacy to states that can show a recognized procedure ran; moves protection, or its denial, to claimants depending on whether they reach the procedure at all.
% ABSENT_VOICES: Refugee-led organizations and claimants themselves have no formal seat in the executive committee, treaty supervision, or compact governance; they would object that process integrity is currently defined for participants rather than with them. Frontline host states are also under-weighted in burden-sharing negotiation relative to the costs geography assigns them.
% DISAPPEARANCE_RATIONALE: If the procedural guarantee vanished overnight, status determination would fragment into purely discretionary national practices, chain refoulement would resume along closed-border sequences, the supervisory machinery and the domestic adjudicative dockets built on the guarantee would lose their object, and the removal-legitimacy that destination states currently trade on would evaporate — the entire architecture of who may hear, decide, and return whom would rearrange.
% FOUNDING_PROBLEM: Post-war mass displacement with no legal category obligating anyone to admit, hear, or refrain from returning the displaced: states expelled and denied entry arbitrarily, and no procedure existed to distinguish flight from persecution-driven fear at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historical-demographic scholarship on mid-century displacement independently attests the founding conditions; ICRC operational records attest the persistence of the underlying displacement and return dynamics; frontline host state submissions to the executive committee attest the live burden-allocation problem from the paying seat; refugee-led testimony attests it from the affected seat. UNHCR documentation also attests it but sits inside the benefiting set and is therefore treated as partial corroboration only.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 for the standing arrangement as this reading assesses it: the safeguard is real for claimants who reach a functioning procedure, but a large and growing class never reaches one — pushbacks, offshore siting, and expedited tracks remove the hearing precisely where deterrence incentives are strongest, and the reading counts a claimant denied a hearing as bearing the arrangement's full downside regardless of what a hypothetical hearing would have concluded. Suppression is 0.52 as a raw structural property (unscaled by power or scope): the treaty holds states to obligations they continuously maneuver against, while the alternatives states prefer — bilateral externalization, designation schemes — remain only partly suppressed. Theater_ratio 0.38 mixes functional adjudication with a thick declaratory layer: commemorative declarations, compact review cycles, and supervision reports that catalog violations without altering them. Accessibility_collapse 0.42: understanding the text's flexibility does not trap states, which construct workarounds readily, and offers claimants no alternative forum since neighboring doors close in sequence. Resistance 0.62 reflects decades of documented non-compliance pressure — border pushback normalization, externalization design, funding leverage, and open defiance in several jurisdictions. All three temporal series share one grid (1951, 1967, 1985, 2001, 2015, 2026). Suppression_requirement is authored because this story specifically traces enforcement-capacity change: soft supervisory reporting hardened into judicialized enforcement peaking around 2015, then partially eroded as externalization architectures were designed to evade adjudicative jurisdiction. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the constituted-function seats should compute differently. From the denied-claimant position the arrangement presents as enforced exposure with no counterweight: no hearing, no forum, no exit. From the court and legal-aid positions the same arrangement presents as the coordination structure that constitutes their professional existence — they experience it as the source of their function, not as a cost. Destination states straddle: they collect removal legitimacy and burden relief while paying funding and occasional litigation losses. Frontline hosts experience geographic cost concentration with constrained exit; a hosting-states coalition could convert their diffuse position into bargaining power, and the possibility is noted rather than assumed — historically such coalitions have fragmented under bilateral side-payments. The engine computes per-seat classifications from these structural positions; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: claimants inside procedure are subsidized by the guarantee; domestic_asylum_courts and immigration_legal_aid_sector have their function constituted by it; unhcr_supervisory_machinery rides on it for mandate and budget; destination_states benefit through removal legitimacy and burden-shifting, tempered by funding costs, placing them nearest symmetric among the beneficiaries. Declared victims derive high directionality: claimants_denied_procedural_access sit at the full-target end — they bear the entire downside with zero exit and no alternative forum; frontline_host_states sit near the target end — structural cost concentration by geography with constrained exit. The victim-set boundary is procedural access itself, which is this reading's distinguishing structural claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mid-century mass displacement with no obligatory hearing and no non-return guarantee — remains live at record displacement levels, so no mandatrophy resolution is declared and none of the lifecycle indicators suggest a dead mandate carried by inertia. The classification discipline matters in both directions: a pure-coordination reading would render the denied-access class invisible, because their harm is precisely the absence of the safeguard and vanishes from any measurement taken over the safeguard's beneficiaries; a pure-extraction reading would erase the genuine protection delivered to the millions who pass through functioning procedures. The hybrid keeps both facts load-bearing and locates the analytic attention at the boundary — procedural access — where the victim set is determined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the refugee_convention_text kernel governs the regime''s operative arrangement — procedural safeguard (this file), unbendable humanitarian mandate, or sovereign-discretion floor — given that each reading assigns a different victim set and a different epsilon to the same treaty text?',
    'Authoritative interpretive consolidation — an ICJ advisory opinion, a treaty-body general comment with state acceptance, or crystallized state practice — revealing which reading commands actual adherence.',
    'Under the expansive sibling the victim set widens to generalized-violence and non-state-persecution claimants and measured extraction rises; under the restrictive sibling the victim set shrinks to those denied even minimal process and measured extraction falls. The values authored in this file hold only for the procedural-integrity instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'This constraint is one reading of the refugee_convention_text kernel; sibling readings reassign the victim set and the epsilon over the identical text.').

omega_variable(
    procedural_outcome_separability,
    'Can fair process actually be delivered while outcomes remain formally subordinate to procedure, or do systematic grant-rate disparities across adjudicators and jurisdictions show the procedure laundering arbitrary outcomes?',
    'Large-N study of decision variance among statistically comparable claimants across adjudicators, regions, and time, controlling for claim characteristics.',
    'If outcomes track adjudicator identity rather than claim merit, the procedural guarantee functions as legitimation rather than safeguard, and the arrangement drifts toward pure extraction despite its formal fairness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_outcome_separability, empirical, 'Whether the reading''s core wager — outcome subordination to process — survives contact with decision-variance data.').

omega_variable(
    offshore_guarantee_possibility,
    'Can any extraterritorial processing arrangement satisfy full procedural guarantees (counsel, competent interpretation, independent appeal, protection against return), or is offshore siting inherently incompatible with them?',
    'Comparative audit of offshore regimes'' grant rates, appeal usage, and counsel access against domestic baselines in the same receiving systems.',
    'If incompatible, externalization is this reading''s defining violation and effective extraction concentrates almost entirely on intercepted claimants; if compatible, part of the measured extraction reflects implementation deficits rather than structural design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offshore_guarantee_possibility, empirical, 'Whether the reading''s offshore-permissible-with-guarantees position describes a realizable configuration.').

omega_variable(
    narrowing_elimination_boundary,
    'Where does permissible definitional narrowing end and elimination of substantive review begin — do safe-third-country designation and first-country-of-asylum doctrines preserve individualized review somewhere, or abolish it in effect?',
    'Comparative doctrine analysis tracking whether claimants routed through designation schemes retain a merits hearing in any jurisdiction.',
    'Fixes the boundary of the victim set: routes preserving review keep claimants inside the safeguard; routes abolishing review move them wholesale into the denied-access class, raising measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrowing_elimination_boundary, conceptual, 'Location of the line the reading draws between flexible thresholds and forbidden abolition of review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_proc_integrity_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t1951, observed).
narrative_ontology:measurement(refugee_proc_integrity_tr_t1967, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t1967, observed).
narrative_ontology:measurement(refugee_proc_integrity_tr_t1985, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t1985, observed).
narrative_ontology:measurement(refugee_proc_integrity_tr_t2001, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t2001, observed).
narrative_ontology:measurement(refugee_proc_integrity_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t2015, observed).
narrative_ontology:measurement(refugee_proc_integrity_tr_t2026, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(refugee_proc_integrity_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(refugee_proc_integrity_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.3).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t1951, observed).
narrative_ontology:measurement(refugee_proc_integrity_be_t1967, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1967, 0.34).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t1967, observed).
narrative_ontology:measurement(refugee_proc_integrity_be_t1985, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t1985, observed).
narrative_ontology:measurement(refugee_proc_integrity_be_t2001, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t2001, observed).
narrative_ontology:measurement(refugee_proc_integrity_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t2015, observed).
narrative_ontology:measurement(refugee_proc_integrity_be_t2026, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(refugee_proc_integrity_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(refugee_proc_integrity_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t1951, observed).
narrative_ontology:measurement(refugee_proc_integrity_su_t1967, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1967, 0.24).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t1967, observed).
narrative_ontology:measurement(refugee_proc_integrity_su_t1985, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1985, 0.34).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t1985, observed).
narrative_ontology:measurement(refugee_proc_integrity_su_t2001, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2001, 0.46).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t2001, observed).
narrative_ontology:measurement(refugee_proc_integrity_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t2015, observed).
narrative_ontology:measurement(refugee_proc_integrity_su_t2026, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(refugee_proc_integrity_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% 'The Refugee Convention' is a colloquial label covering at least three structurally distinct constraints — readings of one kernel — with different victim sets and different epsilon values. This file authors the procedural-integrity reading only, with a single stable epsilon over the standing regime assessed by this reading's lights. The restrictive-sovereignty and expansive-humanitarian siblings are separate stories linked here: the procedural reading sits upstream of the restrictive sibling (its jurisprudence defines the legitimacy conditions restrictive measures must survive) and coexists with the expansive sibling as rival emphases within the same coalition space. Any attempt to average epsilon across the three readings would measure none of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
