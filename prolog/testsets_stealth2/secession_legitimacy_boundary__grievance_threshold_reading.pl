% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Grievance-Threshold Secession Legitimacy Standard
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   The grievance-threshold reading holds that secession becomes legitimate
 *   when federal actions cross a demonstrable threshold of structural
 *   injustice, irrespective of constitutional text. Operationally it
 *   structures every secession dispute running through it: aggrieved regions
 *   must assemble objective evidence of sustained federal overreach;
 *   adjudicating institutions determine whether the threshold is crossed;
 *   federal governments mount defenses; third states calibrate recognition to
 *   the determinations. A genuine coordination function — a shared,
 *   adjudicable standard that opens a legitimacy path for the demonstrably
 *   oppressed while filtering opportunistic exit threats — coexists with
 *   asymmetric costs: proof burdens fall hardest on resource-poor movements,
 *   adjudicating institutions accumulate interpretive authority with every
 *   filing, and federations carry a standing exposure to adjudicated
 *   territorial loss. CONSTRAINT FAMILY NOTE: this file instantiates ONE
 *   reading of the secession-legitimacy kernel; the
 *   constitutional-impossibility, popular-sovereignty, and treaty-primacy
 *   readings are separate constraints with their own epsilon values and
 *   victim sets, linked through network.affects_constraints. The epsilon here
 *   refers to the standing arrangement this reading constitutes — the
 *   threshold-standard regime itself — assessed by this reading's own lights,
 *   not to any sibling's arrangement.
 *
 * KEY AGENTS:
 *   - threshold_adjudicating_institutions: Agenda-setting adjudicator (institutional/constrained) — administers threshold determinations and collects interpretive authority
 *   - aggrieved_regions_above_threshold: Primary beneficiary (moderate/constrained) — receives the legitimacy pathway upon demonstrated federal overreach
 *   - subthreshold_grievance_regions: Primary target (powerless/trapped) — bear continued incorporation plus proof burdens they cannot reach
 *   - federal_governments: Dual-positioned payer/beneficiary (powerful/mobile) — carry dissolution exposure and defensive burdens, shielded from opportunistic exits
 *   - resource_poor_secessionist_movements: Target (organized/identity_locked) — finance documentation and litigation; leadership fused to the cause
 *   - international_recognition_community: Secondary beneficiary (institutional/arbitrage) — collects a decision standard it can apply selectively
 *   - minority_populations_within_seceding_regions: Excluded seat (powerless/trapped) — would contest the region's exit; outside the threshold conversation
 *   - indigenous_treaty_holders: Excluded seat (organized/trapped) — hold consent claims the framework neither requires nor hears
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the full structure across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.52).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.38).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Secession Legitimacy Standard").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '039e38b6-07c8-4c88-9d74-cfe183aafe02').
narrative_ontology:cs_kernel_codification('039e38b6-07c8-4c88-9d74-cfe183aafe02', distributed).
narrative_ontology:cs_authority_grounding('039e38b6-07c8-4c88-9d74-cfe183aafe02', expertise).
narrative_ontology:cs_interpretation_layer_present('039e38b6-07c8-4c88-9d74-cfe183aafe02').
narrative_ontology:cs_reading_relation('039e38b6-07c8-4c88-9d74-cfe183aafe02', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('039e38b6-07c8-4c88-9d74-cfe183aafe02', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('039e38b6-07c8-4c88-9d74-cfe183aafe02', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('039e38b6-07c8-4c88-9d74-cfe183aafe02', foundational, structural_injustice_overrides_constitutional_text).
narrative_ontology:cs_axiom_status(structural_injustice_overrides_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('039e38b6-07c8-4c88-9d74-cfe183aafe02', structural_injustice_overrides_constitutional_text, deontological).
narrative_ontology:cs_axiom('039e38b6-07c8-4c88-9d74-cfe183aafe02', foundational, objective_evidentiary_threshold_required).
narrative_ontology:cs_axiom_status(objective_evidentiary_threshold_required, holdable).
narrative_ontology:cs_axiom_grounding('039e38b6-07c8-4c88-9d74-cfe183aafe02', objective_evidentiary_threshold_required, instrumental).
narrative_ontology:cs_reference_frame('039e38b6-07c8-4c88-9d74-cfe183aafe02', justice_conditioned_legitimacy_standard).
narrative_ontology:cs_drift_state('039e38b6-07c8-4c88-9d74-cfe183aafe02', contemporary_selective_application_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('039e38b6-07c8-4c88-9d74-cfe183aafe02', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_above_threshold).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, threshold_adjudicating_institutions).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, international_recognition_community).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, subthreshold_grievance_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, resource_poor_secessionist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_above_threshold).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, justice_over_legal_form_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, objective_burden_of_proof_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts, independence commissions, and international legal bodies receive secession-dispute filings, set the evidentiary standards for demonstrating sustained federal overreach, issue threshold determinations, and publish reasoned opinions. Each determination expands their jurisdiction and interpretive capital; their dockets, budgets, and scholarly followings grow with the volume of grievance litigation. Declining the role would mean surrendering jurisdiction they currently hold.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, threshold_adjudicating_institutions, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, threshold_adjudicating_institutions, beneficiary).

% Regional governments representing populations subjected to sustained, documentable federal overreach assemble evidentiary records, petition adjudicating bodies, and pursue recognition. Where determinations find the threshold crossed, their exit claims acquire a legitimacy that constitutional channels had denied them. Until exit completes, they remain governed by the federal structure they are arguing against, and they fund the documentation effort throughout.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_above_threshold, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions_above_threshold, payer).

% Regions experiencing real hardship or discrimination that does not meet the evidentiary bar continue under federal rule while funding documentation efforts that adjudicators have repeatedly found insufficient. Their populations cannot relocate out of the federation, and the standard for legitimating their exit remains above what their circumstances can demonstrate.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, subthreshold_grievance_regions, payer,
    powerless, generational, trapped, regional).

% Central governments defend against threshold allegations in courts and international fora, commission counter-evidence, and manage the standing possibility that an adjudicated determination legitimizes territorial loss. The same evidentiary standard that exposes them also screens out opportunistic exit threats from wealthy regions seeking fiscal advantage, and they retain adaptive options — devolution packages, renewed autonomy offers — that can reshape grievances before determinations land.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments, payer,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, federal_governments, beneficiary).

% Movement organizations in poorer regions carry the full cost of the proof requirement — archival documentation, legal teams, international advocacy campaigns — without the fiscal base wealthier regions command. Leadership careers, donor networks, and supporter identities fuse around the cause, making strategic withdrawal from the legitimacy contest personally and politically ruinous even when prospects dim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_poor_secessionist_movements, payer,
    organized, biographical, identity_locked, regional).

% Ethnic, linguistic, and political minorities inside would-be seceding regions often prefer continued federation and would contest the region's exit claim if consulted. Threshold proceedings center the region's grievance against the central government; these populations rarely hold standing in the evidentiary process, and their consent is not among the determinations sought.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, minority_populations_within_seceding_regions, excluded,
    powerless, biographical, trapped, local).

% Treaty nations hold that their agreements predate both federal and regional authority and that no redrawing of sovereignty over their territories is legitimate without their consent. The grievance-threshold framework neither requires their consent nor assigns them a determinative seat; they press their position through parallel litigation and diplomatic channels outside the threshold process.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_holders, excluded,
    organized, civilizational, trapped, regional).

% Third-party states use threshold determinations as the stated basis for extending or withholding recognition of new polities. The standard lowers their case-by-case decision costs, but its application remains theirs: movements aligned with recognizing powers find doors open that similarly situated rivals do not.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_recognition_community, beneficiary,
    institutional, generational, arbitrage, global).

% Academic specialists in comparative federalism and secession doctrine analyze determinations, publish critiques of threshold criteria, and supply the doctrinal vocabulary that adjudicators and movements alike cite. They hold no stake in any particular outcome and observe the full structure across cases and eras.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, threshold_adjudicating_institutions).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, adjudicable standard for determining when exit claims deserve legitimacy — enabling aggrieved regions to prosecute justified exits through demonstration rather than force, while giving federations and third states a filter against opportunistic or fraudulent exit claims.
% TRANSFER_FUNCTION: Moves proof-production costs (documentation, litigation, advocacy) onto secessionist movements and aggrieved regions; moves interpretive authority, jurisdiction, and docket growth to adjudicating institutions; moves defensive-litigation burdens and dissolution exposure onto federal governments; moves legitimacy certifications toward regions able to demonstrate structural injustice.
% ABSENT_VOICES: Internal minorities within would-be seceding regions, who may oppose exit and would contest the grievance framing, sit outside threshold proceedings with no standing in the evidentiary process. Indigenous treaty holders, whose consent claims precede both federal and regional authority, are likewise outside the framework — the doctrine neither requires nor hears them. Populations of regions whose grievances are real but unprovable bear continued incorporation without a hearing. Unanimity within the framework therefore reflects a conversation these seats were never admitted to.
% DISAPPEARANCE_RATIONALE: If the threshold standard vanished overnight, secession disputes would reorganize immediately: movements would revert to pure constitutional argument or to majoritarian and coercive strategies no longer filtered by a proof requirement; federations would lose the procedural arena where they currently manage dissolution risk; third states would lose their stated criterion for recognition and fall back on overt interest alignment; adjudicating institutions would lose a primary jurisdiction. Every seated actor's strategy currently presupposes the standard.
% FOUNDING_PROBLEM: The doctrine was built to close the gap between legal validity and moral legitimacy that strict legal positivism created: when a state's conduct toward a region or people becomes structurally predatory, constitutional text offered the oppressed no lawful exit, and the only available remedies were rebellion or indefinite endurance.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists but is partial: international commission reports in the Badinter tradition, United Nations human-rights mechanisms documenting sustained regional persecution, and federalism scholarship unaffiliated with any secessionist movement all attest that the lawful-exit gap persists. No fully disinterested attestation is available — every adjudicating body qualified to corroborate holds interpretive stakes in the framework's continuation, and this structural fact is itself recorded in the story's receipt surface.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the doctrine extracts proof-production costs from secessionist actors, adjudicative deference from all parties, and status-quo persistence between determinations, offset by the genuine legitimacy pathway it opens and the opportunism filter it provides federations. Suppression 0.38: the doctrine's suppressive force is gatekeeping denial rather than coercion — claims lacking provable grievance are refused legitimacy, and the evidentiary bar prices out poorly resourced claimants — while the sibling readings remain fully live positions, keeping meta-level suppression low. Theater_ratio 0.42: real adjudication occurs, but a growing share of activity is symbolic — grievance dossiers assembled for tribunals that have already signaled reluctance, compliance reviews performed by federations to demonstrate engagement, threshold rhetoric invoked by movements with no viable path to satisfying it. Accessibility_collapse 0.30: alternative readings of the kernel remain entirely available; only within the framework's own logic do alternatives to proof-production collapse. Resistance 0.60: federations reject any extra-textual legitimacy standard, popular-sovereignty advocates reject the proof requirement as disenfranchising democratic majorities, and treaty holders reject the framework's silence on consent. The measurement series run on ONE shared time grid (points 0,4,8,12,16,20,24) with all three metrics authored at every point; trajectories are monotonically rising with no oscillation — extraction accumulates as adjudicative machinery professionalizes and threshold language becomes common rhetorical currency, so no cyclical-pattern analysis is warranted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the adjudicator seat the arrangement is principled standard-application — each determination a reasoned exercise of the framework's purpose. From the subthreshold-region seat the same structure is a gate that has never opened despite grievances the region experiences as severe. From the federation seat it is a standing existential risk managed through procedural delay and counter-evidence. From the movement seat it is a costly proving ground whose entry fee scales inversely with the region's wealth. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-declared seats (above-threshold regions, adjudicating institutions, recognition community) derive directionality near the beneficiary end: the doctrine subsidizes them with legitimacy pathways, jurisdiction, and decision standards. Victim-declared seats (subthreshold regions, resource-poor movements, federal governments) derive near the target end: they bear the proof costs, the continued incorporation, and the dissolution exposure respectively. ONE OVERRIDE: powerful -> 0.62. Victim-listing the federation would drive its derived d toward the full-target end, but the same doctrine that exposes it also shields it — the objective burden screens out opportunistic exit threats from wealthy regions, and its mobile exit options (devolution, autonomy renegotiation) further damp its target-side position. The override corrects the overshoot the pure victim derivation would produce. On the organized atom, movements derive high d correctly from their victim listing; treaty holders, excluded and unlisted in both arrays, take the canonical fallback — their position is carried by their role and situation rather than by d, which is appropriate for a seat the framework does not process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the lawful-exit gap left when strict legal positivism strands oppressed peoples inside hostile states — remains live, so no mandatrophy resolution is declared. The tangled-rope classification guards both error directions: reading the doctrine as pure coordination would erase the proof-burden asymmetry, the adjudicator authority rents, and the status-quo bias baked into any burden-of-proof allocation; reading it as pure extraction would erase the genuine pathway it opens for demonstrably oppressed regions and the opportunism filter that even doctrine-skeptical federations implicitly rely on. The receipt surface sharpens the picture: gains land on the adjudicator seat (interpretive authority, jurisdiction, dockets, scholarly followings), which is why gain_flow names it rather than defaulting to diffuse — and fixing_cost is prohibitive because the standard is embedded in jurisprudence, recognition practice, and movement strategy such that no single seat can retract it without dismantling the surrounding legitimacy architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the grievance_threshold_reading of kernel secession_legitimacy_boundary — would instantiating a sibling reading (constitutional_impossibility, popular_sovereignty, treaty_primacy) change the victim set, directionality structure, or epsilon?',
    'Generate the three sibling stories and compare per-seat classifications and victim sets; divergence in victim sets (none under textual impossibility, referendum losers under popular sovereignty, non-consenting governments under treaty primacy) confirms distinct constraints rather than one constraint measurable through any observable.',
    'If sibling stories converge on this story''s structure, the kernel is less contested than the four-reading framing assumes and this story''s epsilon generalizes; if they diverge as expected, each reading carries its own epsilon and cross-reading aggregation is invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: one of four readings; sibling instantiation would alter the victim set and legitimacy conditions.').

omega_variable(
    threshold_determinacy,
    'Is the objective threshold for structural injustice determinate enough that equivalent grievance profiles receive equivalent determinations across adjudicating bodies?',
    'Code adjudicated secession-dispute outcomes against standardized grievance-severity measures across jurisdictions and eras; test determination variance residual to severity.',
    'If determinate, the proof burden is a genuine filter and measured extraction approximates coordination cost; if indeterminate, the burden operates as discretionary gatekeeping and effective extraction on movements is higher than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determinacy, empirical, 'Whether the objective threshold is determinate or adjudicator-relative.').

omega_variable(
    victim_set_contingency_on_threshold,
    'Does this reading''s victim set exist only after a threshold determination — are sub-threshold aggrieved regions victims of the doctrine itself, or merely outside its protection?',
    'Track regions with documented severe grievances that failed threshold determinations: if their continued incorporation is attributable to the doctrine''s gatekeeping rather than to federal refusal independent of any standard, the doctrine itself generates the victim set.',
    'If the doctrine generates victims pre-determination, suppression is understated and the reading drifts toward extraction-dominant; if victims arise only from post-determination denial, the reading''s coordination framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_contingency_on_threshold, conceptual, 'Contingency of the victim set on threshold crossing — the reading''s declared structural delta.').

omega_variable(
    selective_application_driver,
    'Is variance in threshold application across cases driven by grievance severity or by the seceding region''s alignment with powerful states?',
    'Comparative coding of recognition and adjudication outcomes for parallel grievance profiles receiving divergent treatment, tested against geopolitical alignment variables.',
    'If alignment-driven, theater_ratio is understated — the standard functions as cover and the reading sits closer to extraction-dominant; if severity-driven, the doctrine performs as its reference frame describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_application_driver, empirical, 'Severity-driven versus alignment-driven variance in threshold application.').

omega_variable(
    internal_minority_scope,
    'Are intra-region minorities who oppose exit inside or outside this reading''s adjudicative scope?',
    'Examine whether threshold determinations in practice weigh internal-minority consent alongside federal-overreach evidence.',
    'If in scope, the absent-voice concern is mitigated and consensus provenance strengthens; if out of scope, unanimity within the framework reflects exclusion of a dissenting seat, weakening the reading''s legitimacy claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_minority_scope, conceptual, 'Scope of internal-minority consideration in threshold adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_grievance_threshold_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t0, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t4, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t4, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t8, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t8, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t12, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t12, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t16, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t16, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t20, observed).
narrative_ontology:measurement(secession_grievance_threshold_tr_t24, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(secession_grievance_threshold_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(secession_grievance_threshold_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t0, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t4, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t4, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t8, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t8, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t12, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t12, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t16, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t16, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t20, observed).
narrative_ontology:measurement(secession_grievance_threshold_be_t24, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(secession_grievance_threshold_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(secession_grievance_threshold_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t0, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t4, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t4, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t8, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t8, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t12, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t12, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t16, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t16, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t20, observed).
narrative_ontology:measurement(secession_grievance_threshold_su_t24, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement_basis(secession_grievance_threshold_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'secession legitimacy' conflates four structurally distinct claims about what legitimates exit. This file authors the grievance-threshold claim alone, with its own epsilon (0.52), its own victim set (contingent on threshold determinations), and its own adjudicator-centered receipt surface. The sibling files author the textual-procedure, majoritarian, and treaty-consent claims respectively; their epsilon values and victim sets differ (e.g., the constitutional-impossibility reading has no unilateral-exit victims by construction, while the treaty-primacy reading's victim set centers non-consenting governments). Upstream/downstream structure: the grievance-threshold reading's evidentiary apparatus exerts downstream pressure on how treaty-primacy claims must be framed to gain traction, and stands in direct logical contradiction with the constitutional-impossibility reading's exhaustivity premise. Cross-reading comparison proceeds through per-seat classifications, never through merging epsilons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
