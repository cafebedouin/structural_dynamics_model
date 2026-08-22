% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Findings Compliance Gate — Management Reading
 *   domain: organizational/regulatory
 *
 * SUMMARY:
 *   After the Challenger accident (STS-51-L, January 1986), the Rogers
 *   Commission found that engineers' warnings about O-ring performance at low
 *   temperature failed to reach the officials who authorized launch, and that
 *   program pressure shaped the decision chain. The findings were implemented
 *   at NASA as a compliance architecture rather than a technical boundary or
 *   a quantified acceptance rule: flight readiness now requires
 *   demonstrating, in documented form, awareness of known risks and the
 *   mitigation taken against them. This story instantiates the
 *   management_compliance_narrative reading of the rogers_commission_findings
 *   kernel — the reading under which the findings establish a process
 *   sufficient to proceed once documentation is complete, retaining launch
 *   authority with program management. Under this reading the arrangement has
 *   a genuine coordination function (mandatory written risk communication
 *   across organizational layers, addressing the precise failure the
 *   Commission diagnosed) and a genuine extraction function (field engineers'
 *   halting authority is converted into memo production judged by the same
 *   hierarchy they warn; crews fly under rationales with no physical
 *   ceiling). The claim/metric independence rule applies: claimed_type is
 *   authored from structure (both functions present, enforcement-dependent,
 *   asymmetrically distributed), while the metrics describe observed
 *   operation — including the drift the temporal record shows. CONSTRAINT
 *   FAMILY NOTE: two sibling files decompose the same kernel
 *   (engineering_absolute_threshold, actuarial_risk_acceptance); this file's
 *   epsilon refers only to the compliance arrangement itself, never to the
 *   siblings' arrangements.
 *
 * KEY AGENTS:
 *   - nasa_shuttle_program_management: agenda-setter ([institutional]/[identity_locked]) — administers the documentation gate, judges sufficiency, retains launch authority, receives the converted stopping power
 *   - contractor_senior_managers: beneficiary ([organized]/[constrained]) — supplies documented mitigations, collects contract continuity, reverses its own engineers' recommendations at the customer's pressure
 *   - field_engineers: primary target ([moderate]/[identity_locked]) — halting authority converted to documentation duty, dissent confined to a record their management adjudicates
 *   - shuttle_astronaut_crews: residual bearer ([moderate]/[constrained]) — flies under rationales that bound what is written down, not what the hardware may do
 *   - external_safety_oversight_bodies: excluded ([organized]/[trapped]) — would demand external verification; kept outside the review chain
 *   - congressional_oversight_committees: analytical observer ([institutional]/[analytical]) — hearings and budget levers, no seat in launch reviews
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.7).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.66).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Findings Compliance Gate — Management Reading").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational/regulatory").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'e6fd5900-bfcc-49b6-ab39-52fc00351514').
narrative_ontology:cs_kernel_codification('e6fd5900-bfcc-49b6-ab39-52fc00351514', formalized).
narrative_ontology:cs_authority_grounding('e6fd5900-bfcc-49b6-ab39-52fc00351514', lineage).
narrative_ontology:cs_interpretation_layer_present('e6fd5900-bfcc-49b6-ab39-52fc00351514').
narrative_ontology:cs_reading_relation('e6fd5900-bfcc-49b6-ab39-52fc00351514', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('e6fd5900-bfcc-49b6-ab39-52fc00351514', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('e6fd5900-bfcc-49b6-ab39-52fc00351514', foundational, documented_risk_awareness_and_mitigation_suffice_to_proceed).
narrative_ontology:cs_axiom_status(documented_risk_awareness_and_mitigation_suffice_to_proceed, holdable).
narrative_ontology:cs_axiom_grounding('e6fd5900-bfcc-49b6-ab39-52fc00351514', documented_risk_awareness_and_mitigation_suffice_to_proceed, conventional).
narrative_ontology:cs_axiom('e6fd5900-bfcc-49b6-ab39-52fc00351514', secondary, mitigation_record_production_reduces_recurrence_of_deviance).
narrative_ontology:cs_axiom_status(mitigation_record_production_reduces_recurrence_of_deviance, holdable).
narrative_ontology:cs_axiom_grounding('e6fd5900-bfcc-49b6-ab39-52fc00351514', mitigation_record_production_reduces_recurrence_of_deviance, instrumental).
narrative_ontology:cs_reference_frame('e6fd5900-bfcc-49b6-ab39-52fc00351514', documented_diligence_proceed_standard).
narrative_ontology:cs_drift_state('e6fd5900-bfcc-49b6-ab39-52fc00351514', post_columbia_caib_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e6fd5900-bfcc-49b6-ab39-52fc00351514', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_shuttle_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_senior_managers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, field_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, shuttle_astronaut_crews).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, program_continuity_imperative).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, documented_diligence_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the flight-readiness documentation requirements issued in response to the Rogers report; decides what counts as sufficient demonstration of risk awareness; signs the launch rationale. Final launch authority stays with this seat under the process — the authority the implemented reforms were built around preserving. Career advancement and institutional identity are fused with program continuation; conceding launch judgment to any external body means conceding the program's core operating premise.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_shuttle_program_management, agenda_setter,
    institutional, biographical, identity_locked, national).

% Holds the solid rocket booster contract and participates in launch reviews, supplying the documented mitigation plans the process consumes. Contract renewals and program continuation flow to this seat; its own working engineers' no-fly recommendations arrive through it and reversals happen at its level under customer pressure. Walking away means forfeiting the contract stream that sustains the company.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_senior_managers, beneficiary,
    organized, biographical, constrained, national).

% Produce the risk analyses and mitigation records the process runs on. Their formal authority to halt a launch survives only as a duty to write objections into a record that the management chain then judges for sufficiency. After the accident and the public testimony, dissenting engineers faced professional ostracism and stalled careers. Leaving the program means abandoning the aerospace specialty their working lives are built on; staying means writing memoranda whose adequacy someone else decides.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, field_engineers, payer,
    moderate, biographical, identity_locked, national).

% Fly the missions that documented rationales authorize and bear the residual physical risk those rationales leave unbounded, because nothing in the process sets a limit on acceptable hardware damage — only a limit on what has been written down about it. Crew representatives had no vote in designing the process and limited standing to reject a completed rationale. Declining flight assignments carries a career and identity cost few crews accept after years of training.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, shuttle_astronaut_crews, payer,
    moderate, biographical, constrained, national).

% Independent advisory and audit bodies proposed after the accident to verify risk documentation and mitigation claims from outside the program chain. The implemented process kept verification inside the agency's own review structure instead. They would require external sign-off before launch; their absence from the room is what makes self-documented sufficiency possible.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, external_safety_oversight_bodies, excluded,
    organized, generational, trapped, national).

% Hold hearings, request program documents, and fund investigations into whether the post-accident process functions as intended. They can redirect budgets and mandate reports but do not sit in launch reviews. Their attention spikes after failures and recedes between them.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, nasa_shuttle_program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a mandatory written channel carrying engineering risk knowledge across the organizational layers that separated it from launch decisions — the exact information-suppression failure the Rogers Commission identified — and gives every launch decision a common documented record.
% TRANSFER_FUNCTION: Moves stopping power over launch decisions from field engineers to program management exercised through documented rationale; moves engineer labor into mitigation-record production adjudicated by their own management chain; moves accountability exposure from individual deciders onto the paper trail.
% ABSENT_VOICES: Engineers advocating a hard flight-readiness threshold were not seated in the compliance design; astronaut representatives had no vote; external verification bodies proposed after the accident were left outside the review chain; working-level dissent was channeled into the very documentation system whose adjudication it distrusted.
% DISAPPEARANCE_RATIONALE: If the compliance gate vanished overnight, launch decisions would immediately revert to either undocumented schedule-driven management judgment or engineer refusal without a sanctioned channel — the post-Challenger bargain of flying on documented awareness dissolves, contractor review interfaces collapse, and the manifest cannot be flown as currently arranged. Every named seat's daily work depends on the process existing.
% FOUNDING_PROBLEM: Engineers' warnings about known hardware risk failed to reach the officials who authorized launch, and schedule pressure shaped the decision chain — the process was built so that risk awareness and mitigation must be demonstrated in writing before proceeding.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the Columbia Accident Investigation Board (2003), which found the same broken communication and normalization patterns operating under the fully implemented compliance process seventeen years after Challenger; corroborated academically by independent scholarship on normalization of deviance in the program. The benefiting parties attest the problem was addressed — an assertion discounted accordingly.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70 at interval end) because the process transfers a scarce good — the ability to stop a launch — from the people with the technical basis to use it to the people with the schedule incentive to waive it, while charging the transferors the labor of producing its justification. Suppression (0.66) reflects the enforced narrowing of dissent to a channel owned by the adjudicator, backed by documented post-testimony career consequences; suppression is authored as a raw structural property and is deliberately not scaled by power or scope. Theater_ratio (0.58) is elevated because a growing share of process activity produces records consumed by the process itself — mitigation documentation whose primary reader is the next review board rather than any decision the record could change — while the underlying information-transfer function still operates at some level. Accessibility_collapse (0.42): alternatives do not vanish — whistleblowing channels, congressional access, and the press survived — but the operational alternative that mattered, a sanctioned halt, collapsed into the adjudicated record. Resistance (0.50): sustained internal resistance, public testimony, and recurring external investigation, insufficient to displace the arrangement but sufficient to keep it defended rather than assumed. The measurement series share one time grid (1986–2004, calendar-year points) so no metric is backfilled from an end-state scalar; suppression_requirement is tracked because the enforcement machinery visibly built up (1986–1989) and then hardened into routine — an enforcement-maturation trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute a coordination-forward type: from program management's position the process is the disciplined due-diligence apparatus it built, and every launch is accompanied by proof of care. The engineer seat should compute an extraction-forward type: the same documents are the tombstone of its halting authority, and the sufficiency judgment always lands above its signature. The crew seat experiences a third thing — a bounded paperwork obligation standing in for an unbounded physical exposure. Nothing in the authored claim adjudicates among these; the engine derives them from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management derives a near-beneficiary directionality: it receives the transferred stopping power and owns the sufficiency judgment, paying only administrative cost. Contractor senior managers derive low d as declared beneficiaries (contract continuity flows to them) with a modest cost component from absorbing customer pressure. Field engineers derive high d amplified by identity_locked exit — their professional conscience and career are both invested in the channel that fails them, placing them nearer the full-target end than mobile victims would sit. Astronaut crews derive high-but-not-full d: they bear the residual risk but also receive whatever genuine protective value the documentation produces. External oversight bodies are excluded rather than coordinated — their exclusion is the load-bearing feature the enforcement maintains, so they register as suppressed demand rather than as participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — warnings not reaching deciders — is still live, which blocks a dead-mandate misclassification, but the composition of process activity has drifted toward maintaining the documentation practice itself rather than moving information: theater_ratio crosses 0.5 during the interval, the classic substitution signal. The tangled_rope classification earns its keep by blocking two symmetric errors. Calling this a rope would launder the veto transfer as mere coordination cost; calling it a snare would erase the real communication function the early years performed and the genuine protective residue crews receive. The persistence question is answered by the cost asymmetry, not by benefit concentration alone: fixing the arrangement requires relocating launch judgment out of the program chain — the very authority the process exists to preserve — so whoever could fix it bears the highest cost of fixing. That places this near the captured-constraint cell of the receipt surface: gains concentrate in a named seat, and repair is prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Which structural element of the rogers_commission_findings does the report''s force attach to: documentation sufficiency (this reading), a physical cessation condition (engineering_absolute_threshold), or quantified informed acceptance (actuarial_risk_acceptance)?',
    'Cross-reading comparison of the three sibling stories'' compiled classifications: if the siblings compute divergent types from the same findings text, the disagreement is confirmed as located in the requirement-binding step, not in the findings themselves.',
    'If the disagreement is located in requirement-binding, no further empirical finding about the report resolves it; the three constraints persist as separate regimes with different epsilon, different beneficiary structures, and different victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: this constraint is one reading of the rogers_commission_findings kernel; the contest is over which requirement the findings establish.').

omega_variable(
    documentation_sufficiency_efficacy,
    'Does documented risk awareness and mitigation actually prevent recurrence of normalized-deviance launch decisions, or does the documentation practice itself become the vehicle of normalization?',
    'Compare anomaly-resolution behavior before and under the compliance regime: closure rates of open risk items prior to launch decisions, and recurrence of known-anomaly flights (STS-51-L, STS-27 tile pitting, STS-112 bipod-ramp loss, STS-107 foam-strike acceptance).',
    'If documentation normalizes rather than prevents, effective extraction rises above the authored 0.70 toward snare territory and the theater_ratio understates the mechanism; if it prevents recurrence, extraction falls and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_sufficiency_efficacy, empirical, 'Whether the compliance process changes launch outcomes or launders them.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of engineering dissent structural (hierarchy, career control, ownership of the sufficiency judgment) or internalized (professional identity fused with organizational loyalty, self-censorship that persists after barrier removal)?',
    'Post-exit trajectory of dissenting engineers: if individuals resume open dissent after leaving the reporting chain, suppression was structural; if former program engineers continue deferring to management sufficiency judgments, the suppression is internalized.',
    'Internalized suppression raises effective suppression above the authored scalar and makes the arrangement robust to formal-channel reform; structural suppression falls with channel redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism ambiguity behind the engineered-silence component of the process.').

omega_variable(
    veto_restoration_possibility,
    'Can engineering stopping power, once converted into a documentation duty, be restored within the incumbent institutions, or does restoration require personnel turnover and relocation of authority outside the program chain?',
    'Observe the post-Columbia return-to-flight reforms: whether the independent technical-authority structures acquired genuine stopping power or reproduced documentation adjudication under new letterhead.',
    'If restoration is impossible within the incumbent structure, the prohibitive fixing-cost judgment is confirmed and the arrangement is sticky regardless of beneficiary preference; if restorable, the arrangement resembles a reversible transitional support rather than a settled bargain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(veto_restoration_possibility, empirical, 'Whether the converted veto can be un-converted without replacing the institutions that hold it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 1986, 2004).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t1986, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1986, 0.28).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t1986, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t1989, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1989, 0.33).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t1989, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t1992, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1992, 0.38).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t1992, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t1995, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1995, 0.42).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t1995, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t1998, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1998, 0.46).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t1998, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t2001, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2001, 0.5).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t2001, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t2003, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2003, 0.56).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t2003, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_tr_t2004, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2004, 0.58).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_tr_t2004, observed).

% Extraction over time
narrative_ontology:measurement(rogers_mgmt_compliance_be_t1986, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1986, 0.52).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t1986, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t1989, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1989, 0.57).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t1989, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t1992, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t1992, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t1995, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t1995, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t1998, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1998, 0.66).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t1998, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t2001, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t2001, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t2003, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2003, 0.7).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t2003, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_be_t2004, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2004, 0.7).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_be_t2004, observed).

% Suppression requirement over time
narrative_ontology:measurement(rogers_mgmt_compliance_su_t1986, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1986, 0.42).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t1986, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t1989, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1989, 0.54).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t1989, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t1992, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1992, 0.59).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t1992, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t1995, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t1995, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t1998, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1998, 0.63).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t1998, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t2001, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2001, 0.64).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t2001, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t2003, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2003, 0.66).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t2003, observed).
narrative_ontology:measurement(rogers_mgmt_compliance_su_t2004, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2004, 0.66).
narrative_ontology:measurement_basis(rogers_mgmt_compliance_su_t2004, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% Constraint family: three readings decompose the colloquial label 'Rogers findings' into structurally distinct claims — engineering_absolute_threshold (a physical precondition: operations cease until redesign is certified), actuarial_risk_acceptance (an epistemic precondition: quantified failure probability documented and accepted by informed deciders), and this file (a procedural sufficiency claim: documented risk awareness and mitigation suffice to proceed). Each is a separate constraint with its own epsilon, beneficiaries, victims, and claimed type; the shared upstream object is the findings text itself, invoked as authority by all three. The actuarial sibling shares this reading's document-and-proceed grammar but adds a quantification requirement; the threshold sibling rejects sufficiency outright. Linked bidirectionally per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
