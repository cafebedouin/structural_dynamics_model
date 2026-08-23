% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Grievance-Threshold Secession Legitimacy Gate
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates one reading — the grievance_threshold_reading —
 *   of the contested kernel secession_legitimacy_boundary. Under this
 *   reading, secession becomes legitimate when federal actions cross a
 *   threshold of structural injustice, regardless of constitutional text, and
 *   claims must meet an objective burden of proof. Operated as a constraint,
 *   the reading is an evidentiary gate on legitimacy: movements must
 *   demonstrate overreach before their exit claims acquire standing, and
 *   until they do, incumbent federations retain default insulation. The
 *   victim set is conditional by design — it exists only where the threshold
 *   is demonstrably crossed. Three sibling readings
 *   (constitutional_impossibility, popular_sovereignty, treaty_primacy)
 *   instantiate structurally distinct constraints from the same kernel, with
 *   different epsilon values, different beneficiary/victim sets, and
 *   different classifications; they are linked via
 *   network.affects_constraints and are neither described nor averaged inside
 *   this one. KEY AGENTS (by structural relationship): -
 *   incumbent_federal_governments: Shielded beneficiary with conditional
 *   exposure (institutional/arbitrage) - legitimacy_adjudicating_bodies:
 *   Agenda setter collecting adjudicative mandate (institutional/analytical)
 *   - grievance_documented_secession_regions: Conditional beneficiary paying
 *   compliance costs (organized/constrained) -
 *   under_evidenced_structural_grievance_populations: Primary target
 *   (powerless/trapped) - treaty_holders_indigenous_nations and
 *   provincial_referendum_majorities: Excluded parties -
 *   comparative_constitutional_lawyers: Analytical observer seeing the full
 *   four-gate structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.38).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.3).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Secession Legitimacy Gate").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '0243f55a-321d-4498-9ca7-a73d8fa3b7e3').
narrative_ontology:cs_kernel_codification('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', distributed).
narrative_ontology:cs_authority_grounding('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', distributed).
narrative_ontology:cs_reading_relation('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', foundational, structural_injustice_crossing_confers_legitimacy_regardless_of_text).
narrative_ontology:cs_axiom_status(structural_injustice_crossing_confers_legitimacy_regardless_of_text, holdable).
narrative_ontology:cs_axiom_grounding('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', structural_injustice_crossing_confers_legitimacy_regardless_of_text, empirically_contingent).
narrative_ontology:cs_axiom('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', secondary, objective_evidence_prerequisites_legitimacy_claims).
narrative_ontology:cs_axiom_status(objective_evidence_prerequisites_legitimacy_claims, holdable).
narrative_ontology:cs_axiom_grounding('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', objective_evidence_prerequisites_legitimacy_claims, instrumental).
narrative_ontology:cs_reference_frame('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', justice_conditioned_membership_order).
narrative_ontology:cs_drift_state('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', contemporary_recognition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0243f55a-321d-4498-9ca7-a73d8fa3b7e3', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, incumbent_federal_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, grievance_documented_secession_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, under_evidenced_structural_grievance_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, incumbent_federal_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, grievance_documented_secession_regions).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, remedial_secession_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, objective_grievance_assessability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run multi-region states. When a region's exit claim fails to demonstrate the required showing of federal injustice, continuity over that region is secured by default and no concession is owed; when a region does marshal a qualifying showing, the same standard strips the federal position of its legitimacy defenses and forces negotiation or loss. They shape the evidence environment — archives, statistics, incident records — that claimants must draw on to qualify at all.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, incumbent_federal_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, incumbent_federal_governments, payer).

% International courts, recognition-coordinating ministries, and the commission-and-scholarship complex that convenes inquiries and publishes determinations on whether a region's showing of mistreatment qualifies. Operating the standard is these bodies' mandate and professional livelihood; their findings decide which claims advance and which stall indefinitely.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, legitimacy_adjudicating_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Regions whose histories contain atrocities, expropriation, or systematic exclusion severe enough to document. When their evidentiary portfolio qualifies, they acquire a recognized path out that constitutional argument alone would never grant; until it qualifies they spend scarce resources compiling testimony and forensic records against an adversary that controls much of the underlying archive.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, grievance_documented_secession_regions, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, grievance_documented_secession_regions, payer).

% Live under sustained disadvantage — economic drain, political exclusion, cultural suppression — of a kind woven into ordinary administration rather than concentrated in datable events. Qualifying requires showing a threshold of injustice, but their harms are diffuse, the records sit with the state, and no single incident crystallizes the case; their petitions return unanswered year after year.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, under_evidenced_structural_grievance_populations, payer,
    powerless, biographical, trapped, regional).

% Nations whose sovereignty instruments predate the federation and, under their own account, outrank both federal and provincial authority. Any framework that proceeds without their consent misdescribes their position; they decline the premise that their standing must be argued before someone else's gate, and so are absent from the conversations where threshold criteria are set.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, treaty_holders_indigenous_nations, excluded,
    organized, civilizational, identity_locked, regional).

% Regional electorates that have produced, or expect to produce, clear majorities for independence at the ballot box. Their route runs through counting votes; the evidentiary portfolio this framework requires is not a currency they hold, and they regard being asked to prove victimhood as itself part of the injury.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_referendum_majorities, excluded,
    organized, biographical, constrained, regional).

% Track all four legitimacy frameworks across cases, publish the comparisons, and supply the doctrinal vocabulary every seat argues with. Hold no stake in which framework prevails and are routinely retained by every other seat.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, comparative_constitutional_lawyers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, incumbent_federal_governments).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the adjudication problem for exit disputes: without a shared evidentiary standard, secession claims are decided by force or by ad hoc recognizer preference. The threshold gate gives movements a specifiable target, federations a priced exposure, and third parties a decision rule, so that at least some exit conflicts resolve without war.
% TRANSFER_FUNCTION: Moves legitimacy-standing, and the bargaining leverage attached to it, according to demonstrated injustice: claimants who meet the objective proof threshold acquire standing regardless of constitutional text; claimants who cannot produce the evidentiary signature forfeit standing they would hold under rival gates; the forfeited space accrues to incumbent federal governments as default union security.
% ABSENT_VOICES: Treaty-holding Indigenous nations, whose prior-sovereignty consent requirement this reading omits entirely, and provincial-majority democrats, who hold that a clear referendum majority needs no evidentiary portfolio, are both outside the room where threshold criteria and proof standards are drafted. The standard's authors sit in international legal institutions and recognizer foreign ministries; the diffuse-grievance populations it screens have no seat in its design.
% DISAPPEARANCE_RATIONALE: Overnight removal reverts exit adjudication to whichever rival gate each actor prefers: movements currently assembling proof portfolios would redirect to referendum organizing or litigation; incumbent federations lose the default insulation an unmet threshold provides and would face immediate legitimacy contests in every restive region; recognition bodies would improvise criteria case-by-case. The rearrangement is real but bounded — three rival gates stand ready — yet the specific pricing of exposure this gate imposes would vanish with it.
% FOUNDING_PROBLEM: Built in the decolonization era to solve the trapped-population problem: territorial-integrity settlements froze millions inside states they had not chosen and that ruled them unjustly, while an unconditional exit-right threatened to shatter every multi-national state and reward any determined minority. The threshold standard was meant to thread it: a lawful path out for the demonstrably oppressed, closed to the merely ambitious.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: minority-rights monitoring organizations and international human-rights inquiry mechanisms continue documenting populations ruled against their will under entrenched discrimination, and the secession-ethics literature treats unjust-union entrapment as unresolved. The standard's principal beneficiaries — incumbent federations — attest the opposite (that autonomy mechanisms solve it), which is precisely the signal that corroboration was sought outside that set.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).
:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 at interval end): the gate performs a real service — it makes exit disputes adjudicable and channels standing toward demonstrable victims — but its burden of proof falls wholly on the aggrieved party, and structural injustice is exactly the kind of harm that resists the crisp evidentiary signature an objective threshold demands, particularly when the accused state controls the archives, statistics, and incident records the proof portfolio requires. Suppression (0.30) is discursive and institutional rather than physical: rival readings' claims are dismissed, deferred, or reframed as insufficiently evidenced; no coercive apparatus enforces the gate. Theater (0.25) is moderate-low: threshold inquiries are usually real work, though conclusion-versus-evidence correlation loosens visibly where recognizers' strategic interests engage. Accessibility collapse is low (0.20): all three sibling gates remain fully live, so mastering this standard collapses nothing. Resistance (0.50) is sustained and multi-front: constitutionalists reject the text-indifferent clause, referendum democrats reject proof-gating as such, treaty holders reject the omitted consent requirement. Coalition note: the victim seat is fragmented by construction — each threshold case is jurisdiction-specific, so transnational coalition-building among diffuse-grievance populations yields little pooled leverage. Measurement design: all three tracked series share one grid (decade points across the interval); suppression_requirement is authored deliberately because the story traces enforcement-machinery change (Cold War territorial-integrity hardening, post-1989 recognition buildup, post-2010 doctrinal retrenchment), not merely extraction drift. Suppression is authored as a raw structural property; only extractiveness is directionality- and scope-scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the incumbent-federation seat, the gate is prudential: an unmet threshold is simply the absence of a case, and the union continues undisturbed. From the trapped diffuse-grievance seat, the identical gate is a wall — the proof demanded is of a kind their adversary controls or has destroyed, so genuine suffering never acquires the signature that unlocks standing. From the documented-grievance seat, the same gate is a fair court that finally prices injustice honestly. The adjudicating seat experiences the gate as mandate: operating it constitutes their professional existence. Identity-lock note: treaty-holding nations are identity_locked not for want of alternatives but because their sovereignty claim predates the state whose constitution the gate indexes — engaging the framework would concede the framework, which their self-concept cannot absorb; break that identity frame and their position migrates to ordinary interest-group advocacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure: incumbent_federal_governments and grievance_documented_secession_regions benefit; under_evidenced_structural_grievance_populations pays; legitimacy_adjudicating_bodies administers. Incumbents derive near the beneficiary end — the unmet threshold is their default insulation — tempered by conditional exposure whenever a threshold-crossing case succeeds, hence the secondary payer role. Documented-grievance regions sit nearer symmetric than a naive beneficiary read implies: their benefit is contingent on winning an evidentiary contest their adversary influences, and assembling proof portfolios is itself a real cost, hence their secondary payer role. Under-evidenced populations sit at the full-target end: they supply the gate's denied claims and bear its proof burden while receiving none of its protection. The adjudicating bodies carry a beneficiary tilt beneath their agenda-setter role (they collect mandate and professional rents from operating the gate) that the derivation chain cannot see from role declarations alone. No directionality_overrides are authored: the schema keys overrides by power atom, and both institutional seats share one atom, so a corrective override would smear across them; the residuals are flagged here for the engine to compute instead, and any divergence between computed and narrated directionality is signal, not defect.
 *
 * MANDATROPHY ANALYSIS:
 *   Decomposition is the point. Colloquially, 'the law of secession' presents as a single settled rule — which invites either a false summit (treating the dominant reading as natural law) or a blanket verdict (reading every gate as extraction on self-determination). Splitting the kernel into four readings assigns each its own epsilon and its own victim set. This reading's victim set is conditional by construction, which caps its baseline extraction below a categorical prohibition's and above a pure coordination protocol's. The founding problem — trapped populations under unjust rule needing adjudicable exit — is still live, corroborated from outside the beneficiary set, so no mandatrophy is declared; the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_index,
    'This constraint is one reading (grievance_threshold_reading) of kernel secession_legitimacy_boundary; the sibling readings (constitutional_impossibility, popular_sovereignty, treaty_primacy) are separate constraints. Which reading''s gate actually binds in concrete recognition decisions?',
    'Trace adjudicated, recognized, and rejected exit cases to the decisive gate: was the outcome fixed by constitutional-text availability, referendum results, threshold evidence quality, or treaty-consent posture?',
    'If a sibling gate governs, this story''s beneficiary/victim sets relocate wholesale — under treaty primacy an unconditional consent-based victim set replaces this reading''s conditional one, and incumbent federations flip from insulated to bound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_index, conceptual, 'Committer index: identifies which of the four gates binds in practice and locates the kernel disagreement at the gate-choice level.').

omega_variable(
    threshold_measurability,
    'Can structural injustice be assessed objectively enough that the burden of proof the gate imposes is fair to the parties bearing it?',
    'Compare commission and forensic determinations across comparable cases for inter-adjudicator reliability; measure how often identical evidence profiles produce opposite threshold verdicts across jurisdictions.',
    'Poor reliability means the gate''s burden lands arbitrarily on diffuse-grievance claimants; the payer seat computes toward the full-target end and the arrangement drifts snare-flavored at that seat despite its coordination form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_measurability, empirical, 'Whether the objective burden of proof is achievable or structurally biased against diffuse harms.').

omega_variable(
    conditional_victim_filtering,
    'Does conditioning standing on threshold-crossing systematically silence sub-threshold sufferers — the populations whose grievances are real but never crystallize into a qualifiable case?',
    'Longitudinal tracking of unrecognized grievance movements: do they eventually acquire qualifying evidence, abandon their claims, or escalate outside the framework?',
    'Systematic filtering raises effective extraction on powerless seats beyond the authored scalar; eventual qualification at meaningful rates supports the coordination framing and validates the conditional-victim design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_victim_filtering, empirical, 'Whether the conditional victim set functions as due process or as a silencing filter.').

omega_variable(
    geopolitical_filtering,
    'Are threshold determinations driven by the quality of grievance evidence or by the strategic alignment of the recognizing powers?',
    'Code post-1989 recognition decisions jointly for evidence quality and sponsor alignment; exploit natural experiments where comparable evidentiary portfolios met opposite outcomes.',
    'Alignment-driven determinations inflate the theater ratio and mean the gate operates as cover — payer seats experience extraction the standard''s form does not display, widening the computed divergence between the agenda-setter and payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_filtering, empirical, 'Whether threshold application tracks evidence or recognizer interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement(sece_tr_t60, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(sece_tr_t70, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(sece_be_t60, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(sece_be_t70, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 70, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(sece_su_t60, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(sece_su_t70, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 70, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% 'Secession legitimacy' decomposes into four structurally distinct constraints — one per reading of kernel secession_legitimacy_boundary — because each reading fixes a different epsilon referent, beneficiary set, and victim set (epsilon-invariance). This member carries the grievance-threshold gate; its epsilon is assessed on the threshold-standard arrangement as operated, not on the arrangements its rivals would install. The upstream constitutional-impossibility reading supplies the textual baseline against which this reading's text-indifference clause reacts, which is why the family edge runs from that member into this one's contest environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
