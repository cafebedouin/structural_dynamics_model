% ============================================================================
% CONSTRAINT STORY: protective_fiction_as_distributed_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protective_fiction_as_distributed_liability, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: protective_fiction_as_distributed_liability
 *   human_readable: The Falsified Rescue Report and Its Eleven-Year Redistribution
 *   domain: institutional/testimonial
 *
 * SUMMARY:
 *   An eleven-year-old falsified testimony from a near-fatal rescue dive was
 *   framed at the time as institutional damage control: blame was shifted
 *   from systemic negligence onto a single diver. That act did not resolve
 *   the underlying liability; it converted it into a distributed standing
 *   debt that has redistributed itself onto new bearers as the original
 *   beneficiary aged into a more senior institutional position. The diver
 *   lost his license and his profession. The rescued child, now an
 *   adjudicator, must maintain forced amnesia that has become a professional
 *   necessity rather than a childhood accommodation. The crew's rehearsed
 *   unanimity frays each year as memories degrade and new personnel arrive
 *   who were never briefed on the fiction. The observable is multiple
 *   independently-maintained, non-conflicting cover accounts persisting
 *   across the gap, each borne at increasing and unequal cost to different
 *   parties.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protective_fiction_as_distributed_liability, 0.81).
domain_priors:suppression_score(protective_fiction_as_distributed_liability, 0.76).
domain_priors:theater_ratio(protective_fiction_as_distributed_liability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protective_fiction_as_distributed_liability, extractiveness, 0.81).
narrative_ontology:constraint_metric(protective_fiction_as_distributed_liability, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(protective_fiction_as_distributed_liability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(protective_fiction_as_distributed_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(protective_fiction_as_distributed_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protective_fiction_as_distributed_liability, snare).
narrative_ontology:human_readable(protective_fiction_as_distributed_liability, "The Falsified Rescue Report and Its Eleven-Year Redistribution").
narrative_ontology:topic_domain(protective_fiction_as_distributed_liability, "institutional/testimonial").

domain_priors:requires_active_enforcement(protective_fiction_as_distributed_liability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protective_fiction_as_distributed_liability, warden_cail_at_time_of_original_report).
narrative_ontology:constraint_victim(protective_fiction_as_distributed_liability, ossin_thray).
narrative_ontology:constraint_victim(protective_fiction_as_distributed_liability, torvel_ashe).
narrative_ontology:constraint_victim(protective_fiction_as_distributed_liability, the_rehearsed_crew).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Eleven years ago, as station warden, authorized and signed off on a report that reassigned blame for a near-fatal rescue dive away from institutional negligence and onto a single diver's alleged error. The falsification minimized immediate institutional damage and preserved the warden's own record. As the warden aged into a senior adjudicating role, the original beneficiary now sits above the very testimony that must keep holding; exit from the consequences is nearly complete, since the record now reads as settled fact and the warden administers the body that would have to reopen it.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, warden_cail_at_time_of_original_report, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(protective_fiction_as_distributed_liability, warden_cail_at_time_of_original_report, agenda_setter).

% The diver blamed in the falsified report. Lost diving license and professional standing on the strength of the cover account. Has no institutional standing to reopen the record and no corroborating physical evidence beyond his own testimony, which the institution has already treated as settled and closed. Exile from the profession is total; the cost of the original falsification is borne entirely by him with no compensating benefit.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, ossin_thray, payer,
    powerless, biographical, trapped, local).

% The child rescued in the original incident, now grown and holding an adjudicating position within the same institutional system. Must maintain forced amnesia about the true sequence of events because acknowledging the truth would destabilize both his own career, which was built partly on the sanctioned narrative, and the position of the very body he now serves within. His silence has converted from a childhood accommodation into a professional necessity — his identity as a credible adjudicator depends on the fiction he cannot personally verify or deny without professional self-destruction.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, torvel_ashe, payer,
    moderate, civilizational, identity_locked, local).

% The crew present at the original incident, who independently maintain a consistent, non-conflicting cover account across the eleven-year gap. Each has aged into different roles — some retired, some still active — and the cost of maintaining unanimity rises as memories fray, as new hires ask questions the old crew cannot answer honestly, and as the group must continuously coordinate a story none of them individually benefits from sustaining any longer.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, the_rehearsed_crew, payer,
    organized, biographical, constrained, local).

% The present-day institutional body that inherited the settled record without having authored the falsification. Has no direct culpability but every incentive to let the record stand, since reopening it would implicate a senior adjudicator (Ashe) and cast doubt on procedures the administration currently relies on for legitimacy. Administers the enforcement machinery — licensing boards, incident archives, personnel records — that keeps the cover account load-bearing.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, current_station_administration, agenda_setter,
    institutional, generational, arbitrage, regional).

% Investigators of subsequent, unrelated incidents who would benefit from an honest baseline of how institutional near-misses were historically handled. They are not in the room when the original cover account is maintained and have no access to the true sequence of events; the persistence of the fiction distorts the precedent they inherit for evaluating future institutional failures.
narrative_ontology:constraint_stakeholder(protective_fiction_as_distributed_liability, future_incident_investigators, excluded,
    moderate, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(protective_fiction_as_distributed_liability, warden_cail_at_time_of_original_report).
narrative_ontology:fixing_cost_class(protective_fiction_as_distributed_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the moment of the original incident, the falsified report solved a real coordination problem: preventing a chaotic, multi-party blame cascade in the immediate aftermath of a near-fatal rescue, allowing the station to continue operating without an institution-destabilizing inquiry.
% TRANSFER_FUNCTION: Moves the cost of institutional protection from the original beneficiary (who avoided career damage and kept the station's record clean) onto Ossin Thray (revoked license, foreclosed career), onto Torvel Ashe (forced amnesia now weaponized against his own credibility), and onto the crew (an escalating coordination burden to keep the story consistent across eleven years).
% ABSENT_VOICES: Ossin Thray has no standing in the current adjudication process to challenge the settled record. Future incident investigators who would benefit from an honest precedent are structurally never consulted, since the record reads as closed and uncontested.
% DISAPPEARANCE_RATIONALE: If the cover account collapsed, Ossin Thray's professional exile would be exposed as unjust and potentially reversible, Torvel Ashe's adjudicating authority would be immediately compromised by the revelation that his position rests on a story he cannot personally corroborate, the crew's rehearsed unanimity would fracture into individually inconsistent accounts, and the current administration would face a legitimacy crisis over procedures built atop the falsified precedent.
% FOUNDING_PROBLEM: An institutional near-fatal rescue incident threatened to expose systemic negligence; a single falsified report was authored to contain the immediate reputational and legal fallout by assigning fault to one diver rather than the institution.
% FOUNDING_PROBLEM_CORROBORATION: No party outside the original beneficiary's chain of institutional succession corroborates that the founding problem remains live. Ossin Thray attests, from outside any benefiting seat, that the report was false at the time it was filed. The rehearsed crew, who bear the ongoing cost of maintaining the account without deriving any current benefit from it, are the closest thing to independent corroboration that the arrangement now serves inertia rather than any functioning protective purpose.
narrative_ontology:disappearance_verdict(protective_fiction_as_distributed_liability, world_rearranges).
narrative_ontology:founding_problem_status(protective_fiction_as_distributed_liability, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(protective_fiction_as_distributed_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(protective_fiction_as_distributed_liability, 'none', 1).
narrative_ontology:epsilon_provenance(protective_fiction_as_distributed_liability, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protective_fiction_as_distributed_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(protective_fiction_as_distributed_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(protective_fiction_as_distributed_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises steadily across the interval (0.48 to 0.81) because the cost of maintaining the fiction compounds: the crew must coordinate against increasingly divergent memories, Ashe's identity-lock deepens as his career accrues more institutional weight resting on the unexamined story, and Thray's foreclosure becomes more permanent as re-licensing pathways close with time. Suppression tracks upward similarly (0.50 to 0.76) as the enforcement apparatus required to keep the record settled must work harder against accumulating structural pressure — new hires, generational turnover, and Ashe's own rising visibility as adjudicator all increase the surface area the cover story must survive. Theater ratio rises moderately (0.20 to 0.42) as an increasing share of the maintenance activity becomes performative unanimity rather than any actual continuing protective function — the original danger (institutional collapse from the incident) is long past; what remains is defense of the fiction itself.
 *
 * PERSPECTIVAL GAP:
 *   From Warden Cail's seat, viewed today, the arrangement can appear to have simply become 'the record' — settled fact requiring no active defense, closer to natural history than active extraction. From Thray's seat, the same structure is a continuously operating machine that manufactured his exile and keeps it manufactured. From Ashe's seat, the arrangement is neither pure victimhood nor pure benefit — it is an identity trap where the cost of exposure now exceeds even his own private interest in the truth. The engine should compute these divergently from the same structural facts; the claim of 'snare' is authored from the position that sees the escalating, unevenly distributed cost as the constraint's actual operation, not from any single seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Warden Cail sits nearest the beneficiary end: institutional power, arbitrage-grade exit (having aged into a role that adjudicates the very record in question), and the sole party who profited from the original act without bearing any of its escalating cost. Thray sits at the full-target end: powerless, trapped, professionally destroyed by an act he did not commit, with no institutional standing to contest it. Ashe occupies an unusual position — nominally an adjudicator with moderate power, but identity-locked because his professional legitimacy is now structurally fused with the very fiction that once protected him as a child; his exit options are worse than his power level would suggest, which is why an override toward the target end is warranted rather than trusting derivation from power level alone. The crew sits between: organized, capable of collective action, but constrained by the accumulated cost of eleven years of coordination sunk into a story none of them individually benefits from continuing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — averting institutional collapse in the immediate aftermath of a near-fatal incident — is dead; no live emergency requires the fiction's continuation today. What remains is a mandate that has outlived its function but continues to be serviced because the accumulated cost of admission (to Ashe's career, to the current administration's legitimacy, to the crew's coherence) now exceeds the cost of continued maintenance for those with power to end it. This is precisely the distributed-liability signature: the debt was never discharged, only redistributed onto parties who did not create it and increasingly cannot afford to keep paying it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ashe_complicity_or_captivity,
    'Is Torvel Ashe a knowing participant sustaining the fiction for his own career benefit, or a captive of a story imposed on him as a child that he now cannot safely disown?',
    'Private testimony obtained under confidentiality protection, or comparison against contemporaneous records from immediately after the original incident (before Ashe would have had adult agency to shape the narrative).',
    'If knowing participant, Ashe shifts toward a secondary beneficiary role and the victim count narrows; if captive, the victim classification holds and the identity-lock override is justified as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ashe_complicity_or_captivity, empirical, 'Whether Ashe''s maintenance of the fiction is voluntary self-interest or imposed identity capture.').

omega_variable(
    crew_unanimity_mechanism,
    'Is the crew''s continued unanimity sustained by genuine shared belief in the necessity of the original decision, by fear of individual liability for perjury/falsification if the story breaks, or by simple social conformity pressure within a small closed group?',
    'Structured, separated interviews of individual crew members with legal immunity offered, to see whether accounts diverge once the coordination cost of consistency is removed.',
    'If liability fear dominates, suppression is substantially structural (external, legal); if conformity/belief dominates, a larger share is internalized and the suppression metric under-describes the true mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crew_unanimity_mechanism, empirical, 'Structural vs internalized basis for the crew''s eleven-year testimonial consistency.').

omega_variable(
    reopening_cost_asymmetry,
    'Would reopening the record today cost the current administration more than it would cost to let Thray''s exile and the fraying crew unanimity continue indefinitely?',
    'Institutional risk assessment comparing legal exposure/reputational cost of reopening versus continued operation, ideally conducted by a party without a stake in either outcome.',
    'If reopening is cheap relative to ongoing maintenance cost, the persistence is better explained by inertia/capture than by rational cost-minimization, strengthening the snare classification over a merely tragic-but-necessary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reopening_cost_asymmetry, empirical, 'Whether continued suppression is currently cost-justified or merely inertial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protective_fiction_as_distributed_liability, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prot_tr_t0, protective_fiction_as_distributed_liability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prot_tr_t2, protective_fiction_as_distributed_liability, theater_ratio, 2, 0.26).
narrative_ontology:measurement(prot_tr_t4, protective_fiction_as_distributed_liability, theater_ratio, 4, 0.31).
narrative_ontology:measurement(prot_tr_t6, protective_fiction_as_distributed_liability, theater_ratio, 6, 0.35).
narrative_ontology:measurement(prot_tr_t8, protective_fiction_as_distributed_liability, theater_ratio, 8, 0.39).
narrative_ontology:measurement(prot_tr_t11, protective_fiction_as_distributed_liability, theater_ratio, 11, 0.42).

% Extraction over time
narrative_ontology:measurement(prot_be_t0, protective_fiction_as_distributed_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prot_be_t2, protective_fiction_as_distributed_liability, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(prot_be_t4, protective_fiction_as_distributed_liability, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(prot_be_t6, protective_fiction_as_distributed_liability, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(prot_be_t8, protective_fiction_as_distributed_liability, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(prot_be_t11, protective_fiction_as_distributed_liability, base_extractiveness, 11, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(prot_su_t0, protective_fiction_as_distributed_liability, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(prot_su_t2, protective_fiction_as_distributed_liability, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(prot_su_t4, protective_fiction_as_distributed_liability, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(prot_su_t6, protective_fiction_as_distributed_liability, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(prot_su_t8, protective_fiction_as_distributed_liability, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(prot_su_t11, protective_fiction_as_distributed_liability, suppression_requirement, 11, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protective_fiction_as_distributed_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(protective_fiction_as_distributed_liability, epistemic_inadmissibility_of_tacit_expertise).

% DUAL FORMULATION NOTE:
% This constraint is downstream of epistemic_inadmissibility_of_tacit_expertise (tangled_rope): the upstream constraint governs why Ossin Thray's tacit, embodied expertise as a diver could not be admitted as authoritative testimony against a written institutional report in the first place, which is precisely what made the original falsification possible and durable. The upstream constraint's coordination function (standardizing what counts as admissible institutional knowledge) is what the downstream constraint (this story) exploits for asymmetric extraction — the tacit knowledge that would have exonerated Thray was structurally inadmissible by the rules the upstream constraint enforces, and that inadmissibility is what let the falsified account stand unchallenged for eleven years.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
