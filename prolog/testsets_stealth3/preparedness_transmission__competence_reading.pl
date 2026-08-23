% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Civil Defense Drill and Inspection Transmission Regime — Competence Reading
 *   domain: institutional/civil_defense
 *
 * SUMMARY:
 *   A mature civil-defense jurisdiction mandates recurring evacuation drills
 *   and facility inspections across workplaces, schools, hospitals, and
 *   public buildings, administered by a standing examiner corps. Under the
 *   reading instantiated here, this recurring activity is live exercised
 *   knowledge: each incoming cohort re-validates capability through practice
 *   — veteran instructors carry forward scenario libraries and unwritten
 *   judgment calls, examiners calibrate findings against live exercise
 *   performance, and drill participants improvise effectively when scenarios
 *   vary from the script. The costs are real and recurring: compliance hours
 *   levied on operators and workers, inspection-preparation burden falling
 *   hardest on thinly-staffed premises, and taxpayer funding for the
 *   examining apparatus. KEY AGENTS (by structural relationship):
 *   civil_defense_inspectorate — agenda setter
 *   ([institutional]/[identity_locked]) — administers the schedule, trains
 *   examiners, issues penalties; emergency_response_services — primary
 *   beneficiary ([organized]/[constrained]) — readiness refreshed through
 *   joint exercises; facility_occupants — net beneficiary with incidental
 *   cost ([moderate]/[constrained]); veteran_drill_instructors — knowledge
 *   carrier and status holder ([organized]/[identity_locked]);
 *   large_facility_operators — cost bearer with arbitrage-grade exit
 *   ([powerful]/[arbitrage]); small_facility_operators — cost bearer without
 *   leverage ([moderate]/[constrained]); production_line_workers — direct
 *   time payer with partial voice ([organized]/[constrained]);
 *   informal_settlement_residents — excluded non-participant
 *   ([powerless]/[trapped]); disaster_research_community — analytical
 *   observer ([analytical]/[analytical]). Per the epsilon-invariance
 *   principle this file instantiates ONE reading cleanly: sibling stories
 *   (husk_reading, hybrid_reading) instantiate the rival readings of the same
 *   kernel as separate constraints with their own epsilon, beneficiaries, and
 *   types, linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.45).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.55).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Civil Defense Drill and Inspection Transmission Regime — Competence Reading").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "institutional/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'ca899154-0879-44dc-a351-66925003865e').
narrative_ontology:cs_kernel_codification('ca899154-0879-44dc-a351-66925003865e', formalized).
narrative_ontology:cs_authority_grounding('ca899154-0879-44dc-a351-66925003865e', expertise).
narrative_ontology:cs_interpretation_layer_present('ca899154-0879-44dc-a351-66925003865e').
narrative_ontology:cs_reading_relation('ca899154-0879-44dc-a351-66925003865e', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca899154-0879-44dc-a351-66925003865e', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ca899154-0879-44dc-a351-66925003865e', foundational, rehearsal_preserves_operational_competence).
narrative_ontology:cs_axiom_status(rehearsal_preserves_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('ca899154-0879-44dc-a351-66925003865e', rehearsal_preserves_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('ca899154-0879-44dc-a351-66925003865e', secondary, scenario_variation_builds_novel_failure_recognition).
narrative_ontology:cs_axiom_status(scenario_variation_builds_novel_failure_recognition, holdable).
narrative_ontology:cs_axiom_grounding('ca899154-0879-44dc-a351-66925003865e', scenario_variation_builds_novel_failure_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('ca899154-0879-44dc-a351-66925003865e', live_exercised_capability).
narrative_ontology:cs_drift_state('ca899154-0879-44dc-a351-66925003865e', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ca899154-0879-44dc-a351-66925003865e', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_response_services).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, facility_occupants).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, veteran_drill_instructors).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, small_facility_operators).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, production_line_workers).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, small_facility_operators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, production_line_workers).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, large_facility_operators).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, facility_occupants).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, large_facility_operators).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, practiced_readiness_doctrine).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, skill_decay_without_rehearsal_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the national schedule of mandatory drills and facility inspections, trains and rotates its examiner corps, keeps the finding registers, and issues improvement notices and penalties. Its budget line and staffing requests are argued from inspection volume and drill coverage statistics. Senior examiners advance inside the corps; leaving would mean dissolving the organization they have become, and most serve until retirement.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_inspectorate, agenda_setter,
    institutional, generational, identity_locked, national).

% Fire, rescue, and medical crews take part in joint exercises that refresh inter-agency signaling, staging, and casualty-flow habits before every real deployment needs them. New hires arrive already fluent in the common playbook because the school and workplace drill circuit precedes them. Their operational readiness depends on the exercise calendar continuing; withdrawing would strand coordination habits that decay within months.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_response_services, beneficiary,
    organized, generational, constrained, regional).

% Workers and residents walk the evacuation routes, locate the extinguishers, and learn the alarm meanings on a recurring cycle. The time spent comes out of working or residential hours; the payoff is knowing what to do when a real alarm sounds. Changing employers or buildings resets them into someone else's identical cycle.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, facility_occupants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, facility_occupants, payer).

% Long-serving trainers who carry the scenario libraries and the unwritten judgment calls — when to vary a drill, how far to push a simulated blockage, which cohort needs which surprise. Their standing inside the service rests on being the ones who remember past incidents firsthand. Handing over the tacit material means writing down what is mostly in their heads; most stay until retirement, and the material leaves with them if they go early.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, veteran_drill_instructors, beneficiary,
    organized, biographical, identity_locked, national).

% Run premises on thin margins with no compliance staff. Every drill cycle and inspection visit lands directly on the owner's calendar and payroll, and preparing for a finding competes with billable work. Hiring outside help costs more than the penalty they fear; relocating or switching trades does not leave the national mandate behind. They get safer premises and insured continuity out of the same obligation that eats their week.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, small_facility_operators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, small_facility_operators, beneficiary).

% Give up shift hours to muster, evacuate, and regroup on a schedule set above them. Where collective bargaining exists, representatives negotiate frequency and timing, but the mandate itself is not theirs to set. Moving jobs anywhere in the regulated economy places them under another employer's identical obligation. The evacuation habit they acquire is the same one that gets them out of a real fire.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, production_line_workers, payer,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, production_line_workers, beneficiary).

% Operate major sites with dedicated safety offices. They absorb drill days easily, hire former inspectors as consultants, and hold seats on the committees that draft next year's checklist. Some lobby successfully to tailor requirements to their building classes; a few relocate energy-intensive lines to jurisdictions running lighter schedules. The obligation is a cost center they manage, and partly a channel of influence they use.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, large_facility_operators, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, large_facility_operators, beneficiary).

% Fund the examiner corps, the exercise logistics, and the register infrastructure through appropriations. No individual can opt out of the funding stream, and the benefit arrives as a statistical reduction in catastrophe losses that never announces itself. Support rises sharply after visible incidents and erodes slowly between them.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, taxpayers, payer,
    moderate, generational, constrained, national).

% Live and work in structures that never entered the inspection roll — unpermitted additions, unregistered workshops, dense informal housing blocks. No drill circuit reaches them; when fires and collapses strike these blocks the losses are the highest in the jurisdiction. They have no seat in the consultation rounds that set drill priorities and no channel to request coverage.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, informal_settlement_residents, excluded,
    powerless, immediate, trapped, local).

% Academics and post-event investigators who compile casualty and recovery statistics, compare outcomes across drilled and undrilled sites, and publish skill-retention curves showing how fast evacuation habit decays. Their access depends on cooperation from the services they study; they hold no enforcement power and answer to journals and inquiry commissions rather than to agencies.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, disaster_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, civil_defense_inspectorate).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a perishable, distributed public good — population-scale response habit — that no household or firm would rationally sustain alone: scheduled repetition keeps evacuation behavior, equipment checks, and inter-agency signaling from decaying between rare real events, and inspection cycles catch physical degradation before it compounds. The free-rider problem in preparedness investment is solved by mandating the practice everyone benefits from.
% TRANSFER_FUNCTION: Moves scheduled hours and preparation spending from operators and workers, and appropriations from taxpayers, into the examiner corps and the exercise calendar; moves tacit incident knowledge downward from veteran instructors to each incoming cohort; moves findings upward from site visits into the correction loop; moves penalties from non-compliant operators into public accounts.
% ABSENT_VOICES: Residents and workers in unregistered structures are absent from the priority-setting consultations — they bear the jurisdiction's highest residual losses and hold no seat. Rank-and-file workers enter drill policy mainly through management channels rather than direct representation. Both groups would press for broader geographic coverage of the drill circuit and for worker input into scheduling; their absence flatters the coverage statistics the regime reports about itself.
% DISAPPEARANCE_RATIONALE: If the drill and inspection mandate vanished overnight, the protective fabric would unravel on a decay curve: evacuation habit measurably erodes within months, the instructor corps disperses within a few years, and the first major fire or collapse after the gap would find formerly drilled sites performing like never-drilled ones — with casualty and loss figures to prove it. Insurers would reprice uncertified buildings, the apparatus's thousands of staff would redeploy, and political pressure would rebuild some version of the mandate after the first avoidable mass-casualty event.
% FOUNDING_PROBLEM: Recurring urban fires and industrial disasters showed written escape plans and paper certifications failing in real events: occupants froze at unfamiliar exits, suppression equipment sat unmaintained, and agencies discovered mid-response that their procedures did not interoperate. The arrangement was built to convert paper readiness into embodied, practiced habit that survives workforce turnover — re-walking the routes, re-checking the gear, rehearsing the signals until they hold under stress.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: judicial inquiry commissions and independent fire-investigation units repeatedly attribute lower casualty counts at sites with documented recent exercise histories, and reinsurer loss series show the same gradient across their portfolios — neither source answers to the inspectorate or to operator associations. Operator associations dispute the sizing of the burden but concede the protective gradient itself. The burden-side claims rest mainly on the payers' own testimony, which is the expected asymmetry; the life-safety claim has independent attestation.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.45: the arrangement delivers a genuine protective service, but the compliance stream persistently exceeds marginal delivery cost and the burden concentrates on the least-leveraged payers (small operators, hourly workers, taxpayers) while large operators convert the obligation into consultancy demand and standards-committee influence. Suppression 0.55: the coercive component is structural — penalty schedules, improvement notices, certification withholding, closure exposure — not internalized belief; participants broadly accept the necessity of practice, and unionized workers negotiate timing and frequency rather than existence. Theater 0.23: the majority of drill activity produces rehearsed-but-transferable skill and the majority of inspections surface correctable physical defects; a growing minority is checklist ceremony performed for the register. Accessibility_collapse 0.35: alternatives (voluntary preparedness programs, insurance-tiered incentives, community-run drills) remain partially viable but demonstrably weaker without the mandated spine — they do not collapse. Resistance 0.45: schedule-relief lobbying, deliberate compliance minimization, and bargaining over drill timing are persistent but bounded by post-incident surges in public support. Suppression is authored as a raw structural property and stays unscaled; the engine alone scales extractiveness by directionality and scope. The measurement series run on one single shared time grid (every tracked metric authored at every point 0-30) with mild monotone drift — no oscillation, so no cyclical-reinforcement analysis applies. Identity-lock dynamics: the inspectorate seat shows institutional identity fusion (the organization has become its function — budget arguments, promotion ladders, and doctrine all run through inspection volume, so exit would dissolve the corps); veteran instructors show professional identity fusion (their standing rests on being the ones who remember incidents firsthand, so handing over tacit material is experienced as self-erasure). Coalition consideration: worker payers are partially organized and do extract scheduling concessions, which moderates but does not remove their cost position.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. The agenda_setter seat experiences the arrangement as the thing it built, staffs, and embodies — from inside the corps the exercise calendar is the protective fabric itself and effective extraction from its own vantage is near zero. The trapped and constrained payer seats experience enforced recurring cost decoupled from their risk profiles. The arbitrage seat (large operators) converts the cost into influence and revenue — former inspectors hired as consultants, seats on the checklist committees — so the same obligation reads as manageable overhead with a private upside. The excluded seat experiences not the constraint's weight but its absence: no drill circuit ever reaches them. None of these divergences is adjudicated by the authored claim; the engine computes them from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality (subsidized seats): emergency_response_services receive refreshed readiness and trained recruits; facility_occupants trade modest time for survival-critical habit; veteran_drill_instructors collect status and continuity from the arrangement. Declared victims derive high directionality amplified by exit conditions: small_facility_operators and production_line_workers are trapped or constrained, sitting near the full-target end; taxpayers fund the apparatus with no opt-out. large_facility_operators are victim-declared but their arbitrage-grade exit damps effective extraction substantially — the derivation handles this without an override. The civil_defense_inspectorate is deliberately NOT declared beneficiary or victim: it runs the arrangement rather than merely collecting, and the derivation chain's canonical fallback would misplace a seat whose mandate, budget line, and staffing requests demonstrably ride on inspection volume and drill coverage. A directionality override sets it to d=0.28 — heavily subsidized, bearing only accountability and staffing costs. informal_settlement_residents sit outside the arrangement's coverage entirely: they are authored as excluded for the consensus-provenance check, not as targets — the arrangement extracts nothing from them, which is precisely their complaint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — paper plans failing in real events while capability decays between rare crises — remains live: workforce turnover keeps eroding habit, and novel hazards keep arriving, so periodic re-validation has not outlived its function. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges, a consistent pair producing no zombie flag. The slow theater_ratio climb (0.14 to 0.23) is the quantity to watch: if checklist ceremony continues substituting for varied live exercise, the competence claim degrades toward the husk reading's territory and mandatrophy resolves — the temporal series exists precisely so that transition is dated from data rather than asserted. Classification discipline cuts both ways: reading this arrangement as pure enforced extraction (a snare verdict) would erase the demonstrated protective gradient that post-event inquiries corroborate; reading it as frictionless coordination (a rope verdict) would erase the measurable burden asymmetry between leveraged and leverage-less payers. The tangled-rope claim holds both halves open until the burden-proportionality omega resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_resolution,
    'Does the observed drill and inspection activity instantiate live re-validated competence (this reading), memorial husk, or stratified hybrid transmission?',
    'Blinded scoring of no-notice cross-site exercises for improvisation quality, correlated with post-event forensic outcome divergence across sites differing in prior exercise variety. The sibling stories (husk_reading, hybrid_reading) carry the rival readings and resolve independently; this story''s structural delta — novel-signature recognition and effective improvisation under scenario variation — is the specific quantity the comparison tests.',
    'Resolution toward the husk reading raises theater_ratio and epsilon sharply here and shifts classification toward inertial persistence; resolution toward hybrid forces stratum-level decomposition of this story; confirmation stabilizes the tangled-rope claim with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_resolution, empirical, 'Which reading of the preparedness_transmission kernel the observable activity actually instantiates.').

omega_variable(
    scheduled_vs_no_notice_evidence_gap,
    'Do routinely scheduled drills measure transferable capability or script recall — is the adaptive-capacity delta real, or an artifact of announced exercises?',
    'A randomized no-notice exercise program with blinded scoring against scheduled-exercise baselines, plus longitudinal retention curves tracking cohort performance between drill cycles.',
    'If scheduled exercises measure script recall, this reading''s structural delta is overstated, epsilon rises, and drift toward the husk reading accelerates; if improvisation transfers across scenario variation, the low theater ratio is vindicated and the competence claim firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scheduled_vs_no_notice_evidence_gap, empirical, 'Whether standard exercise formats can distinguish live capability from rehearsed performance.').

omega_variable(
    burden_risk_proportionality,
    'Do compliance costs track site-specific risk profiles, or do they track organizational size and political salience instead?',
    'Regression of inspection hours and drill-day costs against quantified hazard indices per facility class; comparison of finding-severity distributions across operator size bands.',
    'If burden tracks size and salience rather than risk, extraction concentrates on the lowest-leverage payers and the tangled-rope asymmetry deepens toward extraction dominance; proportionate burden supports the coordination-first framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_risk_proportionality, empirical, 'Whether the compliance burden is risk-proportionate or structurally skewed.').

omega_variable(
    authority_locus_underdetermination,
    'Is the regime''s legitimacy grounded in the codified statutory mandate or in the examiner corps'' professional judgment culture — and does the framing choice change the commitment-system classification?',
    'Trace amendment history for divergence between statute text and checklist practice, identifying which governs on the ground when they conflict; interview retired examiners on rule-following versus judgment calls under field conditions.',
    'A statute-grounded framing implies a fixed-text-style kernel buffered by an interpretive corps; a judgment-culture framing implies practice-grounded authority with different drift sensitivity; the commitment-system pattern classification shifts accordingly, and this story currently declares the expertise/judgment framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_locus_underdetermination, conceptual, 'CS-framing under-determination: codified-text versus practitioner-judgment framings of the same authority structure.').

omega_variable(
    excluded_population_coverage_gap,
    'How much aggregate protective value is lost to populations outside the inspection roll, and would inclusion change the declared beneficiary structure?',
    'Casualty and loss data partitioned by building-registration status; a pilot extending the drill circuit into registered informal blocks with follow-up outcome comparison against matched uncovered blocks.',
    'Large uncovered losses would mean the arrangement''s protective reach is narrower than the beneficiary declarations imply, raising the measured cost of exclusion and complicating the net-benefit case presented to the payer seats; negligible residual loss would confirm the current beneficiary structure as complete for practical purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_population_coverage_gap, empirical, 'Protective-value losses concentrated in populations the arrangement does not cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pt_comp_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(pt_comp_tr_t6, preparedness_transmission__competence_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(pt_comp_tr_t12, preparedness_transmission__competence_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(pt_comp_tr_t18, preparedness_transmission__competence_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(pt_comp_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(pt_comp_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.23).

% Extraction over time
narrative_ontology:measurement(pt_comp_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pt_comp_be_t6, preparedness_transmission__competence_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(pt_comp_be_t12, preparedness_transmission__competence_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(pt_comp_be_t18, preparedness_transmission__competence_reading, base_extractiveness, 18, 0.43).
narrative_ontology:measurement(pt_comp_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(pt_comp_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pt_comp_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pt_comp_su_t6, preparedness_transmission__competence_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(pt_comp_su_t12, preparedness_transmission__competence_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(pt_comp_su_t18, preparedness_transmission__competence_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(pt_comp_su_t24, preparedness_transmission__competence_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(pt_comp_su_t30, preparedness_transmission__competence_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'civil defense preparedness transmission' covers rival constitutive claims about one observable body of activity (recurring drills and inspections). Each reading is a separate story with its own epsilon, beneficiary structure, and type: competence_reading (this file — live re-validated capability, moderate bounded extraction, tangled-rope claim), husk_reading (high theater, inertial persistence, piton-side candidate), hybrid_reading (stratum-dependent epsilon, decomposed by infrastructure layer). The competence reading is structurally upstream in the sense that its validation claim is the baseline the other two readings contest; all family members link one another via affects_constraints so contamination analysis can trace whether evidence of ritualization in one reading propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__competence_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
