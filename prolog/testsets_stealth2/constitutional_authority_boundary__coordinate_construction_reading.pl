% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction of Constitutional Authority (Distributed Interpretive Reading)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The constitutional text establishes three co-equal branches, and under
 *   this reading each branch interprets the constitution within its own
 *   sphere with no single final arbiter: judicial rulings bind the parties
 *   before the court but do not command the other branches, which may respond
 *   with legislation, override mechanisms, or non-acquiescence; the
 *   electorate arbitrates deadlocks episodically through elections and
 *   amendment. This story instantiates ONLY the
 *   coordinate_construction_reading of the constitutional_authority_boundary
 *   kernel; the judicial_supremacy_reading and parliamentary_primacy_reading
 *   are separate constraint files with their own epsilon values, beneficiary
 *   structures, and classifications, linked through the network. The epsilon
 *   referent is the standing distributed-authority arrangement as this
 *   reading assesses it by its own lights: a real anti-monopoly coordination
 *   achievement that nonetheless imposes genuine, measurable costs on those
 *   who need constitutional questions settled. The claim/metrics split is
 *   deliberate: the reading CLAIMS tangled_rope (genuine coordination plus
 *   real asymmetric extraction), and the authored metrics independently
 *   describe moderately extractive, actively maintained operation.
 *
 * KEY AGENTS:
 *   - elected_legislative_majorities: co-equal interpreter, dual-positioned ([powerful]/[mobile]) — retains lawmaking latitude, pays confrontation and legitimation costs
 *   - presidential_administrations: co-equal interpreter, dual-positioned ([powerful]/[mobile]) — enforcement discretion gained, credibility paid in standoffs
 *   - constitutional_courts: dual-positioned interpreter ([institutional]/[identity_locked]) — retains voice and jurisdiction, pays when rulings fail to stick
 *   - rights_dependent_minorities: primary target ([powerless]/[trapped]) — protections fluctuate with branch influence
 *   - constitutional_litigants: primary target ([moderate]/[constrained]) — bears indeterminacy, delay, and repeat-litigation costs
 *   - national_electorate: backstop agenda-setter ([organized]/[trapped]) — episodic, bundled arbitration through elections and amendment
 *   - opposition_parties: beneficiary ([organized]/[mobile]) — multi-venue contest surface
 *   - citizens_without_standing: excluded voice ([powerless]/[trapped]) — governed without a procedural seat
 *   - subnational_governments: excluded voice ([organized]/[constrained]) — absorbs federalism spillover from inter-branch deadlock
 *   - comparative_constitutional_scholars: analytical observer ([analytical]/[analytical]) — outside corroboration seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.44).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction of Constitutional Authority (Distributed Interpretive Reading)").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "political/legal").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '331f718b-91ab-4bdf-a220-6954cc7be043').
narrative_ontology:cs_kernel_codification('331f718b-91ab-4bdf-a220-6954cc7be043', fixed_text).
narrative_ontology:cs_authority_grounding('331f718b-91ab-4bdf-a220-6954cc7be043', distributed).
narrative_ontology:cs_reading_relation('331f718b-91ab-4bdf-a220-6954cc7be043', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('331f718b-91ab-4bdf-a220-6954cc7be043', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('331f718b-91ab-4bdf-a220-6954cc7be043', foundational, no_branch_finally_certifies_its_own_constitutionality).
narrative_ontology:cs_axiom_status(no_branch_finally_certifies_its_own_constitutionality, holdable).
narrative_ontology:cs_axiom_grounding('331f718b-91ab-4bdf-a220-6954cc7be043', no_branch_finally_certifies_its_own_constitutionality, deontological).
narrative_ontology:cs_axiom('331f718b-91ab-4bdf-a220-6954cc7be043', secondary, inter_branch_mutual_checking_sustains_legitimacy).
narrative_ontology:cs_axiom_status(inter_branch_mutual_checking_sustains_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('331f718b-91ab-4bdf-a220-6954cc7be043', inter_branch_mutual_checking_sustains_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('331f718b-91ab-4bdf-a220-6954cc7be043', coequal_departmental_balance).
narrative_ontology:cs_drift_state('331f718b-91ab-4bdf-a220-6954cc7be043', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('331f718b-91ab-4bdf-a220-6954cc7be043', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, elected_legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, opposition_parties).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, rights_dependent_minorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, constitutional_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, elected_legislative_majorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, constitutional_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enact statutes, propose amendments, and confirm appointments. They read the constitution for themselves when judging what may lawfully pass, and may answer a judicial ruling they reject with new legislation, override mechanisms, or jurisdiction-limiting proposals rather than treating the ruling as conclusive. Retained lawmaking latitude flows to them; the political capital spent contesting rulings and defending their reading in public flows from them. Leaving the arrangement is unavailable; maneuvering within it is extensive.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, elected_legislative_majorities, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, elected_legislative_majorities, payer).

% Execute the law, issue directives, and appoint officials and judges. They interpret the constitution in deciding what to enforce and how, and may decline to acquiesce in a ruling they read as wrong, at the price of public confrontation and possible electoral penalty. Enforcement discretion calibrated by their own reading flows to them; drained agenda time and credibility in standoffs flow from them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, presidential_administrations, payer).

% Adjudicate constitutional questions brought before them and publish reasoned rulings binding on the parties before them. Their rulings persuade the other branches but do not command them: a ruling the legislature or executive rejects may be overridden, circumvented, or left unenforced. Jurisdiction and interpretive voice flow to them; failed rulings, politicized appointments, and attack from both flanks flow from them. Their composition, procedure, and self-understanding are constituted by the text they interpret; stepping outside the arrangement would mean ceasing to be a constitutional court at all.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_courts, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, constitutional_courts, payer).

% Depend on favorable constitutional interpretation for protections they cannot secure through ordinary politics. When the branch currently sympathetic to them loses influence, their protections waver with it: a protection won in court can be undone by legislation or non-enforcement, and a statute shielding them can be struck or disregarded. They cannot leave the jurisdiction and rarely win the electoral arbitrations that settle inter-branch standoffs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, rights_dependent_minorities, payer,
    powerless, generational, trapped, national).

% Bring cases seeking a definitive answer and may instead receive rulings that trigger further contest: renewed legislation, non-enforcement, dueling official interpretations. They bear delay, repeated litigation cost, and planning uncertainty. Their exits are limited to abandoning the claim or pursuing political remedies outside the courts.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_litigants, payer,
    moderate, immediate, constrained, national).

% Elects the occupants of every branch and alone ratifies amendments. When the branches deadlock over constitutional meaning, elections serve as the backstop arbitration: the side that wins votes sees its reading staffed, funded, and sustained. The arbitration is episodic, blunt, and bundled with every other question on the ballot; individual voters cannot opt out of living under the outcome.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, national_electorate, agenda_setter,
    organized, generational, trapped, national).

% Contest the governing majority from outside office. Distributed interpretive authority gives them multiple venues for that contest: courts open to their filings, executive veto points, legislative blocking positions, and the electoral arbitration itself. Concentrating final interpretive authority in any one branch would narrow their contest surface to that branch's door.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, opposition_parties, beneficiary,
    organized, biographical, mobile, national).

% Are governed by whatever constitutional meaning prevails but hold no procedural seat in its production: no standing, no litigation resources, no agenda access, and contact with the process only through the distant franchise. They would object that inter-branch bargaining trades their protections for institutional balance; they appear in the arrangement only as its subjects.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens_without_standing, excluded,
    powerless, generational, trapped, national).

% States, provinces, or comparable units operate under the same constitutional text and depend on stable allocations between levels of government. Disagreement among the national branches over federalism questions leaves them squeezed between conflicting mandates with no arbiter to appeal to. They lobby, litigate as parties, and absorb the spillover, but are not principals in the tripartite balance.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, subnational_governments, excluded,
    organized, generational, constrained, regional).

% Study the arrangement across countries and centuries: how often each branch's reading prevails, which overrides and non-acquiescence events succeeded, and whether the founding anti-concentration problem remains live. They hold no power inside the arrangement and can be ignored by all three branches, which is precisely what makes their testimony usable as outside corroboration.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents monopolization of constitutional meaning by any single institution: each branch interprets within its sphere, no institution can finally certify its own acts as constitutional, and each branch's interpretive errors remain correctable by the others. The arrangement also forces inter-branch deliberation, since a reading imposed without persuading the other branches invites override or non-acquiescence.
% TRANSFER_FUNCTION: Moves interpretive authority from any would-be final arbiter (the courts under the supremacy reading, the legislature under the primacy reading) to all three branches jointly; moves the costs of unresolved conflict onto litigants (delay, non-final answers, repeated proceedings) and onto rights-dependent minorities (protection volatility); and moves ultimate arbitration to the electorate through elections and amendment.
% ABSENT_VOICES: Citizens without standing and subnational governments would object if seated: the former bear constitutional outcomes with no procedural voice, the latter absorb federalism spillover from inter-branch deadlock. Both are present only as subjects of the arrangement, not participants in it; their absence is what allows the branches to describe the balance as if it cost the branches alone.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, every branch's operating assumptions break: legislatures would face conclusively binding invalidation (or none at all), executives would lose or gain enforcement-final questions, courts would become either sovereign or advisory, and the electoral backstop would arbitrate a different question. Appointment politics, legislative strategy, and litigation incentives all reorganize around whoever the new final arbiter is.
% FOUNDING_PROBLEM: Concentrated interpretive power: the recognition that whichever institution finally defines constitutional limits will bend those limits toward its own aggrandizement. The arrangement was built so that ambition would counteract ambition and no department could certify its own conduct as constitutional.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by comparative constitutional scholarship documenting both historical concentration episodes and the anti-monopoly intent recorded in ratification-era debates; by civic constitutional-monitoring organizations; and implicitly by proponents of the sibling readings, who dispute the mechanism while conceding the anti-concentration motive. No branch's self-attestation is relied upon.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.42, inside the moderate band the reading predicts: the arrangement genuinely coordinates (no interpretive monopoly, redundant correction channels) but genuinely extracts (indeterminacy costs on litigants, protection volatility for minorities, confrontation costs rotating across branches). Suppression is 0.44 and is authored as a raw structural property, unscaled by power or scope: the arrangement does not coerce dissenting readings out of existence (rival readings stay politically live, hence accessibility_collapse 0.45 and resistance 0.55), but it does deny participants the alternative of final resolution, and holding the distribution open requires escalating active assertion against deference accretion. Theater is 0.30 and rising: most inter-branch constitutional activity remains functional, but a growing share is performative positioning for the electoral arbiter. The temporal series run on one shared grid (T=0,40,80,120,160,200,240) with every tracked metric authored at every point: extractiveness rises with a mid-interval dip at the post-confrontation settlement (T=160), theater climbs steadily as constitutional invocation becomes more audience-directed, and suppression_requirement climbs monotonically as deference norms harden and each branch must work harder to hold its interpretive claim open. The enforcement picture is dynamic, which is why suppression_requirement is tracked rather than left to the scalar alone.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the interpreter seats should compute differently. From the litigant and minority seats the arrangement operates as extraction: they pay for its openness with uncertainty and volatility, and they cannot exit. From the branch seats the same structure operates as earned liberty: retained interpretive autonomy worth the confrontation costs. The courts sit genuinely astride the line, collecting voice while paying for failed rulings, which is why the court seat is dual-positioned rather than cleanly beneficiary. The electorate experiences a third thing again: an arbitration burden bundled into ordinary voting. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (both branches plus courts plus opposition parties) drive the derivation toward low d for those seats; the victim declarations (minorities, litigants) drive high d for theirs. Two overrides correct places where the derivation would misread the structure. First, constitutional_courts: derived d from its beneficiary role would sit near the subsidy end, but the court is genuinely dual-positioned — it collects interpretive voice and pays when rulings are overridden or ignored — so the override sets the institutional atom to 0.48, near symmetric. Second, the two elected branches: derived d from beneficiary status would understate what they pay in confrontation capital and legitimation expenditure, so the powerful atom is overridden to 0.27, modestly above the beneficiary pole. Gain_flow is authored as diffuse after checking every seat: interpretive autonomy scatters across three branches, contest surface flows to opposition parties, arbitration prerogative to the electorate, and no single seat demonstrably captures the arrangement's proceeds. Fixing_cost is prohibitive: replacing the arrangement requires amendment-level supermajorities and would be contested at every moment by whichever branch the current balance favors.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Calling this a pure rope would erase the extraction the payer seats demonstrably bear — indeterminacy and protection volatility are real costs routed through the same structure that produces the coordination benefit. Calling it a snare would erase the coordination function — the anti-monopoly redundancy is real, no seat captures the gains, and the arrangement's persistence rests on widespread conviction rather than on suppressing exits. The R5 interview shows the founding problem (concentrated interpretive power) is still live, so the mandate has not outlived its function and mandatrophy is not resolved; the rising theater_ratio series is the monitor to watch, since a coordinate arrangement whose inter-branch contestation became purely performative while deference accreted underneath would be decaying toward inertial maintenance with the founding problem quietly unanswered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the coordinate_construction_reading of the constitutional_authority_boundary kernel; what structural changes would adoption of the sibling readings (judicial_supremacy_reading, parliamentary_primacy_reading) produce?',
    'Comparative adoption analysis across polities that have shifted between readings: identify which seat gains agenda-setter status, how the victim set relocates, and how epsilon migrates under each adoption.',
    'Under the judicial supremacy reading, constitutional_courts becomes sole agenda_setter and legislatures become unremedied payers; under the parliamentary primacy reading, elected_legislative_majorities becomes agenda_setter and courts become subordinate payers. This story''s beneficiary/victim map inverts accordingly, and epsilon re-rates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three readings of the authority-boundary kernel; siblings are separate constraint files.').

omega_variable(
    final_arbiter_location_dispute,
    'Where the kernel disagreement is located: does the constitutional framework contain a final interpretive arbiter at all, and if so which institution holds it?',
    'Not resolvable by data alone: the three readings give mutually exclusive answers to the same structural question (courts / parliament / no one). Resolution comes from which reading a polity''s operative practice and doctrine actually entrench.',
    'The answer determines which of the three family files describes the standing arrangement and where extraction concentrates; the other two files then describe counterfactual or rival arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(final_arbiter_location_dispute, conceptual, 'Location of the inter-reading disagreement: existence and identity of a final interpretive arbiter.').

omega_variable(
    deference_accretion_fragility,
    'Can distributed interpretive authority persist without drifting into de facto judicial supremacy, or does deference accrete to whichever branch is habitually asked the questions, requiring permanently escalating active enforcement?',
    'Longitudinal tracking of override events, executive non-acquiescence incidents, court-curbing proposals, and appointment politics: a declining frequency of successful coordinate assertions indicates accretion; a stable or cyclical frequency indicates sustainability.',
    'If drift is inevitable, this arrangement decays toward soft supremacy, epsilon migrates upward, and the judicial_supremacy_reading file becomes the accurate referent; if sustainable, the arrangement is a stable hybrid with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_accretion_fragility, empirical, 'Whether the coordinate balance is self-sustaining or requires ratcheting enforcement against deference drift.').

omega_variable(
    conflict_cost_incidence,
    'Who actually bears the costs of inter-branch interpretive conflict: litigants facing delay and non-final answers, rights-dependent minorities facing protection volatility, or the branches themselves facing confrontation and legitimacy expenditure?',
    'Case-level tracking of multi-branch disputes: resolution timelines, repeat-litigation rates, and the fate of protections won by politically weak groups when the sympathetic branch loses influence.',
    'Reallocates effective extraction across seats: if costs land mainly on citizen-facing seats, the payer directionality is high and the arrangement is more extractive than branch-level accounting suggests; if costs land on branches, extraction is closer to a coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_cost_incidence, empirical, 'Incidence of the arrangement''s conflict and indeterminacy costs across seats.').

omega_variable(
    soft_supremacy_indistinguishability,
    'If legislative override and executive non-acquiescence are merely rare, tolerated exceptions rather than operative practices, is the standing arrangement coordinate construction or judicial supremacy with safety valves wearing coordinate language?',
    'Frequency-and-threshold analysis: how often invalidating rulings are successfully overridden or refused relative to how often they stand; whether coordinate assertions require extraordinary political coalitions or occur as routine practice.',
    'If exceptions are marginal, this story describes a vanishing arrangement, epsilon and the victim set shift toward the judicial_supremacy_reading profile, and the coordinate reading''s axioms survive only nominally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_supremacy_indistinguishability, conceptual, 'Whether the operative arrangement is genuinely coordinate or soft supremacy under a coordinate label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 160, 0.22).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 200, 0.26).
narrative_ontology:measurement(cons_tr_t240, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 240, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 80, 0.37).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 120, 0.41).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 160, 0.38).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(cons_be_t240, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 240, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 160, 0.38).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 200, 0.41).
narrative_ontology:measurement(cons_su_t240, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 240, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'who interprets the constitution?' decomposes under the epsilon-invariance principle into three structurally distinct claims with distinct epsilon values and distinct victim sets. This file authors the coordinate_construction_reading (distributed authority, no final arbiter, moderate epsilon ~0.42); judicial_supremacy_reading authors concentrated judicial finality (courts as agenda_setter, legislatures as unremedied payers, higher epsilon); parliamentary_primacy_reading authors legislative finality (legislature as agenda_setter, courts subordinate). The coordinate reading is the historical baseline from which the two concentration readings diverge, and each file links the other two through affects_constraints. Measuring 'constitutional interpretive authority' with different observables yields different epsilon values precisely because they are different constraints, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, institutional, 0.48).
constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, powerful, 0.27).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
