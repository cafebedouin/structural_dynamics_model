% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Reading (Judicial Gatekeeping of Detainee Treatment)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading — proportionality_balancing — of the
 *   contested kernel humane_treatment_standard (the Common Article 3
 *   humane-treatment guarantee). Under this reading, the permissibility of
 *   any particular treatment of a detainee is not fixed categorically but
 *   determined case-by-case by a court weighing detainee dignity against
 *   concrete security need: courts become gatekeepers, interrogators operate
 *   under a moderate constraint with procedural safeguards, and neither
 *   absolute prohibition nor unlimited discretion governs. Per the
 *   epsilon-invariance principle, the sibling readings (absolute_prohibition,
 *   contextual_necessity) are separate constraints with their own stories,
 *   linked through the network rather than folded into this one. Epsilon's
 *   referent is the standing arrangement under contest —
 *   proportionality-governed detention treatment as actually administered —
 *   assessed by this reading's own lights: the reading itself claims
 *   moderation and safeguarded process, and the authored metrics describe how
 *   that claim performs in operation. Claim and metrics are independent
 *   authored facts: the structure carries a genuine coordination function
 *   (the universality bargain that made humane-treatment law acceptable to
 *   states that would not sign categorical limits, plus a workable procedure
 *   for genuine value collisions) and a genuine extraction asymmetry
 *   (detainees bear the residual risk of a balance that tilts toward security
 *   under emergency pressure).
 *
 * KEY AGENTS:
 *   - detainees_in_custody: Primary target (powerless/trapped) — bears the contingency of protection; their treatment's permissibility is decided in proceedings they do not join
 *   - state_security_apparatus: Primary beneficiary (institutional/arbitrage) — collects interrogation latitude case-by-case and adapts faster than doctrine
 *   - national_executives: Secondary beneficiary (institutional/constrained) — retains flexibility a categorical rule would remove, pays only reputational costs when the balance fails publicly
 *   - reviewing_courts: Agenda-setter and institutional beneficiary (institutional/constrained) — administers the balance, sets its terms, collects jurisdiction and centrality
 *   - human_rights_litigators: Excluded challengers (organized/mobile) — activate the framework but are pushed out of decisive moments by standing and secrecy rules
 *   - treaty_monitoring_bodies: Analytical observers (organized/analytical) — track erosion of the minimum without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.5).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Reading (Judicial Gatekeeping of Detainee Treatment)").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'f2b60758-76e6-487f-99f6-218b2f5ffef3').
narrative_ontology:cs_kernel_codification('f2b60758-76e6-487f-99f6-218b2f5ffef3', fixed_text).
narrative_ontology:cs_authority_grounding('f2b60758-76e6-487f-99f6-218b2f5ffef3', lineage).
narrative_ontology:cs_interpretation_layer_present('f2b60758-76e6-487f-99f6-218b2f5ffef3').
narrative_ontology:cs_reading_relation('f2b60758-76e6-487f-99f6-218b2f5ffef3', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('f2b60758-76e6-487f-99f6-218b2f5ffef3', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('f2b60758-76e6-487f-99f6-218b2f5ffef3', foundational, dignity_security_case_by_case_balance).
narrative_ontology:cs_axiom_status(dignity_security_case_by_case_balance, holdable).
narrative_ontology:cs_axiom_grounding('f2b60758-76e6-487f-99f6-218b2f5ffef3', dignity_security_case_by_case_balance, instrumental).
narrative_ontology:cs_axiom('f2b60758-76e6-487f-99f6-218b2f5ffef3', foundational, courts_as_treatment_gatekeepers).
narrative_ontology:cs_axiom_status(courts_as_treatment_gatekeepers, holdable).
narrative_ontology:cs_axiom_grounding('f2b60758-76e6-487f-99f6-218b2f5ffef3', courts_as_treatment_gatekeepers, conventional).
narrative_ontology:cs_reference_frame('f2b60758-76e6-487f-99f6-218b2f5ffef3', judicial_balance_equilibrium).
narrative_ontology:cs_drift_state('f2b60758-76e6-487f-99f6-218b2f5ffef3', post_9_11_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f2b60758-76e6-487f-99f6-218b2f5ffef3', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_security_apparatus).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, national_executives).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, reviewing_courts).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees_in_custody).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs interrogation and detention operations. Collects case-by-case latitude whenever a court finds a disputed treatment proportional to a concrete security need, and adapts methods to stay inside whatever line the last judgment drew. Adverse rulings cost it little directly: remedies are rarely individual, evidence can be classified, and the next emergency resets the operating environment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Retains operational flexibility that a categorical rule would remove, and avoids the diplomatic cost of open repudiation of humane-treatment law. Pays when the balance fails publicly — inquiry reports, adverse judgments — and manages that exposure through delay, derogation, and litigation posture rather than by changing the framework.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, national_executives, beneficiary,
    institutional, biographical, constrained, national).

% Administers the balancing test and sets its terms through doctrine: what counts as dignity, what counts as necessity, how heavily each weighs. Decides treatment permissibility case-by-case. Gains jurisdiction, institutional centrality, and a protected role from the framework's existence. Bound by precedent, separation-of-powers pressure, and the docket the other seats choose to send it.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, reviewing_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, reviewing_courts, beneficiary).

% Held in custody at specific detention sites. Whether their treatment is permissible is determined by a weighing they do not participate in, usually years after the treatment occurred, often on a record shaped by state-secrets privilege. Remedies are rare, delayed, and seldom individual. They cannot exit custody, the jurisdiction, or the doctrine governing them.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_in_custody, payer,
    powerless, immediate, trapped, local).

% Bring the petitions and suits that activate the framework but are frequently kept out of the decisive moments: standing barriers, closed evidence sessions, national-security assertions that remove the strongest material from their view. Would argue the balance is structurally rigged and press for categorical floors. Operate across forums and jurisdictions, shifting venue when one closes.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_litigators, excluded,
    organized, generational, mobile, global).

% Committee members, special rapporteurs, and equivalent mechanisms that assess state compliance, publish findings, and conduct dialogue with governments. Hold no enforcement power. Track whether the balancing practice is eroding the minimum the kernel was meant to secure, and whether procedural safeguards are doing the work they appear to do.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, treaty_monitoring_bodies, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, state_security_apparatus).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared adjudicative procedure for the genuine collision between detainee dignity and security need: a common vocabulary of proportionality, necessity, and margin that let sovereignty-protective states accept binding humane-treatment constraints they would have refused as categorical rules, and that lets courts dispose of hard cases one at a time instead of refusing jurisdiction.
% TRANSFER_FUNCTION: Moves decision authority over detainee treatment from categorical rules to case-by-case judicial determination; moves the risk of mistreatment onto detainees, who carry it until and unless a court later finds the balance was violated; moves interpretive power, docket, and institutional centrality to the reviewing courts.
% ABSENT_VOICES: Detainees themselves are structurally absent from the balancing that decides their treatment: cases arrive years late, often brought by relatives, estates, or organizations; evidence is withheld under state-secrets privilege; hearings close. Human rights litigators stand outside the decisive moments. The person whose dignity is being weighed is rarely in the room where it is weighed.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, detention law would reorganize around one of the rival readings: a categorical prohibition would dissolve the litigation infrastructure, doctrinal apparatus, and oversight mechanisms built on balancing, while executive self-judgment would collapse judicial review of treatment altogether. Interrogation practice, oversight bodies, and the case flow feeding the courts would all rearrange around whichever successor took hold.
% FOUNDING_PROBLEM: The 1949 Geneva diplomacy needed humane-treatment rules that sovereignty-protective states would actually accept: several delegations refused categorical language for internal armed conflict, so Common Article 3 was left general enough to admit later judicial concretization. The proportionality reading is the institutional answer to that founding bargain — how to bind states that will not accept non-negotiable limits while still constraining them at all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ICRC commentary and the negotiating record of the 1949 Diplomatic Conference document the drafting compromise; academic international-law scholarship on both sides of the dispute attests it; persistent state reservations and derogations attest that the acceptability problem the bargain addressed has not lapsed. The genealogy does not rest on any beneficiary's self-account.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 because the constraint genuinely binds interrogators — treatments must survive judicial weighing, and doctrine has real bite in ordinary times — while the balance's residual risk lands on detainees, whose protection is contingent on winning a test they do not participate in and whose remedies are retrospective and rare. Suppression is 0.50 as a raw structural property (unscaled by power or scope): the framework channels all contestation into judicial review, procedurally excludes the strongest challengers through standing and state-secrets rules, and forecloses the categorical alternative within its own logic, while leaving external alternatives (self-binding legislation, treaty-body pressure) partly available. Theater is 0.32: safeguards and balancing opinions do real work in ordinary-period cases, but a growing minority of the activity is procedural performance that rarely alters outcomes in security-emergency cases. Accessibility collapse is 0.45 — alternatives to the balancing framework persist and are periodically exercised. Resistance is 0.55 — sustained doctrinal and institutional resistance from human-rights organs, litigators, and scholars who read balancing as erosion of the categorical floor. The temporal series share one grid (every tracked metric authored at every point, 1978-2024) and show a cyclical-ratchet pattern: crisis drives deference and extraction up (peak 0.66 in 2009), scandal and report drive partial correction (0.60 by 2014), but each cycle's trough sits above the last (0.45 to 0.52 to 0.58) — the oscillation is partly an extraction mechanism, since each emergency re-ratchets the baseline before correction arrives. The suppression_requirement series tracks real enforcement-capacity change: declaratory doctrine early, machinery built through the 1990s, an enforcement ratchet and counter-ratchet through the post-9/11 contest, settling into a matured, normalized review apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the detainee seat the arrangement is enforced contingency: protection exists only if a court later agrees, with no exit and no participation — near-full-target extraction. From the security-apparatus seat it is a manageable operating environment: latitude granted case-by-case, adaptation cheaper than compliance, arbitrage-grade mobility. From the executive seat it is the price of legitimacy — flexibility retained at the cost of occasional adverse publicity. From the bench it is the framework itself: the balance is not something done to courts but the thing courts are. Same-power divergence is visible between the two institutional beneficiaries: the security apparatus holds arbitrage exit and a generational horizon, the executive holds constrained exit and a biographical horizon, so identical nominal power produces different experienced constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipts: the security apparatus collects usable interrogation latitude (the operative gain — this is also the seat the extraction demonstrably accrues to, hence gain_flow names it); executives collect strategic flexibility; courts collect jurisdiction and institutional centrality, which is why they carry a secondary beneficiary role alongside agenda-setting. The victim declaration maps to detainees, who pay in unbought risk: the difference between categorical protection and won protection is exactly what the balance transfers from them. Directionality follows from these declarations plus exit: trapped and powerless pushes detainees to the full-target end; arbitrage pushes the security apparatus toward the beneficiary end; constrained exit leaves executives somewhat less subsidized than the apparatus; courts sit between — they administer and collect, but cannot arbitrage away adverse doctrine and bear the adjudicative burden, so their position is more symmetric than pure beneficiary status alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this as tangled_rope prevents two opposite mislabels. As pure rope it would hide the asymmetry: the coordination story (universality, adjudicable hard cases) is real, but the same structure moves risk onto the party with no exit, and the balance tilts toward security exactly when extraction matters most. As pure snare it would erase the coordination function: states that refused categorical limits did accept binding constraints through this framework, and courts have issued real, practice-changing rulings through it. The mandatrophy question is live rather than resolved: the founding problem (binding sovereignty-protective states) is contested — the absolute reading denies it was ever legitimate to solve the problem this way, the necessity reading says the problem dominates everything else. Failure modes to watch in the temporal data: if categorical norms become self-enforcing through customary law and universal jurisdiction, the balancing machinery atrophies into theater (piton trajectory — theater_ratio climbing while function decays); if courts stop finding violations while maintaining the appearance of review, the coordination story becomes cover (snare trajectory — theater_ratio climbing while extraction holds). The current series shows theater peaking at 0.42 in 2009 and receding to 0.32, consistent with a framework whose function is degraded but not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (proportionality_balancing) of the humane_treatment_standard kernel; what structurally changes under the sibling readings, and where exactly does the disagreement bite?',
    'Comparative authoring of the sibling stories (humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity) with their own beneficiary/victim structures and epsilon values; the disagreement is located at the structure of permissibility — categorical never, judicially weighed sometimes, executive override.',
    'Under absolute_prohibition the victim set widens to everyone subjected to proscribed treatment regardless of security outcome and the balancing discretion disappears; under contextual_necessity the judicial gate disappears and detainees lose even retrospective review, raising extraction sharply. Classification of THIS story is stable only within this reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    balance_tilt_systematicity,
    'Does the balance tilt toward security systematically — structural deference, secret evidence, retrospective-only review — or only episodically during declared emergencies?',
    'Cross-jurisdictional outcome statistics on treatment challenges: violation-finding rates in emergency versus ordinary periods, remedial rates, and the evidentiary share states successfully withhold.',
    'A systematic tilt shifts the constraint''s operation toward the snare side (coordination vocabulary as cover over steady extraction); an episodic tilt supports tangled_rope with cyclical dynamics and locates the extraction in emergency windows rather than the framework itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_tilt_systematicity, empirical, 'Whether the balance''s pro-security skew is structural or cyclical.').

omega_variable(
    absent_party_balancing_validity,
    'Can a proportionality balance function as genuine coordination when the person being balanced is absent from the proceeding that decides their treatment?',
    'Compare jurisdictions and periods permitting detainee participation, counsel access, and habeas presence against closed-proceedings regimes; measure remedy rates and doctrinal quality differences.',
    'If absence is load-bearing — if the balance works only because the balanced party cannot contest the weighing — the coordination function thins toward cover and extraction concentrates on the absent seat, pushing the computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absent_party_balancing_validity, conceptual, 'Whether the framework''s coordination survives the structural exclusion of detainees from their own balancing.').

omega_variable(
    universality_bargain_reversibility,
    'The framework traded categorical clarity for near-universal state acceptance; now that adherence is broad, is the trade still necessary — could states re-tighten toward the absolute reading without losing adherence?',
    'State practice following strong categorical rulings and treaty-body findings: did adherence, cooperation, or reporting collapse after courts imposed categorical floors, or did it hold?',
    'If the trade is no longer necessary, the framework is transitional support — scaffold-like — and should carry pressure toward sunset in favor of categorical floors; if still necessary, the tangled_rope classification is entrenched and the extraction asymmetry is the standing price of universality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_bargain_reversibility, preference, 'Whether the founding bargain remains necessary or has become self-serving inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1978, humane_treatment_standard__proportionality_balancing, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(huma_tr_t1987, humane_treatment_standard__proportionality_balancing, theater_ratio, 1987, 0.22).
narrative_ontology:measurement(huma_tr_t1999, humane_treatment_standard__proportionality_balancing, theater_ratio, 1999, 0.28).
narrative_ontology:measurement(huma_tr_t2004, humane_treatment_standard__proportionality_balancing, theater_ratio, 2004, 0.38).
narrative_ontology:measurement(huma_tr_t2009, humane_treatment_standard__proportionality_balancing, theater_ratio, 2009, 0.42).
narrative_ontology:measurement(huma_tr_t2014, humane_treatment_standard__proportionality_balancing, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(huma_tr_t2019, humane_treatment_standard__proportionality_balancing, theater_ratio, 2019, 0.33).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__proportionality_balancing, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(huma_be_t1978, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(huma_be_t1987, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1987, 0.48).
narrative_ontology:measurement(huma_be_t1999, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1999, 0.52).
narrative_ontology:measurement(huma_be_t2004, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2004, 0.62).
narrative_ontology:measurement(huma_be_t2009, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2009, 0.66).
narrative_ontology:measurement(huma_be_t2014, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(huma_be_t2019, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1978, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(huma_su_t1987, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1987, 0.34).
narrative_ontology:measurement(huma_su_t1999, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(huma_su_t2004, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2004, 0.56).
narrative_ontology:measurement(huma_su_t2009, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(huma_su_t2014, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(huma_su_t2019, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2019, 0.51).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2024, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Common Article 3 humane treatment' covers three structurally distinct claims that share one fixed text. This story authors the proportionality_balancing reading (judicial gatekeeping, case-by-case weighing, moderate constraint with safeguards). The absolute_prohibition sibling authors the non-derogable-floor claim (epsilon near zero for compliant states, categorical violation otherwise, no balancing discretion). The contextual_necessity sibling authors the security-override claim (executive self-judgment, detainees lose even retrospective review, highest extraction). The upstream/downstream structure runs from the fixed text through this middle reading: proportionality jurisprudence supplies the vocabulary that necessity-invoking states must now dress their claims in, while the absolute reading supplies the floor that proportionality's critics cite. Each member links the others via affects_constraints; epsilon differs across members because the victim set and decision locus differ, not because the text is measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
