% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement
 *   domain: constitutional/civil_rights/legal
 *
 * SUMMARY:
 *   The standing arrangement under contest is the modern qualified-immunity
 *   doctrine as applied under 42 U.S.C. § 1983: an objective-standard shield
 *   (Harlow v. Fitzgerald, 1982) barring damages liability unless the officer
 *   violated a right 'clearly established' at the time, enforced through
 *   interlocutory appeal, summary judgment, and (since Pearson v. Callahan,
 *   2009) discretionary sequencing that lets courts grant immunity without
 *   reaching the merits. This story instantiates ONE reading of that kernel —
 *   the protective-scaffold reading, which holds the arrangement to be a
 *   necessary transitional protection enabling vigorous law enforcement while
 *   constitutional norms crystallize. Per the ε-referent rule, extractiveness
 *   is authored for the STANDING arrangement as this reading sees it
 *   (moderate: real remedial denial concentrated on a identifiable injured
 *   class, bounded by the clearly-established limit and offset by a genuine
 *   protection function), never for the idealized scaffold the reading
 *   endorses. Claim and metrics are independent authored facts: the scaffold
 *   claim comes from this reading's seat; the metric values describe the
 *   doctrine's actual operation, including its hardening over the interval.
 *   Interval maps 0=1982 (Harlow) to 43=2025. KEY AGENTS (by structural
 *   relationship): - sworn_law_enforcement_officers: Primary beneficiary
 *   (organized/constrained) — collects personal-liability protection -
 *   police_unions: Beneficiary and political defender (organized/arbitrage) —
 *   converts adverse rulings into statutory alternatives - federal_judiciary:
 *   Agenda setter (institutional/constrained) — authors, applies, and refines
 *   the doctrine at summary judgment - constitutional_violation_survivors:
 *   Primary target (powerless/trapped) — bears uncompensated injury and
 *   dismissed-suit costs - civil_rights_plaintiffs_attorneys: Secondary
 *   target (moderate/constrained) — absorbs dismissed-case losses and
 *   screening burden - municipalities_public_employers: Indirect beneficiary
 *   (institutional/constrained) — reduced indemnity exposure -
 *   state_legislatures: Excluded actor (institutional/constrained) — reform
 *   capacity outside the federal adjudicative loop - constitutional_scholars:
 *   Analytical observer (analytical/analytical) — documents operation, holds
 *   no lever
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.7).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, scaffold).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional/civil_rights/legal").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:has_sunset_clause(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'ac2f4636-6627-440c-837c-1437f14fabc9').
narrative_ontology:cs_kernel_codification('ac2f4636-6627-440c-837c-1437f14fabc9', formalized).
narrative_ontology:cs_authority_grounding('ac2f4636-6627-440c-837c-1437f14fabc9', lineage).
narrative_ontology:cs_interpretation_layer_present('ac2f4636-6627-440c-837c-1437f14fabc9').
narrative_ontology:cs_reading_relation('ac2f4636-6627-440c-837c-1437f14fabc9', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac2f4636-6627-440c-837c-1437f14fabc9', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('ac2f4636-6627-440c-837c-1437f14fabc9', foundational, immunity_necessary_for_vigorous_enforcement).
narrative_ontology:cs_axiom_status(immunity_necessary_for_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('ac2f4636-6627-440c-837c-1437f14fabc9', immunity_necessary_for_vigorous_enforcement, instrumental).
narrative_ontology:cs_axiom('ac2f4636-6627-440c-837c-1437f14fabc9', foundational, unsettled_law_liability_unfair_to_good_faith_officers).
narrative_ontology:cs_axiom_status(unsettled_law_liability_unfair_to_good_faith_officers, holdable).
narrative_ontology:cs_axiom_grounding('ac2f4636-6627-440c-837c-1437f14fabc9', unsettled_law_liability_unfair_to_good_faith_officers, deontological).
narrative_ontology:cs_reference_frame('ac2f4636-6627-440c-837c-1437f14fabc9', transitional_liability_shield_pending_norm_crystallization).
narrative_ontology:cs_drift_state('ac2f4636-6627-440c-837c-1437f14fabc9', contemporary_police_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac2f4636-6627-440c-837c-1437f14fabc9', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, sworn_law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipalities_public_employers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs_attorneys).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patrol and make split-second decisions under constitutional rules that evolve case by case. The doctrine shields them from personal damages liability unless they violate a right already clearly established, so a good-faith error under unsettled law costs them nothing out of pocket. Leaving policing for private security or other work is possible but costly — pensions, seniority, and specialized skills tie them in.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, sworn_law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% Negotiate indemnification protections into contracts and defend the doctrine through lobbying, bargaining, and amicus filings. When courts narrow the judicial shield, they pursue statutory replacements in state capitols, giving them routes around adverse rulings that most participants in the arrangement lack.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, agenda_setter).

% Authored the doctrine and keeps refining it: deciding at summary judgment whether a right was clearly established, hearing interlocutory appeals, and managing the volume of civil-rights filings. Individual judges cannot opt out of precedent; they can reshape the doctrine only at the margins, and the bench is visibly divided over its legitimacy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% People injured by officers who crossed constitutional lines. They sue for compensation, then learn at summary judgment that no prior case declared their precise right clearly established in their circuit. The injury goes uncompensated and they absorb the costs of the dismissed suit. Criminal prosecution, administrative discipline, and state tort claims exist but rarely deliver anything to them.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Take these cases on contingency, investing years of work before a dismissal wipes out the fee. The doctrine forces pre-screening that declines novel-but-meritorious claims, repricing the entire docket and shrinking access to counsel for injured people.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs_attorneys, payer,
    moderate, biographical, constrained, national).

% Budget insurance and settlement reserves around the doctrine. When an officer prevails on immunity, the municipality avoids an indemnity payment; when liability attaches anyway, they pay. They defend the arrangement in amicus filings while occasionally bearing its residual costs.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipalities_public_employers, beneficiary,
    institutional, generational, constrained, national).

% Several have enacted statutory causes of action that bypass the federal shield — Colorado and New Mexico most prominently. They would restructure the liability regime more broadly but stand outside the federal adjudicative conversation that maintains the doctrine, and their reforms stop at state lines.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, state_legislatures, excluded,
    institutional, generational, constrained, regional).

% Compile dismissal statistics, publish critiques and defenses, and supply the empirical record that reform debates cite. They hold no lever over the doctrine's day-to-day operation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, sworn_law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the risk of personal liability for constitutional errors during periods when the governing legal standards are unsettled, so that individual officers making good-faith judgments do not bear open-ended personal exposure while the rules crystallize through litigation.
% TRANSFER_FUNCTION: Moves remedial cost away from officers (and derivatively from the municipalities that would indemnify them) and onto the people injured by constitutional violations, who bear uncompensated harm plus the costs of dismissed litigation; it also moves the decision about whether a remedy is available at all from juries to judges ruling at summary judgment.
% ABSENT_VOICES: The people injured by the violations have no seat where the doctrine is elaborated — it develops almost entirely in cases courts decide for officers, so the voices shaping its scope are defendants, their counsel, and judges. State legislatures attempting statutory workarounds are likewise outside the federal adjudicative loop that maintains the arrangement.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, Section 1983 filings would surge, officers would demand contractual indemnification guarantees, municipal insurers would reprice, plea and settlement postures would shift, and policing practice would adjust to personal exposure — the liability landscape would reorganize within a few litigation cycles.
% FOUNDING_PROBLEM: Personal damages liability for constitutional violations committed under color of law threatened to deter capable people from public service and to chill decisive action taken in good faith under legally unsettled conditions; Harlow v. Fitzgerald sought to resolve insubstantial claims early and spare officials burdensome discovery and trial.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the pre-accountability-era judicial consensus that produced the doctrine, by public-administration research on recruitment and decision-chilling under personal exposure, and by reform-oriented scholars who accept a narrowly drawn immunity for good-faith conduct even while rejecting the current scope. The injured-party community and the civil-rights bar dispute that the problem retains anything like its original force, and that dispute is itself part of the record.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.48) and rising: the doctrine demonstrably denies compensation to people with meritorious claims, but the denial is bounded by the clearly-established threshold and coexists with a real protection function, so the reading's honest assessment sits mid-scale rather than at the accountability reading's high pole. Suppression (0.70) is a raw structural property, unscaled by power or scope: the procedural machinery — interlocutory appeals, sua sponte dismissal, merits-skipping — actively forecloses merits adjudication, and the series tracks the enforcement apparatus hardening (Saucier mandatory sequencing, Pearson discretion, per curiam reversals of denials). Theater_ratio (0.50) reflects a stated purpose increasingly performed rather than executed: Harlow itself dismantled the subjective bad-faith inquiry the doctrine's public rationale invokes, so roughly half of doctrinal activity defends a filtering function that no longer operates as described. Accessibility_collapse (0.60): alternatives exist on paper (injunctive relief, state statutes, criminal referral, administrative discipline) but collapse for the compensation-seeking injured person specifically. Resistance (0.75) is among the highest of any living legal doctrine: sustained scholarly critique, repeated federal legislation attempts, intra-Court dissent (Thomas questioning the doctrine's text, Sotomayor's application dissents), and state-level statutory bypasses. has_sunset_clause is authored TRUE on the reading's own structural logic: the clearly-established threshold IS the doctrine's internal sunset — protection terminates for any given right once precedent crystallizes — and the drift data document that this sunset is being obstructed in practice (immunity grant rates have not declined as the precedent base accumulated), which is why the story pairs a true sunset flag with a substantial unacknowledged practice_drift in cs_structure. All three tracked series run on one shared seven-point grid (0, 7, 14, 21, 28, 35, 43) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply different types per seat. From the officer and union seats the arrangement computes as coordination-dominant: a protection they receive, priced at nothing, with political arbitrage available when courts tighten it. From the survivor seat the same structure computes as heavy extraction with no exit: injury plus uncompensated cost plus a closed remedy. From the judiciary seat it computes as case-management doctrine — a procedural tool whose legitimacy question is held at arm's length by the precedent system itself. The scaffold claim belongs to none of these seats exclusively; it is the reading's characterization of the whole, and the divergence between the claim and the payer-seat computation is precisely the signal the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers, unions, and municipalities declare as beneficiaries and derive low directionality — the arrangement subsidizes them, and unions add arbitrage-grade exit (statutory replacement campaigns) pushing them further toward the beneficiary pole. Survivors and plaintiff attorneys declare as the cost-bearing set: survivors combine powerless power with trapped exit (no alternative compensation route), placing them nearest the full-target end; attorneys are moderately powered and professionally constrained. The judiciary is the agenda setter and derives near-symmetric positioning with a mild institutional tilt — it collects no rents from the doctrine but does collect docket manageability and avoidance of retroactive second-guessing, which is why no directionality override is authored: the beneficiary/victim declarations plus exit atoms already place every seated agent correctly, and an override keyed to the institutional power atom would wrongly drag the municipalities (genuine beneficiaries) off their derived low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification disciplines both mislabeling directions. Reading the arrangement as pure coordination (rope) erases the asymmetric cost-bearing — the injured class pays for a protection others receive — which the beneficiary/victim declarations forbid. Reading it as pure extraction (the accountability sibling) erases the genuine risk-allocation function during norm evolution, which the coordination-function answer records. The mandatrophy question for THIS reading is whether the founding problem still justifies the arrangement's current form: the founding problem is authored live (chilling and deterrence concerns persist wherever legal standards are unsettled), so mandatrophy_resolved is FALSE — but the transition mechanism is stalling, since the internal sunset (crystallizing precedent narrowing the shield) has not fired at the rate the scaffold design requires. A scaffold whose sunset is obstructed indefinitely is the exact object the lifecycle detectors exist to catch, and the measurement series supplies that detection surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_uncertainty,
    'Is the protective-scaffold reading the correct characterization of the qualified-immunity kernel''s dominant operation, or do the accountability_void_reading (systematic impunity) or constitutional_fidelity_reading (illegitimate fabrication) better capture it?',
    'Comparative outcome analysis across the three readings'' predictions: dismissal rates of later-vindicated meritorious claims, indemnification flow patterns, and the doctrine''s responsiveness to accumulating precedent. Each reading predicts a distinct signature; the corpus can score them against the same record.',
    'If the accountability reading wins, effective extraction jumps and the family reclassifies toward enforced extraction with unchanged victim sets; if the fidelity reading wins, the defect relocates from magnitude to authorization and outcome-based metrics become secondary; if this reading holds, the scaffold classification stabilizes with the sunset-obstruction finding as its central caveat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_uncertainty, conceptual, 'Which reading of the qualified-immunity kernel correctly characterizes the standing arrangement.').

omega_variable(
    sunset_obstruction_vs_design_failure,
    'Does the clearly-established threshold still function as the doctrine''s internal sunset (protection receding as precedent crystallizes), or has the transition mechanism structurally failed such that the scaffold framing no longer describes the arrangement?',
    'Longitudinal grant-rate analysis conditioned on precedent density: if immunity success rates decline as the clearly-established base thickens, the sunset fires and the scaffold reading stands; if grant rates are flat or rising against accumulating precedent, the transition has stalled and the arrangement is a permanent structure wearing a transitional justification.',
    'A functioning sunset supports the scaffold claim with moderate extraction; a stalled sunset forces reclassification toward a steady-state hybrid — coordination function intact, transition justification dead — with the mandatrophy mismatch flag raised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_obstruction_vs_design_failure, empirical, 'Whether the doctrine''s built-in transition mechanism still operates.').

omega_variable(
    chilling_effect_magnitude,
    'How large is the actual deterrent effect on recruitment, retention, and decisive action if personal liability exposure existed without immunity — the empirical foundation of this reading''s necessity premise?',
    'Natural experiments from jurisdictions operating without the federal shield (post-reform Colorado, New Mexico) and from occupational groups bearing comparable personal liability (physicians): measure attrition, defensive-practice indices, and vacancy rates against matched controls.',
    'A small measured effect undermines the necessity axiom and pushes this reading toward the accountability sibling''s assessment; a large effect strengthens the scaffold claim and raises the floor beneath which extraction remains tolerable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Magnitude of the chilling effect the doctrine exists to prevent.').

omega_variable(
    bad_faith_filter_revivability,
    'Could a workable subjective bad-faith filter be reconstructed — separating vexatious suits from meritorious ones — restoring the doctrine''s stated screening function without the current remedial denial?',
    'Statutory pilots with good-faith defenses and fee-shifting symmetry at state level; observe whether filing volumes bifurcate as the filter predicts or whether the distinction proves as administrable as the pre-Harlow era found it.',
    'A viable filter would let the scaffold reading reclaim its full structure (real sunset, real screening, bounded extraction); an inviable one confirms the screening rationale as performance and drives theater_ratio assessments upward across the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bad_faith_filter_revivability, empirical, 'Whether the doctrine''s stated filtering function is restorable in administrable form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 43).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_scaffold_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(qi_scaffold_tr_t7, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 7, 0.27).
narrative_ontology:measurement(qi_scaffold_tr_t14, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(qi_scaffold_tr_t21, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement(qi_scaffold_tr_t28, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 28, 0.43).
narrative_ontology:measurement(qi_scaffold_tr_t35, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 35, 0.47).
narrative_ontology:measurement(qi_scaffold_tr_t43, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 43, 0.5).

% Extraction over time
narrative_ontology:measurement(qi_scaffold_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(qi_scaffold_be_t7, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 7, 0.31).
narrative_ontology:measurement(qi_scaffold_be_t14, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 14, 0.36).
narrative_ontology:measurement(qi_scaffold_be_t21, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 21, 0.41).
narrative_ontology:measurement(qi_scaffold_be_t28, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 28, 0.44).
narrative_ontology:measurement(qi_scaffold_be_t35, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 35, 0.46).
narrative_ontology:measurement(qi_scaffold_be_t43, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 43, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qi_scaffold_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(qi_scaffold_su_t7, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 7, 0.41).
narrative_ontology:measurement(qi_scaffold_su_t14, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 14, 0.49).
narrative_ontology:measurement(qi_scaffold_su_t21, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 21, 0.56).
narrative_ontology:measurement(qi_scaffold_su_t28, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement(qi_scaffold_su_t35, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 35, 0.66).
narrative_ontology:measurement(qi_scaffold_su_t43, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 43, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three structurally distinct readings of one kernel. This story instantiates the protective-scaffold reading (transitional protection enabling vigorous enforcement; moderate base extractiveness with officers in the beneficiary set and litigation costs shifted to injured parties). The accountability_void_reading reads the same arrangement as systematic impunity machinery (high extractiveness, victims identical, coordination story as cover). The constitutional_fidelity_reading evaluates legitimacy independently of outcomes (doctrine as unauthorized judicial fabrication). All three share the referent — the standing modern doctrine as applied — and differ in assessment; they are linked here so contamination and drift analyses treat them as one family rather than three unrelated disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
