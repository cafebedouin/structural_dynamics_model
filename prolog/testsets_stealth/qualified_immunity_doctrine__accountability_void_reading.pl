% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity — Accountability-Void Reading (Impunity Mechanism)
 *   domain: legal/constitutional/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the qualified-immunity kernel: the
 *   accountability-void reading, under which the doctrine operates as a
 *   systematic mechanism guaranteeing impunity for constitutional violations.
 *   Per the epsilon-referent rule, epsilon (0.82) is authored for the
 *   STANDING arrangement — the clearly-established screen as it actually
 *   operates — assessed by this reading's own lights; it is not hedged across
 *   readings and does not describe the rights-respecting arrangement this
 *   reading would prefer. The sibling readings (protective scaffold,
 *   constitutional fidelity) are separate constraints in separate files,
 *   linked via network.affects_constraints; their epsilon values, victim
 *   sets, and classifications differ structurally, and the contest between
 *   them is routed to the kernel_reading_allocation omega rather than
 *   averaged into this file. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: Primary agenda setter (institutional/constrained) —
 *   authors, applies, and defends the screen; collects docket relief -
 *   law_enforcement_officers: Primary beneficiary (organized/constrained) —
 *   shielded from personal damages proceedings; indemnified against payment -
 *   police_unions: Secondary beneficiary (organized/mobile) — litigates and
 *   lobbies to preserve the shield - municipal_risk_pools: Secondary
 *   beneficiary with payer residue (powerful/constrained) — ledger shrunk at
 *   officer level, Monell exposure survives - constitutional_tort_victims:
 *   Primary target (powerless/trapped) — claims terminated at the threshold;
 *   no alternative remedy path - communities_bearing_unchecked_misconduct:
 *   Diffuse target (powerless/trapped) — bear violation risk and settlement
 *   taxes with no adjudication - civil_rights_plaintiffs_bar: Cost-bearing
 *   intermediary (moderate/mobile) — uncompensated contingency churn -
 *   civil_rights_organizations: Excluded voice (organized/identity_locked) —
 *   heard but not heeded; constitutionally unable to exit the fight -
 *   qi_legal_scholars: Analytical observer (analytical/analytical) — produces
 *   the record all seats argue over
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.82).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.78).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity — Accountability-Void Reading (Impunity Mechanism)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "legal/constitutional/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '9d9b2354-d189-4350-a326-701ba7529855').
narrative_ontology:cs_kernel_codification('9d9b2354-d189-4350-a326-701ba7529855', fixed_text).
narrative_ontology:cs_authority_grounding('9d9b2354-d189-4350-a326-701ba7529855', lineage).
narrative_ontology:cs_interpretation_layer_present('9d9b2354-d189-4350-a326-701ba7529855').
narrative_ontology:cs_reading_relation('9d9b2354-d189-4350-a326-701ba7529855', qualified_immunity_doctrine__protective_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('9d9b2354-d189-4350-a326-701ba7529855', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('9d9b2354-d189-4350-a326-701ba7529855', foundational, constitutional_right_implies_actionable_remedy).
narrative_ontology:cs_axiom_status(constitutional_right_implies_actionable_remedy, holdable).
narrative_ontology:cs_axiom_grounding('9d9b2354-d189-4350-a326-701ba7529855', constitutional_right_implies_actionable_remedy, deontological).
narrative_ontology:cs_axiom('9d9b2354-d189-4350-a326-701ba7529855', foundational, impunity_systematically_increases_violations).
narrative_ontology:cs_axiom_status(impunity_systematically_increases_violations, holdable).
narrative_ontology:cs_axiom_grounding('9d9b2354-d189-4350-a326-701ba7529855', impunity_systematically_increases_violations, empirically_contingent).
narrative_ontology:cs_reference_frame('9d9b2354-d189-4350-a326-701ba7529855', section_1983_full_remedy_frame).
narrative_ontology:cs_drift_state('9d9b2354-d189-4350-a326-701ba7529855', contemporary_post_2020_reform_wave, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9d9b2354-d189-4350-a326-701ba7529855', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_risk_pools).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_tort_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, communities_bearing_unchecked_misconduct).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, municipal_risk_pools).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs_bar).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, common_law_official_immunity_lineage).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, frivolous_litigation_filtering_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and maintains the doctrine through the precedent line running from Pierson v. Ray through Harlow to the contemporary clearly-established cases. Applies the screen case-by-case, defines how closely a prior case must match, and reverses lower courts that deny immunity. Cases ending before discovery relieve its docket; it also absorbs sustained criticism that the doctrine lacks any statutory anchor. Departing from the arrangement would require a majority willing to overrule or radically confine its own precedents.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, beneficiary).

% Individual officers sued under Section 1983 for on-duty conduct. The threshold screen ends their cases before discovery unless a prior, materially similar case established the right. The protection is low-salience day to day and becomes visible only when suit is filed. Employers indemnify nearly all of them, so personal payment is rare; what the arrangement removes is the experience of publicly answering for the incident in court.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% File amicus briefs in every major immunity case, lobby against reform legislation, and negotiate indemnification provisions in collective agreements. Member services and bargaining posture rest partly on the assurance that members will not answer personally for on-duty conduct. Their organizational survival does not depend on the doctrine; they defend it instrumentally.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, biographical, mobile, national).

% Cities, counties, and their insurers fund defense and indemnification. Officer-level claims removed at the threshold shrink the ledger, though claims against the municipality itself under Monell survive. Residual settlement costs spread across taxpayers without any adjudication of the underlying conduct having occurred.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_risk_pools, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, municipal_risk_pools, payer).

% People alleging excessive force, fabricated evidence, unlawful searches, or similar on-duty conduct who bring Section 1983 suits. Their claims terminate at the threshold unless counsel locates a prior case with matching facts; most never reach discovery, deposition, or any ruling on the merits. Alternative routes — Bivens actions, municipal-policy claims, state tort law — are narrow, expensive, or unavailable for their injuries.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_tort_victims, payer,
    powerless, biographical, trapped, national).

% Residents of jurisdictions where on-duty misconduct carries no monetary consequence for anyone. They bear elevated risk of violation and fund settlements through taxation without any proceeding having examined the conduct. Most never file suit and appear in no case record; their harm registers nowhere in the doctrine's own evidentiary stream.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, communities_bearing_unchecked_misconduct, payer,
    powerless, generational, trapped, national).

% Attorneys taking Section 1983 cases on contingency. Threshold dismissals mean years of work with no fee, which prices out representation of weaker-fact cases and concentrates the bar on the strongest ones. Some lawyers leave the practice area entirely; those who stay absorb the churn.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs_bar, payer,
    moderate, biographical, mobile, national).

% Advocacy groups that document dismissal patterns, publish reform agendas, draft model legislation, and back test cases. Courts hear their arguments and decline to adopt them; their program lives outside the interpretive coalition that maintains the doctrine. Their organizational identity is constituted by this fight, and they do not exit it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_organizations, excluded,
    organized, generational, identity_locked, national).

% Academic and empirical researchers across the ideological spectrum studying dismissal stages, indemnification incidence, and the doctrine's legislative and case history. They produce the record the other seats argue over, collect fees from no participant, and decide nothing.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, qi_legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels all federal constitutional-tort claims against individual officers through a single judicial screen that terminates cases before discovery unless a prior, materially similar decision established the right — sparing individual officers personal damages proceedings and sparing courts the management of those proceedings.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations away from officers and their indemnitors and onto the injured: victims absorb uncompensated injury and foreclosed adjudication, plaintiffs' counsel absorbs unrecovered effort, and taxpayers absorb settlement costs that never pass through any examination of the underlying conduct.
% ABSENT_VOICES: Claimants whose cases died at the threshold are structurally absent from the record that shapes the doctrine — dismissed cases generate no precedent, so the clearly-established corpus is built only from survivors, and the people whose injuries defined the problem never enter the conversation that governs it. Future victims are represented by no one at all.
% DISAPPEARANCE_RATIONALE: Section 1983 claims would proceed past the threshold into discovery and merits rulings; indemnification systems and insurance pricing would reprice around restored officer-level exposure; agencies would face direct monetary feedback on conduct. The state-level substitutes already operating in Colorado, New Mexico, and Connecticut sketch the rearranged equilibrium.
% FOUNDING_PROBLEM: Shielding officials from personal ruin and discovery burdens when they enforce duties the law later repudiates: the doctrine's founding cases arose from officers facing claims for enforcing segregation-era law, and its modern form was framed as sparing public servants the cost of defending insubstantial suits.
% FOUNDING_PROBLEM_CORROBORATION: Legal histories of Pierson v. Ray document the segregation-enforcement origin; Joanna Schwartz's empirical work, produced outside any benefiting party, finds indemnification already absorbs payment, undercutting the personal-ruin rationale; Justice Sotomayor's published dissents and the congressional sponsors of the Ending Qualified Immunity Act attest from outside the beneficiary set that the founding rationales no longer describe the doctrine's operation. No source independent of the benefiting parties attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.82: the screen terminates the overwhelming run of Section 1983 claims before discovery, so the arrangement removes not merely money but the adjudication itself — victims absorb injury with no forum in which the conduct is ever examined. Suppression 0.78: the remedy-path closure is structural — Bivens narrowed, Monell demanding, state tort routes inadequate or barred — and the machinery maintaining it (interlocutory appeals, per curiam reversals of denials, tightening of the clearly-established match requirement) matured over the interval. Theater 0.50: the two-step jurisprudence performs principled, fact-specific balancing while outcomes track protection with striking regularity; roughly half the doctrinal activity elaborates a method whose predictive content is thin. Accessibility collapse 0.70: once a claimant understands the screen, alternatives mostly collapse — but the Colorado/New Mexico/Connecticut statutes demonstrate partial alternatives persist, so not 0.85+. Resistance 0.62: sustained and unusual in breadth — cross-ideological academic consensus, dissenting Supreme Court opinions, state abolition statutes, repeated federal legislative proposals — yet the doctrine's core is intact after each wave. Claim and metrics are independent authored facts: snare is claimed because this reading holds the filtering rationale to be cover for impunity maintenance; the metrics describe observed operation without being tuned to any predicted engine verdict. Suppression is authored as a raw structural property and is deliberately NOT reconciled with extractiveness — scaling by directionality and scope belongs to the engine. Coalition note: the victim seats are powerless individually but the class is large; coalition potential exists through civil_rights_organizations, yet each case dies alone before discovery, so no precedent-accumulating coalition forms — the arrangement fragments precisely the constituency that could oppose it.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme here. From the judiciary's seat the arrangement presents as careful, case-by-case adjudication of a genuinely hard line-drawing problem — rope-like. From the officer's seat it is low-salience background protection, noticed only when sued. From the victim's seat it is a categorical wall encountered at the worst moment of their life. Same-power differentiation: officers, police unions, and civil-rights organizations all sit at 'organized', yet their relationships differ completely — the first two collect from the arrangement (with different exit mobility), the third is excluded from it and identity-locked into opposition. The plaintiffs' bar shares the victims' domain but holds mobile exit (practice-area switching), which is why its resistance registers as attrition rather than confrontation. The engine computes these divergent per-seat classifications from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive the target side: constitutional_tort_victims and communities_bearing_unchecked_misconduct are declared victims with trapped exit, placing them near the full-target end (high d, amplified chi). Beneficiary declarations drive the subsidized side: officers, unions, and municipal risk pools get low d; unions' mobile exit and officers' indemnified insulation push them toward the beneficiary pole. Municipal risk pools carry a genuine dual position (beneficiary with payer residue via Monell exposure and defense costs), modeled with secondary_role rather than an override. One override is authored: the federal judiciary holds the 'institutional' power atom but appears in no beneficiary/victim array, so structural derivation has nothing to read and the canonical fallback would misplace it. The override (d = 0.35) records its actual position: it collects docket relief and avoids politically costly anti-police merits rulings (beneficiary-side), while bearing legitimacy costs from the no-statutory-anchor criticism (pulling back toward symmetric). No override is needed for the victim seats — victim-plus-trapped already derives near-maximal d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: the transition-era liability shock that produced Pierson (officers enforcing law the legal order had repudiated) no longer exists, and the modern discovery-burden rationale is undercut by summary-judgment practice and by indemnification absorbing payment in nearly all cases. The arrangement persists anyway, with concentrated beneficiaries who actively defend it — which is why this reads as capture rather than decay. The founding_problem_status=dead combined with disappearance_verdict=world_rearranges should trip the mismatch consumer's zombie/capture flag, cross-checked against the concentrated gain_flow (named seat: officers) and the cheap fixing_cost: the fix is a single statute or a merits overruling, and several states have already built working substitutes. This is the opposite of the piton profile — a piton requires no concentrated capturer and prohibitive fixing cost; here the capturer is named, the fix is cheap, and the arrangement persists because its beneficiaries choose to maintain it. The mandatrophy lens thus prevents the reciprocal error as well: reading the arrangement as pure extraction-with-no-function would miss that a screening function nominally exists; the snare classification encodes that the function is cover while the extraction is the point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Is the qualified-immunity kernel''s operative structure accurately characterized by this accountability-void reading, or by one of its siblings (protective scaffold, constitutional fidelity)?',
    'Compile all three sibling stories and compare computed per-seat classifications, victim-set composition, and epsilon against the shared empirical record: indemnification incidence, dismissal stages, and deterrence studies.',
    'If the scaffold reading computes, epsilon collapses toward the coordination-cost floor and the victim set shrinks to targets of insubstantial suits; if the fidelity reading computes, classification turns on authorization illegitimacy rather than extraction magnitude. This file''s high epsilon is valid only within this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer allocation among the three sibling readings of the qualified-immunity kernel; this story instantiates only the accountability-void reading.').

omega_variable(
    indemnification_offset_question,
    'Does personal liability absent the doctrine actually reach officers, given near-universal indemnification — that is, what exactly does the shield remove?',
    'Natural experiment from state-level abolitions (Colorado 2020, New Mexico, Connecticut): compare personal payout incidence on officers, settlement rates, and misconduct frequency before and after.',
    'If officers would never bear payment anyway, the extracted good is foreclosed adjudication and deterrence rather than transferred money, refining who pays and how much; if officers would bear real personal exposure, the transfer magnitude of the arrangement rises above the authored estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indemnification_offset_question, empirical, 'Whether the shield''s yield is money, adjudication, or deterrence, given indemnification absorbs most payment.').

omega_variable(
    clearly_established_administrability,
    'Is the clearly-established standard determinate enough that outcomes track the law rather than judicial disposition?',
    'Coding studies of grant and denial rates against case-fact similarity; inter-circuit variance analysis for factually comparable conduct.',
    'High indeterminacy pushes theater_ratio above the authored 0.50 and supports reading the screen as discretionary gatekeeping; determinacy would support the judiciary''s rule-of-law account of its own practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_administrability, empirical, 'Whether the screen is a law-administered filter or a disposition-driven gate.').

omega_variable(
    remedy_foreclosure_internalization,
    'Is the measured suppression purely structural (the doctrinal bar itself) or partially internalized (would-be claimants who never file because futility has been culturally learned)?',
    'Post-abolition filing-rate trajectories in abolition states: a sharp rise in filings when the bar lifts indicates prior suppression included an internalized futility component; a flat trajectory indicates the bar alone explains non-filing.',
    'An internalized component means effective suppression exceeds the structural measure and persists after repeal; successor arrangements must be classified accounting for suppression the population carries with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_foreclosure_internalization, empirical, 'Structural versus internalized components of the remedy-path foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_accountability_void_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t1982, observed).
narrative_ontology:measurement(qi_accountability_void_tr_t1992, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t1992, observed).
narrative_ontology:measurement(qi_accountability_void_tr_t2002, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2002, 0.38).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t2002, observed).
narrative_ontology:measurement(qi_accountability_void_tr_t2009, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2009, 0.4).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t2009, observed).
narrative_ontology:measurement(qi_accountability_void_tr_t2017, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2017, 0.46).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t2017, observed).
narrative_ontology:measurement(qi_accountability_void_tr_t2025, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(qi_accountability_void_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(qi_accountability_void_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement_basis(qi_accountability_void_be_t1982, observed).
narrative_ontology:measurement(qi_accountability_void_be_t1992, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement_basis(qi_accountability_void_be_t1992, observed).
narrative_ontology:measurement(qi_accountability_void_be_t2002, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2002, 0.66).
narrative_ontology:measurement_basis(qi_accountability_void_be_t2002, observed).
narrative_ontology:measurement(qi_accountability_void_be_t2009, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2009, 0.7).
narrative_ontology:measurement_basis(qi_accountability_void_be_t2009, observed).
narrative_ontology:measurement(qi_accountability_void_be_t2017, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2017, 0.76).
narrative_ontology:measurement_basis(qi_accountability_void_be_t2017, observed).
narrative_ontology:measurement(qi_accountability_void_be_t2025, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(qi_accountability_void_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(qi_accountability_void_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement_basis(qi_accountability_void_su_t1982, observed).
narrative_ontology:measurement(qi_accountability_void_su_t1992, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1992, 0.56).
narrative_ontology:measurement_basis(qi_accountability_void_su_t1992, observed).
narrative_ontology:measurement(qi_accountability_void_su_t2002, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2002, 0.64).
narrative_ontology:measurement_basis(qi_accountability_void_su_t2002, observed).
narrative_ontology:measurement(qi_accountability_void_su_t2009, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement_basis(qi_accountability_void_su_t2009, observed).
narrative_ontology:measurement(qi_accountability_void_su_t2017, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2017, 0.74).
narrative_ontology:measurement_basis(qi_accountability_void_su_t2017, observed).
narrative_ontology:measurement(qi_accountability_void_su_t2025, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(qi_accountability_void_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file instantiates the accountability-void reading (impunity mechanism; high epsilon; victims = constitutional tort claimants with no remedy path). The protective-scaffold reading (necessary protection for vigorous policing; low epsilon; coordination function genuine) and the constitutional-fidelity reading (judicial fabrication lacking authorization; classification keyed to legitimacy rather than extraction magnitude) are separate files with their own epsilon, beneficiaries, and victims. The fidelity reading is upstream in the discourse (its genealogical attack supplies premises both other readings engage); this reading's empirical record (dismissal stages, indemnification incidence) feeds the legitimacy contest in both siblings. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
