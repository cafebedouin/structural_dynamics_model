% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity Doctrine — Protective Scaffold Reading
 *   domain: legal/constitutional/civil_rights
 *
 * SUMMARY:
 *   Federal courts shield individual officers from personal-damages liability
 *   for constitutional violations unless pre-existing precedent already
 *   condemned essentially identical conduct. This story instantiates ONE
 *   reading of that arrangement — the protective_scaffold_reading — which
 *   holds the shield to be necessary protection enabling vigorous law
 *   enforcement without fear of bad-faith litigation. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement (the doctrine as it currently operates), assessed by this
 *   reading's own lights: the reading grants that the protective function is
 *   real, and it equally concedes — its own structural delta declares it —
 *   that litigation costs are externalized onto survivors of constitutional
 *   violations who are denied remedy, with judicial discretion in the
 *   'clearly established' application setting the base rate. The reading does
 *   not average over its siblings; the accountability_void_reading and the
 *   constitutional_fidelity_reading are separate constraints in the same
 *   kernel family, linked by network edges. The claim/metric independence
 *   rule is honored: claimed_type records this reading's structural judgment
 *   (genuine coordination function plus conceded asymmetric extraction under
 *   active judicial enforcement), while the metrics record the operation as
 *   descriptively observed — including a theater ratio reflecting how far the
 *   bad-faith-litigation justification now stretches past bad-faith cases.
 *   Interval units are abstract: t0 approximates the doctrine's late-1960s
 *   formation, t40 the contemporary hardened-application era.
 *
 * KEY AGENTS:
 *   - - law_enforcement_officers: primary beneficiary (organized/constrained) — carries the liability shield on every encounter, collects its protection without administering it
 *   - - municipal_and_state_governments: secondary beneficiary (institutional/constrained) — receives litigation-risk relief and insurance-pricing stability
 *   - - federal_judiciary: agenda setter (institutional/constrained) — authored the doctrine in case law and administers the 'clearly established' filter case by case
 *   - - constitutional_violation_survivors: primary target (powerless/trapped) — bear the remedy denial; their claims die at summary judgment before discovery
 *   - - civil_rights_attorneys: cost-bearing intermediary (organized/mobile) — absorb screening risk, declined-case losses, and unpaid appellate years
 *   - - police_unions: organized defender-beneficiary (powerful/arbitrage) — convert the shield into membership services and political defense across jurisdictions
 *   - - reform_legislators: excluded voice (moderate/mobile) — propose abolition or replacement outside the judicial forum where the standard is actually set
 *   - - legal_academics_observers: analytical observer (analytical/analytical) — measure the gap between stated rationale and operating pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.58).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.62).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity Doctrine — Protective Scaffold Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "legal/constitutional/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '6873fd04-84e4-41fb-b5b9-ba0a82d5926f').
narrative_ontology:cs_kernel_codification('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', formalized).
narrative_ontology:cs_authority_grounding('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', lineage).
narrative_ontology:cs_interpretation_layer_present('6873fd04-84e4-41fb-b5b9-ba0a82d5926f').
narrative_ontology:cs_reading_relation('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', foundational, officer_protection_prerequisite_to_vigorous_enforcement).
narrative_ontology:cs_axiom_status(officer_protection_prerequisite_to_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', officer_protection_prerequisite_to_vigorous_enforcement, instrumental).
narrative_ontology:cs_axiom('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', secondary, clearly_established_objective_filter_is_proper_price).
narrative_ontology:cs_axiom_status(clearly_established_objective_filter_is_proper_price, holdable).
narrative_ontology:cs_axiom_grounding('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', clearly_established_objective_filter_is_proper_price, conventional).
narrative_ontology:cs_axiom('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', secondary, subjective_good_faith_inquiry_component).
narrative_ontology:cs_axiom_status(subjective_good_faith_inquiry_component, overridden).
narrative_ontology:cs_axiom_grounding('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', subjective_good_faith_inquiry_component, empirically_contingent).
narrative_ontology:cs_reference_frame('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', good_faith_protection_framework).
narrative_ontology:cs_drift_state('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', contemporary_summary_judgment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6873fd04-84e4-41fb-b5b9-ba0a82d5926f', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_and_state_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_attorneys).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, clearly_established_objective_standard).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, deterrence_of_bad_faith_litigation_thesis).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, officer_hesitancy_chilling_effect_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patrol, arrest, and use force under split-second conditions. From their first shift they are covered by a judicial shield that blocks personal-damages suits unless surviving precedent already condemned the precise conduct. They did not design the shield and cannot amend it, but they carry it on every encounter, and negotiated indemnification means the rare judgment that slips through seldom touches their pay. The shield is not something they can decline individually — it attaches to the job.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% Employ and indemnify officers. The shield thins the stream of individual-capacity suits that reaches juries, which stabilizes insurance pricing and weakens settlement leverage against their budgets. Government entities remain exposed on their own policy-conduct liability, so the relief is partial, but finance offices plan around it and their lobbies defend the arrangement's retention. They do not administer it; they inherit its output.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_and_state_governments, beneficiary,
    institutional, generational, constrained, national).

% Created the shield through case law rather than statute and maintains it by deciding, case by case, whether existing precedent clearly established the right an officer is accused of violating. Circuit panels write the operative definitions; the Supreme Court recalibrates them occasionally. Judges cannot set the doctrine aside without overturning their own precedent stack, and every contested application draws legitimacy criticism toward the institution that applies it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% People injured or bereaved by officer conduct that crossed a constitutional line. Their route to compensation runs through a federal civil-rights suit, and the shield stands on that route: unless they can point to prior decided cases condemning nearly identical conduct, their complaint is dismissed at summary judgment, often before any deposition. They cannot shop the claim to a friendlier forum — the doctrine follows the federal question wherever it is filed.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Take these cases on contingency. The shield turns every filing into a wager on precedent-matching that no intake interview can resolve, so experienced practitioners decline whole categories of factually strong cases, and the ones accepted consume years of unpaid appellate work. Some build niches litigating the shield's boundaries themselves. They can move to other practice areas; their clients cannot move anywhere.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_attorneys, payer,
    organized, biographical, mobile, national).

% Negotiate indemnification, fund members' legal defenses, and run the political defense of the shield in legislatures and ballot campaigns. The shield is central inventory: recruiting promises and member services are priced against it. Their resources let them contest reform on many jurisdictional fronts at once and withdraw from fronts they are losing without losing the asset.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    powerful, generational, arbitrage, national).

% Introduce bills to abolish or replace the shield and hold hearings on remedy denial. The adjudicating conversation happens in courtrooms they do not control, and their proposals stall in committee against organized opposition. They hold views the doctrine's administration never has to process; their exclusion from the operative forum is stable across election cycles.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, reform_legislators, excluded,
    moderate, biographical, mobile, national).

% Track dismissal rates, code how the shield is applied across circuits, and publish on the distance between its stated rationale and its operating pattern. They shape the vocabulary of the debate and supply the empirical record reformers cite, but they decide nothing and bear none of the arrangement's costs or benefits.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, legal_academics_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform judicial filter that disposes of weak or retaliatory civil-rights claims against officers at the summary-judgment stage, and insulates split-second enforcement decisions from after-the-fact second-guessing — solving the collective problem of staffing coercive law enforcement without exposing every individual officer to potentially ruinous personal liability for good-faith conduct.
% TRANSFER_FUNCTION: Moves remedy — damages, official acknowledgment, and the deterrence signal that accompanies compensation — away from survivors of constitutional violations; moves litigation-risk relief to officers personally and insurance-stability to employing governments; moves interpretive authority over what counts as settled law to the federal judiciary.
% ABSENT_VOICES: Survivors whose claims are dismissed at summary judgment are structurally absent: their cases generate no hearing record, no precedent, and no settlement statistic, so the population bearing the largest cost is the least visible in the forums where the standard is refined. Families of the killed, reform legislators, and contingency attorneys who have stopped accepting these cases sit outside the judicial conversation entirely.
% DISAPPEARANCE_RATIONALE: Overnight removal would trigger an immediate surge of individual-capacity filings against officers, rapid repricing of municipal insurance and indemnification exposure, a changed use-of-force calculus while departments await empirical adjustment, and a congressional scramble to legislate a replacement filter. The accountability architecture would reorganize around whatever instrument succeeded the shield; nothing about the current litigation ecosystem survives its removal unchanged.
% FOUNDING_PROBLEM: After the civil-rights revival of Section 1983 in the 1960s, federal courts faced surging litigation against officers — much of it weak or retaliatory, some of it meritorious — while individual officers faced potentially ruinous personal liability for good-faith split-second decisions. The doctrine was built to filter the former class early and shield the latter class absolutely.
% FOUNDING_PROBLEM_CORROBORATION: Partial, and partly from outside the benefiting parties: federal-court filing statistics and municipal-insurer loss runs corroborate that officer-directed litigation pressure persists, and plaintiff-side bar experience corroborates that weak claims arrive alongside meritorious ones. No source outside the benefiting parties attests that the current doctrine — as opposed to some narrower filter — is necessary to solve the problem; that necessity claim rests on the doctrine's beneficiaries and on the judiciary that administers it, and this story records the gap rather than papering over it.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58 — moderate-to-substantial, matching the reading's own delta: the shield denies remedy to a conceded victim class, and the denial rate is governed by discretionary 'clearly established' matching rather than by the gravity of the violation. Suppression is 0.62 as a raw, unscaled structural property: the foreclosure is legal-structural (the federal remedy path is barred; no parallel forum exists for the federal question), not a matter of enforcement intensity scaled by anything. Theater is 0.48: genuine filtering of weak claims continues, but a growing share of the doctrine's operation — dismissals of factually serious claims for want of precedent-matching — is performed under a justification (bad-faith litigation) that does not describe it. Accessibility_collapse is 0.55: alternative routes (state tort claims, administrative review, criminal prosecution, entity-level liability) persist but are weak, partial, or unavailable to the typical survivor. Resistance is 0.60: sustained legislative proposals, academic campaigns, state-level variations, and professional-bar dissent meet the doctrine without displacing it. The temporal series run on one shared grid (t = 0, 8, 16, 24, 32, 40) with every tracked metric authored at every point; the trajectories are monotonic — extraction, theater, and enforcement labor all rise as the objective-only standard hardened and summary-judgment dismissal became the doctrine's center of gravity — with no oscillation to model.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply and the engine computes that divergence from the structural data. From the officer and union seats the arrangement presents as protection they receive automatically and cannot individually refuse — coordination-shaped, low burden, high value. From the survivor seat the same structure presents as foreclosure: the injury is conceded, the violating conduct may be conceded, and the remedy dies on a precedent-matching technicality the survivor cannot control. From the attorney seat it presents as screening risk that rations representation. From the judiciary's seat it presents as doctrine-crafting — each application a legitimate exercise of the filter function — while the accumulated legitimacy cost of the pattern lands elsewhere. Same-level differentiation is visible among the organized actors: unions (arbitrage exit, multi-front lobbying) experience the arrangement as an asset they defend, while civil-rights attorneys (mobile exit, case-selection freedom) experience it as a tax on their practice — equal nominal standing, opposite directionalities, differentiated entirely by which side of the transfer each sits on.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: officers and unions sit near the subsidized end (the shield subsidizes them; union arbitrage-grade political mobility pushes furthest toward the beneficiary pole), governments somewhat higher but still net-collectors of risk relief. The declared victim — constitutional_violation_survivors — derives high directionality, amplified by the trapped exit: the doctrine travels with the federal question, so there is no jurisdictional arbitrage available to dilute their target position. Civil-rights attorneys derive elevated-but-damped directionality: they bear real costs but retain mobile exit into other practice areas. The judiciary is the one seat the structural derivation cannot place: it appears in neither the beneficiary nor the victim arrays, so a canonical fallback would guess. The directionality_override sets the institutional seat to 0.45 — near-symmetric with a slight cost-side tilt — because the judiciary administers the doctrine, gains adjudicative authority from administering it, and simultaneously spends legitimacy capital on each contested application; that relationship is a structural fact about the seat, not derivable from who benefits and who pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both halves of this arrangement visible at once. It prevents the protective reading from certifying pure coordination — which would erase the victim set the reading's own delta concedes — and it prevents the extraction-centered sibling readings from deleting the real filtering function that weak-claim disposal performs. On the genealogy interview: the founding problem (the post-1960s surge of officer-directed civil-rights litigation, much of it weak or retaliatory, arriving alongside meritorious claims that threatened officers with ruinous personal liability) is authored as still live, and the disappearance verdict is world_rearranges — the litigation, insurance, and policing arrangements visibly depend on the shield. Status-live crossed with world_rearranges yields no obsolescence flag, which is consistent with this reading's self-understanding; the sibling readings would author the status differently, and that divergence is carried by their files, not averaged into this one. The corroboration entry is deliberately partial: litigation-pressure is corroborated from outside the benefiting parties, the necessity of THIS remedy is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the protective_scaffold_reading of the qualified_immunity_doctrine kernel; what changes structurally if a sibling reading is instantiated instead?',
    'Re-author under each sibling and diff the structural data: the accountability_void_reading expands the victim set to all persons subjected to officer force and authors epsilon toward the high-extraction range; the constitutional_fidelity_reading drops the policy-benefit ledger entirely and classifies on authorization grounds alone.',
    'Per-seat classifications, network edges, and the receipt surface all shift with the reading; the three stories form one constraint family joined by affects_constraints and must be read together for any verdict about the doctrine as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel, with sibling deltas specified.').

omega_variable(
    protective_effect_empirical_status,
    'Does qualified immunity actually produce the vigorous-enforcement and anti-chilling benefit this reading premises, or is the protective effect assumed?',
    'Cross-jurisdictional natural experiments: jurisdictions that narrowed or abolished the shield (state statutory variants, consent-decree cities) compared on use-of-force rates, recruitment, and civil filing volumes against matched controls.',
    'If no measurable protective effect exists, the coordination half of the arrangement is cover and the classification trends toward pure extraction; if the effect is strong, the coordination weighting rises and the reading''s premise is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_effect_empirical_status, empirical, 'Whether the protective function this reading defends is empirically real.').

omega_variable(
    clearly_established_discretion_share,
    'How much of the measured remedy denial flows from judicial discretion in applying the ''clearly established'' test rather than from the doctrine''s existence as such?',
    'Circuit-level comparison of summary-judgment dismissal rates under differing specificity regimes, controlling for case mix; coding of dismissal rationales across circuits.',
    'Discretion-driven denial is addressable by interpretive tightening without removing the shield (a transitional-reform path); existence-driven denial requires structural removal, changing which remedies are even on the table.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_discretion_share, empirical, 'Decomposition of remedy denial into discretionary versus doctrinal components.').

omega_variable(
    counterfactual_filter_design,
    'Could a narrower filter — a subjective good-faith test, or a malice standard — deliver the protection this reading values while returning remedy to survivors of clear violations?',
    'Comparative analysis of pre-Harlow outcome distributions and of state-law statutory immunity schemes that condition protection on objective reasonableness plus absence of malice.',
    'An affirmative answer reframes the reform space as replacing a hardened barrier with a scoped filter, moving the arrangement toward a transitional-support profile with a defined retirement condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_filter_design, conceptual, 'Whether the protection and the remedy denial are separable by design.').

omega_variable(
    dismissed_claimant_invisibility,
    'How large is the population of constitutional violation survivors whose claims terminate at summary judgment and therefore never register in settlement or trial statistics?',
    'Docket sampling of federal civil-rights filings coded for shield-based dismissal before discovery, triangulated with plaintiff-bar intake data.',
    'Undercounted victims depress measured extraction and measured resistance simultaneously; correcting the denominator raises the effective burden on payer seats and may push computed classifications toward the extraction-heavy range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dismissed_claimant_invisibility, empirical, 'Visibility bias in the victim population created by early-stage dismissals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(qual_tr_t0, observed).
narrative_ontology:measurement(qual_tr_t8, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(qual_tr_t8, observed).
narrative_ontology:measurement(qual_tr_t16, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(qual_tr_t16, observed).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(qual_tr_t24, observed).
narrative_ontology:measurement(qual_tr_t32, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement_basis(qual_tr_t32, observed).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(qual_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(qual_be_t0, observed).
narrative_ontology:measurement(qual_be_t8, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement_basis(qual_be_t8, observed).
narrative_ontology:measurement(qual_be_t16, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement_basis(qual_be_t16, observed).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(qual_be_t24, observed).
narrative_ontology:measurement(qual_be_t32, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(qual_be_t32, observed).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(qual_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(qual_su_t0, observed).
narrative_ontology:measurement(qual_su_t8, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(qual_su_t8, observed).
narrative_ontology:measurement(qual_su_t16, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement_basis(qual_su_t16, observed).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(qual_su_t24, observed).
narrative_ontology:measurement(qual_su_t32, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(qual_su_t32, observed).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(qual_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'qualified immunity' decomposes into three structurally distinct constraints — one per reading of the kernel — because the readings assign different epsilon values, different victim sets, and different legitimacy ledgers to the same case-law arrangement. This story (protective_scaffold_reading) is the mainstream judicial-position instantiation: moderate epsilon, conceded victims, defended coordination function. The accountability_void_reading is downstream of the empirical accountability literature; the constitutional_fidelity_reading is downstream of the statutory-genealogy critique. Each file links the other two via affects_constraints; no verdict about 'qualified immunity' as such is well-formed except across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
