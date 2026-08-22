% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Institutional Substitution for Honor-Dispute Resolution (Courts, Banking, Libel Law vs. Dueling)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the institutional_displacement_reading of the
 *   dueling_disappearance_mechanism kernel: dueling's decline from normative
 *   dispute-resolution practice to fringe survival is read as a case of
 *   institutional market substitution. Courts (enforceable civil remedies),
 *   formalized banking and credit instruments (resolving
 *   reputational-financial disputes without combat), and codified
 *   libel/slander law (providing legal recourse for reputational injury)
 *   together offered a lower-variance, lower-cost protocol for the same
 *   coordination problem dueling had solved: settling contested claims to
 *   honor, debt, and standing in a way the relevant community would treat as
 *   final. On this reading, the constraint under evaluation is the surviving
 *   coordination function itself — an institutional-substitution regime for
 *   honor-adjacent disputes — which remains structurally a rope: it solves a
 *   genuine coordination problem, participants who switch are net
 *   beneficiaries (avoiding death/injury variance while achieving comparable
 *   settlement), and dueling persists as an available-but-disfavored option
 *   precisely where institutional coverage has gaps (frontier regions,
 *   certain military contexts), rather than being suppressed by coercive
 *   prohibition. Two sibling readings of the same colloquial phenomenon are
 *   NOT evaluated here: contraction_reading treats the decline as a
 *   normative/cultural shift (dignity culture displacing honor culture)
 *   rather than a market-substitution story, and
 *   overdetermined_composite_reading treats the decline as the joint product
 *   of multiple independently sufficient causes. Each sibling reading authors
 *   its own epsilon and stakeholder structure in separate files; this file's
 *   epsilon (0.12, low, rope-consistent) is specific to the
 *   institutional-substitution claim and should not be averaged with or read
 *   against the siblings' values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Institutional Substitution for Honor-Dispute Resolution (Courts, Banking, Libel Law vs. Dueling)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '1b93dc30-670d-4131-889f-9fbd25847ee9').
narrative_ontology:cs_kernel_codification('1b93dc30-670d-4131-889f-9fbd25847ee9', distributed).
narrative_ontology:cs_authority_grounding('1b93dc30-670d-4131-889f-9fbd25847ee9', practice).
narrative_ontology:cs_interpretation_layer_present('1b93dc30-670d-4131-889f-9fbd25847ee9').
narrative_ontology:cs_reading_relation('1b93dc30-670d-4131-889f-9fbd25847ee9', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b93dc30-670d-4131-889f-9fbd25847ee9', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('1b93dc30-670d-4131-889f-9fbd25847ee9', foundational, institutional_capacity_is_the_operative_variable).
narrative_ontology:cs_axiom_status(institutional_capacity_is_the_operative_variable, holdable).
narrative_ontology:cs_axiom_grounding('1b93dc30-670d-4131-889f-9fbd25847ee9', institutional_capacity_is_the_operative_variable, empirically_contingent).
narrative_ontology:cs_axiom('1b93dc30-670d-4131-889f-9fbd25847ee9', secondary, dueling_decline_requires_no_normative_change_explanation).
narrative_ontology:cs_axiom_status(dueling_decline_requires_no_normative_change_explanation, holdable).
narrative_ontology:cs_axiom_grounding('1b93dc30-670d-4131-889f-9fbd25847ee9', dueling_decline_requires_no_normative_change_explanation, empirically_contingent).
narrative_ontology:cs_reference_frame('1b93dc30-670d-4131-889f-9fbd25847ee9', honor_dispute_resolution_open_market).
narrative_ontology:cs_drift_state('1b93dc30-670d-4131-889f-9fbd25847ee9', late_19th_century_institutional_saturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b93dc30-670d-4131-889f-9fbd25847ee9', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, civil_court_litigants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_banking_sector).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, aspiring_professional_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, traditional_honor_class_remnants).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, institutional_efficiency_thesis).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, dispute_resolution_market_substitution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen with grievances over debt, slander, or contractual breach could, by the mid-to-late 19th century, take a matter to civil court and recover damages or an injunction rather than issuing a challenge. Courts offered a lower-risk protocol that produced enforceable, transferable remedies (money, injunctions) instead of a coin-flip outcome involving death or maiming.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, civil_court_litigants, beneficiary,
    moderate, biographical, mobile, national).

% As credit and commercial reputation became formalized through credit reporting, promissory instruments, and bankruptcy law, disputes over debt and reputation that once could trigger a challenge were increasingly settled through commercial and financial instruments that assigned liability without recourse to combat. Banking's growth created a substitute mechanism for the reputational stakes that had driven a large share of duels.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_banking_sector, beneficiary,
    organized, generational, arbitrage, national).

% Lawyers, physicians, and merchants building careers on institutional standing (bar admission, professional licensing, credit rating) found that submitting to duels jeopardized exactly the institutional legitimacy their livelihood depended on. For this class, using courts and libel actions instead of pistols was a rational, low-cost substitute that protected career capital.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, aspiring_professional_class, beneficiary,
    moderate, biographical, mobile, national).

% Legislatures and courts built out civil remedies for defamation (libel and slander law), enforceable contract and debt-collection procedures, and criminal sanctions for dueling. They administered the growing menu of institutional alternatives without needing to coercively suppress dueling directly in most cases — the alternatives simply outcompeted it on cost and outcome predictability. Where statutes existed they were unevenly enforced, consistent with substitution rather than crackdown driving the decline.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% A shrinking population — often in regions or subcultures with weaker institutional penetration (parts of the antebellum South, military officer corps, isolated frontier communities) — continued to regard dueling as the only legitimate response to certain insults, precisely because courts and libel actions were locally unavailable, slow, or regarded as beneath a gentleman's dignity. They bore the residual risk of death or injury in a system increasingly abandoned by the mainstream, and their perspective is largely absent from the institutional-substitution account, which is written from the vantage of the emerging alternatives' success.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, traditional_honor_class_remnants, excluded,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, traditional_honor_class_remnants, payer).

% Reconstruct the timeline of court capacity, libel-law codification, and banking formalization against dueling frequency data, arguing the correlation reflects institutions winning a competitive market for dispute resolution rather than a change in underlying honor-values.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a lower-cost, outcome-predictable, non-lethal protocol for resolving disputes over debt, reputation, and contractual breach — the same coordination problem dueling solved (settling a contested claim to standing or restitution) but without the fatal-outcome variance.
% TRANSFER_FUNCTION: Moves dispute resolution from a private, honor-coded, violence-backed protocol to public/commercial institutional protocols (courts, banks, libel actions); no systematic transfer of resources from a victim class to a beneficiary class — participants who switch protocols are net beneficiaries of lower variance and enforceable outcomes.
% ABSENT_VOICES: Remnant honor-culture participants (frontier communities, some military officer corps, parts of the antebellum planter class) are structurally outside the institutional narrative because their objection — that courts cannot restore an insulted man's honor the way combat can — is precisely the claim institutional substitution treats as an inefficiency to be competed away, not a legitimate alternative value to be weighed.
% DISAPPEARANCE_RATIONALE: If courts, banking instruments, and libel law vanished overnight, dueling would not resurge as the default dispute mechanism for most disputants — other institutional or informal substitutes (arbitration, reputation markets, mediation) would likely fill the gap, because the underlying pressure driving adoption was cost/predictability, not attachment to any specific institution. Removing this particular constraint does not require the world to reorganize around dueling; it just shifts which substitute institution captures the demand.
% FOUNDING_PROBLEM: Disputants needed a way to settle contested claims over debt, honor, and reputation with an outcome that would be recognized as final and legitimate by the relevant social circle.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians studying court-capacity expansion and libel-suit filing rates (a party with no stake in either dueling's survival or the courts' success) attest that civil docket volume for defamation and debt claims rose sharply in the same decades dueling frequency fell in the same social strata, supporting a substitution account from outside the beneficiary institutions themselves.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at interval end) because the institutional-substitution reading identifies no systematic transfer from a coerced victim class to a captor class — disputants who used courts, banks, or libel suits over pistols did so because the substitute was cheaper and more predictable for them, not because they were forced into an extractive arrangement. Suppression is authored low (0.15) because state prohibition of dueling was historically uneven and lagged behind the decline in dueling frequency in many jurisdictions — the institutional-substitution account's causal claim is that dueling lost a competitive market, not that it was coercively stamped out. Theater ratio stays low and flat (0.03 to 0.08) because the substitute institutions (courts, banks, libel law) were performing real dispute-resolution work throughout the period, not theatrical maintenance of a form whose function had already atrophied — that atrophied-form profile belongs to a different constraint (a piton reading, not authored here).
 *
 * DIRECTIONALITY LOGIC:
 *   Civil court litigants, the banking sector, and the aspiring professional class are declared beneficiaries because the switch from dueling to institutional protocols reduced their exposure to lethal-outcome variance while preserving (or improving) their ability to settle contested claims — low d, near the beneficiary end. State legal authorities sit as agenda_setter/analytical: they administer the growing menu of alternatives but are not shown extracting rents from the substitution itself. Traditional honor-class remnants are the one seat with residual cost exposure (payer secondary role) — in regions or subcultures with weak institutional penetration, they continued bearing dueling's lethal risk precisely because the substitute institutions had not yet reached them; this is a residual/geographic-coverage gap, not a designed victim structure, which is why no victims array is authored for this reading (per the expected structural delta: voluntary substitution, no systematic victim set).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling contested claims over honor, debt, and reputation with a socially legitimate finality) remains live — it did not disappear, it migrated to substitute institutions. This is why founding_problem_status is 'live' rather than 'dead': there is no mandatrophy here in the classic sense of an arrangement persisting after its function evaporated. What this reading documents instead is functional succession — the coordination function outlived the specific mechanism (dueling) that once carried it, and now rides on courts/banking/libel law. Classifying this as rope rather than snare or piton prevents mislabeling a genuine, still-live coordination function as either pure extraction or as an inertial husk; the low theater_ratio and low extractiveness track that the substitute institutions are doing real, not merely performative, work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_cultural_shift_causal_priority,
    'Did institutional substitution (courts, banking, libel law) drive dueling''s decline independently of cultural/normative shift, or did the two co-evolve such that institutional capacity only mattered because dignity-culture norms had already made courts an acceptable venue for honor disputes?',
    'Comparative regional analysis: jurisdictions where court/banking capacity expanded early but honor-culture norms remained strong (or vice versa) would let researchers observe which factor predicts dueling decline independent of the other. Sequencing analysis of court-docket growth versus dueling-frequency decline within the same regions would also help establish temporal priority.',
    'If institutional capacity predicts decline independent of cultural shift, this reading''s causal claim is supported as a standalone mechanism. If the two are inseparable (institutions were only adopted because norms had already shifted, or norms only shifted because institutions offered a face-saving exit), this reading''s claim to be a distinct, sufficient mechanism partially collapses into the overdetermined_composite_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_cultural_shift_causal_priority, empirical, 'Whether institutional substitution operated as an independent causal mechanism or was entangled with cultural/normative change.').

omega_variable(
    voluntary_substitution_vs_state_suppression_boundary,
    'Is the decline of dueling better modeled as voluntary institutional substitution (participants chose the substitute because it was better) or as a case where state anti-dueling statutes, even if unevenly enforced, exerted meaningful coercive pressure that this reading under-weights by emphasizing market competition?',
    'Archival review of prosecution rates and social sanction records (loss of office, military commission revocation) for dueling participants across the period, compared against the timing of institutional-alternative adoption.',
    'If coercive state suppression was a significant independent force, the low suppression score (0.15) authored here understates the constraint''s coercive dimension, and part of what this reading attributes to market substitution may actually belong to a more coercive account of decline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_substitution_vs_state_suppression_boundary, empirical, 'Whether the reading''s low suppression score correctly separates voluntary substitution from underlying state coercion.').

omega_variable(
    residual_honor_class_representation,
    'Does the institutional-displacement account, written from the vantage of the successful substitute institutions, adequately represent the perspective of the traditional-honor-class remnants for whom institutional alternatives were genuinely unavailable or illegitimate, or does it retroactively frame their continued dueling as irrational holdout rather than a coherent alternative value system?',
    'Ethnographic/documentary reconstruction of first-person justifications from late-period duelists (military officers, frontier gentry) to assess whether they framed their choice as lacking access to alternatives versus rejecting the alternatives'' legitimacy.',
    'If remnant duelists rejected institutional alternatives on principle rather than lacking access, the ''excluded voice'' framing understates their agency and this reading''s account of dueling as fringe-because-outcompeted would need revision toward a values-conflict account closer to the contraction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_honor_class_representation, conceptual, 'Whether excluded honor-class holdouts lacked institutional access or actively rejected institutional legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1820, 0.04).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1860, 0.06).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1880, 0.07).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.08).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1820, 0.06).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1840, 0.08).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1860, 0.1).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1880, 0.11).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dueling_disappearance_mechanism kernel, each authored as a separate constraint story per the ε-invariance principle: institutional_displacement_reading (this file, rope, ε=0.12) treats decline as market substitution of dispute-resolution protocols; contraction_reading treats it as a normative/cultural shift (honor-culture axioms displaced by dignity-culture axioms) with its own distinct ε and stakeholder structure; overdetermined_composite_reading treats it as multiple independently sufficient causes acting jointly. The three do not average into one ε — each is a structurally distinct claim about the same colloquial phenomenon, linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
