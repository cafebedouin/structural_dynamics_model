% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Institutional Displacement of the Dueling Protocol (Courts, Credit Instruments, Libel Law)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   Between roughly 1700 and 1880 (interval T0-T180), the dispute-resolution
 *   protocol of the European and North American honor-bearing classes shifted
 *   from the code duello to institutional substitutes: accessible courts with
 *   actionable libel, impersonal credit instruments (bills of exchange,
 *   credit reporting, limited liability), and professionalized commercial
 *   litigation. Under this reading — the institutional_displacement_reading
 *   of the dueling_disappearance_mechanism kernel — dueling declined not
 *   primarily through prohibition (anti-dueling statutes existed for
 *   centuries with weak effect) but because the substitutes outcompeted it on
 *   cost, reliability, and risk; dueling persisted as an
 *   available-but-disfavored option exactly where the substitutes were thin
 *   (military officer corps, the American South, the frontier). The story is
 *   ABOUT the standing substitution arrangement, and epsilon is authored for
 *   that arrangement as this reading sees it: voluntary adoption, net
 *   benefit, no victim set. Claim and metrics are independent authored facts:
 *   the claimed type is rope, and the metrics describe low, declining
 *   extraction with low suppression, authored from the historical record
 *   rather than reconciled to the claim. The sibling readings (contraction,
 *   overdetermined composite) are separate constraints, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   gentry_and_officer_classes: primary beneficiary (powerful/constrained) —
 *   adopted courts and libel actions for affairs of honor; reverting to a
 *   challenge remained possible at rising cost - commercial_merchants:
 *   beneficiary (organized/mobile) — earliest adopters; credit instruments
 *   replaced personal-risk reputation defense - state_legal_establishment:
 *   agenda_setter and receipt seat (institutional/constrained) — built and
 *   administers the substitutes; collects fees, fines, and jurisdiction -
 *   honor_traditionalists: residual beneficiary (powerful/mobile) — slowest
 *   adopters; retained the dueling option in institutional gaps -
 *   military_officer_corps: gap-population beneficiary
 *   (organized/constrained) — last large honor-economy population; regimental
 *   rules bound members - non_elite_litigants: excluded seat
 *   (powerless/trapped) — bore dueling's casualties, priced out of both
 *   protocols, no seat in the transition - legal_historians: analytical
 *   observer — sees the full displacement pattern across jurisdictions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.16).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Institutional Displacement of the Dueling Protocol (Courts, Credit Instruments, Libel Law)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical sociology/legal history/cultural anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '61a1d0ae-978c-4eba-88e5-5ad5374313a7').
narrative_ontology:cs_kernel_codification('61a1d0ae-978c-4eba-88e5-5ad5374313a7', distributed).
narrative_ontology:cs_authority_grounding('61a1d0ae-978c-4eba-88e5-5ad5374313a7', distributed).
narrative_ontology:cs_reading_relation('61a1d0ae-978c-4eba-88e5-5ad5374313a7', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('61a1d0ae-978c-4eba-88e5-5ad5374313a7', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('61a1d0ae-978c-4eba-88e5-5ad5374313a7', foundational, institutional_substitution_sufficient_explanation).
narrative_ontology:cs_axiom_status(institutional_substitution_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('61a1d0ae-978c-4eba-88e5-5ad5374313a7', institutional_substitution_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_axiom('61a1d0ae-978c-4eba-88e5-5ad5374313a7', secondary, voluntary_adoption_no_victim_set).
narrative_ontology:cs_axiom_status(voluntary_adoption_no_victim_set, holdable).
narrative_ontology:cs_axiom_grounding('61a1d0ae-978c-4eba-88e5-5ad5374313a7', voluntary_adoption_no_victim_set, empirically_contingent).
narrative_ontology:cs_reference_frame('61a1d0ae-978c-4eba-88e5-5ad5374313a7', competing_dispute_resolution_market).
narrative_ontology:cs_drift_state('61a1d0ae-978c-4eba-88e5-5ad5374313a7', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('61a1d0ae-978c-4eba-88e5-5ad5374313a7', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentry_and_officer_classes).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_merchants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_traditionalists).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_establishment).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, law_adequate_to_reputational_disputes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen whose affairs of honor — insults, slandered courage, disputed creditworthiness — were once settled by the code duello. Across the interval they shifted to retaining attorneys, filing libel actions, and accepting court verdicts. Exit looks like reverting to a challenge: still possible through the whole interval, but increasingly costly in legal risk and social standing as their peers adopted the courts.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentry_and_officer_classes, beneficiary,
    powerful, biographical, constrained, national).

% Merchant houses whose creditworthiness once rested on personal reputation defended at risk of life. Bills of exchange, credit reporting, and limited liability let them establish reliability impersonally, and contract courts enforced obligations without blood. They move easily among banks, arbitration, and litigation, and were the substitution's earliest and most enthusiastic adopters.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_merchants, beneficiary,
    organized, generational, mobile, continental).

% Judges, legislators, and the bar who built and administer the substitutes: expanded court access, actionable libel, enforceable commercial instruments. They set the terms on which reputational and credit disputes are adjudicated, collect fees and fines, and gain legitimacy and jurisdiction as disputants arrive without compulsion. They cannot abandon the arrangement without dissolving their own function.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_establishment, beneficiary).

% Code duello defenders — Southern planters, army officers, frontier gentlemen — who kept the old protocol alive where courts and credit were thin. They adopted the institutional substitutes latest and most reluctantly, and in the gaps they retained a real alternative: a challenge remained available to them through the interval's end, at a rising price in legal exposure and social standing.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_traditionalists, beneficiary,
    powerful, biographical, mobile, regional).

% European and American officer corps, the last large population inside the honor economy. Courts-martial and regimental honor courts partially took over the dispute function, but regimental culture kept duels occurring into the late nineteenth century; discipline and commission-holding bound members to the corps' own dispute rules regardless of civilian court availability.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, military_officer_corps, beneficiary,
    organized, biographical, constrained, national).

% Laborers, servants, and the poor, who were never honor-culture participants and could not afford litigation or credit instruments. They bore the honor economy's casualties — fathers and sons killed in duels over slights they had no standing to answer — and had no seat in the elite transition that reallocated dispute resolution. The substitutes priced them out for most of the interval.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, non_elite_litigants, excluded,
    powerless, immediate, trapped, local).

% Scholars of law and violence who reconstruct the substitution record: court statistics, credit-instrument diffusion, dueling incidence by region and decade. They take no side in the honor economy and can see the whole displacement pattern across jurisdictions and over the full interval.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, state_legal_establishment).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dispute-resolution problem the code duello solved: converting insults, slandered reputation, and disputed creditworthiness into settled, binding outcomes without lethal private violence — courts adjudicate reputational injury, commercial instruments establish credit impersonally, and contract law enforces obligations.
% TRANSFER_FUNCTION: Moves dispute-settlement authority from private self-help to public institutions: reputational verdicts move from the honor community to the courts, credit assurance moves from personal risk-taking to financial instruments, and fees, fines, and legal costs move from disputants to courts, lawyers, and banks.
% ABSENT_VOICES: Non-elite men and women who bore dueling's casualties and were priced out of both protocols had no seat in the transition; honor traditionalists were progressively marginalized in the reform discourse that declared substitution progress. The adopting classes decided the reallocation among themselves.
% DISAPPEARANCE_RATIONALE: If courts, libel actions, and credit instruments ceased to settle affairs of honor overnight, the gentry's dispute load would revert to self-help — challenges, seconds, and honor courts — until new institutions emerged; commerce would lose impersonal credit assurance and contract enforcement, forcing reputation back onto personal risk.
% FOUNDING_PROBLEM: Private lethal violence as the dispute-resolution protocol among armed gentlemen, and the unreliability of personal reputation as a credit instrument: the arrangement was built to give disputants a non-lethal, binding path and merchants an impersonal assurance of payment.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is attested from outside the beneficiary set: period anti-dueling reformers documented that courts were adopted precisely because the honor protocol kept killing; modern quantitative scholarship on the long decline of interpersonal violence treats the state's adjudication monopoly as the operative replacement; and honor traditionalists themselves conceded the function, filing libel actions by the interval's end.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.16 at interval end) because the arrangement's costs to participants — court fees, legal costs, procedural delay — are service prices that fell as courts professionalized and credit instruments standardized; the series shows epsilon declining from 0.28 to 0.16 across the grid rather than accumulating. Suppression is low (0.18) because the mechanism is outcompetition, not coercion: the arrangement does not foreclose the dueling alternative, and the residual legal risk attached to dueling belongs to the prohibition regime, which this reading treats as a distinct and largely ineffective constraint. Accessibility collapse is correspondingly low (0.35) — the alternative remained genuinely available through the interval, which is this reading's signature claim. Resistance is moderate-low (0.30): honor traditionalists defended the code in treatises and legislatures and kept dueling alive in gaps, but adoption was voluntary and the resistance faded with the honor economy itself. Theater is low (0.12): courts and credit instruments genuinely performed their function throughout; the slight rise tracks procedural formalism, not functional atrophy. All series run on one shared time grid (T0, 30, 60, 90, 120, 150, 180); suppression_requirement is deliberately not tracked because the enforcement picture is static under this reading — the base_properties scalar carries it. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the gentry seat the arrangement is a service voluntarily purchased — a better product that replaced a costlier one. From the state seat it is jurisdiction consolidated and legitimacy gained as disputants arrived without compulsion. From the honor traditionalist seat it is a slow displacement of a status economy whose alternative exit (the challenge) stayed open but grew expensive. From the excluded seat it is a service that never priced them in: the same institutions that absorbed the gentry's dispute load left the poor outside both protocols. The engine derives per-seat classifications from the power, exit, and role data; the divergence between the beneficiary seats and the excluded seat is where this story's classification is most contestable.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated participant in the honor economy is declared a beneficiary, which drives their directionalities toward the beneficiary end: adoption was voluntary, the substitutes were net-beneficial, and no group is declared a victim — the absent victim set is this reading's central structural claim, not an omission. The state_legal_establishment is dual-positioned (agenda_setter, secondary beneficiary): it collects fees and jurisdiction, and under this reading those receipts are service-priced compensation rather than concentrated rent. The excluded seat (non_elite_litigants) sits outside the benefit circle but is unserved by the arrangement rather than harvested by it — the arrangement did not extract from them; it never reached them. That boundary — unserved versus harvested — is exactly what the honor_capital_devaluation_cost and substitution_vs_coercion_mechanism omegas police.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing lethal self-help with binding, non-lethal dispute resolution — remains live: courts and credit instruments still perform the function the code duello once performed, so there is no mandate outliving its function and no sunset question. The classification's protective work here runs in the opposite direction from the usual case: it prevents mislabeling a low-extraction voluntary coordination arrangement as extraction by refusing to manufacture a victim set the historical record does not support, while the omegas keep the no-victim declaration falsifiable. R5 status (live) crossed with the disappearance verdict (world_rearranges) shows no dead-mandate mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the dueling_disappearance_mechanism kernel (institutional_displacement_reading). Which causal mechanism a story assigns determines its victim structure and epsilon: what would the sibling readings change structurally?',
    'Comparative classification across the three sibling stories: if the composite reading''s prohibition component or the contraction reading''s cultural mechanism yields a victim set (coerced holdouts, prosecuted duelists) where this reading finds voluntary substitution, the disagreement is located in the mechanism attribution, not in the historical record itself.',
    'If a sibling reading is better supported, this story''s no-victim-set declaration and low-extraction profile are artifacts of the substitution framing; the same historical interval would instantiate a structurally different constraint with higher epsilon under a prohibition-led or coercion-inclusive mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the disappearance mechanism: substitution versus cultural contraction versus overdetermination.').

omega_variable(
    substitution_vs_coercion_mechanism,
    'Was dueling''s displacement genuinely voluntary substitution, or did legal prohibition and prosecution coerce the holdouts — creating a victim set this reading does not declare?',
    'Enforcement records: prosecution rates and outcomes for duelists across jurisdictions and decades, compared against court-density and credit-instrument diffusion. If dueling incidence tracks institutional availability rather than enforcement intensity, substitution was operative.',
    'If coercion was operative, epsilon rises, honor traditionalists become a declared victim set, and the classification shifts away from pure coordination toward a hybrid with enforced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_coercion_mechanism, empirical, 'Whether the displacement was competitive substitution or coerced suppression.').

omega_variable(
    gap_persistence_pattern,
    'Dueling persisted into the late nineteenth century exactly where institutional substitutes were thin (US South, officer corps, frontier). Does that residual pattern confirm institutional availability as the operative variable, or did honor ideology sustain dueling independently of institutions?',
    'Cross-regional panel: dueling incidence against court access, credit-instrument diffusion, and measured honor-culture indicators, holding enforcement constant within jurisdictions.',
    'If incidence tracks institutional thinness, this reading''s mechanism is confirmed and the available-but-disfavored residual is real; if ideology predicts persistence with institutions held constant, the contraction reading gains ground and this story''s epsilon and classification are unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_persistence_pattern, empirical, 'Whether residual dueling maps onto institutional gaps or onto honor ideology.').

omega_variable(
    honor_capital_devaluation_cost,
    'Honor traditionalists lost a functioning status economy — honor capital and the code duello''s social currency — as substitution proceeded. Is that devaluation a cost the arrangement imposes (a candidate victim set) or the ordinary price of losing a voluntary competition?',
    'Conceptual analysis plus period sources: whether traditionalists experienced the loss as imposed harm (petitions, treatises framing substitution as confiscation of their standing) or as a contest they declined to join; whether the arrangement actively devalued honor capital or merely offered a superior alternative.',
    'If devaluation counts as imposed extraction, a victim set emerges, epsilon rises, and the no-victim-set declaration fails; if it is competitive loss, the declaration stands and the excluded-seat boundary is unserved-versus-harvested, not harvested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_capital_devaluation_cost, conceptual, 'Whether devalued honor capital constitutes a victim cost or competitive loss.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddm_institutional_disp_tr_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t0, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t30, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t30, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t60, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t60, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t90, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 90, 0.09).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t90, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t120, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 120, 0.09).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t120, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t150, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 150, 0.11).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t150, observed).
narrative_ontology:measurement(ddm_institutional_disp_tr_t180, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 180, 0.12).
narrative_ontology:measurement_basis(ddm_institutional_disp_tr_t180, observed).

% Extraction over time
narrative_ontology:measurement(ddm_institutional_disp_be_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t0, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t30, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t30, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t60, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t60, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t90, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 90, 0.21).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t90, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t120, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 120, 0.19).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t120, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t150, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 150, 0.17).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t150, observed).
narrative_ontology:measurement(ddm_institutional_disp_be_t180, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 180, 0.16).
narrative_ontology:measurement_basis(ddm_institutional_disp_be_t180, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The kernel dueling_disappearance_mechanism decomposes by epsilon-invariance into three readings, each a separate constraint story: this institutional-substitution reading (voluntary substitution; no victim set), the contraction reading (dignity-culture displacement of honor axioms; mechanism is cultural, not institutional), and the overdetermined composite (multiple independent sufficient causes including legal prohibition, which implies coerced holdouts and a candidate victim set). The readings share the historical referent but assign different mechanisms, victim structures, and epsilon values; they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
