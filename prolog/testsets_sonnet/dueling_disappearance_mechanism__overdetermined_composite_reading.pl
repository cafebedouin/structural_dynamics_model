% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Decline as Causally Overdetermined Composite Mechanism
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the OVERDETERMINED COMPOSITE reading of the
 *   dueling-disappearance kernel: dueling's decline is attributed not to any
 *   single dominant causal mechanism but to four independently sufficient
 *   conditions operating simultaneously and non-separably across the 19th
 *   century United States — legal prohibition, institutional modernization
 *   (courts, banks, insurance), cultural shift, and Civil War trauma. Under
 *   this reading the constraint is a tangled_rope precisely because it has
 *   FOUR distinct beneficiary institutions each capturing a different
 *   fragment of dueling's former function, rather than one coherent
 *   coordination mechanism with one extraction pathway. No single epsilon is
 *   cleanly measurable because the causal pathways are entangled — the
 *   extractiveness value authored here represents the composite's aggregate
 *   structural cost to the payer class, not any one mechanism's isolated
 *   contribution. This is a sibling of contraction_reading (which attributes
 *   the decline to cultural dignity-displacement alone) and
 *   institutional_displacement_reading (which attributes it to institutional
 *   substitution alone); this composite reading does not choose between them
 *   but claims BOTH were independently operative alongside prohibition and
 *   war trauma, which the other two readings each treat as secondary or
 *   absent.
 *
 * KEY AGENTS:
 *   - state_judicial_apparatus: agenda_setter/beneficiary (institutional/arbitrage) - captures dispute-resolution rents via litigation
 *   - commercial_credit_institutions: beneficiary (organized/mobile) - captures reputational-verification function
 *   - postbellum_political_elites: beneficiary (powerful/mobile) - redirects martial prestige to electoral machines
 *   - life_insurance_industry: beneficiary (organized/arbitrage) - captures actuarial risk-management rents
 *   - traditional_honor_class_gentry: payer (powerful/constrained) - loses core status mechanism
 *   - dueling_code_duello_practitioners: payer (moderate/trapped) - practice criminalized and ridiculed simultaneously
 *   - southern_planter_descendants: payer (moderate/constrained) - regional trauma compounds status collapse
 *   - historians_of_the_decline: observer (analytical) - adjudicates causal apportionment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.55).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Decline as Causally Overdetermined Composite Mechanism").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'ed714282-3937-4f78-8e8d-1cd027e4198d').
narrative_ontology:cs_kernel_codification('ed714282-3937-4f78-8e8d-1cd027e4198d', distributed).
narrative_ontology:cs_authority_grounding('ed714282-3937-4f78-8e8d-1cd027e4198d', distributed).
narrative_ontology:cs_reading_relation('ed714282-3937-4f78-8e8d-1cd027e4198d', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed714282-3937-4f78-8e8d-1cd027e4198d', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('ed714282-3937-4f78-8e8d-1cd027e4198d', foundational, causal_pathways_are_jointly_sufficient_and_non_separable).
narrative_ontology:cs_axiom_status(causal_pathways_are_jointly_sufficient_and_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('ed714282-3937-4f78-8e8d-1cd027e4198d', causal_pathways_are_jointly_sufficient_and_non_separable, empirically_contingent).
narrative_ontology:cs_axiom('ed714282-3937-4f78-8e8d-1cd027e4198d', secondary, no_single_mechanism_bears_primary_explanatory_weight).
narrative_ontology:cs_axiom_status(no_single_mechanism_bears_primary_explanatory_weight, holdable).
narrative_ontology:cs_axiom_grounding('ed714282-3937-4f78-8e8d-1cd027e4198d', no_single_mechanism_bears_primary_explanatory_weight, empirically_contingent).
narrative_ontology:cs_reference_frame('ed714282-3937-4f78-8e8d-1cd027e4198d', antebellum_honor_culture_baseline).
narrative_ontology:cs_drift_state('ed714282-3937-4f78-8e8d-1cd027e4198d', reconstruction_era_convergence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ed714282-3937-4f78-8e8d-1cd027e4198d', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_judicial_apparatus).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_political_elites).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, life_insurance_industry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_class_gentry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_code_duello_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_descendants).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, multi_causal_social_change_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State legislatures criminalized dueling with escalating penalties (disenfranchisement, capital charges in some jurisdictions) across the 19th century, while courts simultaneously expanded civil remedies for slander and assault. This apparatus both writes the prohibition and captures the dispute-resolution function that dueling formerly occupied, converting honor disputes into litigable claims it adjudicates and taxes through court fees.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_judicial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, state_judicial_apparatus, beneficiary).

% As banking and credit-reporting networks matured, a man's commercial reputation became verifiable through institutional records rather than personal combat readiness. Banks and credit bureaus benefited from a stable, non-lethal reputational infrastructure that let commerce proceed without dueling's periodic removal of counterparties from the economy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_institutions, beneficiary,
    organized, generational, mobile, national).

% After the Civil War's mass casualties, political and military elites who survived found dueling's remaining prestige value collapsed relative to its now-trivial-seeming risk; they benefited from redirecting martial credibility toward electoral and institutional politics rather than the code duello, consolidating power through party machines instead of personal combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_political_elites, beneficiary,
    powerful, generational, mobile, national).

% Nascent life insurance companies wrote exclusion clauses for death by dueling and lobbied against the practice as an actuarially destabilizing risk category; they benefited from a social order where death was statistically predictable rather than subject to voluntary combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, life_insurance_industry, beneficiary,
    organized, generational, arbitrage, national).

% Gentlemen whose social standing had been defined through willingness to duel found their central status-conferring mechanism criminalized, medicalized, and culturally mocked simultaneously. They bore the cost of a status system's collapse without a replacement that preserved their relative position; many absorbed reputational loss they could no longer redeem through combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_class_gentry, payer,
    powerful, biographical, constrained, regional).

% Individuals still embedded in honor-culture social networks who wished to duel found the practice increasingly prosecutable, socially ridiculed, and stripped of seconds willing to participate; their exit from the practice was not chosen but foreclosed from multiple directions at once, leaving some in a status limbo where neither dueling nor litigation restored honor.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_code_duello_practitioners, payer,
    moderate, biographical, trapped, regional).

% The regional stronghold of dueling culture (the antebellum South) suffered the most concentrated post-Civil War trauma and economic collapse, which independently eroded the material basis for honor-culture display; descendants inherited a devalued status economy without dueling as an option and without the Northern institutional alternatives fully available to them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_descendants, payer,
    moderate, generational, constrained, regional).

% Historical sociologists examine the decline and must decide whether to attribute it to a single dominant mechanism or treat the causal pathways as non-separable and jointly sufficient; this composite reading is itself a methodological stance about how to apportion causal credit among institutions that all benefited from dueling's end.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historians_of_the_decline, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordination function exists under this reading; instead multiple independent institutional projects (judicial expansion, credit infrastructure, martial-prestige redirection, actuarial risk management) each solved a DIFFERENT coordination problem, and their simultaneous operation jointly extinguished dueling as a side effect rather than a shared target.
% TRANSFER_FUNCTION: Status-conferring and dispute-resolution functions formerly held by the dueling code transferred outward to courts (litigation fees, judicial authority), banks (credit verification), political machines (electoral prestige), and insurers (actuarial control) — a fragmentation of a single function into four institutional beneficiaries, at the cost of the honor-class practitioners who had no equivalent replacement.
% ABSENT_VOICES: The dueling practitioners and honor-class gentry whose status system was dismantled left few institutional records defending the practice on its own terms once it became legally and socially indefensible; their perspective survives mostly in personal letters and literature, not in the historical record the winning institutions produced.
% DISAPPEARANCE_RATIONALE: Historians dispute what would happen if any ONE of the four conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma) were removed while holding the others constant — the overdetermination thesis specifically claims each was independently sufficient, so removing any single one would plausibly still see dueling decline via the remaining three, but removing all simultaneously is not historically observable, making the counterfactual irreducibly contested.
% FOUNDING_PROBLEM: Dueling itself was originally built to solve elite dispute resolution and status verification outside formal law; its DECLINE mechanism (the subject of this constraint) was not 'built' by any single architect but assembled from four institutions each solving their own separate problem (crime control, credit risk, political consolidation, actuarial stability) that happened to converge on eliminating dueling as a byproduct.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (outside any of the four beneficiary institutions) corroborate via statute chronology that prohibition, credit-network expansion, and insurance exclusions all matured independently in overlapping decades (1840s-1880s) without central coordination; the corroboration is methodological convergence across independent archival records (state penal codes, bank ledgers, insurance policy language) rather than testimony from a single outside authority, since no single outside observer tracked all four causal streams as one process at the time.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).
:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and suppression (0.55) are authored at a MODERATE level reflecting that this is a composite of four partial mechanisms, none individually as extractive or coercive as a dedicated snare would be — each institution (courts, banks, political machines, insurers) captured only a fragment of the total value transferred away from the honor-class payers. Theater ratio (0.4) is elevated because a substantial share of anti-dueling legislation was performatively enacted (statutes rarely enforced with capital penalties) while the REAL suppressive work was done by social ridicule and institutional substitution rather than the law itself — this matches the composite thesis that legal prohibition was one of several sufficient conditions, not the operative one in most documented cases. The time series runs 1800-1900 on one shared grid, with all three tracked metrics rising through the antebellum period, accelerating sharply around the Civil War (1860-1870), and plateauing by 1880 as the multiple mechanisms reached their combined saturation point — dueling was, by 1880, extinguished by the joint operation of all four conditions and no further suppression was needed.
 *
 * PERSPECTIVAL GAP:
 *   From the state judicial apparatus's seat, this looks like successful law enforcement solving a public-order problem (payer seat computes this as legitimate coordination). From the honor-class gentry's seat, the SAME period looks like simultaneous assault from four directions with no coordinated adversary to resist or negotiate with — you cannot litigate against 'modernization,' negotiate with 'cultural shift,' or duel 'war trauma.' This diffuseness of the extracting agent is itself structurally significant: a tangled_rope with four independent beneficiaries and no single administrator is harder for payers to resist than a single-actor snare would be, because there is no concentrated target for grievance or coalition resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (judicial apparatus, credit institutions, political elites, insurers) each derive low-to-moderate directionality because each captured only a PARTIAL rent from dueling's decline — none experienced dueling's end as their sole or even primary source of institutional gain, which is why the composite is a tangled_rope rather than four separate snares. Payers (honor-class gentry, code duello practitioners, planter descendants) derive high directionality because the constraint's operation demonstrably extracted their status-capital without offering an equivalent replacement mechanism, and their exit options were structurally foreclosed from multiple directions (trapped/constrained) rather than a single blocking actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists mandatrophy mislabeling in a specific way: because no single institution can be pointed to as THE cause, none is at risk of being wrongly credited with (or blamed for) the entire coordination/extraction outcome. This is precisely the analytical value of the overdetermined framing — it prevents any one beneficiary institution's origin myth (e.g. 'the courts civilized society') from being taken as the full explanation, while also preventing any one payer grievance narrative (e.g. 'the Yankees destroyed Southern honor') from claiming exclusive causal weight. The founding_problem field reflects this: the DECLINE mechanism has no single founder or mandate to expire, which is structurally distinct from the other two readings where a specific displacement process (cultural or institutional) can be dated and evaluated for whether its founding problem is live or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_overdetermination_vs_single_dominant_cause,
    'Is the overdetermination thesis itself correct, or does one of the four conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma) actually dominate causally, with the others merely correlated or epiphenomenal?',
    'Comparative case analysis across jurisdictions where the four conditions were NOT co-present in the same timing (e.g. regions with early legal prohibition but delayed institutional modernization, or cultural shift without comparable war trauma) — if dueling persisted or declined at different rates matching the presence/absence of specific conditions rather than all four together, overdetermination weakens in favor of a dominant-cause reading.',
    'If overdetermination is not sustained empirically, this constraint should be retired in favor of whichever single-mechanism sibling reading (contraction or institutional_displacement) the comparative evidence supports, and the tangled_rope classification with four beneficiaries would collapse into a simpler rope or snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_overdetermination_vs_single_dominant_cause, empirical, 'Whether the four-condition overdetermination is empirically sustained or a historiographic artifact of retrospective narrative-smoothing.').

omega_variable(
    beneficiary_boundary_ambiguity,
    'Are the four named beneficiary institutions (judiciary, credit, political elites, insurers) genuinely independent beneficiaries, or do they share a common underlying beneficiary (e.g. an emergent bourgeois-industrial order) that this reading artificially fragments into four?',
    'Prosopographic analysis of interlocking directorates and personnel overlap between 19th-century judicial appointees, bank officers, party machine leadership, and insurance executives — high overlap would suggest a single elite beneficiary class rather than four structurally distinct institutions.',
    'If the four beneficiaries collapse into one elite class, this reading''s tangled_rope classification (which depends on multiple distinct beneficiaries) would be undermined and the constraint might better be classified as a snare with a single, if institutionally diffuse, beneficiary class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary_ambiguity, conceptual, 'Whether the four beneficiary institutions are structurally independent or a single elite class wearing four institutional faces.').

omega_variable(
    victim_set_indeterminacy,
    'The expected structural delta for this reading notes victim set unclear (depends on which mechanism dominated) — does the honor-class gentry, the code duello practitioners specifically, or the broader Southern regional population bear the primary cost, and does this depend on resolving the overdetermination question first?',
    'Disaggregate the payer analysis by mechanism: identify which victim group was harmed distinctly by prohibition (practitioners facing prosecution), by institutional displacement (gentry losing status-verification function), by cultural shift (gentry losing normative legitimacy), and by war trauma (Southern planter descendants specifically) — if these victim populations diverge sharply, the composite''s single victims array is an oversimplification.',
    'A finer-grained victim analysis might require splitting this single tangled_rope story into multiple stories per mechanism, which would itself resolve the overdetermination question by making the mechanisms separable after all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_indeterminacy, conceptual, 'Whether a single composite victim set is defensible or masks mechanism-specific victim populations that would resolve overdetermination if disaggregated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.22).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.28).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1870, 0.35).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.4).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.22).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.3).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.34).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1870, 0.4).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.42).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.3).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.4).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.48).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1870, 0.53).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.55).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.1).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is the OVERDETERMINED_COMPOSITE member of a three-story kernel family on dueling's disappearance mechanism. contraction_reading isolates cultural dignity-displacement as the operative mechanism (likely a rope or tangled_rope with a narrower beneficiary set centered on the emerging dignity-culture normative class). institutional_displacement_reading isolates institutional substitution (courts, banking, libel law) as the operative mechanism (likely a tangled_rope with beneficiaries limited to formal dispute-resolution institutions). This composite reading claims BOTH of those mechanisms were operative simultaneously ALONGSIDE legal prohibition and Civil War trauma, yielding a four-beneficiary tangled_rope with a correspondingly higher-uncertainty victim set. The three stories are mutually informative: if empirical work on either sibling strongly confirms a SINGLE dominant mechanism, this composite reading's overdetermination premise weakens correspondingly (see the causal_overdetermination_vs_single_dominant_cause omega).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
