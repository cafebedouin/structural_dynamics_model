% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Judicial supremacy is one reading of a contested constitutional kernel:
 *   who holds final interpretive authority over what the constitution means?
 *   This reading claims that courts, through specialized legal expertise and
 *   independence from electoral pressure, should hold that authority. The
 *   constraint operates by courts reviewing legislation, striking down laws
 *   they deem unconstitutional, and instructing other institutions on how to
 *   comply with their reading of constitutional text and doctrine. The
 *   reading generates extraction: legislative majorities and electoral losers
 *   bear the cost of having their will displaced when courts exercise veto;
 *   the judiciary and legal professions benefit from authority and the
 *   gatekeeping role that specialization creates. The measurement series
 *   track how extractiveness and enforcement requirements have drifted from
 *   the constraint's founding justification (limiting arbitrary majorities)
 *   toward a more entrenched institutional authority structure.
 *
 * KEY AGENTS:
 *   - Judiciary as authority holder (institutional, agenda-setter) — exercises final interpretive power, sets terms for legislative action, claims apolitical expertise.
 *   - Legislative majorities (institutional, payer) — face veto when courts overturn their legislation; bear gridlock costs; excluded from final interpretation.
 *   - Electoral majorities and minorities (moderate power, payer) — experience their democratic will displaced; constrained exit; can attempt amendment or justicial replacement.
 *   - Legal professionals (organized, beneficiary) — careers and professional status depend on specialized constitutional expertise; gate-keeping role.
 *   - Minority rights advocates (moderate power, beneficiary) — benefit from courts protecting them against majoritarian legislation.
 *   - Competing institutional authorities (excluded seat) — parliaments, executives, and the people have no final say if courts hold supremacy; sibling readings give these actors authority.
 *   - Analytical observer (seat) — sees the full distribution of authority and cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3').
narrative_ontology:cs_kernel_codification('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', formalized).
narrative_ontology:cs_authority_grounding('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', extraction).
narrative_ontology:cs_interpretation_layer_present('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3').
narrative_ontology:cs_reading_relation('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', foundational, judicial_expertise_enables_neutral_interpretation).
narrative_ontology:cs_axiom_status(judicial_expertise_enables_neutral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', judicial_expertise_enables_neutral_interpretation, instrumental).
narrative_ontology:cs_axiom('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', foundational, democratic_legitimacy_requires_constitutional_constraint).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_constitutional_constraint, holdable).
narrative_ontology:cs_axiom_grounding('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', democratic_legitimacy_requires_constitutional_constraint, deontological).
narrative_ontology:cs_reference_frame('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', constitutional_supremacy_through_judicial_review).
narrative_ontology:cs_drift_state('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', contemporary_appointment_politics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e5a5ecc-8d9f-4edc-9eb8-dbba179c15d3', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institutional_authority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_professionals).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_losers_in_judicial_review).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 at interval end because the constraint systematically displaces the democratic will of electoral majorities when courts overturn legislation, and legal professionals benefit substantially from the specialization regime it creates. Suppression at 0.58 reflects that the constraint's persistence depends on courts actively defending their exclusive interpretive authority — overruling legislatures, resisting amendment, training new judges in the doctrine. Theater has risen modestly from 0.18 to 0.28 over the interval, suggesting that an increasing share of judicial activity is devoted to maintaining the supremacy claim itself (institutional defense, doctrinal refinement) rather than to genuine dispute resolution. The measurement series are authored on a single shared time grid: all three metrics appear at every examined time point so temporal alignment is preserved. The rhythm shows extractiveness rising steeply in the first 30 time units (as courts became more active in high-stakes political cases), then plateauing as institutional contestation stabilized — the courts are no longer expanding their reach, but maintaining the authority they established.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is genuine coordination it built and maintains: courts provide stability, predictability, and protection of constitutional principle against majoritarian impulse. From the legislative and electoral seats, the same constraint operates as forced deference to judges whose accountability runs through judicial ethics, not elections — a substitution of expert authority for democratic consent. The engine computes this divergence from the structural data. The judiciary will compute this constraint type as rope (coordination, minimal extraction) from its seat; legislative majorities will compute it as tangled_rope or snare (coordination function shadowed by extraction they bear). The authored claim at 0.62 extractiveness and 0.58 suppression means the constraint computes as tangled_rope overall, capturing the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from the constraint: it confers authority, professional prestige, and the power to set terms. Their directionality is near the beneficiary end (d ≈ 0.15–0.25). Legislative majorities and electoral voters bear the cost of veto and gridlock; their directionality is near the target end (d ≈ 0.75–0.85). Legal professionals benefit from specialization but do not set the constraint themselves — their secondary role as beneficiary gives them d ≈ 0.35. Minority rights advocates sit near symmetric benefit: they benefit from judicial protection, but they also depend on the underlying democratic legitimacy that courts claim to constrain — if courts become fully delegitimized, their protection vanishes — so their d is near 0.45–0.55. The analytical observer has analytical exit and no structural stake; d = 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint risks mandatrophy if the founding problem (preventing majorities from treating ordinary law as constitutional law) becomes obsolete while the constraint persists. The founding_problem_status is authored as 'contested' to capture this: courts assert the problem is live (majorities still need checking), while parliamentary sovereigntists assert it is dead (legislatures can be trusted with interpretive authority). The measurements show extractiveness rising then plateauing — not a sign of deadness, but of maturation of the institutional capture: courts established their authority, now maintain it at a steady level. If courts began expanding their reach further (extractiveness rising in the 40–60 interval), or if theater_ratio surged (indicating mostly performance), mandatrophy would sharpen. As authored, the constraint sits at high-extraction equilibrium, not active mandatrophy — but the contested founding_problem_status flags the uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_vs_legitimacy_boundary,
    'Does the judiciary''s specialized legal expertise and apolitical structure actually constitute superior competence in constitutional interpretation, or does it create the appearance of neutrality that masks contested value choices?',
    'Comparative study of judicial decisions across constitutional courts: do independent legal experts identify a coherent, principled methodology that judges follow consistently, or do outcomes cluster around the judges'' political preferences? Analysis of how judges'' prior positions (e.g., as lawyers for particular parties) predict their constitutional rulings.',
    'If expertise is real and neutral, extraction is lower and the constraint genuinely coordinates on principle. If expertise is cover for value imposition, extraction is substantially higher and the constraint is closer to snare — judges impose their preferred constitutional reading while claiming neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_vs_legitimacy_boundary, empirical, 'Whether judicial expertise and apoliticality are real or performative.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (preventing majorities from treating ordinary legislation as constitutional law) still live, or has it become obsolete in modern democracies where legislatures are educated in constitutional law and constitutional protections are multiply entrenched?',
    'Historical case study: count instances in the last 50 years where elected legislatures attempted to override constitutional protections; compare instances where courts had to step in versus instances where constitutional procedures worked without judicial intervention. Survey of legislative behavior in Westminster systems that lack judicial review of legislation.',
    'If the founding problem is dead, the constraint becomes a zombie — persisting authority without current justification, pure extraction. If the problem is live, the constraint is genuinely needed coordination. If contested, the constraint is being used by both sides (courts to justify their veto, legislatures to resist it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether the founding problem that justifies judicial supremacy is still a live issue.').

omega_variable(
    appointment_politics_capture,
    'Does the constraint''s independence from electoral pressure actually reduce political influence on courts, or does it simply displace that influence into the appointment process where presidents and senators effectively select judges for their ideological positions?',
    'Analysis of judicial appointment processes: how much variation in judicial outcomes correlates with the appointing president''s political coalition? Do judicial decisions become more predictable based on appointment history than on legal principle? Comparison of appointment-influenced courts versus courts with mandatory retirement and non-partisan selection.',
    'If appointment politics are weak, courts truly are independent and less extractive. If strong, then the constraint amounts to giving political power to judges appointed for their ideology, with the apolitical framing as cover — extraction rises, and the constraint becomes partly snare (suppression of alternative legislative readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appointment_politics_capture, empirical, 'Whether judicial independence is undermined by appointment politics.').

omega_variable(
    reading_kernel_contest,
    'Is judicial supremacy the correct reading of the constitutional kernel, or do the sibling readings (parliamentary sovereignty or popular constitutionalism) better capture what authority structures actually sustain constitutional meaning?',
    'Cross-national constitutional analysis: which institutional authority structure (courts, parliaments, continuous public contestation) best predicts constitutional stability, protection of rights, and amendment patterns? Genealogical investigation of which reading has actually won over time in different jurisdictions and what forces moved the outcome.',
    'If parliamentary supremacy or popular constitutionalism better capture the actual kernel, then the judicial_supremacy reading is a false summit — a claim that courts hold authority that other institutions actually retain. If judicial supremacy is correct, then the other readings are misdiagnosing which institution actually holds interpretive power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether the judicial_supremacy reading correctly identifies which institutional authority actually holds final interpretive power over constitutional meaning.').

omega_variable(
    access_and_standing_bias,
    'Does judicial review systematically favor certain groups (wealthy, organized, legal-expertise-rich) over others in their ability to challenge legislation as unconstitutional?',
    'Empirical study of Supreme Court amicus participation, legal aid availability, and outcomes by party type: do wealthy institutions, repeat players, and ideological coalitions win at higher rates than individual plaintiffs or poorly-resourced groups? Does the constraint functionally protect minorities or primarily serve those with access to the courts?',
    'If access bias is severe, the constraint redistributes power not to merit or principle but to legal resources — extraction rises, and beneficiaries are not minorities but well-resourced institutional actors (corporations, ideological movements). If access is equalized, the constraint is closer to its coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_and_standing_bias, empirical, 'Whether judicial review actually protects minorities or primarily serves well-resourced institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Judicial supremacy is one reading of the basic_law_interpretive_authority kernel. This reading claims courts hold final interpretive authority; sibling readings give final authority to parliaments or to continuous popular contestation. The ε-invariance principle requires three separate constraint stories because the beneficiary/victim structures differ fundamentally across readings. Judicial supremacy benefits the judiciary and legal professions; parliamentary sovereignty benefits elected legislatures; popular constitutionalism distributes authority across the political community. This story models judicial supremacy only — its measured extraction, suppression, and enforcement profile — without averaging or hedging across the other readings. The network links establish the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
