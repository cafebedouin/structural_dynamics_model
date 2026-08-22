% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Authority to Compel Medical Intervention for Collective Harm Prevention
 *   domain: public_health_policy/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the
 *   coercion-legitimacy-boundary kernel: the state may compel medical
 *   intervention whenever collective harm-prevention outweighs individual
 *   autonomy, without the case-by-case proportionality calibration the
 *   sibling reading demands or the categorical prohibition the
 *   bodily-autonomy reading asserts. Under this reading's own lights,
 *   unvaccinated individuals and objectors are the coerced subjects (victims)
 *   and the immunocompromised/unvaccinable populations who depend on herd
 *   immunity are the protected beneficiaries — this is the reading's
 *   characteristic beneficiary/victim split, structurally distinct from the
 *   sibling readings which draw the line differently (bodily-autonomy-primary
 *   would have no victims-of-coercion category at all because it denies the
 *   compulsion's legitimacy outright; the proportionality reading would split
 *   the victim set by disease severity rather than by vaccination status).
 *   The apparatus that enforces this reading — mandate statutes, exemption
 *   bureaucracies, exclusion penalties — is real, active, and has grown in
 *   scope and enforcement intensity over the tracked interval, which grounds
 *   the elevated ε and suppression scores.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter/beneficiary (institutional/analytical) — designs and enforces compulsion, gains institutional legitimacy from demonstrated capacity
 *   - unvaccinated_individuals: primary payer (powerless/trapped) — bears direct coercive cost regardless of individualized risk
 *   - religious_and_philosophical_objectors: payer (powerless/constrained) — narrowing exemption pathways
 *   - immunocompromised_populations: beneficiary (powerless/trapped) — depends entirely on others' compelled compliance
 *   - herd_immunity_dependent_infants: beneficiary (powerless/trapped) — too young to consent or dissent, invoked as justification
 *   - vaccine_injury_susceptible_minorities: payer (powerless/trapped) — individualized risk poorly served by categorical rule
 *   - courts_and_legislatures: observer/agenda_setter (institutional/analytical) — adjudicates the kernel contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.71).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Authority to Compel Medical Intervention for Collective Harm Prevention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'b9fb3aae-cc09-4869-a9e0-339180469f27').
narrative_ontology:cs_kernel_codification('b9fb3aae-cc09-4869-a9e0-339180469f27', distributed).
narrative_ontology:cs_authority_grounding('b9fb3aae-cc09-4869-a9e0-339180469f27', distributed).
narrative_ontology:cs_reading_relation('b9fb3aae-cc09-4869-a9e0-339180469f27', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b9fb3aae-cc09-4869-a9e0-339180469f27', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('b9fb3aae-cc09-4869-a9e0-339180469f27', foundational, collective_harm_prevention_can_outweigh_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_prevention_can_outweigh_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('b9fb3aae-cc09-4869-a9e0-339180469f27', collective_harm_prevention_can_outweigh_individual_autonomy, instrumental).
narrative_ontology:cs_axiom('b9fb3aae-cc09-4869-a9e0-339180469f27', secondary, categorical_compulsion_threshold_independent_of_severity_calibration).
narrative_ontology:cs_axiom_status(categorical_compulsion_threshold_independent_of_severity_calibration, holdable).
narrative_ontology:cs_axiom_grounding('b9fb3aae-cc09-4869-a9e0-339180469f27', categorical_compulsion_threshold_independent_of_severity_calibration, conventional).
narrative_ontology:cs_reference_frame('b9fb3aae-cc09-4869-a9e0-339180469f27', police_power_epidemic_threshold_doctrine).
narrative_ontology:cs_drift_state('b9fb3aae-cc09-4869-a9e0-339180469f27', contemporary_post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9fb3aae-cc09-4869-a9e0-339180469f27', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, herd_immunity_dependent_infants).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccine_injury_susceptible_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccination mandates, school-entry requirements, quarantine orders, and workplace intervention rules, invoking epidemiological modeling and herd-immunity thresholds as justification. Administers exemption processes, penalties, and compliance tracking. Its institutional legitimacy and funding are partly built on the demonstrated capacity to compel compliance during outbreaks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, public_health_agencies, beneficiary).

% Face exclusion from schools, workplaces, and public accommodations, fines, or direct compulsion depending on jurisdiction, regardless of their individualized risk calculus or reasons for refusal. Exit requires relocation to a jurisdiction with different mandate regimes, litigation, or acceptance of exclusion from major social institutions — none of which is readily available to most.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Hold sincerely held objections to compelled intervention that the mandate apparatus treats as administratively inconvenient rather than as a competing legitimacy claim. Exemption pathways exist in some jurisdictions but are narrowing over time, and asserting them carries reputational and access costs.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, constrained, national).

% Cannot be vaccinated themselves or receive reduced protection from vaccination, and depend entirely on the surrounding population's compliance to reduce their exposure risk. Under this reading, the coercive apparatus applied to others is what makes their participation in ordinary public life possible.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Too young to be vaccinated against certain diseases and protected only by population-level compliance among those around them. Have no voice in the mandate debate but are named as the population whose vulnerability grounds the compulsion argument.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, herd_immunity_dependent_infants, beneficiary,
    powerless, biographical, trapped, national).

% A small subpopulation with elevated individual risk of adverse reaction, for whom the compulsion apparatus's individualized exemption process is frequently slow, bureaucratic, or unresponsive to their specific risk profile. Bear the cost of a categorical rule that is calibrated to population-level harm reduction, not individual risk.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_injury_susceptible_minorities, payer,
    powerless, biographical, trapped, national).

% Adjudicate challenges to compulsion authority, set the constitutional boundary of state police power over the body, and can narrow or expand the enforcement apparatus through ruling and statute. Their doctrine is the site where this reading's premise is tested against the sibling readings.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, courts_and_legislatures, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity to prevent disease transmission that would otherwise harm people who cannot protect themselves individually — a genuine collective-action problem where individual non-participation imposes costs on others that market or voluntary mechanisms under-provide against.
% TRANSFER_FUNCTION: Moves bodily autonomy and decisional authority from the individual to the state, and moves epidemiological risk away from immunocompromised and unvaccinable populations onto individuals compelled to accept intervention against their own risk calculus or convictions.
% ABSENT_VOICES: Individuals with idiosyncratic medical circumstances, religious convictions, or documented adverse-reaction histories rarely have a forum calibrated to their specific case; the mandate apparatus is built for population thresholds, not individualized adjudication, so their objections are heard, if at all, only through slow or narrow exemption channels.
% DISAPPEARANCE_RATIONALE: If state compulsion authority vanished overnight, vaccination rates in some populations would likely fall below herd-immunity thresholds, immunocompromised individuals and unvaccinable infants would lose a structural protection they currently depend on, and disease outbreaks would reallocate risk back onto vulnerable populations who have no other collective mechanism to secure it — the arrangement is load-bearing for those beneficiaries specifically.
% FOUNDING_PROBLEM: Contagious disease outbreaks (smallpox, polio) demonstrated that voluntary individual choice under-produces population immunity, leaving medically vulnerable people exposed to risks they cannot mitigate through their own action, and historical outbreaks caused mass mortality that voluntary compliance alone failed to prevent.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and immunocompromised-advocacy organizations attest the founding problem remains live, citing measles and pertussis resurgence data. Civil liberties organizations and a minority of constitutional scholars, outside the beneficiary set, attest that the modern apparatus has expanded well past the original epidemic-threshold justification into routine administrative compulsion with declining proportionality review — corroboration exists on both sides of the contest, which is itself the structural fact this reading must be evaluated against.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the compulsion apparatus, under this reading, transfers real decisional authority and bodily control from a powerless, trapped population to the state, with no individualized calibration to the specific objector's risk or belief — the reading treats population-threshold logic as sufficient justification regardless of the individual case. Suppression (0.71) is high because maintaining population-level compliance requires active enforcement machinery: exclusion penalties, tightening exemption criteria, and legal deterrence, and this machinery has intensified over the tracked interval as documented in the suppression_requirement series. Theater ratio is comparatively low (0.22) because the coordination function is largely genuine — the epidemiological mechanism (herd immunity) is real and load-bearing for the beneficiary populations, not primarily performative. Accessibility collapse (0.58) is moderate: exemption pathways nominally exist but have narrowed, so alternatives have not fully collapsed but are meaningfully constrained. Resistance (0.62) is substantial and organized, reflecting active legal and political contestation from objector populations and civil liberties advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the agency's institutional seat, this looks like coordination succeeding at scale — the same mechanism that public health agencies experience as legitimate function is experienced by unvaccinated individuals as coercive extraction with no meaningful individualized appeal. The engine should compute divergent seat classifications from these declared structural positions rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies sit at the beneficiary/agenda-setter end: they design the compulsion, administer its exemptions, and derive institutional legitimacy from it, giving them low d. Unvaccinated individuals, religious/philosophical objectors, and vaccine-injury-susceptible minorities are targets: the reading extracts compliance from them with trapped or constrained exit, giving them high d and correspondingly amplified effective extraction. Immunocompromised populations and herd-immunity-dependent infants are beneficiaries under this reading specifically — this is the structural delta the kernel context specifies: this reading, unlike bodily-autonomy-primary, places these populations in the protected class rather than leaving them exposed. Courts and legislatures occupy an analytical/adjudicative position, testing the reading's boundary against the sibling readings without being extraction targets themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic mortality from under-provided voluntary immunity) is contested as live versus dead: outbreak resurgence data supports 'live' for specific diseases at specific coverage thresholds, but the apparatus has expanded to cover a broader range of interventions and populations than the original epidemic-threshold justification would strictly require, which is the signal this reading's high and rising ε over the interval is meant to surface rather than obscure. The classification does not resolve whether the current scope is proportionate — that is precisely the boundary the proportionality_reading sibling exists to test — but it does prevent this reading's beneficiary structure from being mislabeled as either pure coordination (ignoring the coerced-subject victim set) or pure extraction (ignoring the genuine, load-bearing protection immunocompromised populations receive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the categorical public-health-primary framing (harm-prevention outweighs autonomy without disease-specific calibration) the structurally correct reading of state compulsion authority, or does the proportionality_reading''s severity-scaled framing better describe how courts and agencies actually behave?',
    'Comparative doctrinal analysis of case law and mandate design across disease types (measles vs. seasonal influenza) to determine whether compulsion legitimacy tracks severity/transmissibility (supporting proportionality_reading) or is applied categorically once a collective-benefit threshold is met (supporting this reading).',
    'If courts and agencies in practice calibrate compulsion to disease severity, this reading''s categorical framing overstates the uniformity of the compulsion apparatus and the true operative constraint is better captured by proportionality_reading, with lower ε for low-severity-disease mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether public-health-primary or proportionality framing better describes actual compulsion practice.').

omega_variable(
    beneficiary_victim_boundary_contested,
    'Is the boundary placing immunocompromised populations as beneficiaries and unvaccinated individuals as victims a stable structural fact, or does it depend on contestable empirical claims (vaccine efficacy, herd immunity thresholds, individual risk-benefit ratios) that could shift the boundary?',
    'Track epidemiological consensus and dissenting minority scientific literature on vaccine efficacy and herd immunity thresholds over time; a shift in consensus would relocate populations between the beneficiary and victim sets.',
    'If herd immunity thresholds are empirically lower than assumed, or vaccine efficacy claims are overstated, the beneficiary class shrinks and the extraction imposed on the victim class becomes harder to justify even within this reading''s own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_boundary_contested, empirical, 'Whether the beneficiary/victim split rests on stable or contestable epidemiological premises.').

omega_variable(
    enforcement_apparatus_scope_creep,
    'Has the enforcement apparatus built to address epidemic-threshold diseases expanded to cover interventions and populations beyond what the founding problem would justify, and if so, at what point does this reading''s compulsion authority become mandatrophic?',
    'Longitudinal audit of mandate scope (which diseases, which populations, which settings) against epidemiological necessity thresholds at each point of expansion.',
    'Scope creep beyond epidemic-threshold justification would support reclassifying this reading''s apparatus toward snare for the affected margin, even while the core epidemic-threshold compulsion remains within this reading''s tangled_rope structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_apparatus_scope_creep, empirical, 'Whether enforcement scope has outrun the founding epidemiological justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 16, 0.16).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 32, 0.2).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the coercion_legitimacy_boundary kernel. bodily_autonomy_primary denies the legitimacy of compelled intervention categorically (no victim-of-coercion set exists under that reading because there is no legitimate coercion to begin with). proportionality_reading calibrates legitimacy to disease severity and transmission dynamics, producing a variable ε that rises for high-transmissibility diseases (measles) and falls toward zero for low-severity ones (seasonal flu). This reading (public_health_primary) authors a single, higher, disease-severity-independent ε reflecting a categorical harm-prevention threshold. The three stories share the same underlying kernel text and factual substrate but diverge on where compulsion authority's boundary lies, producing genuinely different beneficiary/victim structures and different ε values — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
