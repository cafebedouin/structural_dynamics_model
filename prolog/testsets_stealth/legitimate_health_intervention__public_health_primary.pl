% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public-Health-Primary Legitimacy Standard for Coercive Health Intervention
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This file instantiates ONE reading — public_health_primary — of the
 *   contested kernel legitimate_health_intervention. The standing arrangement
 *   under contest is the coercive mandate architecture: school-entry and
 *   occupational vaccination requirements, testing-or-termination policies,
 *   and venue access restrictions, all legitimized by a single criterion —
 *   measurable reduction in population-level morbidity and mortality. Under
 *   this reading, individual refusal is not a self-regarding choice but
 *   externality imposition on third parties, which converts refusal into a
 *   proper object of state and institutional coercion. The epsilon authored
 *   here refers to that standing arrangement as this reading itself assesses
 *   it: even by its own lights, the arrangement extracts severely from
 *   refusers (employment termination, exclusion from venues and schools)
 *   while subsidizing the immunologically vulnerable with protection they
 *   cannot purchase. Sibling readings (bodily_autonomy_primary,
 *   proportionality_reading) are separate constraints with their own epsilon
 *   values and victim sets; they are linked through the network, not folded
 *   into this story.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/constrained) — defines the outcome metric, administers mandates and exemption review, collects authority, surveillance budgets, and legitimacy capital
 *   - immunocompromised_patients: primary beneficiary (powerless/trapped) — subsidized by others' compliance; cannot take the intervention effectively and cannot exit shared air
 *   - elderly_high_risk_populations: beneficiary (organized/constrained) — high mortality risk, partial ability to isolate, politically organized
 *   - hospital_systems: beneficiary (institutional/constrained) — avoids surge crises and nosocomial outbreaks; bears implementation costs
 *   - unvaccinated_employees: primary target (moderate/trapped) — bears termination, loss of insurance and tenure; sector-wide mandates shrink exit
 *   - conscientious_refusers: target (moderate/identity_locked) — refusal constitutive of religious or philosophical identity; the comply-and-move-on exit is unavailable at any price
 *   - vaccine_injury_claimants: excluded voice (powerless/trapped) — bear iatrogenic harm outside the legitimizing metric; compensation channels backlogged and adversarial
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicate police-power versus bodily-integrity challenges; reconfigure enforcement without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.74).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.74).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public-Health-Primary Legitimacy Standard for Coercive Health Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d').
narrative_ontology:cs_kernel_codification('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', distributed).
narrative_ontology:cs_authority_grounding('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', expertise).
narrative_ontology:cs_interpretation_layer_present('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d').
narrative_ontology:cs_reading_relation('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', foundational, population_outcome_sufficiency).
narrative_ontology:cs_axiom_status(population_outcome_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', population_outcome_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', foundational, refusal_is_externality_imposition).
narrative_ontology:cs_axiom_status(refusal_is_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', refusal_is_externality_imposition, empirically_contingent).
narrative_ontology:cs_reference_frame('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', population_outcome_legitimacy_baseline).
narrative_ontology:cs_drift_state('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', contemporary_post_mandate_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ea9c685-6fe9-4d9f-a30d-f8790a49dc7d', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, elderly_high_risk_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, hospital_systems).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_employees).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, conscientious_refusers).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, population_outcome_utilitarianism).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, externality_imposition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the outcome metrics that legitimize intervention, recommend and impose mandates, and operate exemption-review processes. Administering the arrangement expands their authority, surveillance budgets, and standing; exiting would mean career and institutional ruin inside the systems they lead. They experience the constraint as the coordination mechanism they built and defend.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Cannot mount protective immunity from vaccination themselves and depend on community coverage to lower their infection risk. They have no purchasable substitute for others' compliance and no exit from shared air, workplaces, and hospitals. Everything the arrangement delivers flows to them; nothing they do maintains it.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Face elevated mortality from circulating respiratory disease and benefit from reduced transmission. They can partially isolate, relocate, or advocate politically, but cannot fully escape community exposure. Their advocacy organizations lobby for the mandate architecture that protects them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, elderly_high_risk_populations, beneficiary,
    organized, biographical, constrained, national).

% Avoid surge-capacity crises and nosocomial outbreaks when staff and patient populations are highly covered. They bear screening and compliance-administration costs and cannot exit licensure obligations or their duty to admit the infected. Staff mandates are among the tools they administer.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, hospital_systems, beneficiary,
    institutional, generational, constrained, regional).

% Face testing-or-termination policies and lose employment, health insurance, and tenure for refusal. Complying trades bodily integrity they did not consent to surrender; refusing costs their livelihood now. Finding an exempt employer or going self-employed shrinks as mandates generalize across sectors, so the exit narrows faster than they can move through it.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_employees, payer,
    moderate, immediate, trapped, national).

% Refusal is fused with religious or philosophical identity: compliance would betray constitutive commitments, so the standard exit — take the injection and move on — is unavailable to them at any price. They accept exclusion from venues, schools, and professions, absorb social ostracism, and litigate or organize instead. Their costs persist regardless of what the arrangement delivers to anyone else.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, conscientious_refusers, payer,
    moderate, biographical, identity_locked, national).

% Bear iatrogenic harm from the same interventions the arrangement mandates. Compensation programs are backlogged and adversarial, and their injuries sit outside the population-outcome metric that legitimizes the whole structure. They would object that the legitimizing accounting discounts their harms; they hold no seat in the administrative process that declares legitimacy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_injury_claimants, excluded,
    powerless, biographical, trapped, national).

% Adjudicate challenges pitting police power and institutional duty against bodily integrity and free exercise. They take testimony from every other seat, commission none of the enforcement, and collect none of its gains; their rulings reconfigure what enforcement is possible without themselves paying its costs.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: no individual can purchase herd protection alone, voluntary uptake undershoots the coverage threshold, and centralized mandates align millions of private decisions with a population-level coverage target.
% TRANSFER_FUNCTION: Moves bodily compliance and employment or venue access from refusers to the enforcing state and covered institutions; moves infection risk away from the immunologically vulnerable and onto those who decline; moves enforcement costs — termination, exclusion, litigation — onto refusers and their households.
% ABSENT_VOICES: Vaccine-injured claimants whose harms fall outside the legitimizing metric, dissenting medical ethicists who reject outcome-sufficiency as a legitimacy criterion, and workers terminated without individualized hearing. They stand outside the administrative process: legitimacy is declared by agencies measuring population outcomes, and no seat inside that process represents iatrogenic harm or consent-based objection.
% DISAPPEARANCE_RATIONALE: Employer mandates, school exclusions, and access restrictions would lapse overnight; coverage would drift toward the voluntary equilibrium; the immunocompromised and elderly would lose protection they cannot purchase elsewhere; agencies would lose the authority and surveillance infrastructure built on administering the arrangement; and the litigation doctrine balancing police power against bodily integrity would revert to open questions. Arrangements across employment, education, and healthcare reorganize around the constraint's absence.
% FOUNDING_PROBLEM: Recurrent epidemic mortality — smallpox, polio, influenza, COVID-19 — together with the free-rider collapse of voluntary vaccination below herd-protective thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historical mortality registries and actuarial excess-death series document the epidemics; independent serosurveillance and WHO reporting document continuing transmission threats; and courts hostile to compelled intervention have nonetheless conceded in the Jacobson line that the underlying disease threat is real. The founding problem's liveness does not rest on agency testimony.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the sanction set — termination, school exclusion, venue denial — falls entirely on a minority whose compliance the majority receives as protection; this reading deems the imposition justified, but justified imposition is still imposition, and the reading-indexed epsilon over the fixed referent remains high. Suppression (0.78) is a raw structural property, unscaled by power or scope: exemption routes were deliberately narrowed over the interval (personal-belief exemptions eliminated, mandates generalized across sectors), and persistence depends on actively closing exits rather than on participant preference. Theater ratio (0.30) is low-to-moderate: the protective function is real and measurable, though performative compliance artifacts (blanket policies indifferent to naturally immune or recently infected staff, dashboard rituals) grew during the COVID-era intensification. Accessibility collapse (0.50) is mid-range: exits exist (jurisdictional arbitrage, remote work, medical exemptions) but narrowed materially. Resistance (0.68) is high: litigation waves, political repeal movements, and organized refusal. The measurement series run on one shared seven-point grid with all three tracked metrics authored at every point; the steepening between T16 and T24 corresponds to the pandemic mandate wave. The claimed type (tangled_rope) reflects my structural judgment that both halves are real — genuine herd-protection coordination AND asymmetric extraction flowing through the same enforcement machinery; the metrics are authored independently as descriptive estimates, and the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structure. From the agency seat the arrangement is a functioning coordination mechanism it built, measures, and defends — rope-like. From the trapped employee seat the same machinery is extraction with no offsetting benefit — snare-like. From the immunocompromised seat it is a lifeline whose dependence on strangers' compliance is invisible from the administrative center. From the court seat it is a balancing problem between police power and bodily integrity. The divergence is computed by the engine from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: immunocompromised patients (trapped, powerless) sit nearest the full-beneficiary end — the constraint subsidizes them with protection they cannot buy. Elderly high-risk populations and hospital systems sit slightly higher but remain net beneficiaries. Public health agencies collect authority, surveillance budgets, and legitimacy capital from administering the arrangement — a real captured gain recorded on the receipt surface — though their derived d stays near the beneficiary end. Victim declarations drive high d: unvaccinated employees (trapped) sit near the full-target end; conscientious refusers (identity_locked) sit at the extreme, because identity fusion removes the comply-and-move-on exit and effective extraction concentrates hardest on them. National spatial scope modestly amplifies effective extraction by raising verification difficulty. Only extractiveness is scaled by the engine; suppression enters as authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrent epidemic mortality and the free-rider collapse of voluntary coverage below herd-protective thresholds — remains live, so no mandatrophy resolution is declared. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds coherence: no zombie flag. The tangled_rope classification prevents mislabeling in both directions: reading the arrangement as pure rope hides the terminated workers and identity-locked refusers whose extraction funds the coordination; reading it as pure snare erases the measurable mortality reduction that no voluntary equilibrium delivered. Per-seat computation keeps both facts visible — who experiences the coordination and who experiences the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (public_health_primary) of the kernel legitimate_health_intervention; what would adoption of a sibling reading change structurally?',
    'Comparative classification across the sibling files: adopt bodily_autonomy_primary and the same enforcement machinery loses legitimacy outright, moving unvaccinated refusers from victim set to rights-holder set; adopt proportionality_reading and enforcement becomes conditional on disease-severity thresholds, making epsilon vary with threat level.',
    'Under bodily_autonomy_primary the identical arrangement computes as pure extraction of bodily liberty; under proportionality_reading epsilon becomes threat-indexed rather than fixed. This file''s classification is valid only within this reading''s framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one-of-three readings; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    externality_premise_pathogen_dependence,
    'Is the externality premise empirically robust across pathogens and vaccine profiles, or does it hold only for transmission-blocking (sterilizing) immunity?',
    'Pathogen-by-pathogen transmission modeling: measure whether vaccinated-and-infected individuals transmit at materially reduced rates under the deployed products.',
    'For sterilizing vaccines the refusal-as-externality foundation strengthens and the coordination half deepens; for leaky products that reduce severity but not transmission, the externality premise weakens, the victim framing of refusers erodes, and epsilon''s justification narrows toward self-protection rationales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_premise_pathogen_dependence, empirical, 'Whether refusal imposes third-party externalities is product- and pathogen-dependent.').

omega_variable(
    outcome_denominator_selection,
    'Which morbidity and mortality count toward the legitimizing metric — disease-specific outcomes only, or all-cause burdens including iatrogenic harm and displacement effects?',
    'Standardized outcome accounting with pre-registered denominators spanning disease-specific deaths, adverse-event registries, and all-cause excess mortality.',
    'A narrow denominator inflates the measured benefit that legitimizes coercion; a broad denominator that counts vaccine-injury claimants'' harms raises epsilon and pushes the arrangement toward the snare boundary of the tangled-rope band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcome_denominator_selection, conceptual, 'The legitimacy metric''s denominator is chosen, and the choice moves epsilon.').

omega_variable(
    herd_protection_delivery_gap,
    'Does the arrangement actually deliver herd-level protection to its declared beneficiaries, or do waning immunity and imperfect transmission blocking leave the immunocompromised short of the protection the framing promises?',
    'Longitudinal serosurveillance correlated with breakthrough-infection rates among the cannot-vaccinate population.',
    'If delivery falls short, the beneficiaries'' directionalities drift toward symmetric, the coordination half of the tangled rope thins, and the arrangement migrates toward pure extraction sustained by enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_protection_delivery_gap, empirical, 'Whether the coordination function pays its beneficiaries what the externality framing promises them.').

omega_variable(
    enforcement_backlash_equilibrium,
    'Will the rising suppression trajectory continue, or does political repudiation (mandate bans, passport prohibitions, exemption expansion) cap enforcement intensity at a new equilibrium?',
    'Longitudinal tracking of mandate adoption, repeal, and preemption legislation across jurisdictions.',
    'Continued intensification dates a further drift toward snare-flavored operation; a durable cap suggests stabilization inside the tangled-rope band with jurisdictional fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_backlash_equilibrium, empirical, 'Whether the enforcement ratchet or the backlash sets the long-run suppression level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__public_health_primary, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(legi_tr_t4, observed).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__public_health_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(legi_tr_t8, observed).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__public_health_primary, theater_ratio, 16, 0.23).
narrative_ontology:measurement_basis(legi_tr_t16, observed).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(legi_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__public_health_primary, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(legi_be_t4, observed).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__public_health_primary, base_extractiveness, 8, 0.57).
narrative_ontology:measurement_basis(legi_be_t8, observed).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__public_health_primary, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(legi_be_t16, observed).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.74).
narrative_ontology:measurement_basis(legi_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__public_health_primary, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(legi_su_t4, observed).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__public_health_primary, suppression_requirement, 8, 0.53).
narrative_ontology:measurement_basis(legi_su_t8, observed).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__public_health_primary, suppression_requirement, 16, 0.6).
narrative_ontology:measurement_basis(legi_su_t16, observed).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(legi_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'vaccine mandate legitimacy' conflates three structurally distinct constraints — one per reading of the legitimate_health_intervention kernel. Each sibling file carries its own epsilon, beneficiary/victim structure, and claimed type; this file links to both siblings via network.affects_constraints. Direction of influence: this reading's evidentiary apparatus (surveillance systems, seroprevalence data, excess-death series) supplies the parameters the proportionality_reading weighs, creating structural downstream pressure without foreclosure; against bodily_autonomy_primary the two legitimacy criteria are mutually contradictory within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
