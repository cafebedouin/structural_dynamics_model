% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Coercion Legitimacy: Public Health Primary Reading
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the PUBLIC_HEALTH_PRIMARY reading of the
 *   coercion_legitimacy_boundary kernel. The reading holds that state
 *   coercion is legitimate when collective harm-prevention outweighs
 *   individual bodily autonomy. Under this reading, unvaccinated individuals
 *   enter the victim set as coerced subjects; immunocompromised populations
 *   exit the victim set as protected beneficiaries. The constraint's
 *   operation is substantially extractive (ε=0.68 at interval end) because
 *   enforcement depends on suppressing alternatives (medical refusal, exit
 *   options) rather than on voluntary coordination. The founding problem
 *   (pandemic mortality, system overwhelm) is contested: authorities attest
 *   it remains live; bodily-autonomy advocates attest it was substantially
 *   solved by supply expansion and social distancing, and that mandates
 *   created backlash that undermined compliance. No external
 *   corroboration—the problem statement itself is reading-indexed.
 *
 * KEY AGENTS:
 *   - state_health_authority: institutional agenda-setter; sets coercion thresholds; collects legitimacy benefit
 *   - immunocompromised_population: powerless beneficiary; trapped at protected status; cannot exit even when mandates become costly
 *   - unvaccinated_individuals: moderate-power payers; face employment loss, social exclusion, forced intervention; constrained exit
 *   - medical_refusers: moderate-power payers; identity-locked refusers; face coercion + exclusion from voice in threshold-setting
 *   - public_health_professionals: dual agenda-setter/beneficiary; operate enforcement infrastructure; constrained by politicization
 *   - bodily_autonomy_advocates: excluded; argue coercion is categorically impermissible; not seated at threshold-setting
 *   - proportionality_advocates: excluded; argue mandates should scale with disease severity; framework not adopted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.76).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Coercion Legitimacy: Public Health Primary Reading").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '62b87181-6416-4082-aaa7-2499e9be276a').
narrative_ontology:cs_kernel_codification('62b87181-6416-4082-aaa7-2499e9be276a', formalized).
narrative_ontology:cs_authority_grounding('62b87181-6416-4082-aaa7-2499e9be276a', extraction).
narrative_ontology:cs_interpretation_layer_present('62b87181-6416-4082-aaa7-2499e9be276a').
narrative_ontology:cs_reading_relation('62b87181-6416-4082-aaa7-2499e9be276a', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('62b87181-6416-4082-aaa7-2499e9be276a', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('62b87181-6416-4082-aaa7-2499e9be276a', foundational, epidemiological_benefit_overrides_autonomy).
narrative_ontology:cs_axiom_status(epidemiological_benefit_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('62b87181-6416-4082-aaa7-2499e9be276a', epidemiological_benefit_overrides_autonomy, instrumental).
narrative_ontology:cs_axiom('62b87181-6416-4082-aaa7-2499e9be276a', secondary, state_may_calculate_collective_harm_thresholds).
narrative_ontology:cs_axiom_status(state_may_calculate_collective_harm_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('62b87181-6416-4082-aaa7-2499e9be276a', state_may_calculate_collective_harm_thresholds, conventional).
narrative_ontology:cs_reference_frame('62b87181-6416-4082-aaa7-2499e9be276a', epidemiological_harm_minimization).
narrative_ontology:cs_drift_state('62b87181-6416-4082-aaa7-2499e9be276a', post_acute_pandemic_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62b87181-6416-4082-aaa7-2499e9be276a', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_apparatus).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, medical_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces medical intervention mandates (vaccination requirements, quarantine orders, isolation protocols) when epidemiological modeling indicates collective harm exceeds individual autonomy costs. Justifies coercion as public health necessity. Sets the threshold for triggering enforcement (R₀ > threshold, hospitalization rate > ceiling, etc.). Bears the legitimacy cost of coercion but collects the population-level protection benefit.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated safely or tolerate infection risk; depend entirely on collective immunity (others vaccinated/protected) for survival. Receive protection from mandates without bearing coercive costs themselves. Their protected status is the normative justification for the mandate but also the mechanism that makes enforcement asymmetric — the protected cannot exit even if mandates become costly, because protection is their only option.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Face medical coercion (vaccination mandates, exclusion from schools/employment/public spaces, quarantine orders) justified by collective harm prevention. Cannot refuse without bearing substantial costs (employment loss, social exclusion, legal penalties, or forced medical intervention). Their refusal framed as imposing externality justifies the enforcement structure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Individuals who refuse medical intervention on grounds of conscience, religious belief, or bodily autonomy principles. Face not only coercion but also denials of voice in the mandate-setting process — their principled objection is treated as epidemiological misinformation rather than a legitimate position on the state's coercive authority. Exit requires abandoning identity commitments (religious faith, autonomy principles) or geographic relocation.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medical_refusers, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, medical_refusers, excluded).

% Listed as non-agent vindicated proposition: the reading vindicates epidemiological risk-quantification methods and the legitimacy of using R₀, hospitalization forecasts, and transmission dynamics to set policy thresholds. The constraint's operation ratifies disease modeling as a valid ground for state authority over bodily autonomy.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, epidemiological_science, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(coercion_legitimacy_boundary__public_health_primary, epidemiological_science).

% Argue that medical intervention without consent is categorically impermissible and that collective benefit never justifies coercion. Excluded from mandate-setting authority; their position is framed as naive about collective action rather than as a legitimate competing principle on state authority limits. No seat at the deliberation table where thresholds are set.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Argue for calibration of coercion legitimacy to disease severity, transmissibility, and treatment efficacy — mandates justified for measles (high R₀, severe outcomes) but not flu (low severity-to-mortality ratio). Excluded from the reading that prevails in this constraint's operation; their threshold-setting framework is not adopted.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, proportionality_reading_advocates, excluded,
    organized, generational, constrained, national).

% Operate the enforcement and surveillance infrastructure (contact tracing, immunization registries, quarantine enforcement). Benefit from expanded authority and resources justified by emergency/collective-harm framing. Simultaneously constrained by politicization: when coercion becomes unpopular, their professional recommendations face institutional capture pressure and lose credibility.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_professionals, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, public_health_professionals, agenda_setter).

% Records the structural divergence: immunocompromised beneficiaries cannot consent to or refuse the protection mandates provide (trapped at the beneficiary seat); unvaccinated victims can refuse but face escalating costs (employment loss, social exclusion, forced intervention); medical refusers face additional identity-lock suppression. The constraint's operation creates asymmetric burdens that cannot be dissolved even by agreement.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, state_health_authority).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of insufficient vaccination coverage: individuals have incentive to free-ride (receive herd immunity protection without bearing vaccination cost); mandates internalize the positive externality by making vaccination a binding condition of participation in protected spaces (schools, healthcare, employment). Coordinates the immunized population against the disease vector.
% TRANSFER_FUNCTION: Moves the burden of medical coercion from the immunocompromised (who cannot be vaccinated safely and would bear the cost of infection) to the unvaccinated (who bear the cost of forced vaccination, social exclusion, and identity suppression). The constraint transfers bodily autonomy from the unvaccinated to the immunocompromised, who use it as a survival condition.
% ABSENT_VOICES: Bodily autonomy advocates who hold that coercion is categorically impermissible regardless of collective benefit. Proportionality advocates who argue mandates should scale with disease severity and transmission dynamics. Medical refusers whose principled objection is excluded from the threshold-setting conversation and treated as epidemiological noise rather than a position on state authority. Their absence is structural: the reading that mandates require only collective-benefit justification has already excluded the frameworks within which their objections would make sense.
% DISAPPEARANCE_RATIONALE: If the reading's enforcement apparatus vanished overnight, unvaccinated individuals would no longer face workplace exclusion or quarantine orders. Vaccination rates would decline, particularly among identity-locked refusers. Immunocompromised population would face higher infection risk unless alternative protection strategies (universal masking, isolation, therapeutic monitoring) filled the gap. The public health apparatus would lose institutional authority over bodily autonomy as a valid harm-prevention tool. State legitimacy to mandate medical intervention would require explicit re-negotiation in each new context.
% FOUNDING_PROBLEM: Early in the 2019 coronavirus pandemic, vaccination coverage fell short of herd-immunity thresholds in many regions; transmission continued to pose severe risk to immunocompromised individuals who could not be vaccinated and to overwhelm hospital capacity. The state lacked sufficient voluntary compliance mechanisms to achieve protection. Collective harm (death of immunocompromised, healthcare system collapse) appeared to outweigh individual cost of mandated vaccination.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest the founding problem was live and severe during 2020–2022 (hospital capacity constraints, immunocompromised mortality risk documented in medical literature). Bodily-autonomy advocates attest that vaccination coverage was substantially determined by supply constraints and hesitancy factors independent of mandate efficacy, and that coercion created backlash that reduced subsequent compliance. Independent public-health economists and ethicists dispute both the quantification of collective harm and the causal attribution of protection to mandates specifically. No consensus corroboration outside the benefiting parties (state health authority, public health professionals); corroboration splits along the reading divide itself.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.70 over the interval (t=0 to t=24) as the enforcement apparatus operationalizes: initially (t=0–8) mandates are suggested/requested; by t=16 they are enforced (employment exclusion, school denial); by t=24 they peak (forced quarantine, threatened forced intervention). The trajectory then stabilizes and slightly declines (t=32–48) as public resistance hardens and enforcement capacity erodes. Suppression tracks extraction closely but more steeply: it reaches 0.79 at t=24 (maximum enforcement intensity) because the constraint persists not by voluntary participation but by active suppression of refusal options. Theater_ratio shows a different pattern: early (t=0–16) the reading's framing dominates ('protect the vulnerable'); by t=24 performative elements are visible (propaganda, social-media campaigns treating dissent as dangerous misinformation); t=32+ theater stabilizes as enforcement becomes normalized theater—the coercion is still active but no longer justified as emergency, now as routine public health. The temporal grid is shared: every metric is authored at every time point so the engine can sample all metrics at all examined times without grid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The state_health_authority seat and the unvaccinated_individuals seat compute divergent types from identical structural data because directionality is opposite: the authority collects legitimacy and population-level benefit (d near 0.0, beneficiary); unvaccinated individuals bear coercion costs and lose autonomy options (d near 1.0, target). From the authority's seat the constraint is rope + tangled (genuine coordination problem solved, plus asymmetric extraction from non-compliers). From the payer's seat it is snare (coercion defended by invoking coordination, but the coordination would be achievable through lower-coercion mechanisms—education, supply provision, voluntary prioritization—had those been tried first). Medical refusers compute further toward snare because they bear both extraction AND identity suppression: their principled objection is treated as noise rather than a position, which amplifies suppression beyond the baseline. Immunocompromised beneficiaries compute as rope/scaffold from their seat: the constraint provides essential protection they cannot produce themselves; the extraction is paid by others. The engine computes these per-seat types from beneficiary/victim declarations + power + exit_options + directionality derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (state_health_authority, immunocompromised_population, public_health_professionals): low d (near 0.0–0.3). The authority sets the rules and collects institutional authority expansion; immunocompromised population cannot refuse protection (trapped exit) and gains survival benefit; public-health professionals gain expanded mandate and resources. Victim directionality (unvaccinated_individuals, medical_refusers): high d (near 0.7–1.0). Both face coercion, employment/school exclusion, and identity suppression. Medical refusers additionally lose voice in the process that determines their coercion—identity_locked exit plus excluded role creates higher d than baseline unvaccinated (d approx 0.80 vs 0.70). Excluded advocates (bodily_autonomy, proportionality) carry d near 0.5 (symmetric/observer): they bear no direct extraction but are structurally prevented from participating in threshold-setting, which could be read as asymmetric suppression of their framework. No directionality overrides needed: the derivation from beneficiary/victim + exit + power produces the observed seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims type tangled_rope (genuine coordination problem + asymmetric extraction). Mandatrophy resolves here by distinguishing the coordination function from its enforcement mechanism. (1) Genuine coordination: insufficient voluntary vaccination coverage does create a collective-action problem, and mandates do internalize the immunocompromised's positive externality. (2) Extraction asymmetry: enforcement persists even when alternatives become available—supply expansion, improved vaccine accessibility, social distancing—that could achieve coordination with lower coercion. The theater_ratio rise (0.22 to 0.43 to 0.41) indicates that by t=24+ a growing share of enforcement activity is performative (propaganda, threat), not functional problem-solving. (3) Mandatrophy resolution: the constraint is not a snare (pure extraction disguised as coordination) because the coordination problem is genuine. It is not a rope (pure coordination) because suppression is necessary to maintain it, not because it's naturally aligned with voluntary preference. It is tangled_rope because both elements operate: real coordination problem + real extraction from the identity-locked and excluded seats. The constraint would resolve toward rope if the reading shifted to a proportionality frame (mandate measles, not flu, given R₀ and outcomes), because proportionality-mandated constraints would be narrower, enforcement would be lower, and extracted costs would decline. It would resolve toward snare if the coordinated seats (vaccine-accepting population) were discovered to benefit independently of the coercion (i.e., if mandates were revealed as unnecessary for the coordination)—but the immunocompromised's dependence on herd immunity is not contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Was pandemic-era vaccination coverage shortfall a genuine collective-action problem requiring mandates, or was it substantially driven by supply constraints and voluntary hesitancy factors that supply expansion and education could have solved without coercion?',
    'Counterfactual analysis: jurisdictions that relied on supply expansion + education (no mandates) and jurisdictions with mandates achieve comparable coverage over similar timeframes; or empirical studies isolating the causal effect of mandates vs. supply on uptake after controlling for hesitancy.',
    'If supply-driven: the founding problem dissolves and the constraint becomes snare (coercion defended by false necessity). If mandate-driven: the founding problem is validated and the constraint remains tangled_rope (genuine coordination problem requiring enforcement). The classification divergence hinges entirely on this empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether vaccination shortfall was a collective-action problem or a solvable supply/education problem.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.76 at t=24–48) structural (external employment loss, social exclusion) or internalized (individuals internalize the shame of refusal, self-isolate from public discussion, suppress their own objections)?',
    'Post-mandate suppression trajectory: after mandates are lifted or selectively enforced, do unvaccinated individuals reappear in public discourse and professional contexts, or do they remain suppressed? If they reappear, suppression was structural; if suppressed absence persists, suppression is internalized.',
    'If structural: the constraint''s suppression is lower than measured (external barriers can be removed). If internalized: the suppression is higher than measured and persists even after mandate removal. Identity-locked refusers are more likely to show internalized suppression (belief that objection is shameful or dangerous), which would increase effective suppression above the baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression of medical refusal.').

omega_variable(
    alternative_thresholds_reading_contest,
    'Would adoption of the PROPORTIONALITY_READING (mandates scale with disease severity) have produced a structurally different constraint, or would it have simply shifted the threshold without changing the extraction/suppression profile?',
    'Comparative institutional analysis: jurisdictions adopting proportionality thresholds (e.g., mandates for measles R₀>12, not flu) show lower average enforcement intensity and suppression than jurisdictions adopting the PUBLIC_HEALTH_PRIMARY reading (epidemiological benefit justifies coercion regardless of severity). Measure average suppression, enforcement frequency, and theater_ratio across the two classes.',
    'If proportionality reduces extraction/suppression: the PUBLIC_HEALTH_PRIMARY reading is responsible for material harm above the baseline of collective-action coordination; policy pivot toward proportionality would lower ε and suppression. If proportionality does not reduce extraction: all three readings produce similar enforcement profiles, meaning the reading distinction is rhetorical rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_thresholds_reading_contest, conceptual, 'Whether PROPORTIONALITY_READING is a distinct constraint or a rhetorical variant of PUBLIC_HEALTH_PRIMARY.').

omega_variable(
    immunocompromised_autonomy_paradox,
    'Is the immunocompromised population''s ''protection'' via collective mandates actually a constraint on their autonomy (they cannot choose to exit protection or modify its enforcement) rather than a benefit? Are they beneficiaries or trapped payers?',
    'Qualitative research on immunocompromised experience: do they experience protection as autonomy-enhancing (enabling participation they could not otherwise afford) or as paternalistic (decisions made for them without their input on enforcement intensity or alternative protection strategies)? Structural test: do they have voice in mandate-setting decisions affecting them?',
    'If protection is experienced as paternalistic/autonomy-restricting: immunocompromised shift toward victim set (beneficiaries become payers); the constraint becomes more extractive as it coerces two populations (unvaccinated + immunocompromised under paternalism) rather than one. Extraction could rise above 0.68. If protection is experienced as enabling: the beneficiary classification holds and the constraint''s asymmetry is justified by genuine dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_autonomy_paradox, empirical, 'Whether immunocompromised protection is autonomy-enabling or autonomy-restricting.').

omega_variable(
    kernel_reading_foreclosure_dynamics,
    'Do the three kernel readings (bodily_autonomy_primary, proportionality, public_health_primary) genuinely coexist across different parties, or does adoption of one reading by a dominant institutional actor foreclose the others through institutional capture and resource starvation?',
    'Track institutional changes over the interval (t=0–48): (a) do bodily-autonomy and proportionality advocates maintain independent research programs, institutional positions, and policy influence, or do they become marginal voices? (b) does institutional capture of public-health authority by the public_health_primary reading reduce funding/credibility for alternative readings? (c) are there moments where a different reading briefly dominates (proportionality surge at t=12–16), and if so, is it later suppressed?',
    'If readings coexist without foreclosure: they are separate constraints with separate classifications. If public_health_primary reading forecloses others through institutional capture: the bodily_autonomy_primary and proportionality_reading constraints may be misclassified as live alternatives when they are actually dormant/foreclosed. The kernel itself becomes contested not in principle but in fact—one reading has captured institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_dynamics, conceptual, 'Whether the three readings coexist as live contestations or whether one forecloses others through institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(coer_tr_t8, observed).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(coer_tr_t16, observed).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(coer_tr_t24, observed).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(coer_tr_t32, observed).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(coer_tr_t40, observed).
narrative_ontology:measurement(coer_tr_t48, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 48, 0.41).
narrative_ontology:measurement_basis(coer_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(coer_be_t8, observed).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(coer_be_t16, observed).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(coer_be_t24, observed).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(coer_be_t32, observed).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(coer_be_t40, observed).
narrative_ontology:measurement(coer_be_t48, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(coer_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(coer_su_t8, observed).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(coer_su_t16, observed).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(coer_su_t24, observed).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 32, 0.77).
narrative_ontology:measurement_basis(coer_su_t32, observed).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.75).
narrative_ontology:measurement_basis(coer_su_t40, observed).
narrative_ontology:measurement(coer_su_t48, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 48, 0.76).
narrative_ontology:measurement_basis(coer_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, pandemic_response_authority_escalation).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, public_health_emergency_declarations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the coercion_legitimacy_boundary kernel. The reading divergence is not empirical (all parties agree on disease parameters) but normative: whether state authority over bodily autonomy should be constrained by categorical principles (bodily_autonomy_primary), proportionality factors (proportionality_reading), or epidemiological calculation (public_health_primary, this constraint). Each reading instantiates a different ε, victim set, and suppression profile. They affect each other through institutional capture: dominance of public_health_primary reading in public-health institutions can foreclose the alternative readings' institutional presence, making them appear dormant rather than coexistent. The constraint family should be analyzed jointly to assess whether institutional dominance has foreclosed genuine contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
