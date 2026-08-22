% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Vaccine Mandate Legitimacy (Proportionality Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   Under the proportionality reading of vaccine mandate legitimacy, mandates
 *   are justified when three conditions are met: (1) the disease poses
 *   serious risk to vulnerable populations, (2) the vaccine's safety and
 *   efficacy profile supports population-level use, and (3) less restrictive
 *   alternatives (testing, isolation, treatment) are insufficient to achieve
 *   necessary immunity thresholds. This reading PERMITS mandates for measles
 *   or polio (high severity, high vaccine efficacy, minimal alternatives)
 *   while NOT permitting them for seasonal influenza (moderate-to-low
 *   severity, uncertain vaccination uptake effect, alternatives exist). The
 *   constraint is CLAIMED as tangled_rope because it coordinates herd
 *   immunity while extracting bodily autonomy costs from vaccine-hesitant
 *   individuals. The measurement series shows rising extractiveness and
 *   suppression in the early interval (years 5–15, corresponding to COVID-19
 *   era mandate expansion), a peak in suppression as legal challenges
 *   mounted, and subsequent stabilization as courts narrowed mandate scope
 *   and the disease severity baseline recalibrated. Theater ratio rises
 *   during the period of over-broad mandate application (COVID mandates
 *   applied to low-risk populations with high alternative options), then
 *   stabilizes as proportionality constraints tighten enforcement.
 *
 * KEY AGENTS:
 *   - Public health authorities (institutional, agenda-setter): set enforcement thresholds based on disease/vaccine parameters
 *   - Vulnerable populations (powerless, beneficiary): immune-compromised, infants, elderly — depend on herd immunity, cannot exit
 *   - Unvaccinated individuals (moderate, payer): face restrictions on employment/school/travel; can exit by accepting vaccination or relocating
 *   - Vaccine hesitant populations (organized, payer): object on religious/safety/autonomy grounds; politically organize to challenge mandate scope
 *   - Clinical epidemiologists (institutional, observer): provide disease severity and vaccine safety data that anchor proportionality decisions
 *   - Constitutional courts (institutional, observer): adjudicate whether mandates meet proportionality standards
 *   - Bodily autonomy advocates (organized, excluded): would reject proportionality framing itself as misconstruing bodily integrity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.58).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.71).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Vaccine Mandate Legitimacy (Proportionality Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '75c1ee54-f4a3-4c3d-b91f-301c0f26d827').
narrative_ontology:cs_kernel_codification('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', formalized).
narrative_ontology:cs_authority_grounding('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', lineage).
narrative_ontology:cs_interpretation_layer_present('75c1ee54-f4a3-4c3d-b91f-301c0f26d827').
narrative_ontology:cs_reading_relation('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_axiom('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', foundational, mandate_legitimacy_requires_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', mandate_legitimacy_requires_proportionality, deontological).
narrative_ontology:cs_axiom('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', secondary, alternatives_availability_limits_necessity).
narrative_ontology:cs_axiom_status(alternatives_availability_limits_necessity, holdable).
narrative_ontology:cs_axiom_grounding('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', alternatives_availability_limits_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', informed_consent_primacy_with_collective_exception).
narrative_ontology:cs_drift_state('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', contemporary_post_covid, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('75c1ee54-f4a3-4c3d-b91f-301c0f26d827', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines when disease severity, vaccine safety data, and availability of alternatives warrant mandatory vaccination. Sets enforcement thresholds and exemption scope. Claims mandates are calibrated to the specific pathogen's threat profile and the vaccine's safety record, and that they issue only when alternatives (isolation, testing, treatment) are insufficient.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Immuno-compromised, infants, elderly, and others who cannot be vaccinated or for whom vaccination fails. Depend on herd immunity from the vaccinated surrounding population to avoid exposure to serious disease. Unvaccinated individuals around them create transmission risk they have no exit from.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Face restrictions on employment, school attendance, healthcare facility access, or travel when mandates are in effect. May object on grounds of religious belief, medical concern, or bodily autonomy principle. Can exit by accepting vaccination, relocating to non-enforcing jurisdictions, or accepting the restrictions.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Hold concerns about vaccine safety, long-term effects, or government overreach. Mandates force a choice between vaccination over objection or accepting exclusion from social/economic participation. Organize politically to challenge the scope and evidence basis of mandate decisions.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations, payer,
    organized, biographical, constrained, national).

% Provide the disease-severity and vaccine-safety data that legitimate proportionality-scaled mandates. Their assessments anchor whether a given pathogen crosses the threshold for mandatory vaccination under this reading. Debate among them about methodology, endpoints, and evidence quality directly shapes what the constraint permits.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, clinical_epidemiologists, observer,
    institutional, generational, analytical, global).

% Adjudicate whether mandates meet proportionality standards: whether disease severity justifies the incursion, whether less restrictive alternatives exist, and whether the vaccine's safety/efficacy profile supports the mandate scope. Their rulings shift what mandates can be enforced and what counts as proportional.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Would argue that ANY medical intervention without informed consent violates bodily integrity, regardless of collective benefit or proportionality metrics. Their framing is structurally excluded from this reading, which accepts that proportional public health mandates can override individual bodily autonomy claims.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves herd immunity thresholds sufficient to protect populations incapable of vaccination from serious disease, through a coordinated population-level intervention that individual vaccination decisions alone would not reach.
% TRANSFER_FUNCTION: Transfers the bodily autonomy cost of mandatory vaccination from individuals to the collective (unvaccinated individuals accept injection against objection), in exchange for collective disease suppression that protects vulnerable populations who cannot be vaccinated.
% ABSENT_VOICES: Bodily autonomy absolutists are structurally excluded from this reading's framework — they would object that the proportionality calculus itself misconstrues the problem by treating bodily integrity as tradeable against collective health. Medical freedom advocates also do not sit at the table; they would dispute whether mandates can ever be properly calibrated and would argue for individual choice as the primary value.
% DISAPPEARANCE_RATIONALE: If vaccine mandates ceased, vaccination rates for serious pathogens (measles, polio) would drop to levels insufficient for herd immunity, disease would re-emerge in vulnerable populations, and the system would reorganize around outbreak containment and isolation rather than prevention. For low-severity pathogens (seasonal flu), the world would largely rearrange unchanged — individual vaccination choice would sustain near-current rates.
% FOUNDING_PROBLEM: Population immunity to dangerous pathogens requires vaccination rates above what voluntary uptake historically achieved; vulnerable individuals depend on others' immunity to survive. The founding problem is: how to achieve necessary population-level protection without accepting absolute medical conscription or accepting endemic disease in the vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Clinical epidemiology literature documents pre-vaccine measles mortality (410 per 100,000 infected) and shows that voluntary vaccination rates for measles stabilize ~85% when outbreaks are not salient, below the 95% herd immunity threshold. Public health agencies and immune-compromised advocacy organizations attest the problem persists — vulnerable populations face ongoing measles risk in low-coverage regions. Bodily autonomy advocates dispute that the problem justifies mandates, but do not dispute the factual epidemiology; constitutional courts have recognized both the founding problem and the mandate-proportionality framework as live legitimate concerns.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because mandates impose bodily intervention against objection, but the proportionality constraint limits victim scope: only unvaccinated individuals whose non-vaccination meaningfully increases risk for vulnerable populations are legitimate targets. Flu mandates would apply to a broader population (nearly everyone is exposed risk-wise), so flu extractiveness would approach 0.65–0.72 under proportionality logic; measles mandates apply only where hesitancy is active, so measles extractiveness is lower (~0.50–0.55). The ε value reported here (0.58) represents the AVERAGE across pathogen types present in the interval. Suppression is high (0.71) because enforcement requires active exclusion from employment, schools, healthcare facilities, and travel — substantial structural barriers. Theater rises notably (0.28→0.45 at year 15) during the COVID-19 interval when mandates were applied to young healthy populations with minimal severe-disease risk and multiple alternatives (testing, isolation, treatment), inflating the measured theater because the functional risk-reduction benefit was modest while the performance of pandemic control remained center-stage. Theater stabilizes (0.42) as courts renarrow mandate application to high-severity pathogens and high-risk populations, reducing the gap between stated function and actual operation. Suppression requirement stays high (0.71) because the constraint's persistence depends on actively maintaining employment/healthcare exclusions, not on participant preference or natural adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary perspective (vulnerable populations and public health authorities), the constraint is genuine coordination — achieving herd immunity to protect the unvaccinated-incapable. From the payer perspective (unvaccinated and hesitant individuals), the constraint is enforced extraction — bodily intervention imposed against objection, with limited exit. The proportionality reading acknowledges both seats but privileges the beneficiary framing when disease severity justifies it. Courts computing from the payer seat would emphasize accessibility collapse (how completely alternatives have been foreclosed) and resistance (how much active objection exists); courts computing from the beneficiary seat emphasize coordination necessity and vulnerable-population welfare. The engine computes per-seat, so the divergence becomes transparent in the compiled output.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations (powerless, trapped exit, local scope) are structural beneficiaries — the constraint's primary function is to protect them. Their directionality is near 0.0 (full beneficiary). Unvaccinated individuals (moderate power, constrained exit) are near 0.8–0.9: they bear the bodily autonomy cost directly, though constrained exit (relocation, acceptance) moderates extraction. Vaccine-hesitant populations (organized power) sit at 0.75–0.85: they have higher exit options (political mobilization, litigation), which would lower d, but their objection is structural not circumstantial, making exit harder. Public health authorities are not a 'participant seat' in the usual sense — they are the constraint setter — but their directionality under the proportionality reading is analytically ~0.35: they benefit from successful epidemic prevention (career legitimacy, public trust) but bear costs of court challenge and political friction. The proportionality constraint itself mitigates extraction by requiring disease-severity and alternative-availability evidence: authorities cannot mandate for low-threat pathogens without evidence, which lowers the effective extraction they can impose compared to unconstrained authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (achieve population immunity thresholds without conscripting medical decision-making) remains live but contested. It has NOT yet resolved into mandatrophy because: (1) the proportionality reading continues to gate new mandates (courts strike down low-severity mandates, supporting the functional scoping), (2) alternative mechanisms (voluntary vaccination + targeting of vulnerable populations) have not yet demonstrated superiority as a population-level strategy (measles re-emergence in low-coverage regions continues), and (3) the constraint's beneficiary structure (protecting vulnerable populations) remains operative. However, if disease severity continues to decline (through vaccine uptake in prior decades or viral evolution), or if alternatives (rapid testing, outpatient treatment, better prophylaxis) become economically scalable, the founding problem would resolve dead while the enforcement infrastructure persists — that is the mandatrophy risk point. The measurement series shows theater rising to 0.45, signaling that performance is outpacing function in certain years (COVID era), which is early-stage mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calibration_ambiguity,
    'What disease severity threshold, vaccine efficacy threshold, and alternative-availability threshold jointly define ''proportional''? How are these three dimensions weighted when they point in different directions (high severity + moderate efficacy + weak alternatives)?',
    'Constitutional precedent establishing quantitative or qualitative benchmarks (e.g., case-fatality rate > 1%, vaccine efficacy > 90%, no alternative with <50% adoption). Comparative jurisprudence across nations establishing de facto thresholds through mandate strike-downs.',
    'Different calibrations change the victim set: strict calibration (high severity only) narrows to measles/polio mandates; loose calibration permits influenza/RSV/COVID mandates. The ε value reported (0.58) assumes moderate calibration; strict would produce ε ≈ 0.45–0.50, loose would produce ε ≈ 0.65–0.75.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_calibration_ambiguity, conceptual, 'Ambiguity in how severity, efficacy, and alternatives are combined to set proportionality boundaries.').

omega_variable(
    vulnerable_population_scope_ambiguity,
    'Does ''vulnerable populations'' include immuno-compromised, infants, elderly only? Or also includes economically precarious populations (more likely to experience severe disease from comorbidities), and if so, how does the victim set change?',
    'Epidemiological stratification: studies separating disease severity by age, immune status, comorbidity, and socioeconomic status; public health definition of who qualifies for protection under proportionality logic.',
    'Broader vulnerable definition → larger population to protect → higher justifiable mandate scope → higher victims (more unvaccinated individuals forced into vaccination to protect broader populations). Narrow definition (immuno-compromised only) → ε ≈ 0.40–0.45; broad definition (+ comorbid populations) → ε ≈ 0.65–0.70.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_scope_ambiguity, empirical, 'Whether vulnerable-population scope expands with better understanding of risk stratification.').

omega_variable(
    alternatives_substitutability_ambiguity,
    'Can testing + isolation + treatment substitute for vaccination as herd-immunity mechanisms? Or is vaccination structurally necessary because alternatives require active compliance that vulnerable populations cannot guarantee?',
    'Natural experiment from jurisdictions that replaced mandates with test-and-isolate regimes: do vulnerable populations maintain protection levels? Do economically precarious populations accept repeated testing + isolation costs that vaccine compliance would avoid?',
    'If alternatives are true substitutes, mandate legitimacy drops (less restrictive alternative exists) → ε ≈ 0.40–0.45. If alternatives require active compliance that vulnerable populations cannot guarantee (testing access, isolation feasibility for homeless/precarious populations), mandates remain necessary → ε ≈ 0.55–0.65 (current value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_substitutability_ambiguity, empirical, 'Whether less-restrictive alternatives can achieve proportional mandate goals.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Does the proportionality reading COEXIST with the bodily_autonomy_primary reading, or does proportionality FORECLOSE bodily autonomy absolutism by accepting that autonomy can be overridden when severity justifies it?',
    'Logical analysis: proportionality accepts bodily autonomy as a value but subordinates it to collective welfare in certain cases — this is not foreclosure of the axiom that bodily autonomy matters, but subordination of it. Foreclosure would require denying that bodily autonomy has any claim whatsoever, which proportionality does not do.',
    'If coexists: both readings remain live positions held by different parties, neither eliminates the other. If forecloses: the proportionality reading has settled the dispute by establishing that autonomy claims yield to proportionate collective necessity, collapsing bodily_autonomy into a defeated alternative. Current jurisprudence shows coexistence (different constitutional courts and different political factions hold different readings), not foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether proportionality reading logically eliminates bodily autonomy reading or coexists with it.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.71) primarily structural (legal exclusion from employment/schools) or partially internalized (hesitant individuals internalize shame, medical distrust, or identity fusion with resistance communities such that suppression persists even after mandate removal)?',
    'Post-mandate relaxation studies: when jurisdictions cease enforcement, do hesitant vaccination rates rebound immediately (structural suppression) or remain depressed (internalized suppression from identity formation, medical distrust reinforcement, community polarization)?',
    'If primarily structural: removing the mandate removes the constraint. If partially internalized: the constraint''s effective suppression persists beyond formal removal because the resistance identity and medical distrust have become self-maintaining. Internalization would suggest the measured suppression (0.71) understates the total constraint cost to hesitant individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression persists after mandate removal due to identity fusion and internalized distrust.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(mand_tr_t5, observed).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(mand_tr_t10, observed).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(mand_tr_t15, observed).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(mand_tr_t20, observed).
narrative_ontology:measurement(mand_tr_t25, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(mand_tr_t25, observed).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(mand_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(mand_be_t5, observed).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(mand_be_t10, observed).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(mand_be_t15, observed).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(mand_be_t20, observed).
narrative_ontology:measurement(mand_be_t25, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(mand_be_t25, observed).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(mand_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(mand_su_t5, observed).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(mand_su_t10, observed).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(mand_su_t15, observed).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(mand_su_t20, observed).
narrative_ontology:measurement(mand_su_t25, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(mand_su_t25, observed).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(mand_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% The mandate_legitimacy_scope kernel has three distinct readings: bodily_autonomy_primary (bodily integrity is inviolable), public_health_primary (state can mandate for any collective health benefit), and proportionality_reading (mandates justified only when disease severity, vaccine efficacy, and lack of alternatives jointly support it). These are three separate constraint stories, each with its own ε, beneficiary/victim structure, and classification. The proportionality reading influences both siblings by establishing intermediate grounds and requiring evidence-based gating; it coexists with both rather than foreclosing either. Each story should be authored independently and linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
