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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Mandate Legitimacy via Proportionality (Disease Severity, Safety, Alternatives)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'mandate_legitimacy_scope': the proportionality reading. This reading
 *   grounds mandate legitimacy in a three-part test: (1) disease severity
 *   exceeds a threshold (mortality, transmissibility, disability burden); (2)
 *   vaccine safety and efficacy are established and acceptable; (3) no less
 *   restrictive alternative (targeted protection, voluntary incentives) is
 *   available or effective. Under this reading, a measles mandate is
 *   legitimate (high severity, established vaccine, no effective
 *   alternative), while a seasonal-flu mandate is not (low severity, vaccine
 *   efficacy varies, targeted protection of elderly is feasible). The
 *   constraint's extractiveness (0.58) varies by pathogen — it is moderate
 *   and conditional, not absolute. The sibling readings
 *   (bodily_autonomy_primary, public_health_primary) stake different ground:
 *   autonomy primary denies any mandate is legitimate; public health primary
 *   asserts mandate legitimacy on disease presence alone, without the
 *   proportionality conditions. This story generates the proportionality
 *   reading clean — as ε-invariant as it can be for a conditional constraint
 *   — and routes the kernel contest to omega variables and cs_structure
 *   rather than embedding it in the metrics.
 *
 * KEY AGENTS:
 *   - public_health_authorities: institutional agenda-setter, sets mandate scope and enforcement thresholds; collects no direct rents but derives authority from the mandate's perceived legitimacy
 *   - vulnerable_populations: powerless beneficiaries, depend on herd immunity; cannot exit via relocation or individual protection
 *   - unvaccinated_individuals: moderate-power payers, face constrained exit (medical intervention vs. social/economic exclusion)
 *   - vaccine_hesitant_populations: organized payers with identity-locked exit; comply under coercion but resist normatively
 *   - medical_practitioners: powerful observers; implement the mandate but can contest its scope within professional channels
 *   - legislative/judicial authorities: institutional observers; review mandate proportionality and can overturn or narrow it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.58).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.64).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Mandate Legitimacy via Proportionality (Disease Severity, Safety, Alternatives)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '6462c8a4-6ea4-49a9-aa72-e2765cfa0412').
narrative_ontology:cs_kernel_codification('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', distributed).
narrative_ontology:cs_authority_grounding('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', lineage).
narrative_ontology:cs_interpretation_layer_present('6462c8a4-6ea4-49a9-aa72-e2765cfa0412').
narrative_ontology:cs_reading_relation('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', foundational, mandate_legitimacy_conditional_on_severity_and_safety).
narrative_ontology:cs_axiom_status(mandate_legitimacy_conditional_on_severity_and_safety, holdable).
narrative_ontology:cs_axiom_grounding('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', mandate_legitimacy_conditional_on_severity_and_safety, deontological).
narrative_ontology:cs_axiom('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', foundational, less_restrictive_alternatives_are_prerequisite).
narrative_ontology:cs_axiom_status(less_restrictive_alternatives_are_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', less_restrictive_alternatives_are_prerequisite, deontological).
narrative_ontology:cs_reference_frame('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', proportionality_balancing_framework).
narrative_ontology:cs_drift_state('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', contemporary_mandate_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6462c8a4-6ea4-49a9-aa72-e2765cfa0412', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, proportionality_principle_in_public_health).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, conditional_collective_good_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce vaccination mandate policy. Claim authority to compel vaccination when disease severity, vaccine safety, and absence of less restrictive alternatives align. They set the thresholds for what counts as 'severe enough' and 'safe enough.' Defend the mandate as protecting vulnerable populations and maintaining herd immunity thresholds.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Depend on herd immunity provided by mandated vaccination of others (immunocompromised individuals, infants, elderly). They cannot be vaccinated themselves and have no alternative protection if coverage falls below herd immunity thresholds. They benefit directly from mandate enforcement but bear no direct compliance cost.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Face mandatory vaccination or exclusion from employment, education, healthcare facilities, or public spaces. They bear the direct compliance cost (medical intervention, side effect risk, bodily autonomy constraint). Their exit options are constrained by law and social enforcement; geographic exit is theoretically available but costly.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Comply under mandate pressure rather than voluntary choice, bearing the psychological cost of coerced medical intervention and the identity cost of perceived government overreach. They benefit incidentally from protection against the mandated disease but frame the mandate as violation of autonomy-based identity. Exit from the mandate would require either relocation or public identity shift (rejecting their hesitancy frame).
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_populations, beneficiary).

% Administer mandated vaccines and document adverse events. They operate under clinical guidelines that incorporate the proportionality framework: assessing individual patient contraindications, counseling on safety/efficacy data, and referring to public health guidance for mandate scope. They can contest overly broad mandates within professional channels.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_practitioners, observer,
    powerful, biographical, mobile, national).

% Review mandate authority and proportionality in law. They assess whether disease severity, vaccine safety/efficacy metrics, and availability of less restrictive alternatives meet the constitutional or statutory thresholds for mandate legitimacy. They can overturn or narrow mandates found to lack proportionality justification.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, legislative_and_judicial_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents transmission of serious communicable disease to vulnerable populations unable to protect themselves through vaccination or natural immunity. Solves a collective-action problem: individual vaccination decisions do not account for protection provided to others, so mandates coordinate individual and collective benefit when thresholds are met.
% TRANSFER_FUNCTION: Transfers bodily autonomy (forced medical intervention) and autonomy-signaling capacity (identity autonomy over vaccine choice) from unvaccinated/hesitant populations to vulnerable populations (who receive herd immunity protection). In moderate cases, the transfer is conditional and proportionate; in overextended cases, it becomes extraction divorced from the coordination problem it claims to solve.
% ABSENT_VOICES: Individuals with strong bodily-autonomy commitments (some religious/philosophical groups, medical autonomy advocates) are structurally excluded from mandate-setting processes; they would argue the proportionality test itself is illegitimate because any mandate violates fundamental bodily integrity. Their position is dismissed rather than heard in the design of threshold criteria.
% DISAPPEARANCE_RATIONALE: If mandate authority under the proportionality reading disappeared, vaccination rates for serious pathogens (measles, polio) would fall sharply below herd immunity thresholds, vulnerable populations would face elevated risk, and public health would need to shift to non-coercive strategies (education, access expansion, social incentives). The disease control landscape would reorganize around voluntary participation and lower coverage targets.
% FOUNDING_PROBLEM: In the early 20th century, vaccine-preventable diseases (smallpox, polio, measles) killed or disabled millions; vaccination rates were low and uneven; vulnerable populations had no alternative protection. Mandatory vaccination was built to solve the collective-action failure when voluntary rates could not sustain herd immunity against severe, widely-circulating diseases.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists attest that for measles and polio, the founding problem remains live — wild-type circulation still occurs in low-coverage regions and poses ongoing risk. Bodily autonomy advocates and some legal scholars attest the founding problem is overstated and has been rhetorically extended to diseases (seasonal influenza) where the original justification does not apply. Independent historical and epidemiological analysis from outside public health advocacy shows the problem was acute in the early 20th century and remains significant for certain pathogens but not others.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).

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
 *   Extractiveness is 0.58 (moderate, conditional) because the constraint solves a real coordination problem (herd immunity for vulnerable populations) but extracts bodily autonomy from unvaccinated/hesitant populations as the price. The proportionality reading allows variance: high-severity pathogens (measles) approach rope-level (coordination benefits exceed autonomy costs); low-severity pathogens (flu) approach snare-level (coordination benefits are unclear and extraction is dominant). The base properties reflect the AVERAGE case across the pathogen spectrum; individual pathogen cases would warrant separate stories (ε-invariance principle). Suppression (0.64) is high because mandate persistence depends on legal enforcement and social exclusion, not on voluntary acceptance by the payer populations. Theater ratio (0.42) is moderate-high: some enforcement activity is genuine disease prevention (surveillance, contact tracing), but a growing share defends mandate scope against exemption requests and alternative-pathway arguments. The measurement series show base_extractiveness and theater rising through the interval (political expansion of mandate scope to lower-severity cases, defensive rhetoric about 'pandemic preparedness') then declining slightly at the endpoint (legal challenges, exemptions reinstated) — the cyclical pattern reflects contestation over where the proportionality threshold should be. Theater peaks at t=32 during maximum mandate enforcement pressure, then retreats as judicial review narrows scope.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat and the unvaccinated-individual seat should compute different constraint types from this structural data. From the authority seat: the constraint is a legitimate, proportionate balancing of individual and collective good — tenuous rope or scaffold (temporary emergency measure). From the payer seat: the same structure is coercive extraction justified by rhetorical claims about severity and alternatives that may not hold for every case the mandate covers — tangled rope trending toward snare. The engine computes this divergence from power (institutional vs. moderate), exit options (arbitrage vs. constrained), and beneficiary/victim declarations. The claim-metric gap is deliberate: I claim tangled rope (the proportionality reading's own framing — legitimate under conditions) while authoring metrics that describe extraction in practice. Where the computed type diverges from the claim, that divergence indicates WHERE the proportionality reading is failing in operation: a computed snare on a claimed rope signals that disease severity, safety evidence, or alternative availability are not being assessed as the reading requires.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities: d near 0.15 (beneficiary-adjacent; they set the rules and derive authority from their legitimacy but collect no extraction rents directly — derectionality via vindicated propositions rather than beneficiary group). Vulnerable populations: d near 0.1 (strong beneficiary; they receive herd immunity protection and bear no compliance cost; they are trapped but protected). Unvaccinated individuals: d near 0.85 (strong target; they bear the medical intervention cost and face constrained exit; their bodily autonomy is extracted). Vaccine-hesitant populations: d near 0.75 (high-target; they comply under coercion and lose autonomy-signaling capacity via identity-locking; they benefit incidentally from herd immunity but experience the mandate as violation). Medical practitioners: d near 0.5 (symmetric; they implement the mandate but can contest it, and they gain authority/liability clarity from explicit mandate authority). Legislative/judicial authorities: d near 0.4 (mild beneficiary; they gain institutional capacity to review and constrain executive public health power, which they exercise). The directionality pattern shows asymmetric extraction concentrated on moderate-power payer populations and trapped vulnerable beneficiaries, with institutional authorities holding the definitional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading prevents mislabeling by tying mandate legitimacy to measurable conditions (severity, safety, alternatives) rather than to health benefit alone. A mandate that persists after disease severity falls below the threshold, or after safer alternatives become available, or after vaccine adverse events rise above acceptable levels, is no longer justified by the proportionality test — it becomes extraction wearing coordination's clothes. The constraint's potential for mandatrophy is high: founding problem (vaccine-preventable disease risk) remains live for certain pathogens (measles, polio) but is contested or dead for others (seasonal flu). The measurement series capture this drift: rising extractiveness through the interval reflects mandate scope expansion to pathogens where the proportionality test does not hold, and the theater ratio rise reflects increasingly defensive rhetoric about why the mandate must persist despite weakened severity/alternatives justification. The declining values at the endpoint reflect legal pushback: courts begin to apply the proportionality test and overturn mandates for low-severity cases. Mandatrophy is already visible in the interval as the mandate's original function (protect vulnerable populations from severe disease) atrophies and persists as institutional inertia and precedent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_ambiguity,
    'What epidemiological metrics define ''disease severity'' sufficient to justify mandate legitimacy under the proportionality reading? Is it mortality rate, hospitalization rate, disability burden, transmissibility, or a weighted combination?',
    'Explicit legislative or regulatory definition of severity thresholds (case fatality rate, attack rate, disability-adjusted life years) applied prospectively to new pathogen-candidate pairs. Comparison with how thresholds have been applied across historical and contemporary cases.',
    'A vague or shifting definition of ''severe enough'' allows authority drift: mandates expand to pathogens (seasonal influenza) where the threshold was never clearly met. A clear, pre-committed definition constrains mandate scope and supports the proportionality reading''s legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Whether disease severity has stable, transparent criteria or drifts with political pressure.').

omega_variable(
    alternative_measures_availability,
    'Are less restrictive alternatives (targeted protection of vulnerable populations, voluntary vaccination incentives, public education campaigns, improved healthcare access) genuinely unavailable, or are they dismissed as politically infeasible?',
    'Jurisdictional comparison: natural experiments from regions using alternative strategies (no mandate but high voluntary rates via education/access; targeted protection via quarantine/isolation of vulnerable groups). Historical case analysis of what alternatives were actually considered in mandate decisions.',
    'If alternatives are materially unavailable (e.g., vulnerable population protection requires total isolation, not feasible), the mandate becomes structurally necessary and the proportionality reading holds. If alternatives were available but rejected for efficiency or precedent reasons, the mandate becomes extractive rather than coordinative and the bodily autonomy reading gains structural ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_measures_availability, empirical, 'Whether the proportionality test''s ''no less restrictive alternative'' criterion is genuinely met.').

omega_variable(
    vaccine_safety_efficacy_evidentiary_standard,
    'What evidentiary standard determines ''adequate safety/efficacy'' for mandate coverage? Pre-market trial data? Post-market surveillance? How much adverse event signal triggers mandatory reassessment? Who decides, and how transparent is the process?',
    'Explicit regulatory criteria for vaccine approval in mandate context vs. individual clinical use. Tracking of adverse event reports and mandate scope adjustments in response. Independent expert review of causality assessment for reported harms.',
    'A rigorous, transparent evidentiary standard supports the proportionality reading''s legitimacy. A low threshold or opaque process shifts the reading toward extraction and supports the bodily autonomy objection that informed consent cannot be meaningful under a mandate with unclear safety data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vaccine_safety_efficacy_evidentiary_standard, empirical, 'Whether vaccine safety/efficacy assessment is transparent and appropriately rigorous for mandate justification.').

omega_variable(
    reading_vs_bodily_autonomy_boundary,
    'Does the proportionality reading constitute a genuine middle position between public health primary and bodily autonomy primary, or is it a disguised version of one of the two?',
    'Apply the proportionality test to a case where disease severity is moderate (not trivial, but not pandemic-scale) and vaccine safety data is mixed: if the proportionality reading mandates, it forecloses bodily autonomy primary; if it forbids the mandate, it coexists with it. If the test is indeterminate or admits both decisions, the reading is genuinely distinct.',
    'If the proportionality reading forecloses bodily autonomy primary in all cases, they are logically incompatible and the kernel contest is actually binary. If the proportionality reading leaves space for bodily autonomy arguments under some conditions, the three readings genuinely coexist and the test is a framework for adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_bodily_autonomy_boundary, conceptual, 'Whether the proportionality reading is a distinct position or a disguise for one of its siblings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.64) primarily structural (legal penalties, employment exclusion, mandatory testing) or internalized (vaccine-hesitant populations accepting the mandate as legitimate even if coercive)?',
    'Post-mandate exemption or repeal: if suppression persists after the legal/employment coercion is lifted (hesitant individuals remain reluctant to vaccinate even when voluntary), suppression is partly internalized. Survey of hesitant populations pre/post mandate on perceived legitimacy and willingness to vaccinate voluntarily.',
    'Structural suppression is more easily removable by policy change. Internalized suppression indicates the mandate has shifted identity-level commitments and may carry lasting psychological/relational costs beyond the compliance period. High internalization raises the effective suppression cost of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Proportion of mandate suppression that is structural (legal coercion) vs. internalized (belief-adoption).').

omega_variable(
    kernel_contest_foreclosure_risk,
    'Does the proportionality reading''s three-part test (severity, safety, alternatives) genuinely allow for autonomy-primary objections, or does the test function as a rhetorical cover for public-health-primary expansion?',
    'Historical tracking: are there documented cases where mandate scope was NARROWED or RESCINDED because the proportionality test was not met? Or does the test always rubber-stamp public health authority decisions?',
    'If the test functions as genuine constraint on mandate expansion, the proportionality reading coexists with bodily autonomy primary (both can be applied). If the test always ratifies authority decisions, the proportionality reading forecloses autonomy primary and the true contest is binary (public health vs. bodily autonomy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_risk, empirical, 'Whether the proportionality test is a genuine legal/ethical constraint or a post-hoc justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(mand_tr_t8, observed).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(mand_tr_t16, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(mand_tr_t24, observed).
narrative_ontology:measurement(mand_tr_t32, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement_basis(mand_tr_t32, observed).
narrative_ontology:measurement(mand_tr_t40, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(mand_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(mand_be_t8, observed).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(mand_be_t16, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(mand_be_t24, observed).
narrative_ontology:measurement(mand_be_t32, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(mand_be_t32, observed).
narrative_ontology:measurement(mand_be_t40, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(mand_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(mand_su_t8, observed).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement_basis(mand_su_t16, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement_basis(mand_su_t24, observed).
narrative_ontology:measurement(mand_su_t32, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement_basis(mand_su_t32, observed).
narrative_ontology:measurement(mand_su_t40, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(mand_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__proportionality_reading, 0.18).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% The mandate_legitimacy_scope kernel decomposes into three structurally distinct constraints with different ε values and victim/beneficiary sets. bodily_autonomy_primary (Mountain, ε~0.0, no victims named): bodily autonomy violation is categorically impermissible. public_health_primary (Tangled Rope, ε~0.72): state authority to compel vaccination is legitimate when disease presents risk, high extraction. proportionality_reading (Tangled Rope, ε~0.58, this file): mandate legitimacy is conditional on severity/safety/alternatives, moderate extraction conditional on pathogen. All three are linked by affects_constraints; each story applies the same reading to the kernel independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
