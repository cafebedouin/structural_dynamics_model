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
 *   human_readable: Public-Health-Primary Reading of the Coercion Legitimacy Boundary
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the state's operative power to
 *   compel medical intervention - principally vaccination mandates enforced
 *   through school-entry conditionality, employment requirements, civil
 *   penalties, and emergency orders - legitimated by the claim that
 *   collective harm-prevention outweighs individual autonomy where the two
 *   conflict. This file instantiates ONE reading (public_health_primary) of
 *   the coercion_legitimacy_boundary kernel; the sibling readings
 *   (bodily_autonomy_primary, proportionality_reading) are separate
 *   constraints with their own epsilon values and victim sets. Metrics here
 *   are reading-indexed to the public-health-primary seat: they assess the
 *   standing compulsion arrangement as this reading itself evaluates it,
 *   acknowledging real transfers onto objectors while endorsing the warrant.
 *   The claim/metric pairing is deliberate and unreconciled: the reading
 *   CLAIMS tangled_rope (genuine coordination function plus a named payer
 *   seat plus active enforcement), and the authored metrics describe that
 *   structure descriptively; the engine computes per-seat types from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda setter (institutional/constrained) - drafts schedules, runs enforcement, accrues authority and appropriations from the mandate portfolio
 *   - unvaccinated_objectors: primary target (moderate/identity_locked) - bears fines, school and employment exclusion; compliance is physically cheap, identitatively prohibitive
 *   - immunocompromised_individuals: primary beneficiary (powerless/trapped) - cannot vaccinate; protection wholly dependent on others' enforced compliance
 *   - vaccine_ineligible_infants: beneficiary (powerless/trapped) - pre-eligibility window protected only by surrounding coverage
 *   - medically_exempt_individuals: beneficiary (moderate/constrained) - excused from the intervention yet shielded by it
 *   - vaccinated_majority_public: beneficiary (moderate/mobile) - complies at trivial cost; the constituency that stabilizes enforcement
 *   - employers_and_schools: enforcement intermediary (institutional/constrained) - terminates access under rules it did not design; absorbs administrative friction
 *   - frontline_healthcare_workers: dual payer/beneficiary (organized/constrained) - mandated as a work condition, maximally protected by the same rule
 *   - civil_liberties_advocates: excluded (organized/constrained) - litigates finished rules; holds no seat where thresholds are set
 *   - constitutional_courts: analytical observer (institutional/analytical) - polices the boundary case by case against the Jacobson lineage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.66).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.58).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.66).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public-Health-Primary Reading of the Coercion Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f').
narrative_ontology:cs_kernel_codification('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', formalized).
narrative_ontology:cs_authority_grounding('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', lineage).
narrative_ontology:cs_interpretation_layer_present('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f').
narrative_ontology:cs_reading_relation('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', foundational, collective_harm_prevention_outweighs_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_prevention_outweighs_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', collective_harm_prevention_outweighs_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', secondary, state_police_power_extends_to_compulsory_health_measures).
narrative_ontology:cs_axiom_status(state_police_power_extends_to_compulsory_health_measures, holdable).
narrative_ontology:cs_axiom_grounding('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', state_police_power_extends_to_compulsory_health_measures, conventional).
narrative_ontology:cs_reference_frame('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', contemporary_post_covid, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2d14b5cc-0eba-4ef3-8ac7-a8b94cbc5c1f', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccine_ineligible_infants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, medically_exempt_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccinated_majority_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, frontline_healthcare_workers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, employers_and_schools).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, frontline_healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft immunization schedules and coverage targets through advisory committees, issue isolation and quarantine orders, operate school-entry record verification, and levy civil penalties for noncompliance. Their appropriations and statutory authority scale with the mandate portfolio they administer; they cannot waive the boundary they enforce, since legislatures and courts set its outer limits.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Refuse mandated vaccines on conscientious, religious, or institutional-distrust grounds and bear the consequences: civil fines, exclusion of their children from school, termination from covered employment, and barred access to venues during outbreaks. Taking the injection is physically trivial, but for the committed core acceptance would repudiate community membership and self-concept; relocating to a permissive jurisdiction is the main material exit and it is costly.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_objectors, payer,
    moderate, biographical, identity_locked, national).

% Cannot be vaccinated safely (transplant recipients, chemotherapy patients) and rely entirely on the coverage of people around them for protection. They hold no lever over coverage levels; their safety rises and falls with enforcement they do not administer and cannot opt out of needing.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Are too young to complete the multi-dose series and are protected only by the immunity of everyone around them until they reach eligibility. Their exposure is set by local coverage, which school-entry conditionality props up.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_ineligible_infants, beneficiary,
    powerless, immediate, trapped, local).

% Hold physician-documented contraindications, are excused from the mandate, and receive the same herd protection as the vaccinated without taking the intervention. Their exemption depends on medical gatekeeping they do not control, and they carry social suspicion during outbreaks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medically_exempt_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Comply at low personal cost and receive protection plus the option value of outbreak-free schooling, travel, and work. Exiting the arrangement is trivial for them because compliance is cheap; they are the constituency that keeps enforcement politically stable.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccinated_majority_public, beneficiary,
    moderate, biographical, mobile, national).

% Operate the exclusion rules day to day: verify records, deny enrollment or shifts, process exemption paperwork, and absorb the friction of staffing shortfalls, litigation, and parental conflict when enforcement bites. They did not design the requirement, but they are the point at which access is actually terminated.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, employers_and_schools, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, employers_and_schools, payer).

% Face state and employer mandates as a condition of clinical work while gaining the sharpest protection from the same rule, since occupational exposure to vaccine-preventable disease is highest exactly where the mandate binds. Unionized segments negotiate carve-outs and testing alternatives; leaving the profession is the only full exit and few take it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, frontline_healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, frontline_healthcare_workers, beneficiary).

% Litigate mandate expansions and campaign for exemption rights but hold no seat on the advisory committees that set coverage targets or in the emergency processes that trigger generalized compulsion. They meet the arrangement as finished rules to be challenged after adoption.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Review compulsion claims against the Jacobson lineage and its modern glosses, deciding case by case whether a given mandate remains inside the harm-prevention warrant. They neither administer nor fund the machinery; their output is boundary doctrine.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in population immunity: each person's infection risk depends on others' uptake, so purely voluntary provision undersupplies protection and coverage sags below outbreak thresholds. Compulsion internalizes the externality and holds coverage above the level at which transmission chains break.
% TRANSFER_FUNCTION: Moves bodily autonomy, and where penalties attach, money and access (school seats, employment, venues) from unvaccinated objectors to the collective; confers reduced-transmission protection onto the immunocompromised, vaccine-ineligible infants, the medically exempt, and the compliant majority.
% ABSENT_VOICES: Bodily-integrity absolutists and civil-liberties campaigners would object to any threshold of compulsion whatsoever; conscientious-objector communities would object to the specific schedules and penalties. None of them sits on the advisory committees that set coverage targets or in the emergency-order processes that switch generalized compulsion on; they encounter the rules only after adoption, in court.
% DISAPPEARANCE_RATIONALE: School-enrollment conditionality, employment health requirements, outbreak-response playbooks, and pandemic-preparedness statutes all presuppose the compulsion power. Overnight removal would force improvised reconstruction during the next epidemic and measurably raise outbreak probability in under-immune communities, as post-liberalization resurgence episodes have shown.
% FOUNDING_PROBLEM: Recurrent smallpox epidemics in dense cities: voluntary vaccination left coverage below the level needed to interrupt transmission, so states made vaccination a condition of school entry and authorized compulsory vaccination backed by fines (the Massachusetts model, upheld in Jacobson v. Massachusetts, 1905).
% FOUNDING_PROBLEM_CORROBORATION: WHO's certified global eradication of smallpox (1980) attests, from entirely outside the benefiting parties, that the founding pathogen no longer circulates; the historical-epidemiology literature corroborates that the mandate architecture was built for smallpox. No party disputes the extinction of the founding problem; the live dispute is whether successor warrants (measles, pertussis, pandemic response) legitimately inherit the machinery, argued among public-health commissions, courts, and civil-liberties scholars.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.66: the transfers (fines, school and venue exclusion, terminated employment) are real and decoupled from individual culpability, but the arrangement also delivers the protection it charges for, so extraction is substantial without being confiscatory. Suppression 0.58 reflects exclusion-based coercion with functioning exemption channels and jurisdictional exit; the modern state stops short of physical force, unlike the t=0 era of outright compulsory vaccination. Theater 0.24: the function is demonstrably live (coverage responds to enforcement; outbreaks follow its relaxation), with performative residue in certificate rituals and symbolic mandates. Accessibility collapse 0.38: alternatives persist - medical and religious exemptions, homeschooling, remote work, interstate mobility - so understanding the constraint does not close the option set. Resistance 0.61: sustained litigation, exemption surges, and political backlash are documented across the interval. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. The shared eleven-point grid (year = 1905 + t) shows a crisis-driven cycle: coercion ratchets during epidemics (smallpox campaigns, the 2015 measles response, the 2020-21 COVID peak) and relaxes between them, with a post-eradication theater bump near t=75 when enforcement idled against a vanished target. Base metrics were measured at interval end (t=120), on the descending limb after the COVID peak. Receipt-surface check performed before authoring gain_flow: penalty receipts scatter to treasuries, forfeited wages disperse, and the protection dividend lands on the beneficiary seats as intended function rather than captured rent; the agencies' authority-accrual was examined and judged second-order (budgetary justification, not receipt of the extracted value), so gain_flow is authored as an affirmative 'diffuse'. fixing_cost is 'prohibitive': for the seats that could fix the constraint (legislatures, courts), removal trades relief for a concentrated minority against diffuse outbreak risk and institutional reversal costs, an asymmetry the post-liberalization resurgence record documents.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently, and the structural data forces that divergence. From the objector's position the arrangement is enforced extraction wearing a coordination alibi; from the immunocompromised seat it is survival infrastructure; from the agency seat it is ordinary administration of a statutory duty. Same-level divergence is equally sharp: the vaccinated majority and unvaccinated objectors hold identical civic standing and similar power atoms, but exit differs absolutely - compliance costs the majority minutes and the objector their community standing - so power alone under-describes the asymmetry and the exit-options axis carries it. Inter-institutionally, employers and schools administer a rule they did not author and bear its friction, while courts consume the same rule as doctrine; the constraint is a different object at each institutional seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive d toward the subsidized end for the four protected seats; the unvaccinated_objectors declaration drives d toward the full-target end, amplified by identity_locked exit (locked targets sit nearer the full-target pole than mobile ones). The agency seat derives low-but-nonzero d: it collects authority and appropriations rather than protection, a mild self-interest the structural derivation registers without an override. Employers and schools derive near-symmetric d (administration costs against compliant environments gained). Courts derive analytical neutrality. No directionality_overrides were needed: the beneficiary/victim declarations plus differentiated exit options already separate every seat the derivation would otherwise blur.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (smallpox control) is externally certified dead - WHO eradication, 1980 - while the machinery persists and has expanded, so the R5 mismatch (dead founder x world_rearranges) will flag, and should. The classification prevents mislabeling in both directions: reading the arrangement as pure rope ignores the named payer seat and the enforcement asymmetry; reading it as snare ignores the demonstrated coordination dividend (coverage held above outbreak thresholds) and the absence of any seat capturing the extraction as rent (receipt surface: diffuse, affirmatively checked). The honest structure is tangled_rope carrying a zombie-trend watch: whether successor warrants (measles, pertussis, pandemic response) independently sustain the machinery, or whether it persists on inherited authority alone, is exactly what the dead_founder_successor_warrant omega tracks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition_stability,
    'This constraint is one reading (public_health_primary) of the coercion_legitimacy_boundary kernel; is the victim/beneficiary partition authored here stable, or is it an artifact of the reading seat?',
    'Generate the sibling stories (bodily_autonomy_primary, proportionality_reading) and compare victim sets and epsilon values across the kernel family.',
    'Under bodily_autonomy_primary the arrangement computes as pure extraction with no coordination warrant and the victim set swells to every coerced subject; under proportionality_reading the victim set contracts to objectors against severe-disease mandates only. This story''s epsilon (0.66) is reading-indexed to public_health_primary and is not comparable across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition_stability, conceptual, 'Committer structure: one reading of a three-reading kernel; partition and epsilon are seat-relative.').

omega_variable(
    outweighing_threshold_discretion,
    'What operational metric determines when collective harm-prevention ''outweighs'' individual autonomy - and how discretionary is its application in practice?',
    'Comparative analysis of statutory and regulatory thresholds (incidence rates, reproduction numbers, hospital-capacity triggers) and of the composition and minutes of the advisory committees that apply them.',
    'If the threshold is effectively discretionary, extraction scales with administrator judgment rather than epidemiological fact, pushing the arrangement toward snare; if tied to observable triggers, the coordination reading strengthens and excess extraction narrows to enforcement overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outweighing_threshold_discretion, empirical, 'Whether the outweighing test is rule-bound or administrator-discretionary.').

omega_variable(
    jurisdictional_suppression_patchwork,
    'Does the constraint''s suppressive force describe a single national arrangement or a patchwork of sharply different exemption and penalty regimes?',
    'Cross-jurisdiction comparison of exemption categories, waiver grant rates, and penalty schedules at matched disease incidence.',
    'In no-exemption jurisdictions the payer seat computes nearer full-target with high effective suppression (snare-leaning per-seat results); in broad-exemption jurisdictions the same nominal constraint computes nearer rope. Story-level metrics average over a heterogeneous ensemble.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_suppression_patchwork, empirical, 'Suppression heterogeneity across jurisdictions under one nominal constraint.').

omega_variable(
    objector_identity_lock_fraction,
    'What fraction of unvaccinated objectors are identity-locked (refusal constitutive of worldview or community membership) versus price-sensitive (would comply if costs rose)?',
    'Longitudinal survey of objector cohorts under varying penalty levels, plus exemption-application and compliance data following penalty changes.',
    'If most objectors are price-sensitive, suppression is structural and d-amplification is moderate; if the core is identity-locked, effective extraction concentrates on a hard core that coalition remedies and penalty escalation cannot reach, raising per-seat chi for that subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objector_identity_lock_fraction, empirical, 'Composition of the payer seat between identity-locked and price-responsive refusers.').

omega_variable(
    dead_founder_successor_warrant,
    'Does the extinction of the founding problem (smallpox, eradicated 1980) delegitimate the inherited machinery, or do successor disease warrants independently sustain it?',
    'Doctrinal history tracing which mandates rest on post-1980 independent justifications versus inherited authority, combined with outbreak data following exemption liberalization.',
    'If the machinery persists on inheritance alone, the arrangement trends piton/zombie and the R5 mismatch flag is the whole story; if successor warrants bind independently, the dead founder is genealogically interesting but structurally irrelevant and the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dead_founder_successor_warrant, conceptual, 'Whether the arrangement''s persistence post-founder reflects live successor warrants or inertial inheritance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_public_health_primary_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clb_public_health_primary_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.14).
narrative_ontology:measurement(clb_public_health_primary_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.18).
narrative_ontology:measurement(clb_public_health_primary_tr_t60, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 60, 0.22).
narrative_ontology:measurement(clb_public_health_primary_tr_t75, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 75, 0.38).
narrative_ontology:measurement(clb_public_health_primary_tr_t90, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 90, 0.3).
narrative_ontology:measurement(clb_public_health_primary_tr_t105, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 105, 0.24).
narrative_ontology:measurement(clb_public_health_primary_tr_t112, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 112, 0.2).
narrative_ontology:measurement(clb_public_health_primary_tr_t115, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 115, 0.16).
narrative_ontology:measurement(clb_public_health_primary_tr_t118, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 118, 0.2).
narrative_ontology:measurement(clb_public_health_primary_tr_t120, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 120, 0.24).

% Extraction over time
narrative_ontology:measurement(clb_public_health_primary_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(clb_public_health_primary_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(clb_public_health_primary_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(clb_public_health_primary_be_t60, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(clb_public_health_primary_be_t75, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(clb_public_health_primary_be_t90, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 90, 0.48).
narrative_ontology:measurement(clb_public_health_primary_be_t105, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 105, 0.55).
narrative_ontology:measurement(clb_public_health_primary_be_t112, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 112, 0.63).
narrative_ontology:measurement(clb_public_health_primary_be_t115, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 115, 0.74).
narrative_ontology:measurement(clb_public_health_primary_be_t118, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 118, 0.7).
narrative_ontology:measurement(clb_public_health_primary_be_t120, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 120, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(clb_public_health_primary_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(clb_public_health_primary_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(clb_public_health_primary_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(clb_public_health_primary_su_t60, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(clb_public_health_primary_su_t75, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(clb_public_health_primary_su_t90, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 90, 0.44).
narrative_ontology:measurement(clb_public_health_primary_su_t105, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 105, 0.5).
narrative_ontology:measurement(clb_public_health_primary_su_t112, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 112, 0.58).
narrative_ontology:measurement(clb_public_health_primary_su_t115, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 115, 0.76).
narrative_ontology:measurement(clb_public_health_primary_su_t118, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 118, 0.68).
narrative_ontology:measurement(clb_public_health_primary_su_t120, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 120, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate legitimacy' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that share one kernel (coercion_legitimacy_boundary) but differ in epsilon, victim sets, and failure modes. This story is the public_health_primary member (general collective-outweighing warrant; unvaccinated objectors as victims; immunocompromised as protected beneficiaries). bodily_autonomy_primary is the categorical-prohibition member (empty coerced-subject warrant; all compulsion illegitimate). proportionality_reading is the severity-calibration member (victim set partitioned by pathogen). Upstream/downstream structure: this reading's general warrant is the doctrinal authority that proportionality implementations cite and operate within (influences edge), while bodily_autonomy_primary is its logical contradictory (forecloses edge). All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
