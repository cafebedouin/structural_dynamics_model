% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Coercion Legitimacy Scaled by Disease Severity (Proportionality Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of the
 *   coercion-legitimacy boundary kernel: the claim that medical coercion
 *   (vaccination mandates) is justified proportionally to disease severity
 *   and transmission dynamics. High-R0, high-mortality pathogens like measles
 *   (R0~15, mortality ~0.2%) justify mandatory vaccination; low-R0,
 *   low-mortality pathogens like seasonal influenza (R0~1-2, mortality ~0.1%)
 *   do not. The reading grounds legitimacy in measurable epidemiological
 *   parameters, creating a principled boundary between justified collective
 *   action and extractive bodily infringement. It sits between two sibling
 *   readings: the bodily-autonomy reading (which rejects coercion
 *   categorically) and the public-health-primary reading (which permits
 *   coercion whenever collective benefit exceeds individual cost, regardless
 *   of disease severity). The proportionality reading claims to thread this
 *   needle: protecting medical autonomy where severity does not justify
 *   override, while permitting coercion where genuine collective harm
 *   requires it.
 *
 * KEY AGENTS:
 *   - Public health authorities (agenda-setters): set and enforce disease-specific thresholds; benefit from stable demand for mandatory vaccination; bear responsibility for boundary-drawing accuracy.
 *   - Vaccine-hesitant individuals (payers): bear bodily integrity cost; accept mandates for measles, resist for flu; have constrained exit options.
 *   - Disease-naive and immunocompromised populations (beneficiaries): depend on herd immunity; cannot exit the disease risk; benefit from mandatory vaccination of others.
 *   - Medical autonomy advocates (payers/excluded): argue for categorical medical autonomy; see proportionality as partial compromise but insufficient protection.
 *   - Exemption seekers (payers): navigate uncertain boundary-drawing, bearing cognitive and legal cost of adjudication.
 *   - Epidemiologists (observers): provide severity/transmission data that feeds threshold-setting; remain analytically positioned but influence the boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.58).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.62).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Coercion Legitimacy Scaled by Disease Severity (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'd1cafe25-5024-4468-b9a4-15e09db3d7d4').
narrative_ontology:cs_kernel_codification('d1cafe25-5024-4468-b9a4-15e09db3d7d4', formalized).
narrative_ontology:cs_authority_grounding('d1cafe25-5024-4468-b9a4-15e09db3d7d4', extraction).
narrative_ontology:cs_interpretation_layer_present('d1cafe25-5024-4468-b9a4-15e09db3d7d4').
narrative_ontology:cs_reading_relation('d1cafe25-5024-4468-b9a4-15e09db3d7d4', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('d1cafe25-5024-4468-b9a4-15e09db3d7d4', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('d1cafe25-5024-4468-b9a4-15e09db3d7d4', foundational, coercion_legitimacy_scales_with_severity).
narrative_ontology:cs_axiom_status(coercion_legitimacy_scales_with_severity, holdable).
narrative_ontology:cs_axiom_grounding('d1cafe25-5024-4468-b9a4-15e09db3d7d4', coercion_legitimacy_scales_with_severity, empirically_contingent).
narrative_ontology:cs_axiom('d1cafe25-5024-4468-b9a4-15e09db3d7d4', foundational, measurable_epidemiological_parameters_govern_boundaries).
narrative_ontology:cs_axiom_status(measurable_epidemiological_parameters_govern_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('d1cafe25-5024-4468-b9a4-15e09db3d7d4', measurable_epidemiological_parameters_govern_boundaries, instrumental).
narrative_ontology:cs_reference_frame('d1cafe25-5024-4468-b9a4-15e09db3d7d4', proportionality_scaled_medical_authority).
narrative_ontology:cs_drift_state('d1cafe25-5024-4468-b9a4-15e09db3d7d4', post_pandemic_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d1cafe25-5024-4468-b9a4-15e09db3d7d4', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, disease_naive_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, medical_autonomy_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_adverse_event_sufferers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces disease-specific coercion thresholds based on R0, mortality rate, and transmission dynamics. For measles (R0~12-18, mortality ~0.2%), mandatory vaccination is justified; for seasonal flu (R0~1-2, mortality ~0.1%), coercion is not. Administers exemption processes, surveillance, and enforcement machinery. Collects epidemiological data and adjusts thresholds as pathogen characteristics change.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Gain protection from high-transmission, high-severity diseases through mandatory vaccination of others. They cannot opt out of the disease risk; the mandate protects them by raising community immunity. Their only exit is geographic relocation to lower-prevalence regions, which is constrained by economic and legal barriers.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, disease_naive_populations, beneficiary,
    organized, biographical, constrained, national).

% Depend entirely on herd immunity from mandatory vaccination because they cannot be vaccinated themselves (medical contraindication). A disease like measles that is prevented by others' vaccination is their only defense against severe or fatal illness. They cannot exit this dependence; it is a structural feature of their medical condition.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Bear the cost of mandatory vaccination: needle access, time, bodily integrity infringement, and potential side effects (rare but real). For high-severity diseases they may accept the tradeoff; for mild diseases they experience the mandate as disproportionate. Their exit options include medical/religious exemptions (available but contentious), geographic relocation, or legal challenge—all costly and uncertain.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Argue that medical autonomy is categorically protected and that proportionality-scaling legitimizes the boundary-case coercions they oppose. They see the proportionality reading as capturing some autonomy protection (rejecting mandates for mild diseases) while still permitting coercion for severe diseases—a compromise they view as insufficient. Their voice is present in policy debates but often sidelined by the collective-benefit framing.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_autonomy_advocates, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, medical_autonomy_advocates, excluded).

% Navigate exemption processes (medical, religious, philosophical) seeking to avoid mandatory vaccination while remaining in their community. The proportionality reading creates case-by-case adjudication that leaves them uncertain: a measles mandate will likely hold, but a flu mandate might not. They bear the cognitive and legal cost of navigating the boundary; exclusion from public institutions follows non-vaccination regardless of the underlying pathogen severity.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, exemption_seekers, payer,
    powerless, biographical, constrained, local).

% Individuals who experience rare but severe vaccine adverse events carry permanent injury. The proportionality reading does not exempt them post-hoc; they are identifiable only after mandatory vaccination occurs. They cannot exit the constraint once the injury is realized, and compensation systems are typically limited. For high-severity diseases the reading permits their sacrifice; for borderline cases the reading's boundary-drawing becomes their lived extraction.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_adverse_event_sufferers, payer,
    powerless, biographical, trapped, local).

% Provide evidence and analysis on R0, mortality, transmission dynamics, and herd-immunity thresholds. They are asked to inform the boundary-drawing but remain analytically positioned; their evidence feeds the authorities' threshold-setting. Different epidemiologists may interpret the same data differently (e.g., is a novel pathogen's true R0 known or uncertain?), creating ambient ambiguity in the constraint's operation.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, disease_control_epidemiologists, observer,
    institutional, generational, analytical, global).

% Benefit from mandatory vaccination demand (stable, predictable purchase volume from governments). They are partially excluded from the constraint itself (they do not decide thresholds), but they have capacity to shape threshold discussions through research funding, advisory board presence, and regulatory relationships. The proportionality reading creates ongoing demand for severity-threshold data, which feeds their research agenda.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, pharmaceutical_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, pharmaceutical_manufacturers, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents high-transmission, high-severity diseases from circulating by requiring immunity across populations, protecting those who cannot be vaccinated and those too young to be vaccinated. The disease-severity threshold coordinates around a legitimate collective-harm problem: enabling immunity-dependent individuals to participate safely in society.
% TRANSFER_FUNCTION: Transfers bodily integrity infringement (vaccination needle, time, rare side effects) from hesitant individuals to the disease-control benefit collected by disease-naive and immunocompromised populations. The transfer is justified by the proportionality reading ONLY for high-severity pathogens; for low-severity pathogens it becomes pure extraction without legitimate collective benefit.
% ABSENT_VOICES: Pre-vaccination baseline populations (those who relied on natural immunity in earlier eras) are structurally absent; they would object to the premise that disease elimination justifies coercion. Individuals with genuine medical contraindications to vaccination but who are not immunocompromised are marginally present (they form a small residual category). Liability-bearing manufacturers' perspectives on acceptable risk are present only indirectly through regulatory capture; transparent manufacturer-risk-allocation negotiation is excluded.
% DISAPPEARANCE_RATIONALE: If this proportionality-scaled coercion constraint vanished, vaccination rates would drop, herd immunity thresholds would fail for high-R0 diseases, and disease circulation would resume. Measles, whooping cough, and polio would reemerge in unvaccinated clusters. The constraint's enforcement machinery (school-entry requirements, employment conditions, public-institution access rules) would need replacement or redesign. The boundary itself would disappear: policy would revert to either categorical coercion (all vaccines mandatory regardless of severity) or categorical autonomy (no vaccine mandates, regardless of disease severity).
% FOUNDING_PROBLEM: Vaccine-preventable diseases caused high childhood mortality and disability; immunity-dependent populations (infants, immunocompromised individuals) depended on herd immunity from others' vaccination. The founding problem required coordinating vaccination behavior across populations to reach thresholds above which disease transmission breaks. Early vaccine development was premised on collective benefit justifying individual bodily intervention.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the founding problem is live: measles and whooping cough still cause severe outcomes in unvaccinated populations, and immunity-dependent individuals remain at risk. Medical autonomy advocates and exemption-seeking communities attest the problem is partially solved (disease severity has declined in vaccinated populations) and the mandate persists beyond legitimate collective-harm grounds. Independent epidemiologists confirm measles remains high-severity (R0~15, mortality ~0.2%) while seasonal flu remains low-severity (R0~1-2, mortality ~0.1%). The boundary itself is contested: vaccine-hesitant communities argue even high-R0 diseases are controllable through other means; authorities argue the proportionality reading represents a principled compromise.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint coordinates legitimate collective benefit (disease control for high-R0 pathogens) while extracting bodily infringement from hesitant individuals. The extraction is NOT universal—it scales with pathogen severity, making it less extractive than a constraint that mandates all vaccines regardless of severity. Suppression is substantial (0.62) because enforcement depends on exclusion from schools, employment, and public institutions; the suppression is active and ongoing. Theater is low-to-moderate (0.28) because the severity-threshold reasoning is genuinely operative—authorities do refuse mandates for genuinely low-severity pathogens—but also because some enforcement activity defends institutional credibility and political stability beyond the disease-control function. The measurement series show extractiveness rising from 0.42 to 0.58 over the interval, reflecting increasing resistance to the constraint and authorities' tightening of enforcement in response. Theater peaks at t=24 (pandemic era, when fear-driven policy increases theatrical justification) then falls as post-pandemic empirical reassessment reduces the severity narrative. Accessibility collapse is high (0.71) because once the pathogen's severity is established, alternatives disappear: one cannot opt out of a measles mandate while remaining in the community; one can only exit geographically or legally challenge. Resistance is also high (0.73) because medical autonomy advocates and hesitant populations mount continuous pushback: legal challenges, exemption filing, and refusal campaigns are live and ongoing.
 *
 * PERSPECTIVAL GAP:
 *   Authority and beneficiary seats compute lower extractiveness; payer seats compute higher. Authority sees: measured epidemiological parameters driving justified coercion. Payer sees: bureaucratic threshold-setting justifying bodily infringement. The gap is structural, rooted in power asymmetry and control over what counts as 'sufficiently severe'.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are institutional and have high power relative to hesitant individuals. They set the boundary, enforce it, and bear no direct cost of vaccination themselves—their directionality is beneficiary-side (low d). Disease-naive populations are beneficiaries: they gain protection without incurring vaccination risk, and their exit options are constrained (they cannot avoid disease risk without vaccination)—their directionality is beneficiary-side (low d). Vaccine-hesitant individuals are victims: they bear bodily infringement, have constrained exit (exemptions exist but are increasingly contested), and their power is moderate but dispersed—their directionality is target-side (high d). Immunocompromised individuals are trapped: they cannot be vaccinated and depend entirely on others' vaccination for survival; they are beneficiaries but also the most vulnerable, with zero exit options. The proportionality reading creates directional asymmetry: for measles (high severity), hesitant individuals' high-d/target extraction is justifiable as the price of protecting disease-naive and immunocompromised populations; for flu (low severity), the same high-d extraction becomes unjustifiable because the collective benefit is negligible. The boundary-drawing itself becomes a directional fact: authorities have power to place the boundary; hesitant individuals must accept or navigate exemptions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy reclassification because it retains a genuine coordination function: disease control for immunocompromised and disease-naive populations is a real collective-action problem that mandatory vaccination solves. However, mandatrophy emerges asymmetrically across pathogens. For measles, the founding problem (protecting disease-naive populations from high-R0 transmission) remains live, and coercion is proportionate. For flu, the founding problem is substantially solved (flu is low-severity and non-novel), yet authorities continue mandatory vaccination campaigns justified by marginal risk reduction—the constraint persists without mandate grounding. The proportionality reading attempts to prevent mandatrophy by anchoring coercion legitimacy to measurable parameters. Theater-ratio rise from 0.12 to 0.28 suggests narrative theater is increasing (justifications becoming more rhetorical, less empirically grounded), a pre-mandatrophy signal. The dip at t=40 suggests post-pandemic de-escalation, indicating the constraint responds to public pressure when theater becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_ambiguity,
    'What epidemiological parameters define ''sufficiently severe'' to justify coercion? Where is the boundary between high-R0/high-mortality (justified) and low-R0/low-mortality (unjustified)?',
    'Prospective rulemaking: authorities publish explicit R0 and mortality thresholds that trigger mandatory vaccination. Compare actual policy decisions against these thresholds to detect whether severity-scaling is operationally real or rhetorical cover for uniform coercion.',
    'If authorities cannot or will not specify thresholds, the proportionality reading collapses into public-health-primary (coercion whenever authorities claim collective benefit). If thresholds are specified but inconsistently applied, the constraint operates as a snare with a proportionality-stage to legitimize case-by-case extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Operational definition and consistency of severity-threshold enforcement.').

omega_variable(
    kernel_axiom_conflict,
    'Can proportionality-scaling coexist with categorical medical autonomy, or does permitting any coercion foreclose autonomy-primary readings?',
    'Normative analysis: does a principled proportionality position (no coercion for low-severity diseases, mandatory coercion for high-severity) coherently defend medical autonomy, or does permitting any state-mandated medical intervention violate the axiom that bodily autonomy is inviolable?',
    'If proportionality and bodily-autonomy axioms are incompatible, this reading forecloses the autonomy-primary reading within any single coherent framework. If they coexist in different parties'' commitments, the kernel exhibits genuine contestation with no logical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_axiom_conflict, conceptual, 'Whether proportionality-scaled coercion is logically compatible with categorical medical autonomy protection.').

omega_variable(
    pathogen_emergence_vs_retroactivity,
    'When a novel pathogen (e.g., COVID-19) emerges with unknown severity and transmission dynamics, does the proportionality reading permit provisional coercion until severity is established, or does uncertainty prevent justified coercion?',
    'Case study: examine COVID-19 mandate history. Were initial mandates imposed under uncertainty (provisional coercion)? Were thresholds adjusted as severity became clearer? Were mandates that persisted beyond initial uncertainty justified under proportionality or under public-health-primary?',
    'If proportionality permits provisional coercion under uncertainty, the constraint functionally permits pre-emptive medical conscription that may exceed post-hoc justified limits. If uncertainty prevents coercion, the proportionality reading fails under novel-pathogen conditions that are the most consequential for public health.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathogen_emergence_vs_retroactivity, empirical, 'Whether proportionality-scaling permits coercion under epistemic uncertainty.').

omega_variable(
    measurement_capture_risk,
    'Do public health authorities and pharmaceutical manufacturers have structural incentives to classify pathogens as ''more severe'' than empirical data warrant, in order to justify coercion and ensure vaccine demand?',
    'Compare official severity estimates (R0, mortality) against independent epidemiological studies not funded by manufacturers or health authorities. Track how estimates change over time as new data arrives. Investigate funding relationships between authority scientists and manufacturers.',
    'If measurement capture is real, the constraint operates as a snare disguised as a proportionality-scaled rope: the severity boundary-drawing is rigged to permit maximum coercion. If measurement capture is absent, the constraint''s proportionality is operationally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_capture_risk, empirical, 'Whether severity-threshold determinations are subject to capture by beneficiaries.').

omega_variable(
    identity_lock_vaccine_hesitancy,
    'To what extent is vaccine hesitancy a rational response to constrained exit options versus an identity-fused rejection of medical authority?',
    'Longitudinal study: track hesitant individuals'' responses to explicit severity disclosure and clear exemption pathways. If hesitancy persists after exit options improve and severity is acknowledged to be low, identity-fusion is present. If hesitancy declines when proportionality is operationalized (no mandates for low-severity diseases), it reflects rational constraint response.',
    'If hesitancy is identity-locked, suppression is internalized and persists even after constraint removal; the measured suppression (0.62) understates true extraction. If hesitancy is rational constraint response, suppression is structural and would decline if coercion were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vaccine_hesitancy, empirical, 'Whether vaccine hesitancy reflects identity-fusion or rational constraint response.').

omega_variable(
    sibling_reading_committer,
    'This reading (proportionality) claims to coexist with bodily-autonomy and public-health readings. Are these genuinely coexistent positions held by different institutional actors, or do they logically foreclose each other?',
    'Examine actual policy disputes: do authorities defending proportionality-based mandates (for measles) simultaneously reject autonomy-primary challenges and public-health-primary mandates (for flu)? Or do they collapse toward one of the pure readings when pressed?',
    'If proportionality genuinely coexists with both siblings (authorities defend high-severity mandates while respecting low-severity autonomy), the kernel is contested but structurally coherent. If proportionality collapses toward one sibling under pressure, it is a cover story and the kernel exhibits foreclosure rather than contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_committer, conceptual, 'Whether proportionality-reading is a stable intermediate position or a rhetorical cover for underlying public-health or autonomy commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_prop_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(coer_prop_tr_t0, observed).
narrative_ontology:measurement(coer_prop_tr_t8, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(coer_prop_tr_t8, observed).
narrative_ontology:measurement(coer_prop_tr_t16, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(coer_prop_tr_t16, observed).
narrative_ontology:measurement(coer_prop_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(coer_prop_tr_t24, observed).
narrative_ontology:measurement(coer_prop_tr_t32, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(coer_prop_tr_t32, observed).
narrative_ontology:measurement(coer_prop_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(coer_prop_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(coer_prop_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(coer_prop_be_t0, observed).
narrative_ontology:measurement(coer_prop_be_t8, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(coer_prop_be_t8, observed).
narrative_ontology:measurement(coer_prop_be_t16, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(coer_prop_be_t16, observed).
narrative_ontology:measurement(coer_prop_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(coer_prop_be_t24, observed).
narrative_ontology:measurement(coer_prop_be_t32, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement_basis(coer_prop_be_t32, observed).
narrative_ontology:measurement(coer_prop_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(coer_prop_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_prop_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(coer_prop_su_t0, observed).
narrative_ontology:measurement(coer_prop_su_t8, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(coer_prop_su_t8, observed).
narrative_ontology:measurement(coer_prop_su_t16, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(coer_prop_su_t16, observed).
narrative_ontology:measurement(coer_prop_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(coer_prop_su_t24, observed).
narrative_ontology:measurement(coer_prop_su_t32, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(coer_prop_su_t32, observed).
narrative_ontology:measurement(coer_prop_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(coer_prop_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel. The bodily_autonomy_primary reading anchors legitimacy entirely to individual consent; the public_health_primary reading anchors it entirely to collective benefit; the proportionality_reading (this constraint) anchors it to measured pathogen severity and transmission dynamics. Each reading has distinct ε, distinct beneficiary/victim structures, and distinct classification. The three stories are linked via network.affects_constraints; each documents the alternative readings in its cs_structure.reading_relations block. The kernel contest is not resolvable by metrics—it is a normative disagreement about what grounds medical coercion legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
