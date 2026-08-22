% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy — Public Health Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the vaccine_mandate_legitimacy
 *   kernel: the public_health_primacy_reading, which holds that the state's
 *   duty to prevent collective harm justifies mandate authority and treats
 *   unvaccinated status as an externality others may lawfully price. The ε
 *   referent is the standing arrangement under contest — the actual mandate
 *   regime as it operates (school-entry requirements, workplace rules,
 *   exclusion and penalty machinery) — assessed by this reading's own lights,
 *   never the voluntary or rights-respecting arrangement this reading's
 *   opponents would install. The claim and the metrics are independent
 *   authored facts: claimed_type is tangled_rope because the structure
 *   genuinely coordinates a collective-action problem (herd immunity) while
 *   asymmetrically extracting from identifiable refusers under active
 *   enforcement; the metrics describe observed operation without being tuned
 *   to that claim. The colloquial label 'vaccine mandate legitimacy'
 *   decomposes into three structurally distinct constraints (this reading
 *   plus the bodily_autonomy and risk_stratification siblings), linked via
 *   network.affects_constraints; each carries its own ε, victim set, and
 *   axioms. KEY AGENTS (by structural relationship): -
 *   public_health_bureaucracy: agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — writes and enforces the rules, collects
 *   authority and appropriation - vaccine_refusers: primary target
 *   (moderate/constrained) — bear termination, exclusion, fines -
 *   religious_conscience_objectors: secondary target
 *   (moderate/identity_locked) — bear identical exclusions; exit equals
 *   abandoning the belief - vaccinated_majority: coordinated beneficiary with
 *   diffuse cost share (moderate/constrained) - immunocompromised_patients:
 *   pure beneficiary (powerless/trapped) — protected by others' compliance -
 *   employers_and_school_administrators: enforcement intermediaries
 *   (powerful/constrained) - infection_recovered_individuals: excluded voice
 *   (moderate/constrained) — hold serological counter-evidence, no seat -
 *   constitutional_courts: analytical observer (institutional/analytical) —
 *   adjudicate the legitimacy boundary
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: agenda-setter and primary beneficiary (institutional/arbitrage) — writes and enforces the rules, collects authority and appropriation
 *   - vaccine_refusers: primary target (moderate/constrained) — bear termination, exclusion, fines; a committed core is effectively identity-locked
 *   - religious_conscience_objectors: secondary target (moderate/identity_locked) — exit equals abandoning the belief itself
 *   - vaccinated_majority: coordinated beneficiary with diffuse cost share (moderate/constrained)
 *   - immunocompromised_patients: pure beneficiary (powerless/trapped) — protected by others' compliance
 *   - employers_and_school_administrators: enforcement intermediaries (powerful/constrained) — administer exclusion, absorb friction
 *   - infection_recovered_individuals: excluded voice (moderate/constrained) — serological counter-evidence, no seat in rule-setting
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicate the legitimacy boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.44).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy — Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e6b7f573-bcb6-458b-ae33-1479aa53b9b6').
narrative_ontology:cs_kernel_codification('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', formalized).
narrative_ontology:cs_authority_grounding('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', lineage).
narrative_ontology:cs_interpretation_layer_present('e6b7f573-bcb6-458b-ae33-1479aa53b9b6').
narrative_ontology:cs_reading_relation('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', foundational, collective_harm_duty_justifies_medical_coercion).
narrative_ontology:cs_axiom_status(collective_harm_duty_justifies_medical_coercion, holdable).
narrative_ontology:cs_axiom_grounding('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', collective_harm_duty_justifies_medical_coercion, instrumental).
narrative_ontology:cs_axiom('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', secondary, unvaccinated_status_is_priceable_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_priceable_externality, holdable).
narrative_ontology:cs_axiom_grounding('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', unvaccinated_status_is_priceable_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', collective_harm_prevention_supremacy).
narrative_ontology:cs_drift_state('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', post_pandemic_litigation_retrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e6b7f573-bcb6-458b-ae33-1479aa53b9b6', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_conscience_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_school_administrators).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, herd_immunity_collective_action_theory).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, externality_internalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts mandate rules, adjudicates exemptions, and enforces school-entry and workplace requirements through health departments. Receives budget appropriations, statutory authority expansions, and staffing tied to program administration, and can redirect enforcement intensity across jurisdictions and diseases. Exit would mean ceding statutory authority, which no internal career path rewards.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Cannot receive certain vaccines for medical reasons and depend on neighbors' coverage for protection. They receive the mandate's protection without paying its compliance costs, and cannot exit either their medical condition or their exposure to community transmission.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, regional).

% Complies with required schedules for children's school entry and for many jobs. Pays through taxes funding programs, appointment time, and small personal risk, and receives reduced infection probability in return. Reversing course individually means losing school and workplace access, so exit is costly once compliance is established.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority, payer).

% Decline required vaccines on secular-cautious grounds. Bear employment termination, school exclusion for children, fines where levied, and social stigma; narrow medical exemptions rarely reach them. Alternatives — homeschooling, job change, relocation — carry heavy costs. A committed core treats refusal as non-negotiable, while a larger fringe responds to price signals.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Decline on faith or conscience grounds, supported by organized litigation networks. The objection is constitutive of their commitments — complying would mean abandoning the belief itself. They bear the same exclusions as secular refusers and fund legal challenges through congregational and civil-liberty organizations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_conscience_objectors, payer,
    moderate, generational, identity_locked, national).

% Implement verification, exclusion, and reporting duties under health-department rules. Absorb administrative cost, workforce loss, and litigation exposure. Cannot opt out of statutory duties but lobby over their shape and pace.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_school_administrators, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_school_administrators, payer).

% Have documented prior infection with antibody evidence and are mandated identically to never-infected persons under blanket rules. Hold serological data arguing equivalent protection but had no seat in rule-setting; their exclusion from the conversation is what blanket design presupposes.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, infection_recovered_individuals, excluded,
    moderate, biographical, constrained, national).

% Adjudicate mandate challenges under police-power and religious-liberty doctrines. Neither collect nor pay; their rulings redefine enforcement boundaries — striking the federal employer mandate while upholding state exclusion regimes — shifting the constraint's effective reach.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in infectious-disease control: individual vaccination decisions underweight collective protection, so voluntary uptake undersupplies coverage relative to herd-immunity thresholds. Universal requirements push coverage past the threshold, solving a genuine collective-action problem.
% TRANSFER_FUNCTION: Moves compliance, autonomy, and fines from refusers to the state and public health apparatus; moves risk reduction diffusely to the whole population; moves authority, budget, and staffing to public health institutions.
% ABSENT_VOICES: Infection-recovered individuals holding serological evidence of equivalent protection, denied medical-exemption applicants, and disability-rights advocates concerned with exclusionary enforcement were outside the rule-setting room; blanket design presupposed their absence.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, coverage would fall below thresholds in pockets, outbreaks would recur in schools and workplaces, private institutions would improvise their own admission rules, and litigation would shift from mandate challenges to negligence claims — the disease-control economy would reorganize around whatever voluntary and private substitutes emerged.
% FOUNDING_PROBLEM: Uncontrolled epidemic disease — smallpox, then measles and polio — producing mass mortality and school closures, with free-rider dynamics preventing voluntary coverage from reaching protective thresholds; addressed by early compulsory-vaccination statutes upheld in Jacobson v. Massachusetts (1905).
% FOUNDING_PROBLEM_CORROBORATION: Historical vital statistics and WHO eradication records attest the founding disease burden from outside the benefiting parties, and Jacobson-era court records accepted it as real. Contemporary civil-liberties litigators and health-policy analysts attest the legacy problems are substantially solved while disputing the present necessity of blanket mandates — corroboration for the problem's historical reality is strong; corroboration for its current liveness is disputed from outside the beneficiary set.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.44 at interval end) is moderate by this reading's lights: the bulk of the burden on refusers is priced as legitimate externality internalization, with residual extraction located in blanket application to naturally immune and negligible-risk persons, enforcement beyond demonstrated transmission risk, and authority accumulation. Suppression (0.58) is a raw structural property, unscaled by power or scope: the legal machinery (exclusion, termination, fines, emergency powers) exists and is used, concentrating on the refuser seats. Theater (0.24) is low-moderate — campaigns and enforcement are mostly functional, with a symbolic layer that spiked during the pandemic-emergency phase. Accessibility collapse (0.5) and resistance (0.6) fit a tangled rope: costly but real alternatives persist, and organized resistance (litigation coalitions pooling otherwise-moderate refuser power) struck the federal employer mandate and rolled back several state regimes. The measurement series run on ONE shared time grid (t=0..12, mapping to calendar years 2013–2025) with every tracked metric authored at every point. The trajectory is cyclical, not monotonic: a flat routine phase (t0–t6), crisis intensification at t8 (employer mandates, passports, emergency powers), then retrenchment (t10–t12) after judicial strike-down and state legislative bans. The oscillation is driven by exogenous epidemic shocks, not intermittent reinforcement — but each cycle leaves residue: the post-cycle extractiveness floor (0.44) sits above the pre-cycle level (0.31), an authority ratchet visible across cycles. Base properties are measured at interval end, post-retrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the bureaucracy's seat the arrangement is coordination it built and staffs — rope-like, subsidized, low directionality. From the refuser seats the same structure operates as enforced extraction with concentrated costs — snare-flavored, high directionality, amplified further for the identity-locked objectors whose exit is unthinkable without abandoning the belief. The vaccinated majority sits near symmetric: genuine protection received, diffuse costs paid. Courts occupy the analytical seat and see a constitutional tradeoff rather than a type. Coalition capacity matters for the powerless-adjacent seats: refusers individually hold only moderate power, but pooled litigation coalitions achieved concrete reversals, which is why resistance (0.6) is real rather than nominal.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the bureaucracy (agenda-setter collecting authority and budget) sits nearest the beneficiary pole; immunocompromised patients are pure beneficiaries with trapped exit; the vaccinated majority sits mildly beneficiary-side with a diffuse cost share. Victim declarations map to high directionality: vaccine_refusers bear the concentrated costs with constrained exit, placing them near the full-target end; religious_conscience_objectors, identity-locked, sit nearest the pole. Employers and schools are enforcement intermediaries with a secondary payer position — mid-range. National spatial scope raises verification difficulty, which the engine scales into effective extraction; suppression stays unscaled. Infection_recovered_individuals hold the excluded seat: no directionality until admitted to the conversation, which is precisely the structural fact their exclusion encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Calling the mandate a snare erases the genuine coordination function — coverage thresholds demonstrably bind outbreak frequency, so the coordination story is not cover. Calling it a rope erases the identifiable victims bearing concentrated costs under active enforcement. Tangled_rope holds both facts: coordination function plus asymmetric extraction requiring enforcement. Piton is avoided because the function is demonstrably live (the founding_problem_status is contested, not dead — new pathogens revive the original problem), and the theater ratio never approaches performative dominance. The R5 mismatch consumer reads founding_problem_status x disappearance_verdict: contested x world_rearranges produces no dead-but-persisting zombie flag, correctly — the arrangement persists because arrangements still depend on it, while the parties dispute whether the ORIGINAL problem or a bureaucratic successor sustains it. mandatrophy_resolved is deliberately not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_instantiation_of_kernel,
    'This constraint is the public_health_primacy_reading of the vaccine_mandate_legitimacy kernel; what structural delta would govern if a sibling reading were instantiated instead?',
    'Compile the sibling stories side-by-side: bodily_autonomy_primacy moves every mandate subject into the victim set and drives epsilon toward the coercion ceiling; risk_stratification shrinks victims to blanket-mandated low-risk persons and splits the arrangement into targeted (coordination-heavy) and blanket (extraction-heavy) components.',
    'Sibling instantiation changes the victim-set boundary, the epsilon value, and potentially the computed type; this file''s classification holds only for the public_health_primacy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_of_kernel, conceptual, 'Kernel-membership and sibling-delta record for the committer frame.').

omega_variable(
    epsilon_reading_indexing,
    'Epsilon here is authored by the public_health_primacy reading''s lights over the fixed referent of the standing mandate arrangement; how much of the measured extraction is reading-indexed rather than referent-indexed?',
    'Cross-read the three sibling stories over the same referent; divergence in authored epsilon isolates the reading-indexed component (OQ-26: epsilon is a property of a reading, not a topic).',
    'Expected wide spread (bodily_autonomy near ceiling, risk_stratification mid-high, this reading moderate) is signal about the readings, not noise about the arrangement; comparisons must hold the referent fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_indexing, conceptual, 'Reading-indexed epsilon over a shared referent.').

omega_variable(
    externality_vs_extraction_boundary,
    'Is the burden imposed on refusers legitimate externality internalization (this reading''s frame) or extraction riding on a coordination function?',
    'Identify minimal-coercion designs achieving statistically equal coverage — targeted incentives, testing-and-masking alternatives, serology-informed rules; coercion surplus above the minimum is candidate extraction.',
    'If equal coverage is achievable at materially lower coercion, the surplus is extraction and the computed type drifts snare-ward; if not, the burden is the price of the coordination itself and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_vs_extraction_boundary, conceptual, 'Boundary between Pigouvian correction and rent in mandate design.').

omega_variable(
    natural_immunity_equivalence,
    'Does documented prior infection confer protection equivalent to vaccination for mandate-relevant outcomes?',
    'Pooled cohort studies comparing reinfection and transmission rates in recovered versus vaccinated populations, stratified by variant.',
    'If equivalent, mandating recovered individuals is extraction without coordination gain and epsilon rises; if inferior, blanket inclusion is justified and the externality axiom stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_immunity_equivalence, empirical, 'Empirical premise beneath the blanket-application design.').

omega_variable(
    refuser_identity_lock_fraction,
    'What fraction of refusers are identity-locked (refusal constitutive of worldview) versus price-responsive (would comply under sufficient cost)?',
    'Compliance elasticity studies across enforcement intensities and post-mandate conversion tracking.',
    'A large locked fraction raises the suppression cost of any enforcement path and stabilizes the tangled_rope reading; a small one implies mandates mostly price compliance and the extraction surface is thinner than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuser_identity_lock_fraction, empirical, 'Heterogeneity of the refuser seat''s exit condition.').

omega_variable(
    bureaucratic_authority_tracking,
    'Does the bureaucracy''s accrued authority and appropriation track epidemiological need or budget persistence?',
    'Audit program budgets and staffing against disease-burden time series across the interval; test whether authority expansions reverse when burden falls.',
    'Authority persisting past burden decline supports the capture hypothesis inside the tangled_rope and pushes the computed type snare-ward; full reversal supports pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_authority_tracking, empirical, 'Whether the beneficiary seat''s gain tracks function or self-perpetuation.').

omega_variable(
    herd_threshold_variability,
    'Are herd-immunity thresholds stable, reachable parameters for the diseases in question, or behavior-dependent moving targets?',
    'Seroprevalence and transmission modeling per pathogen across behavioral regimes.',
    'Unreachable thresholds weaken the coordination-function half of the tangled_rope and shift weight toward extraction; stable reachable thresholds anchor the coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(herd_threshold_variability, empirical, 'Stability of the scientific premise anchoring the coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement_basis(vacc_tr_t2, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(vacc_tr_t6, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(vacc_tr_t10, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2, 0.29).
narrative_ontology:measurement_basis(vacc_be_t2, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(vacc_be_t6, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(vacc_be_t10, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement_basis(vacc_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement_basis(vacc_su_t2, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(vacc_su_t6, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(vacc_su_t10, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(vacc_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'vaccine mandate legitimacy' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — a single story cannot carry one stable epsilon across readings that disagree on the victim-set boundary. This story (public_health_primacy) is the operative reading: courts historically adopted its frame (Jacobson lineage), so it sits UPSTREAM of the siblings, shaping the legitimacy conditions under which they operate — hence the influences edge to risk_stratification and the forecloses edge to bodily_autonomy (whose categorical prohibition contradicts this reading's justification premise in any single framework). Each member links to the others via network.affects_constraints; epsilon values differ by reading over the shared referent of the standing mandate arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
