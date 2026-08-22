% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality-Bounded Immunization Requirement Authority
 *   domain: public health ethics/constitutional law/medical autonomy
 *
 * SUMMARY:
 *   The colloquial label 'vaccine mandate legitimacy' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   constraints sharing the mandate_legitimacy_scope kernel; this story
 *   authors exactly one of them, the proportionality reading, as a clean
 *   single-epsilon constraint. The referent of epsilon is the standing
 *   landscape of immunization requirements as the proportionality reading
 *   itself assesses it: requirements for severe disease with safe, effective
 *   products and no less restrictive alternative sit near zero extraction
 *   (the measles side), while requirements for mild disease, products with
 *   unfavorable risk profiles, or goals achievable by testing and masking
 *   carry the extraction (the influenza-side and universal-adult side).
 *   Averaged across the landscape the reading lands mid-range, and the victim
 *   set is conditional on pathogen parameters — the expected structural
 *   delta. The sibling files instantiate the other readings with their own
 *   epsilon: the public_health_primary reading assesses the same landscape as
 *   substantially protective, and the bodily_autonomy_primary reading
 *   assesses any non-consensual compulsion as maximally extractive.
 *   Structurally, the necessity doctrine is the historical upstream that both
 *   siblings cite or reject; the proportionality reading disciplines its
 *   application downstream. All three are linked via
 *   network.affects_constraints. Interval mapping: t=0 corresponds to
 *   approximately 1905 (Jacobson v. Massachusetts), t=120 to 2025.
 *
 * KEY AGENTS:
 *   - - public_health_institutions: Agenda setter (institutional/constrained) — drafts the requirement schedule, runs exemption review, enforces by exclusion and penalty
 *   - - vaccine_manufacturers: Primary material beneficiary (powerful/arbitrage) — guaranteed demand with liability insulation
 *   - - immunocompromised_patients: Protection beneficiary (powerless/trapped) — relies on surrounding coverage, gains nothing from mild-disease requirements
 *   - - disproportionately_mandated_individuals: Conditional target (powerless/constrained) — bears requirements that fail the severity, safety, or alternatives conditions
 *   - - conscientious_exemption_seekers: Identity-bound target (powerless/identity_locked) — objection is constitutive, exit means exiting the conviction
 *   - - frontline_workers_with_prior_infection: Excluded challenger (organized/constrained) — holds the strongest tailoring argument with no procedural seat
 *   - - constitutional_courts: Analytical observer (institutional/analytical) — sets how demanding the tailoring inquiry is
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.55).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.52).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Bounded Immunization Requirement Authority").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public health ethics/constitutional law/medical autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '97a43659-8308-4ee7-9ef7-7e0dfc533136').
narrative_ontology:cs_kernel_codification('97a43659-8308-4ee7-9ef7-7e0dfc533136', distributed).
narrative_ontology:cs_authority_grounding('97a43659-8308-4ee7-9ef7-7e0dfc533136', lineage).
narrative_ontology:cs_interpretation_layer_present('97a43659-8308-4ee7-9ef7-7e0dfc533136').
narrative_ontology:cs_reading_relation('97a43659-8308-4ee7-9ef7-7e0dfc533136', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('97a43659-8308-4ee7-9ef7-7e0dfc533136', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('97a43659-8308-4ee7-9ef7-7e0dfc533136', foundational, compulsion_requires_disease_severity_threshold).
narrative_ontology:cs_axiom_status(compulsion_requires_disease_severity_threshold, holdable).
narrative_ontology:cs_axiom_grounding('97a43659-8308-4ee7-9ef7-7e0dfc533136', compulsion_requires_disease_severity_threshold, empirically_contingent).
narrative_ontology:cs_axiom('97a43659-8308-4ee7-9ef7-7e0dfc533136', foundational, least_restrictive_alternative_prerequisite).
narrative_ontology:cs_axiom_status(least_restrictive_alternative_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('97a43659-8308-4ee7-9ef7-7e0dfc533136', least_restrictive_alternative_prerequisite, instrumental).
narrative_ontology:cs_axiom('97a43659-8308-4ee7-9ef7-7e0dfc533136', secondary, risk_benefit_balance_conditions_legitimacy).
narrative_ontology:cs_axiom_status(risk_benefit_balance_conditions_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('97a43659-8308-4ee7-9ef7-7e0dfc533136', risk_benefit_balance_conditions_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('97a43659-8308-4ee7-9ef7-7e0dfc533136', bounded_police_power_tailored_compulsion).
narrative_ontology:cs_drift_state('97a43659-8308-4ee7-9ef7-7e0dfc533136', post_pandemic_mandate_retrenchment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97a43659-8308-4ee7-9ef7-7e0dfc533136', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, high_risk_elderly_adults).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_institutions).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, disproportionately_mandated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, conscientious_exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, parents_of_schoolchildren).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, police_power_proportionality_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design school-entry and workplace immunization requirements, decide which diseases trigger them, run exemption review, and enforce compliance through exclusion and penalty. Each upheld requirement enlarges their administrative reach; each narrowed one costs them caseload and standing. They cannot simply stop administering the programs without ceding the requirement apparatus back to legislatures.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Cannot receive certain vaccines or mount adequate responses, so their protection comes from the coverage around them. Requirements aimed at severe, transmissible disease are the difference between ordinary life and isolation; requirements aimed at mild disease confer little extra protection on them while still costing others. They cannot exit exposure by relocating away from pathogens.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, national).

% Face elevated mortality from respiratory and vaccine-preventable disease and organize through advocacy groups to press for coverage requirements. Age and chronic illness tie them to their exposure environment; moving or opting out is not available to them.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, high_risk_elderly_adults, beneficiary,
    organized, biographical, trapped, national).

% Sell every dose the requirement schedule guarantees and operate under compensation schemes that channel injury claims into administered funds rather than tort suits. Demand follows the requirement calendar rather than consumer choice, and production capacity can shift across jurisdictions and products.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Must produce compliance documentation for school entry, manage appointment schedules and records, and absorb the cost of the alternatives — private school, home instruction — if they decline. Their leverage concentrates in periodic legislative fights over exemption categories.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, parents_of_schoolchildren, payer,
    moderate, biographical, constrained, national).

% Are subject to requirements whose underlying disease is mild, whose product carries nontrivial adverse-event risk, or whose stated goal could be met by testing or masking — and have no individualized proceeding in which to argue their own case, because the requirements attach by category rather than by assessment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, disproportionately_mandated_individuals, payer,
    powerless, biographical, constrained, national).

% Hold objections tied to their convictions about bodily integrity and medical authority; seeking exemption marks them within their communities and workplaces. Relinquishing the objection would mean acting against the conviction itself, so the practical alternatives to compliance are private school, home instruction, or changing jobs.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, conscientious_exemption_seekers, payer,
    powerless, biographical, identity_locked, regional).

% Carry documented infection-derived protection yet fall outside statutory exemption categories, so testing-based arrangements that would fit their situation are unavailable to them. They pressed this through unions and litigation during the recent requirement wave and were answered with category rules rather than individual assessment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, frontline_workers_with_prior_infection, excluded,
    organized, biographical, constrained, national).

% Hear challenges to immunization requirements, decide how demanding the tailoring inquiry is, and set the doctrinal boundary between permissible compulsion and overreach. Their rulings reshape what agencies draft the next session; they bear no compliance burden themselves.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises population immunity above transmission thresholds for severe vaccine-preventable disease, solving the free-rider problem in which individual risk calculus underweights each person's contribution to protecting those who cannot be protected directly.
% TRANSFER_FUNCTION: Moves vaccination-decision authority and residual risk-bearing from individuals to collective management; moves compliance costs — scheduling, adverse-event risk, penalty and exclusion exposure — onto required persons; and converts required doses into guaranteed purchases and liability insulation for manufacturers, plus enforcement authority for agencies.
% ABSENT_VOICES: People with documented prior infection or contraindications falling outside statutory exemption categories have no per-person hearing: the tailoring judgment is applied categorically at the statute and policy level, so the individual bearing a disproportionate requirement cannot litigate her own parameters. Immunocompromised patients who cannot benefit from coverage against a given pathogen likewise hold no seat when that pathogen's requirement is drafted.
% DISAPPEARANCE_RATIONALE: If the proportionality condition vanished overnight, mandate disputes would collapse toward the sibling positions: jurisdictions following the necessity-first reading would impose compulsion irrespective of pathogen parameters, autonomy-first jurisdictions would lose the doctrinal basis for any compulsion, and the existing patchwork of conditional requirements, exemption channels, and judicial tailoring would reorganize around whichever pole captured each forum.
% FOUNDING_PROBLEM: Bounding the police power exposed by early compulsory-vaccination statutes: the Jacobson-era Court upheld compulsion against smallpox while flagging that arbitrary or oppressive exercises — requirements untethered to threat severity — would exceed authority. The proportionality condition was built to separate epidemic-scale threats warranting compulsion from endemic mild disease where persuasion and targeted measures suffice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the Supreme Court's own limiting language in Jacobson, the rejection of an untailored universal adult requirement in NFIB v. OSHA, constitutional scholarship on police-power limits, and the bioethics literature on least restrictive means all attest the founding problem and its continuing liveness. Public health agencies also attest it, but they sit inside the benefiting set and so carry no corroborating weight here.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.55 is the landscape-average the reading's own lights produce: severe-disease requirements approach coordination cost while mild-disease and category-wide requirements extract well above it, and the share of the latter has grown across the interval. Suppression 0.52 is authored as a raw structural property — compulsion backed by school exclusion and employment condition, but with exemption channels and jurisdictional variation leaving alternatives partly open; it is deliberately not scaled by anything here, since only extractiveness is scaled downstream. Theater 0.32 reflects a real clinical function increasingly wrapped in compliance ritual (documentation regimes, attestation paperwork). Accessibility collapse 0.35: private schooling, home instruction, relocation, and testing-based regimes remain visible and usable at meaningful cost, so alternatives are degraded, not eliminated. Resistance 0.6: sustained litigation, recurring exemption legislation, and organized worker pushback. Claim and metrics are independent authored facts: the tangled_rope claim rests on structure (genuine coverage coordination plus a conditional asymmetric burden plus active enforcement), while the metric values rest on the descriptive record; where the engine's per-seat computations diverge from the claim, that divergence is data. The measurement series share one grid (t = 0, 20, 40, 60, 80, 100, 120) so every tracked metric is authored at every examined point. The suppression_requirement series is intentionally cyclical rather than monotone: smallpox-era coercive enforcement, mid-century relaxation as uptake normalized, school-entry tightening, the pandemic spike, and post-spike retrenchment. The cycle is not noise — crisis phases are precisely when tailoring discipline lapses and disproportionate requirements enter, so the spike phase of the cycle is itself the mechanism by which overbroad requirements accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently from the same structural record. From the immunocompromised patient's position the requirement schedule is a lifeline and its enforcement is welcome; from the disproportionately mandated individual's position the identical enforcement machinery operates as uncompensated imposition with no hearing; from the manufacturer's position it is a demand guarantee; from the court's position it is a doctrine to be calibrated. The engine computes these divergences from the structural data — the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (immunocompromised_patients, high_risk_elderly_adults, public_health_institutions) derive directionality toward the beneficiary end; declared victims (disproportionately_mandated_individuals, conscientious_exemption_seekers) derive toward the target end, with the identity_locked exit of the latter sitting nearer full-target than the merely constrained former. Parents of schoolchildren sit mid-range: real compliance cost, real protection received. Vaccine manufacturers derive strongly beneficiary-side — they collect on every dose regardless of whether the underlying requirement survives tailoring. Frontline workers with prior infection are positioned by role=excluded rather than by a victims declaration; no directionality override is authored because the override surface keys on power atoms, and the two organized seats in this story (high_risk_elderly_adults, beneficiary-side; frontline_workers_with_prior_infection, target-side) share the organized atom, so any single override would distort one of them. The story accepts the derivation's imprecision on that seat rather than fabricating a lever the schema does not cleanly offer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — new pathogens recurrently reopen the severity question, and the R5 mismatch flag should not fire (status=live paired with world_rearranges is the coherent cell). The tangled_rope classification is what prevents mislabeling in both directions: reading the landscape as pure coordination (the necessity-first temptation) hides the conditional victim set and licenses scope creep; reading it as pure extraction (the autonomy-first temptation) erases the severe-disease coordination that immunocompromised patients depend on for ordinary life. The temporal series shows the actual risk mode: not mandate atrophy but extraction accumulation — base_extractiveness rising monotonically across the interval as requirements extended beyond severe disease, with suppression spiking episodically. That is drift within a live mandate, not a dead mandate kept alive theatrically, so mandatrophy_resolved is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pathogen_parameter_variability,
    'Which pathogens'' requirements actually satisfy the severity, safety-efficacy, and less-restrictive-alternatives conditions, and how does the victim set shift as variant parameters move?',
    'Pathogen-specific proportionality audits scoring each requirement against measured severity, vaccine effectiveness and adverse-event profiles, and the feasibility of testing or masking substitutes.',
    'Membership in the victim set flips with parameters: a requirement above the severity threshold coordinates and its subjects are not victims; below it, the same machinery extracts. The landscape-level epsilon of 0.55 is an average over a distribution whose tails differ widely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_parameter_variability, empirical, 'Conditional victim set: epsilon varies by pathogen parameters.').

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the mandate_legitimacy_scope kernel; how would instantiating a sibling reading instead change the victim set and epsilon?',
    'Comparative classification across the three reading files of the kernel: classify each reading independently and compare victim sets, epsilon, and computed types.',
    'Under bodily_autonomy_primary every compelled person enters the victim set and epsilon approaches maximum; under public_health_primary victims appear only where the protective function fails and epsilon drops toward coordination cost; the proportionality reading''s conditional victim set is the intermediate structure this file authors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: reading-relative victim sets and epsilon within the shared kernel.').

omega_variable(
    exemption_channel_genuineness,
    'Do medical and religious exemption processes operate as genuine exits or as gauntlets whose delay, documentation burden, and denial rates functionally close them?',
    'Audit of exemption request approval rates, turnaround times, and documentation requirements across jurisdictions, compared against the underlying statutory entitlement.',
    'If the channels are gauntlets, the authored suppression of 0.52 understates effective coercion, and the trapped/identity_locked characterization extends beyond conscientious seekers to nominally exemptible classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_channel_genuineness, empirical, 'Whether nominal exemption exits are real or ceremonial.').

omega_variable(
    natural_immunity_equivalence,
    'Does documented prior infection provide protection equivalent to vaccination for the purposes of a given requirement?',
    'Head-to-head protection studies comparing infection-derived and vaccine-derived immunity by pathogen and variant.',
    'If equivalent, the disproportionately_mandated class is far larger than statute recognizes — every prior-infected person subject to a no-testing requirement joins the victim set — raising effective extraction above the authored 0.55.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_immunity_equivalence, empirical, 'Size of the unrecognized less-restrictive-alternative class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(mand_tr_t20, observed).
narrative_ontology:measurement(mand_tr_t40, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(mand_tr_t40, observed).
narrative_ontology:measurement(mand_tr_t60, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(mand_tr_t60, observed).
narrative_ontology:measurement(mand_tr_t80, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(mand_tr_t80, observed).
narrative_ontology:measurement(mand_tr_t100, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 100, 0.29).
narrative_ontology:measurement_basis(mand_tr_t100, observed).
narrative_ontology:measurement(mand_tr_t120, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 120, 0.32).
narrative_ontology:measurement_basis(mand_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(mand_be_t20, observed).
narrative_ontology:measurement(mand_be_t40, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(mand_be_t40, observed).
narrative_ontology:measurement(mand_be_t60, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement_basis(mand_be_t60, observed).
narrative_ontology:measurement(mand_be_t80, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement_basis(mand_be_t80, observed).
narrative_ontology:measurement(mand_be_t100, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 100, 0.51).
narrative_ontology:measurement_basis(mand_be_t100, observed).
narrative_ontology:measurement(mand_be_t120, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement_basis(mand_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement_basis(mand_su_t20, observed).
narrative_ontology:measurement(mand_su_t40, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(mand_su_t40, observed).
narrative_ontology:measurement(mand_su_t60, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement_basis(mand_su_t60, observed).
narrative_ontology:measurement(mand_su_t80, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement_basis(mand_su_t80, observed).
narrative_ontology:measurement(mand_su_t100, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement_basis(mand_su_t100, observed).
narrative_ontology:measurement(mand_su_t120, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(mand_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, resource_allocation).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the single colloquial label 'vaccine mandate legitimacy' decomposes into three structurally distinct constraints sharing the mandate_legitimacy_scope kernel, linked pairwise via affects_constraints. The public_health_primary reading is the historical upstream (Jacobson-era necessity doctrine) that both siblings cite or reject; this proportionality reading disciplines its application downstream; the bodily_autonomy_primary reading rejects the necessity premise outright. Epsilon differs across the family by construction: low where the protective function is genuine (public_health_primary's assessment), moderate and pathogen-conditional here, maximal for any compulsion (bodily_autonomy_primary's assessment). Each file authors its own epsilon, beneficiaries, victims, and type; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
