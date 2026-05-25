% ============================================================================
% CONSTRAINT STORY: pharmaceutical_safety_obfuscation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_safety_obfuscation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pharmaceutical_safety_obfuscation
 *   human_readable: Pharmaceutical Safety Obfuscation Through Regulatory Capture and Data Control
 *   domain: pharmaceutical/regulatory/health
 *
 * SUMMARY:
 *   Pharmaceutical safety obfuscation operates as a structural constraint
 *   where manufacturers maintain control over safety data, regulatory
 *   agencies become captured by industry funding and revolving-door
 *   employment, and patients and prescribers face information asymmetries and
 *   suppressed exit options. The constraint combines genuine coordination
 *   requirements (drug development requires massive capital and regulatory
 *   verification) with extractive mechanisms (data control, selective
 *   publication, regulatory capture). The theater ratio has increased from
 *   0.48 to 0.68 over the measurement interval, indicating that the
 *   performative components of safety verification (journal publications,
 *   regulatory committee presentations, marketing claims) have grown faster
 *   than functional safety mechanisms (independent verification, adverse
 *   event detection, post-market accountability). The extractiveness has
 *   increased from 0.35 to 0.58, reflecting both the accumulation of rents
 *   through regulatory pathways and the hardening of capture mechanisms as
 *   financial dependencies deepen.
 *
 * KEY AGENTS:
 *   - Patients and Consumers: Primary victims (powerless/trapped) — depend on medications; structurally locked in by therapeutic necessity; bear full cost of safety failures
 *   - Prescribing Physicians: Secondary victims (moderate/constrained) — face liability exposure and information asymmetry; constrained by reliance on manufacturer-provided clinical data
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (powerful/arbitrage) — control trial data, funding streams, and regulatory access; generate extraction through captured approval pathways
 *   - Regulatory Agencies (FDA, EMA, others): Captured beneficiaries (institutional/arbitrage) — maintain gatekeeping function but rely on manufacturer data and funding; capture rents through accelerated approval fees
 *   - Academic Research Community: Degraded verification actor (institutional/mobile) — once independent; now theatrically compliant due to pharmaceutical funding of research and education
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes both genuine coordination function and extractive layering; cannot resolve from single observable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_safety_obfuscation, 0.58).
domain_priors:suppression_score(pharmaceutical_safety_obfuscation, 0.72).
domain_priors:theater_ratio(pharmaceutical_safety_obfuscation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pharmaceutical_safety_obfuscation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_safety_obfuscation, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_safety_obfuscation, "Pharmaceutical Safety Obfuscation Through Regulatory Capture and Data Control").
narrative_ontology:topic_domain(pharmaceutical_safety_obfuscation, "pharmaceutical/regulatory/health").

domain_priors:requires_active_enforcement(pharmaceutical_safety_obfuscation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_safety_obfuscation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_safety_obfuscation, regulatory_agencies_captured).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, patients_and_prescribers).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, public_health_infrastructure).
narrative_ontology:constraint_victim(pharmaceutical_safety_obfuscation, independent_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT CONSUMER (SNARE) — Structurally trapped. Patients face unavoidable dependency on medications vetted by captured regulatory systems. No exit option exists for those requiring specific therapeutic classes. Information asymmetry is maximal: manufacturers control trial data, regulatory summaries are incomprehensible to lay consumers, adverse event reporting is fragmented and inaccessible. Patients bear full extraction cost without agency.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRESCRIBING PHYSICIAN (SNARE) — Constrained by liability exposure, medical training that emphasizes manufacturer-provided literature, and reliance on pharmaceutical representatives for clinical updates. High-quality independent safety data is expensive and time-consuming to access. Physicians are structurally trapped between manufacturer marketing and patient trust. Exit option is severely constrained: abandoning pharmaceutical treatment is not clinically feasible for most conditions.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (TANGLED ROPE) — Genuine coordination function exists: manufacturers develop complex drugs requiring massive capital investment. The regulatory system must verify safety and efficacy before market entry — this is legitimate coordination. However, asymmetric extraction layer: manufacturers control clinical trial data, can cherry-pick favorable studies for publication, lobby for regulatory loosening, and capture agency personnel. The manufacturer experiences the constraint as enabling coordination (innovation incentives) while extracting from downstream actors.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Faces genuine coordination requirements: must balance innovation incentives with public safety. Captured dynamics introduce asymmetry: funding relationships with industry, revolving-door employment, reliance on manufacturer-submitted safety data, and pressure to approve drugs quickly. The agency experiences the constraint as coordination (drug approval is necessary) while capturing rents through accelerated pathways (priority review fees) and post-approval enforcement leverage.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC RESEARCH COMMUNITY (PITON) — Once an independent verification mechanism, academic pharmacology has degraded into performative compliance. Academic researchers depend on pharmaceutical funding for grants; manufacturers fund medical schools and continuing education; publication bias favors positive findings funded by pharma. Independent safety research persists but is theatrically marginalized. The mechanism is largely inert — academic independence that once provided real verification has atrophied through financial capture.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, pharmaceutical safety coordination is necessary and solves a real collective action problem: information asymmetries between manufacturers and consumers require institutional gatekeeping. The coordination function is real. However, the analytical perspective cannot resolve whether the observed obfuscation is inherent to coordination or extractive rent-seeking layered on top. The base extraction value (0.58) suggests mixed function rather than pure coordination.
constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_safety_obfuscation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_safety_obfuscation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_safety_obfuscation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_safety_obfuscation, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_safety_obfuscation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Manufacturers extract substantial rents through regulatory leverage, data control, and exclusivity periods. The extraction is not maximal (snare-level 0.70+) because genuine coordination still occurs — drug development and safety verification are real functions. However, the extraction exceeds coordination by a significant margin. Suppression (0.72): High. Multiple suppression mechanisms operate in parallel: information asymmetry (manufacturers control trial data and design), regulatory capture (agency reliance on industry submissions), liability barriers (physicians face malpractice risk if they question manufacturer claims), and market barriers (patients cannot easily access independent safety information or choose alternatives within therapeutic classes). Theater ratio (0.68): High and rising. Performative elements include: journal publications funded by manufacturers with publication bias, regulatory committee presentations emphasizing approval timelines over safety comprehensiveness, marketing campaigns disguised as patient education, and medical school curricula funded by pharmaceutical companies. Independent functional verification (adverse event tracking, long-term outcome monitoring, comparative effectiveness research) persists but is theatrically marginalized and underfunded. The rising trajectory indicates that performative components are outpacing functional ones.
 *
 * PERSPECTIVAL GAP:
 *   The Snare perspective (patients/prescribers) experiences pure extraction because exit is suppressed and information asymmetry is maximal. The Rope perspective (manufacturers) experiences coordination because they control the mechanism and benefit from it. The Tangled Rope perspective (regulators) experiences both coordination (approving drugs is necessary) and extraction (captured by manufacturer preferences). The Piton perspective (academic researchers) experiences their own degradation — they once provided independent verification but now perform theater. The analytical perspective faces the Oracle Gap: the constraint cannot be classified from a single position because its essence IS the asymmetry between positions. The question 'is this Rope or Snare?' has no single answer — the constraint IS a Tangled Rope because it produces different classifications from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural position relative to the constraint. Patients/prescribers are victims with trapped or constrained exit: structural mobility exists (they could theoretically exit by refusing pharmaceuticals) but is blocked by therapeutic necessity and information barriers — d ≈ 0.90. Manufacturers are beneficiaries with arbitrage exit: they can choose which markets to enter, which trials to conduct, which data to publish — they have structural mobility and use it to their advantage — d ≈ 0.10. Regulatory agencies are nominally beneficiaries (they gate-keep access to markets) but are partially captured: they have some mobility (could strengthen data requirements) but face pressure to approve quickly — d ≈ 0.35. Academic researchers have mobile exit options (could refuse pharma funding) but are identity-locked into pharmaceutical funding streams (career and institution survival depend on research funding) — d ≈ 0.65. The directionality distribution — trapped victims and identity-locked researchers vs. arbitrage-enabled manufacturers — creates the asymmetric extraction signature that justifies Tangled Rope classification at the moderate and institutional power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The claimed_type is Tangled Rope, but the analytical observer's perspective potentially classifies the constraint as Rope (coordination-only) if viewed through the lens of 'pharmaceutical innovation requires these institutional arrangements.' The mandatrophy is resolvable only by empirical comparison: if independent regulatory systems (e.g., European regulators with less industry capture) produce equivalent safety outcomes with lower extractiveness, then obfuscation is extractive rent-seeking, and Tangled Rope is correct. If independent systems produce worse outcomes (longer approval times, less innovation), then obfuscation may be necessary, and the constraint is higher-order Rope. The measurement trajectory (extractiveness rising from 0.35 to 0.58, theater rising from 0.48 to 0.68) suggests that extraction is accumulating faster than coordination function improves, which supports Tangled Rope over pure Rope. However, the rising trajectory itself could indicate either increasing extraction or increasing analytical visibility into existing extraction. Omega variables are required to disambiguate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_transparency_vs_ip_protection,
    'How much of pharmaceutical safety obfuscation is necessary IP protection vs. extractive data control?',
    'Comparative analysis of pharmaceutical transparency across regulatory regimes with different IP enforcement levels. Meta-analysis of drug safety outcomes in systems with vs. without mandatory trial data registration.',
    'If obfuscation is mainly IP protection: constraint should reclassify as lower-extraction Rope. If mainly extractive: remains high-extraction Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_transparency_vs_ip_protection, empirical, 'Whether safety obfuscation is necessary for IP protection or extractive data control').

omega_variable(
    regulatory_capture_vs_expertise_delegation,
    'Is regulatory reliance on manufacturer data inherent to technical complexity or evidence of capture?',
    'Examination of regulatory agencies with differential manufacturer funding levels; analysis of approval timelines and safety recalls in high vs. low capture jurisdictions; audit of regulator-industry employment flows.',
    'If inherent to complexity: moderate Tangled Rope throughout. If capture: manufacturers classify as extractors (higher d), regulators as captured beneficiaries (lower d despite institutional power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_vs_expertise_delegation, empirical, 'Whether regulatory data reliance reflects technical necessity or institutional capture').

omega_variable(
    patient_harm_attribution_lag,
    'What lag exists between drug approval and detection of serious adverse events? Is this lag inherent or exploited?',
    'Longitudinal analysis of safety event timelines: phase 3 trial detection rates vs. post-market surveillance detection rates. Comparison with efficacy signals detected in the same trials. Analysis of trial design features that minimize adverse event detection.',
    'If lag is inherent to statistical power: moderate theater and suppression. If exploited (small trial n, short follow-up, narrow outcome capture): high theater and suppression, stronger case for extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_harm_attribution_lag, empirical, 'Whether safety event detection lag is inherent or exploited').

omega_variable(
    identity_lock_in_medical_training,
    'To what extent are prescribers identity-locked into reliance on manufacturer-controlled information sources?',
    'Survey and interview data on prescriber awareness of alternative information sources and perceived barriers to using them. Analysis of prescriber behavior change when given access to independent safety databases. Examination of medical school curricula and continuing education funding sources.',
    'If prescribers are constrained (high cost to switch): moderate extraction from this perspective. If identity-locked (cannot see alternatives as legitimate): higher suppression, different classification outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_medical_training, empirical, 'Whether prescribers are constrained or identity-locked into manufacturer information').

omega_variable(
    open_pharmacovigilance_feasibility,
    'Can distributed, crowd-sourced adverse event reporting provide safety verification comparable to or better than centralized regulatory systems?',
    'Pilot programs testing decentralized pharmacovigilance; comparison of detection sensitivity and specificity against FDA MedWatch. Analysis of false positive rates and action thresholds in crowd-sourced systems.',
    'If feasible and scalable: constraint becomes temporary (Scaffold). If limited by false positives or gaming: remains entrenched (Tangled Rope or Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_pharmacovigilance_feasibility, empirical, 'Whether decentralized pharmacovigilance can replace centralized safety systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_safety_obfuscation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_safety_tr_t0, pharmaceutical_safety_obfuscation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pharma_safety_tr_t5, pharmaceutical_safety_obfuscation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(pharma_safety_tr_t10, pharmaceutical_safety_obfuscation, theater_ratio, 10, 0.68).
narrative_ontology:measurement(pharma_safety_tr_t15, pharmaceutical_safety_obfuscation, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(pharma_safety_be_t0, pharmaceutical_safety_obfuscation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharma_safety_be_t5, pharmaceutical_safety_obfuscation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pharma_safety_be_t10, pharmaceutical_safety_obfuscation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(pharma_safety_be_t15, pharmaceutical_safety_obfuscation, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_safety_obfuscation, resource_allocation).
narrative_ontology:boltzmann_floor_override(pharmaceutical_safety_obfuscation, 0.2).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, clinical_trial_design_bias).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, post_market_surveillance_gaps).
narrative_ontology:affects_constraint(pharmaceutical_safety_obfuscation, regulatory_approval_timelines).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific trial design biases and post-market surveillance failures. Each downstream constraint has its own ε reflecting domain-specific obfuscation mechanisms; this story captures the structural architecture that enables and sustains obfuscation across all downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_safety_obfuscation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
