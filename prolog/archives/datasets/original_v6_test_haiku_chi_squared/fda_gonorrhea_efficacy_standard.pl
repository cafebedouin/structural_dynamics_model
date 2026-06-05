% ============================================================================
% CONSTRAINT STORY: fda_gonorrhea_efficacy_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_gonorrhea_efficacy_standard, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fda_gonorrhea_efficacy_standard
 *   human_readable: FDA Unified Efficacy Standard for Gonorrhea Treatment
 *   domain: regulatory/pharmaceutical/public_health
 *
 * SUMMARY:
 *   The FDA's unified efficacy standard for gonorrhea treatment (exemplified
 *   by rejection of a novel oral antibiotic despite clinical promise) creates
 *   a structural constraint that simultaneously coordinates drug safety and
 *   extracts from patients, developers, and public health systems. The
 *   standard requires microbiological cure in ≥90% of subjects in controlled
 *   trials with sample sizes of at least 100 per arm. This threshold was
 *   established decades ago when gonorrhea susceptibility to antibiotics was
 *   near-universal; today, with widespread resistance, the standard operates
 *   in a fundamentally changed epidemiological context. The advisory panel's
 *   rejection reflects adherence to the standard even when it conflicts with
 *   public health need — patients with resistant infections lack treatment
 *   options, and developers of promising new antibiotics face approval delays
 *   that may make development uneconomical. The constraint exhibits
 *   properties of a Tangled Rope: it coordinates legitimate functions
 *   (ensuring new drugs actually work, preventing approval of ineffective
 *   treatments) while extracting from those outside the coordination
 *   framework (patients already resistant, developers with novel approaches,
 *   the broader public health system trying to manage resistance).
 *
 * KEY AGENTS:
 *   - Patients with resistant gonorrhea: Primary victim (powerless/trapped) — have no exit from the constraint; bear full cost of treatment limitation
 *   - Novel antibiotic developers: Secondary victim (moderate/constrained) — face high development costs and delayed market entry; constrained by FDA approval requirement
 *   - Existing antibiotic manufacturers: Primary beneficiary (institutional/arbitrage) — protected from competitor entry by high standard; can shift to other markets if pressured
 *   - FDA regulatory authority: Powerful institutional actor (powerful/mobile) — exercises gate-keeping power; experiences constraint as coordination mandate; can adjust standards but is politically constrained
 *   - Public health systems: Organized victim (organized/constrained) — benefit from safety coordination but extracted from by narrow treatment options; constrained by FDA authority
 *   - Clinical trial infrastructure: Institutional maintainer (institutional/arbitrage) — preserves performative efficacy standard through inertia; beneficiary of standardization
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent standard as immutable scientific requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, 0.58).
domain_priors:suppression_score(fda_gonorrhea_efficacy_standard, 0.72).
domain_priors:theater_ratio(fda_gonorrhea_efficacy_standard, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, extractiveness, 0.58).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_gonorrhea_efficacy_standard, tangled_rope).
narrative_ontology:human_readable(fda_gonorrhea_efficacy_standard, "FDA Unified Efficacy Standard for Gonorrhea Treatment").
narrative_ontology:topic_domain(fda_gonorrhea_efficacy_standard, "regulatory/pharmaceutical/public_health").

domain_priors:requires_active_enforcement(fda_gonorrhea_efficacy_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, existing_antibiotic_manufacturers).
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, regulatory_agency_authority).
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, clinical_trial_standardization).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, patients_with_resistant_infections).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, novel_antibiotic_developers).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, public_health_treatment_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESISTANT-INFECTION PATIENT (SNARE) — No exit from the constraint. Standard-of-care oral antibiotics have failed. The patient faces limited options: expensive IV/IM cephalosporin, prolonged treatment, or risk of untreated infection complications. Cannot choose a different regulatory regime. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NOVEL ANTIBIOTIC DEVELOPER (TANGLED ROPE) — Victim of extraction: high efficacy standard raises development costs and time-to-market. But also benefits from coordination: the standard ensures their eventual product (if approved) will have credible efficacy claims and market exclusivity. Constrained exit (product viability depends on FDA approval). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXISTING ANTIBIOTIC MANUFACTURER (ROPE) — Primary beneficiary. High efficacy standard delays/blocks competitor entry, protecting their market position and pricing power. Experiences constraint as coordination of market access and quality assurance. Arbitrage exit available (shift to other markets, other drugs). d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FDA REGULATORY AUTHORITY (TANGLED ROPE) — Experiences constraint as coordination (ensuring drug safety/efficacy), but also exercises extraction (gate-keeping power, standard-setting authority, industry control). Powerful position with mobile exit (can adjust standards). But also constrained by congressional mandate and public health pressure. d≈0.45, f(d)≈0.55, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH SYSTEM (TANGLED ROPE) — Benefits from coordination: high efficacy standard ensures approved drugs actually work. But extracted from: limited treatment options, narrowed clinical pathways, rising costs of non-oral alternatives. Organized (can petition for standard revision) but constrained (bound by FDA authority). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLINICAL TRIAL INFRASTRUCTURE (PITON) — Maintains a high efficacy standard that persists partly through institutional inertia. The standard is performative in a key sense: microbiological cure rates (the primary efficacy measure) don't perfectly predict clinical outcomes or resistance development. The constraint persists because the alternative (outcome-based standards accounting for resistance dynamics) is harder to measure and would destabilize the entire approval apparatus. theater_ratio=0.65 reflects this performative quality. d≈0.15, f(d)≈0.08, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN?) — From a civilizational view, some efficacy standard is inherent to any medical approval system: you cannot approve drugs without evidence they work. The constraint appears as an immutable natural law. However, the structural data (ε=0.58, suppression=0.72, beneficiaries, victims) contradicts mountain classification — the specific standard chosen (90% microbiological cure rate, single-dose, n≥100 trials) is contingent institutional design, not law of nature. This is a false summit.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_gonorrhea_efficacy_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fda_gonorrhea_efficacy_standard, TR),
    TR >= 0.70.

:- end_tests(fda_gonorrhea_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The standard extracts from several groups: patients with resistant infections lack options; developers face high barriers; public health systems have narrowed treatment pathways. But the extraction is not maximal because the standard also performs genuine coordination (prevents approval of ineffective drugs, ensures drug quality). The 0.58 value reflects that the standard's coordination function is real but increasingly misaligned with epidemiological reality — as resistance rises, the standard extracts without proportional coordination benefit. The trajectory from 0.35 (0-year value) to 0.58 (30-year value) reflects increasing misalignment: the standard was reasonable when resistance was rare; it becomes extractive as resistance becomes common. Suppression (0.72): High. Significant barriers to alternative approaches: (a) changing the standard requires regulatory reform that is slow and risky; (b) developers cannot easily exit by selling to other regulators (many international regulators harmonize with FDA); (c) patients cannot exit by choosing non-approved drugs without assuming legal and medical risk. Theater ratio (0.65): Moderate-high. The efficacy standard is performative in a key sense: microbiological cure rates in controlled trials don't perfectly predict real-world clinical outcomes or resistance development. The standard persists because outcome-based alternatives (e.g., measuring long-term resistance in treated populations) are harder to execute and would destabilize the entire trial infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a severe perspectival gap between beneficiaries and victims. The existing antibiotic manufacturer sees a Rope (coordination, market protection, legitimate efficacy assurance). The FDA sees a Tangled Rope (coordination mandate with gate-keeping power). The public health system sees a Tangled Rope (coordination benefit but extraction via limited options). The novel developer sees a Snare (high extraction, constrained exit). The resistant-infection patient sees a Snare (no exit, no choice). The clinical trial infrastructure sees a Piton (performative, sustained by inertia). The analytical observer risks seeing a Mountain (naturalized as inherent to medicine). The gap reveals that the 'beneficiary' (existing manufacturers, trial infrastructure) experience genuine coordination, while the 'victims' (developers with new approaches, patients needing alternatives) experience pure extraction. The standard is not inherently coordination or extraction — it depends on whether resistance prevalence makes it adaptive or maladaptive.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing antibiotic manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They have exit options (other drugs, other markets) and benefit from the standard's market protection. FDA regulatory authority: Mixed (coordination function + gate-keeping power) + mobile → d≈0.45, f(d)≈0.55. Moderate extraction. Can adjust standards but politically constrained. Public health system: Victim (limited options) + organized but constrained → d≈0.52, f(d)≈0.68. Significant extraction. Novel developers: Victim (high barriers) + constrained (must have FDA approval) → d≈0.68, f(d)≈1.05. High extraction. Resistant-infection patients: Victim (no options) + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Clinical trial infrastructure: Institutional + arbitrage → d≈0.15, f(d)≈0.08. Net beneficiary; maintains standard through inertia (piton). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risks naturalizing contingent standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially unresolved (mandatrophy_resolved: false). The core question: is the FDA standard genuine coordination (preventing ineffective drugs from harming patients) or extraction (protecting incumbent manufacturers and trial infrastructure at the expense of patients with resistant infections and developers with novel approaches)? The answer is: both, and context-dependent. When resistance prevalence is low (historical baseline), the standard is primarily coordination with side effects of market protection. When resistance prevalence is high (current status), the standard is primarily extraction justified by outdated coordination logic. The constraint has crossed a threshold where its original rationale no longer holds, but the institutional infrastructure (trial design, regulatory procedures, manufacturer lobbying) maintains it. The mandatrophy is resolved only if one of: (1) the FDA revises the standard to account for resistance dynamics (sunset the current standard, adopt a new one); (2) resistance rises to the point where the standard's harm becomes undeniable and political pressure forces revision; (3) alternative regulators (EU, other nations) adopt lower standards, fragmenting the global approval landscape and reducing the FDA's gate-keeping power. Until one of these occurs, the constraint remains an unresolved mandatrophy: labeled Tangled Rope (coordination + extraction) but increasingly functioning as a Snare (extraction justified by increasingly-distant coordination rationale).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resistance_dynamics_threshold,
    'What level of resistance prevalence should trigger a revision of the efficacy standard to include resistance-aware measures?',
    'Longitudinal surveillance of resistance rates; correlation between approved drug efficacy (by current standard) and clinical treatment failure in the field',
    'If threshold is already crossed: current standard is dangerously inadequate, and the extraction mechanism is severe (false assurance of drug efficacy). If threshold is distant: current standard is precautionary and appropriate, reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resistance_dynamics_threshold, empirical, 'Resistance prevalence threshold for standard revision').

omega_variable(
    microbiological_vs_clinical_alignment,
    'How well do microbiological cure rates (the primary standard measure) predict clinical treatment success and prevent resistance emergence in the population?',
    'Post-approval surveillance data; comparison of microbiological outcomes in trials vs. real-world clinical outcomes; population-level resistance emergence curves',
    'If poorly aligned: the standard is theater (performative, not functional), elevating the piton perspective. If well-aligned: the standard is genuine coordination, reducing piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(microbiological_vs_clinical_alignment, empirical, 'Alignment of microbiological cure rates with clinical outcomes').

omega_variable(
    alternative_standard_feasibility,
    'Could an alternative efficacy standard (e.g., resistance-adjusted, outcome-based, adaptive trial designs) be implemented without destabilizing the entire drug approval apparatus?',
    'Pilot implementation of alternative standard in regulatory framework; cost-benefit analysis vs. current standard; international precedent review',
    'If feasible: the current standard is a chosen constraint, not a necessary one, increasing mandatrophy. If infeasible: the current standard is locked in by infrastructure, increasing piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_standard_feasibility, conceptual, 'Feasibility of alternative efficacy standards').

omega_variable(
    market_entry_barriers_causation,
    'How much of the delayed/blocked market entry for novel antibiotics is due to the high efficacy standard vs. other factors (manufacturing scale-up, patent/IP timing, clinical trial logistics)?',
    'Developer interviews; comparative analysis of approval timelines for antibiotics vs. other drug classes; country-level variation in standards',
    'If standard is primary barrier: extraction mechanism is clear and severe. If secondary: extraction is real but smaller, and supply constraints are shared with other domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_entry_barriers_causation, empirical, 'Causal weight of efficacy standard on market entry barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_gonorrhea_efficacy_standard, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fda_gono_tr_t0, fda_gonorrhea_efficacy_standard, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fda_gono_tr_t15, fda_gonorrhea_efficacy_standard, theater_ratio, 15, 0.55).
narrative_ontology:measurement(fda_gono_tr_t30, fda_gonorrhea_efficacy_standard, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(fda_gono_be_t0, fda_gonorrhea_efficacy_standard, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fda_gono_be_t15, fda_gonorrhea_efficacy_standard, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(fda_gono_be_t30, fda_gonorrhea_efficacy_standard, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_gonorrhea_efficacy_standard, enforcement_mechanism).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, antibiotic_resistance_emergence).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, drug_development_timeline_pressure).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, alternative_treatment_access).

% DUAL FORMULATION NOTE:
% The FDA gonorrhea efficacy standard is downstream of the broader antibiotic resistance crisis (antibiotic_resistance_emergence) but represents a distinct regulatory constraint. The standard's increasing extractiveness is driven by epidemiological change (rising resistance prevalence) that makes the original coordination logic obsolete. Constraint family decomposition: (1) the original standard (ε≈0.25, primarily coordination) from when resistance was rare; (2) the current standard (ε≈0.58, mixed) as resistance rises; (3) a hypothetical revised standard (ε≈0.15, primarily coordination) that accounts for resistance dynamics. These are distinct constraints with different ε values because the observable (resistance prevalence in the population being treated) changes the structural relationship between the standard and treated populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fda_gonorrhea_efficacy_standard, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
