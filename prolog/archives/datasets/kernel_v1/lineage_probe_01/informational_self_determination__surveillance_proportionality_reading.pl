% ============================================================================
% CONSTRAINT STORY: informational_self_determination__surveillance_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informational_self_determination__surveillance_proportionality_reading, []).

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
 *   constraint_id: informational_self_determination__surveillance_proportionality_reading
 *   human_readable: Informational Self-Determination as Surveillance Proportionality Auditor (Karlsruhe Reading)
 *   domain: legal/constitutional/surveillance
 *
 * SUMMARY:
 *   The Bundesverfassungsgericht's 1983 informational self-determination
 *   judgment and its 40-year engagement with surveillance architectures
 *   constitutes a singular institutional constraint on state monitoring
 *   capacity. This constraint is ONE READING of a contested constitutional
 *   kernel — the right to informational self-determination itself. The
 *   surveillance proportionality reading models the Court as a standing
 *   auditor: each retention scheme, each data category, each screening
 *   program is measured against constitutional limits and struck or narrowed.
 *   This is distinct from the census-origin reading (birth of the right as a
 *   refusal of databases) and the data-protection-constitutionalization
 *   reading (the right seeding a regulatory architecture). All three readings
 *   share the same kernel (informational self-determination) but instantiate
 *   different structural constraints with different extractiveness profiles,
 *   beneficiary/victim structures, and temporal dynamics. This reading
 *   focuses on the proportionality-auditing function: the constraint operates
 *   as serial suppression of bulk-collection programs, with benefit accruing
 *   to unsuspected surveilled populations and cost borne by dragnet security
 *   designs. The measurement trajectory shows extractiveness declining from
 *   0.68 (1983, when the constraint was new and security apparatus had
 *   maximum freedom to test boundaries) to 0.55 (2023, when the constraint
 *   has stabilized but operational escape has increased). Theater ratio has
 *   risen from 0.25 to 0.38, indicating increasing performativity — the
 *   constraint's formal authority has been maintained while operational
 *   practice has partially escaped the frame through classified exemptions
 *   and lower-court degradation.
 *
 * KEY AGENTS:
 *   - Bundesverfassungsgericht: Primary beneficiary and auditor (institutional/arbitrage) — captures institutional role expansion and jurisprudential authority; sees constraint as legitimate coordination of state power
 *   - Unsuspected Surveilled Populations: Primary beneficiary (powerless/trapped) — benefit from narrowed dragnet schemes but lack direct agency in constraint enforcement
 *   - Dragnet Security Architectures: Primary victim (powerful/constrained) — each judgment narrows operational scope while the apparatus adapts at boundaries
 *   - Civil Liberties Organizations: Secondary actor (moderate/constrained) — bear litigation costs while diffuse beneficiaries reap gains; benefit from precedent-building network effects
 *   - Intelligence Apparatus (Operational Level): Secondary victim (powerful/mobile) — experiences constraint as piton: formal compliance with public doctrine masks de facto operational escape through classified exemptions
 *   - Lower Courts and Administrative Agencies: Tertiary actor (powerful/constrained) — implement Court judgments but show measurable degradation in compliance and scope narrowing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informational_self_determination__surveillance_proportionality_reading, 0.55).
domain_priors:suppression_score(informational_self_determination__surveillance_proportionality_reading, 0.48).
domain_priors:theater_ratio(informational_self_determination__surveillance_proportionality_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informational_self_determination__surveillance_proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(informational_self_determination__surveillance_proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(informational_self_determination__surveillance_proportionality_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informational_self_determination__surveillance_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(informational_self_determination__surveillance_proportionality_reading, "Informational Self-Determination as Surveillance Proportionality Auditor (Karlsruhe Reading)").
narrative_ontology:topic_domain(informational_self_determination__surveillance_proportionality_reading, "legal/constitutional/surveillance").

domain_priors:requires_active_enforcement(informational_self_determination__surveillance_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(informational_self_determination__surveillance_proportionality_reading, 'a26ae04c-d5a7-45ae-bf28-6963b9c9fe70').
narrative_ontology:cs_kernel_codification('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', formalized).
narrative_ontology:cs_authority_grounding('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', lineage).
narrative_ontology:cs_interpretation_layer_present('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70').
narrative_ontology:cs_reading_relation('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', informational_self_determination__census_origin_reading, influences).
narrative_ontology:cs_reading_relation('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', informational_self_determination__data_protection_constitutionalized_reading, coexists_with).
narrative_ontology:cs_axiom('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', foundational, proportionality_is_constitutional_arbiter_of_surveillance).
narrative_ontology:cs_axiom_status(proportionality_is_constitutional_arbiter_of_surveillance, holdable).
narrative_ontology:cs_axiom_grounding('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', proportionality_is_constitutional_arbiter_of_surveillance, deontological).
narrative_ontology:cs_axiom('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', foundational, unsuspected_subjects_retain_dignity_across_bulk_collection).
narrative_ontology:cs_axiom_status(unsuspected_subjects_retain_dignity_across_bulk_collection, holdable).
narrative_ontology:cs_axiom_grounding('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', unsuspected_subjects_retain_dignity_across_bulk_collection, deontological).
narrative_ontology:cs_reference_frame('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', proportionality_auditing_authority).
narrative_ontology:cs_drift_state('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', contemporary_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a26ae04c-d5a7-45ae-bf28-6963b9c9fe70', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(informational_self_determination__surveillance_proportionality_reading, informational_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informational_self_determination__surveillance_proportionality_reading, unsuspected_surveilled_populations).
narrative_ontology:constraint_victim(informational_self_determination__surveillance_proportionality_reading, dragnet_security_architectures).
narrative_ontology:constraint_victim(informational_self_determination__surveillance_proportionality_reading, bulk_retention_schemes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSUSPECTED SURVEILLED SUBJECT (SNARE) — The individual whose communications are swept into dragnet schemes has no practical exit; retention decisions are made without their knowledge or consent. They experience the constraint as pure extraction of behavioral data with no coordination benefit. Maximum suppression: the mechanism's existence is often secret; the individual cannot meaningfully exit the German telecommunications network. The beneficiary of surveillance (security apparatus) captures value while the cost (privacy erosion, chilling effects) falls entirely on the unsuspected subject.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL LIBERTIES ORGANIZATIONS (TANGLED ROPE) — Constrained by resource limits and institutional capacity but benefiting from precedent-building and network effects in litigating successive schemes. Experience both genuine coordination (establishing evidentiary standards for proportionality challenges) and asymmetric extraction (bearing litigation costs while diffuse beneficiaries reap benefits). The constraint enforces cooperation through sequential challenge — each victory narrowing the next scheme — creating a coordination function (proportionality auditing) that simultaneously extracts asymmetric labor.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (ROPE) — Experiences the constraint as pure coordination: each judgment establishes criteria for evaluating the next scheme, building a jurisprudential standard that structures state behavior. The Court has arbitrage capacity — it can moderate or accelerate review — and sees the constraint as legitimate coordination of state power against constitutional limits. The court is the beneficiary of its own institutional role expansion; the constraint enables them to auditor surveillance architectures systematically. Low experienced extraction because the court's preferences align with the constraint's logic.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECURITY AND INTELLIGENCE APPARATUS (TANGLED ROPE) — Powerful but constrained by successive judicial narrowing. Each judgment extracts a cost (losing a retention scheme or data category) while the apparatus retains coordination benefits (knowing which schemes will survive review, which cannot be defended). Experiences the constraint as extraction (lost operational capacity) paired with genuine coordination (judicial review provides clarity on what works and what doesn't). The apparatus responds by designing new schemes at the boundary of the proportionality frame, engaging in iterative game with the court.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE INTELLIGENCE MANAGEMENT (PITON) — At the operational implementation level, the constraint has become largely performative: agencies continue surveillance operations while nominally complying with judicial limits; the gap between judgment and practice has widened. The constraint persists through institutional inertia and formal deference to Karlsruhe while operational practice has partially escaped the frame. Theater ratio high: public compliance theater masks de facto bulk operations conducted under classified frameworks. This reading is the piton perspective — the operationalization has decayed while the formal structure persists.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, surveillance is an inherent feature of state power, and the balance between security and privacy is an immutable structural tension. This perspective naturalizes the surveillance-proportionality constraint as a permanent feature of modern governance. However, the structural data (identifiable beneficiaries and victims, serial suppression of specific schemes rather than emergence of 'natural' balance) indicates this is a false summit: the constraint is a contingent institutional arrangement (Karlsruhe's specific doctrine and enforcement power) rather than a law of politics. The engine will flag this as FSM candidate.
constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informational_self_determination__surveillance_proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informational_self_determination__surveillance_proportionality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informational_self_determination__surveillance_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informational_self_determination__surveillance_proportionality_reading, TR),
    TR >= 0.70.

:- end_tests(informational_self_determination__surveillance_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high, declining trajectory. The constraint's extractiveness is substantial because the security apparatus loses real operational capacity — each judgment forecloses a retention scheme or data category. But extractiveness is not maximal (0.72+) because the apparatus retains coordination benefits: the proportionality frame provides clarity on what works and what doesn't, allowing iterative design at the boundary. The declining trajectory (0.68 → 0.55) reflects learning: early in the constraint's life, security schemes were built with little thought to proportionality testing; by 2023, the apparatus pre-screens designs for constitutionality, reducing the extraction shock. However, the decline plateaus at 0.55, not approaching rope-level (0.35), because the apparatus cannot escape the constraint entirely — it remains formally bound by Karlsruhe's proportionality frame. Suppression (0.48): Moderate. The Court's enforcement mechanism (constitutional review) is powerful but not total. Lower courts degrade Karlsruhe's holdings; classified operations claim state-security exemptions; the gap between judgment and operational practice has widened. Suppression is not high (0.60+) because the constraint's public face remains enforced and successive judgments do narrow schemes. Theater ratio (0.38): Moderate-low, rising trajectory. The proportionality constraint is not primarily performative — it produces real narrowing of surveillance schemes. But theater has increased from 0.25 to 0.38 as the gap between formal doctrine and operational practice has widened. The piton perspective captures this: at the operational level, compliance is increasingly theatrical. The initial low theater (0.25) reflected genuine Court power to restructure security apparatus practice; the rising theater reflects institutional inertia masking partial escape.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits all six DR types from different structural positions. The unsuspected surveilled subject sees a snare: extraction of behavioral data without consent or exit. Civil liberties organizations see tangled rope: genuine coordination (building proportionality standards) paired with asymmetric extraction (bearing litigation costs). The Court sees rope: legitimate coordination of state power through jurisprudential reasoning. The security apparatus sees tangled rope from the strategic level: both extraction (losing schemes) and coordination (knowing what works). At the operational level, the apparatus sees a piton: formal compliance with doctrine that masks de facto escape through classified exemptions and lower-court degradation. The analytical observer risks seeing a mountain: surveillance is a permanent structural feature of modern states, and proportionality is an immutable balance. The structural data reveals this as a false summit — the constraint is not a law of politics but a contingent institutional arrangement (Karlsruhe's specific doctrine and 40-year enforcement commitment). The Court's ability to continue auditing surveillance depends on generational stability of the doctrine and avoidance of classified exemption expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position in the extraction flow. Unsuspected subjects are pure beneficiaries (d~0.05) but trapped, yielding the snare classification from the powerless perspective. Civil liberties organizations are moderate victims of the constraint's asymmetric labor burden (d~0.65), constrained by resource limits, yielding tangled rope. The Court is a beneficiary with arbitrage capacity (d~0.15), experiencing low extraction because its institutional preferences align with the constraint. The security apparatus is a victim at the strategic level (d~0.70) but has arbitrage capacity to design at boundaries, yielding tangled rope. At operational level, the apparatus treats the constraint as piton (d changes based on classified escape routes — not captured in canonical derivation, but reflected in theater ratio rise). The analytical observer (d~0.72) would derive a snare classification but the mountain is forced by naturalizing assumptions in the observation frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by establishing that the surveillance-proportionality constraint is a genuine institutional structure (not an aspiration), maintaining both coordination function (proportionality standards clarify what survives review) and extraction (security apparatus loses schemes). The constraint is not pure coordination (rope) because the apparatus does not benefit equally — the beneficiaries (unsuspected subjects, diffuse public) are powerless and unaware, while the apparatus bears costs. It is not pure extraction (snare) because the Court and civil liberties organizations extract value from the coordination function and precedent-building. The tangled_rope classification at security apparatus and civil liberties levels is the correct summary. The snare classification from the unsuspected subject's perspective and rope classification from the Court's perspective are valid perspectival readings of the same constraint. The piton classification at operational level reflects the growing gap between formal doctrine and practice — the constraint's function has partially atrophied through escape routes (classified exemptions, lower-court degradation), but the formal structure persists through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_escape_velocity,
    'At what operational complexity level does the proportionality frame lose explanatory power? Do classified intelligence operations effectively escape Karlsruhe auditing?',
    'Structural analysis of Karlsruhe''s access to classified decision-making; comparison of public judgments against leaked/disclosed operational scope; investigation of exemptions claimed under state security privilege (Staatswohl) that bypass proportionality review',
    'If escape velocity is low (court can audit most schemes): constraint functions as genuine proportionality auditor (tangled_rope from security apparatus perspective holds). If escape velocity is high (classified ops exempt or shielded): constraint is degraded piton — formal authority over public schemes masks de facto dragnet at classified level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_escape_velocity, empirical, 'Whether classified operations escape proportionality review').

omega_variable(
    interagency_compliance_degradation,
    'Do lower courts and administrative agencies systematically narrow Karlsruhe''s holdings? Is there measurable drift between published precedent and operational implementation?',
    'Analysis of judicial review dockets for successive schemes; interviews with civil liberties litigators on compliance patterns; structural comparison of Karlsruhe judgment scope vs. actual agency practice across 20-year window',
    'If compliance is high and degradation is low: constraint sustains extractiveness suppression as intended (tangled_rope). If compliance is degraded: constraint operates as aspirational piton — Karlsruhe audits, but operational reality has escaped the frame. Theater ratio shifts accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interagency_compliance_degradation, empirical, 'Whether lower courts degrade Karlsruhe''s surveillance limits').

omega_variable(
    reading_contest_within_karlsruhe,
    'Does Karlsruhe''s own jurisprudence foreclose, coexist with, or influence the other readings of informational self-determination (census origin, data protection constitutionalization)?',
    'Close reading of Karlsruhe''s own judgments on data protection law, census constitutional status, and surveillance; tracking of how the Court has framed the relationship between the reading instantiated here (surveillance proportionality) and the census-origin and data-protection-constitutionalization readings across 40+ years of doctrine',
    'If this reading forecloses the others: they are mutually exclusive doctrinal claims and cannot coexist within Karlsruhe''s framework. If coexists_with: each reading reflects a different moment or coalition within Karlsruhe''s evolution. If influences: this reading (proportionality auditing) creates structural conditions that shape how the other readings develop (e.g., proportionality review becomes the enforcement mechanism for data protection norms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_within_karlsruhe, conceptual, 'Relationship between this reading and sibling readings within Karlsruhe doctrine').

omega_variable(
    beneficiary_epistemic_gap,
    'Are unsuspected surveilled populations (declared beneficiaries) aware they are beneficiaries of this constraint? Or is the benefit structural but unperceived?',
    'Survey data on awareness of constitutional surveillance limits; analysis of public understanding of Karlsruhe''s role in data retention and communications security; comparison of declared benefit (narrowed dragnet schemes) against perceived benefit (subjective privacy confidence)',
    'If beneficiaries are aware and perceive benefit: constraint sustains experienced benefit and coordination function. If beneficiaries are unaware: constraint is genuinely extractive on the beneficiary side — they incur no cost but receive no perceived benefit, only structural protection they don''t know exists. This could alter beneficiary classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_epistemic_gap, empirical, 'Beneficiary awareness of protection provided by proportionality constraint').

omega_variable(
    constitutional_reading_stability,
    'Is this reading (surveillance proportionality) stable across generational turnover in the Court, or does it depend on specific Justices'' commitments (particularly those trained in the post-1983 informational self-determination doctrine)?',
    'Historical analysis of Karlsruhe composition changes; tracking of proportionality doctrine robustness through Court transitions; assessment of whether newer appointees sustain or narrow the proportionality frame',
    'If stable: the constraint has institutional grip independent of individual personalities. If personality-dependent: the constraint''s long-term viability is threatened by generational turnover, and should be reclassified toward piton (institutional inertia) as older justices retire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_reading_stability, empirical, 'Generational stability of surveillance proportionality doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informational_self_determination__surveillance_proportionality_reading, 1983, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isdr_theater_1983, informational_self_determination__surveillance_proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(isdr_theater_1993, informational_self_determination__surveillance_proportionality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(isdr_theater_2003, informational_self_determination__surveillance_proportionality_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(isdr_theater_2013, informational_self_determination__surveillance_proportionality_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(isdr_theater_2023, informational_self_determination__surveillance_proportionality_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(isdr_extractiveness_1983, informational_self_determination__surveillance_proportionality_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(isdr_extractiveness_1993, informational_self_determination__surveillance_proportionality_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(isdr_extractiveness_2003, informational_self_determination__surveillance_proportionality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(isdr_extractiveness_2013, informational_self_determination__surveillance_proportionality_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(isdr_extractiveness_2023, informational_self_determination__surveillance_proportionality_reading, base_extractiveness, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informational_self_determination__surveillance_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(informational_self_determination__surveillance_proportionality_reading, informational_self_determination__census_origin_reading).
narrative_ontology:affects_constraint(informational_self_determination__surveillance_proportionality_reading, informational_self_determination__data_protection_constitutionalized_reading).

% DUAL FORMULATION NOTE:
% These three constraint stories (census_origin_reading, data_protection_constitutionalized_reading, surveillance_proportionality_reading) are distinct structural constraints instantiated from the same contested kernel (informational_self_determination). They are not three perspectives on one constraint; they are three constraint families with different extractiveness values and temporal dynamics. Census-origin reading focuses on founding refusal (mountain candidate, ε~0.05). Data-protection reading focuses on regulatory architecture seeding (rope, ε~0.25). Surveillance-proportionality reading (this file) focuses on 40-year audit function (tangled rope, ε=0.55). Each has its own perspectives, beneficiary/victim structure, and measurements. All three are linked by network.affects_constraints to mark the family relationship and the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informational_self_determination__surveillance_proportionality_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
