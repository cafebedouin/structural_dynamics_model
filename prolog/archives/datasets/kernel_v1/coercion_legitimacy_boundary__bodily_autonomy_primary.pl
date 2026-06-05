% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Coercion Legitimacy Boundary (Bodily Autonomy Primary Reading)
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   The bodily_autonomy_primary reading asserts that medical intervention
 *   without informed consent is categorically impermissible, regardless of
 *   collective health benefit. This is one articulation of a contested
 *   kernel: the legitimacy boundary for coercion in medical contexts. The
 *   constraint instantiates a fundamental normative commitment — the
 *   inviolability of bodily integrity — and declares that this commitment
 *   cannot be overridden by utilitarian calculations of collective health
 *   outcomes. The measurement trajectory (extractiveness 0.38→0.52→0.68)
 *   reflects the institutional ratchet effect: as public health authorities
 *   deploy coercive mechanisms (employment mandates, school exclusions,
 *   healthcare access restrictions), the suppression and extraction
 *   experienced by non-compliant individuals escalate. The reading enters
 *   mandate enforcers as beneficiaries because enforcement consolidates
 *   institutional authority and provides compliance metrics; it enters
 *   vaccine-hesitant individuals and medical conscience objectors as victims
 *   because coercion is applied to them; it enters immunocompromised
 *   individuals as victim-adjacent because the constraint shifts them from
 *   beneficiary (in the public_health_primary reading) to collateral in a
 *   frame that uses their vulnerability to justify overriding others'
 *   autonomy. The constraint's theater ratio (0.55) reflects that consent
 *   doctrine is nominally present in medical law but functionally eroded by
 *   accumulated exceptions.
 *
 * KEY AGENTS:
 *   - Vaccine-Hesitant Individuals: Primary victim (powerless/trapped) — face employment mandates, school exclusions, healthcare access restrictions with no legitimate exit path
 *   - Medical Conscience Objectors: Secondary victim (moderate/constrained) — face professional licensing consequences for refusing to administer mandated interventions
 *   - Public Health Authorities/Mandate Enforcers: Primary beneficiary (institutional/arbitrage) — gain institutional authority, compliance metrics, and population-level control levers
 *   - Immunocompromised Individuals: Victim-adjacent (powerful/constrained) — their vulnerability is cited to justify coercion of others, but they bear costs of backlash or non-compliance escalation
 *   - Legal Doctrine (Consent Requirement): Institutional actor (institutional/arbitrage) — maintains nominal consent requirement while exceptions absorb most of its force (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent normative commitment as an inviolable law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Coercion Legitimacy Boundary (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '6dae46c8-cb9d-4d3a-8257-f658ec3940af').
narrative_ontology:cs_kernel_codification('6dae46c8-cb9d-4d3a-8257-f658ec3940af', formalized).
narrative_ontology:cs_authority_grounding('6dae46c8-cb9d-4d3a-8257-f658ec3940af', lineage).
narrative_ontology:cs_interpretation_layer_present('6dae46c8-cb9d-4d3a-8257-f658ec3940af').
narrative_ontology:cs_reading_relation('6dae46c8-cb9d-4d3a-8257-f658ec3940af', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('6dae46c8-cb9d-4d3a-8257-f658ec3940af', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6dae46c8-cb9d-4d3a-8257-f658ec3940af', foundational, bodily_integrity_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('6dae46c8-cb9d-4d3a-8257-f658ec3940af', bodily_integrity_inviolable, deontological).
narrative_ontology:cs_axiom('6dae46c8-cb9d-4d3a-8257-f658ec3940af', foundational, consent_non_negotiable).
narrative_ontology:cs_axiom_status(consent_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6dae46c8-cb9d-4d3a-8257-f658ec3940af', consent_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('6dae46c8-cb9d-4d3a-8257-f658ec3940af', absolute_bodily_autonomy_protection).
narrative_ontology:cs_drift_state('6dae46c8-cb9d-4d3a-8257-f658ec3940af', contemporary_mandate_escalation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6dae46c8-cb9d-4d3a-8257-f658ec3940af', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional_public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_framework).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_conscience_objectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL (SNARE) — Faces coercive pressure: employment mandates, school exclusion, healthcare access restrictions, social stigma. No legitimate exit path within the framework — the constraint treats bodily autonomy as subordinate to collective benefit determination made by authorities. Bears full suppression and extraction. Maximum experienced severity.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL CONSCIENCE OBJECTOR (SNARE) — Constrained by professional licensing and employment law; faces loss of credentials or career if refuses to administer coercive interventions against conscience. Higher agency than the unvaccinated individual (professional standing, credentialing bodies) but still trapped by institutional enforcement. Significant extraction.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY / MANDATE ENFORCER (ROPE) — Experiences the constraint as coordination: enforcing vaccination requirements solves a genuine collective action problem (free-rider vaccination avoidance). Net beneficiary from mandate enforcement — gains institutional authority, population-level metrics, compliance levers. Sees constraint as legitimate coordination of collective interest.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED INDIVIDUALS / EXPOSED GROUP (TANGLED ROPE) — Powerful collective (adult patient advocacy organizations) with constraints. The bodily autonomy reading shifts them from beneficiary (public_health_primary reading) to victim-adjacent: their vulnerability is cited to justify coercion of others, but they bear costs if coercion fails or produces backlash (anti-vaccination escalation). Mixed: benefit from others' vaccination, but victimized by the frame that treats their protection as justification for overriding bodily autonomy.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / ABSOLUTIST VIEW (MOUNTAIN) — Positions bodily autonomy as an inviolable, natural-law moral boundary: no calculus of collective benefit can override individual bodily integrity. Medical intervention without consent is categorically impermissible. This perspective risks naturalizing what is a contested normative commitment rather than a discovered law of nature — the reading's own kernel is under contestation by sibling readings.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LEGAL DOCTRINE / NOMINAL CONSENT REQUIREMENT (PITON) — Medical ethics and constitutional law nominally require informed consent; this requirement persists across most jurisdictions. But the doctrine is substantially performative: exceptions (emergency care, incapacity, public health) have absorbed most of the consent requirement's force. The doctrine is maintained through institutional inertia and precedent, not because it functions as a binding constraint on public health authority. Theater ratio high because the consent requirement is theatrically invoked while substantively eroded.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__bodily_autonomy_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The bodily_autonomy_primary reading defines non-compliance with vaccine mandates as victims, and mandate enforcers as beneficiaries. The extractiveness reflects the asymmetry: individuals lose bodily autonomy decision-making authority; authorities gain enforcement capacity and compliance metrics. The trajectory (0.38→0.68) shows institutional ratcheting — as mandates accumulate (employment, school, healthcare access), the extraction mechanism intensifies. Suppression (0.72): High. Victims face material barriers (job loss, school exclusion, healthcare denial) and social suppression (stigma, professional exclusion). The trajectory (0.55→0.72) reflects enforcement escalation: early mandates are less suppressive; as legal and social enforcement mechanisms mature, barriers to exit multiply. Theater ratio (0.55): Moderate. Medical consent doctrine persists in legal codes, but its functional scope has been eroded by recognized exceptions (emergency, incapacity, public health). The constraint is not primarily performative (piton would have theater > 0.7), but significant theatrical elements persist: consent forms are signed while exceptions disable meaningful choice. Claimed type (snare): The high extractiveness, high suppression, and high effective coercion (χ ≥ 0.66) justify snare classification. Victims have no legitimate exit path within this reading's framework; the constraint relies on suppression (legal and social barriers) to maintain its force.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a fundamental perspectival conflict. The mandate enforcer (institutional/arbitrage) experiences the constraint as legitimate coordination (rope) — enforcing population-level immunity prevents free-rider vaccination avoidance. The vaccine-hesitant individual (powerless/trapped) experiences it as pure extraction (snare) — coercive authority overrides bodily autonomy with no legitimate exit. The immunocompromised individual (powerful/constrained) occupies a contradictory position: beneficiary in the public_health_primary reading (their vulnerability justifies mandates) but victim-adjacent in this reading (coercion is applied to third parties citing their protection). The legal doctrine (institutional/arbitrage) persists nominally (piton) — courts maintain that consent is required — while accepting exceptions that have inverted the rule. The analytical observer risks a false summit (mountain) — positioning bodily autonomy as an immutable moral law — but the constraint is actually a live disputed reading of a contested kernel. The perspectival gaps reveal the core mandatrophy: mandates may be justified under public_health_primary reading but unjustified under bodily_autonomy_primary. Both readings coexist in public discourse with no single framework adjudicating between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The bodily_autonomy_primary reading assigns low directionality (d) to mandate enforcers (beneficiary status with arbitrage options → institutional power derives benefit from enforcement authority), producing low/negative effective extraction for that agent. It assigns high directionality (d) to victims (vaccine-hesitant individuals, medical conscience objectors trapped with no exit), producing high effective extraction. Immunocompromised individuals receive moderate d because they occupy a contradictory position: the reading acknowledges their vulnerability (victim-adjacent) but treats them as collateral to the core autonomy conflict. The piton perspective receives low d despite institutional power because its enforcement mechanism (theater) produces minimal substantive extraction — the perspective sees the constraint as largely degraded. The analytical observer receives canonical d for analytical power (0.72) because they are not engaged in enforcement but in meta-assessment of the framework's logical consistency.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by instantiating ONE logical position within a contested kernel. The mandatrophy is not internal to this reading but BETWEEN readings: public_health_primary and bodily_autonomy_primary foreclose each other at the axiom level. Within bodily_autonomy_primary, all six classifications are coherent: mandates are snares (victims' experience), ropes (enforcers' experience), mountains (absolutist view), pitons (legal doctrine's state), tangled ropes (immunocompromised groups' experience). The mandatrophy resolves when the reader recognizes that no single type is correct — instead, the presheaf of types indexed by observer position accurately models the constraint. However, the kernel-level mandatrophy (which reading is legitimate?) cannot be resolved by the DR framework alone — it requires normative judgment about whether bodily autonomy is categorically inviolable or whether it can be scaled by disease severity and transmission dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_measurement_basis,
    'Is bodily autonomy a foundational right discoverable through natural law reasoning, or a contingent normative commitment grounded in particular legal/philosophical traditions?',
    'Comparative constitutional law analysis: identify jurisdictions that reject absolute bodily autonomy in medical contexts and assess whether their rejection is irrational or reflects coherent alternative frameworks.',
    'If discoverable natural law: this reading''s mountain claim is justified; sibling readings are conceptually incoherent. If contingent commitment: this reading is one live position coexisting with others; classification downgrades to snare (extraction mechanism) or tangled_rope (hybrid).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_measurement_basis, conceptual, 'Whether bodily autonomy is natural law or contingent normative commitment').

omega_variable(
    consent_exception_proliferation,
    'How many exceptions to informed consent doctrine have accumulated in legal and medical practice? Do exceptions now exceed the rule?',
    'Systematic review of medical law and case law: enumerate recognized exceptions (emergency, incapacity, public health, research, etc.); compute proportion of medical decisions affected by at least one exception; assess whether the nominal rule (consent required) has been substantively inverted.',
    'If exceptions > 50% of medical decisions: the piton classification is confirmed — consent doctrine is degraded to theater. If exceptions < 30%: the consent doctrine retains functional force and the piton is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_exception_proliferation, empirical, 'Scope of accumulated exceptions to consent requirement').

omega_variable(
    sibling_framework_coherence,
    'Can a single legal/moral framework hold the bodily_autonomy_primary reading AND the proportionality_reading simultaneously, or do they logically foreclose one another?',
    'Formal analysis of the axioms: bodily autonomy primary asserts that bodily integrity is categorically inviolable; proportionality asserts that coercion legitimacy scales with disease threat. Test whether both can be true in the same framework without contradiction.',
    'If forecloses: reading_relations should be ''forecloses''; sibling readings are mutually exclusive within any single framework. If coexists: reading_relations should be ''coexists_with''; different parties can hold both without logical incoherence, reflecting genuine normative pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_framework_coherence, conceptual, 'Whether bodily autonomy absolutism forecloses proportionality reading').

omega_variable(
    non_enforcement_extraction_magnitude,
    'If coercive vaccine mandates are lifted (adopting this reading), what is the magnitude of disease-burden increase among unvaccinated and exposed populations?',
    'Counterfactual modeling: compare disease burden and mortality in populations with vs. without mandates, controlling for voluntary vaccination rates and behavior change. Estimate health cost of non-enforcement.',
    'If health cost is severe (>10% excess mortality in exposed populations): the non-enforcement extractiveness is high (~0.8), amplifying the snare classification. If cost is moderate (1-3%): extractiveness moderate (~0.4-0.5). If cost is low: the mandate may be indefensible even under public_health_primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_enforcement_extraction_magnitude, empirical, 'Health burden from non-enforcement of vaccine mandates').

omega_variable(
    reading_authority_grounding,
    'What epistemic or normative authority grounds the bodily_autonomy_primary reading? Is it constitutional text, natural law reasoning, international human rights law, or philosophical principle?',
    'Source analysis: identify the primary texts/authorities this reading cites (e.g., Nuremberg Code, constitutional liberty clauses, bioethics consensus documents). Assess their coherence and whether they exclude the proportionality reading.',
    'If grounding is textual (constitutional): the reading can claim doctrinal status in jurisdictions with that constitution. If grounding is philosophical/natural law: the reading competes with sibling readings on normative merit, not on legal authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authority_grounding, empirical, 'Authority source for bodily autonomy priority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_ba_theater_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.48).
narrative_ontology:measurement(coer_ba_theater_t3, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 3, 0.52).
narrative_ontology:measurement(coer_ba_theater_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(coer_ba_ext_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(coer_ba_ext_t3, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(coer_ba_ext_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coer_ba_supp_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(coer_ba_supp_t3, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(coer_ba_supp_t6, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_informed_consent_doctrine__nominal_requirement).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, pandemic_emergency_authority__scope_expansion).

% DUAL FORMULATION NOTE:
% This constraint is the bodily_autonomy_primary reading of a contested kernel. Two sibling readings (public_health_primary and proportionality_reading) are separate constraint stories with different ε values and beneficiary/victim structures. The bodily_autonomy_primary reading assigns mandatees as beneficiaries and vaccine-hesitant individuals as victims. The public_health_primary reading reverses this: it assigns immunocompromised individuals and the public health collective as beneficiaries and mandate-resisters as victims. The proportionality_reading splits the difference: benefits scale with disease severity. All three are linked via network.affects_constraints and represent incompatible readings of the same legal/ethical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
