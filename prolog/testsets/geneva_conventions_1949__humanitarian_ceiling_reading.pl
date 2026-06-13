% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading
 *   domain: legal/political/humanitarian
 *
 * SUMMARY:
 *   The humanitarian ceiling reading of the 1949 Geneva Conventions asserts
 *   that states must maintain absolute minimum protections for civilians,
 *   detainees, and irregular combatants regardless of whether adversaries
 *   reciprocate or comply. This reading suppresses security rationales as
 *   grounds for derogation: operational necessity, asymmetric threat, and
 *   adversary violations do not permit suspending protections. The constraint
 *   operates as a tangled rope — it coordinates protection standards globally
 *   while extracting compliance costs asymmetrically from state militaries
 *   and security practitioners who must forgo tactical advantages and absorb
 *   procedural overhead. The humanitarian ceiling is actively enforced by
 *   international institutions, advocacy networks, and doctrine communities;
 *   its persistence depends on this enforcement against constant
 *   countervailing pressure from security-maximization reasoning. This is one
 *   reading of a contested kernel; sibling readings adopt conditional
 *   reciprocity or security maximization frames.
 *
 * KEY AGENTS:
 *   - international_humanitarian_law_community: Agenda-setter institutional seat; interprets and enforces the absolute-minimum reading through ICRC, treaty organs, advocacy networks.
 *   - state_militaries_conventional: Payer institutional seat; bears enforcement burden of maintaining protections regardless of reciprocity.
 *   - protected_civilians: Beneficiary powerless seat; receive immunity from targeting and protection from abuse independent of compliance by irregular forces.
 *   - detainees_and_prisoners: Beneficiary powerless seat; receive humane treatment and protection from torture regardless of POW status or adversary reciprocity.
 *   - security_doctrine_practitioners: Payer powerful seat; face constraints on counterterrorism and interrogation doctrine; security rationales suppressed as override grounds.
 *   - competing_security_readings: Excluded institutional seat; advocates for conditional reciprocity or security maximization are structurally suppressed within this reading's authority space.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.31).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "legal/political/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'ce016d08-520f-403c-9eae-8a3428de3485').
narrative_ontology:cs_kernel_codification('ce016d08-520f-403c-9eae-8a3428de3485', fixed_text).
narrative_ontology:cs_authority_grounding('ce016d08-520f-403c-9eae-8a3428de3485', extraction).
narrative_ontology:cs_interpretation_layer_present('ce016d08-520f-403c-9eae-8a3428de3485').
narrative_ontology:cs_reading_relation('ce016d08-520f-403c-9eae-8a3428de3485', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce016d08-520f-403c-9eae-8a3428de3485', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('ce016d08-520f-403c-9eae-8a3428de3485', foundational, human_dignity_absolute).
narrative_ontology:cs_axiom_status(human_dignity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ce016d08-520f-403c-9eae-8a3428de3485', human_dignity_absolute, deontological).
narrative_ontology:cs_axiom('ce016d08-520f-403c-9eae-8a3428de3485', foundational, non_derogable_rights_doctrine).
narrative_ontology:cs_axiom_status(non_derogable_rights_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('ce016d08-520f-403c-9eae-8a3428de3485', non_derogable_rights_doctrine, deontological).
narrative_ontology:cs_reference_frame('ce016d08-520f-403c-9eae-8a3428de3485', absolute_humanitarian_minimums).
narrative_ontology:cs_drift_state('ce016d08-520f-403c-9eae-8a3428de3485', asymmetric_conflict_era_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ce016d08-520f-403c-9eae-8a3428de3485', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_prisoners).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, security_doctrine_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31 at interval end) because the constraint creates real costs for states — they must provide medical care to wounded adversaries, conduct fair trials for detainees, protect civilians even in occupied enemy territory — but these costs are not pure extraction; they serve the coordination function of establishing a shared humanitarian floor. Suppression is high (0.72) because the constraint's persistence depends on actively suppressing security rationales and alternative interpretations: states that would benefit from conditional reciprocity or security derogation must be prevented from adopting those readings through institutional pressure, legal precedent, and norm enforcement. Theater ratio is low (0.18) because the constraint's core function — preventing a race to the bottom in protections — remains genuine; the growing theater component reflects increased rhetorical defense of the reading as asymmetric conflict creates pressure to erode it. Accessibility collapse is very high (0.89) because the constraint codifies absolute minimums in law and international norm; alternatives (returning to reciprocal derogation, security-maximization readings) are legally and institutionally suppressed. Resistance is high (0.68) because security practitioners and military strategists actively resist the constraint's suppression of security rationales; they argue that asymmetric warfare justifies flexibility, and they mount countervailing pressure through doctrine revision, legal reinterpretation, and operational practice that tests the constraint's boundaries. The measurement series shows extractiveness rising slightly over the interval as asymmetric conflict proliferates and states bear growing costs; suppression rises as enforcement pressure increases to maintain the ceiling against those pressures.
 *
 * PERSPECTIVAL GAP:
 *   From the humanitarian law community's seat, the constraint is genuine coordination that prevents a race to the bottom. From state militaries' seats, it is an asymmetric burden that requires them to protect adversaries who do not reciprocate. From security practitioners' seats, it is an operational constraint that suppresses legitimate security reasoning. From detainees' and civilians' seats, it is a protective ceiling they depend on. The engine computes these divergent types from the structural data: the humanitarian law seat experiences coordination with moderate overhead; the military seats experience extraction with asymmetric enforcement; the protected seats experience subsidy (protections are provided at state cost). The authored claim (tangled rope) reflects the structural reality that the constraint both coordinates (establishes global humanitarian floor) and extracts (creates asymmetric costs for state compliance).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as protected_civilians, detainees_and_prisoners, and irregular_combatants — these groups receive protections without bearing enforcement costs, so they sit toward the beneficiary end of directionality (d near 0.0). Victims are state_militaries and security_doctrine_practitioners — they bear enforcement costs (maintaining detention facilities, conducting fair trials, restraining interrogation, protecting civilians in occupied territory) without receiving corresponding benefits, so they sit toward the target end (d near 1.0). The international humanitarian law community is declared as agenda_setter (institutional power, analytical exit) — they set and interpret the constraint but do not directly bear enforcement costs or directly collect benefits, placing them near symmetric (d ~ 0.5) or slightly beneficiary (d ~ 0.35) because they derive authority and institutional standing from the reading's maintenance. The overarching directionality flow: those who must comply with humanitarian protections (state militaries) are targets; those who receive protections (civilians, detainees) are beneficiaries; those who enforce the reading (humanitarian law institutions) are agenda-setters with modest benefit (institutional authority) and modest cost (enforcement labor).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to prevent a race to the bottom in humanitarian protections during armed conflict. The founding problem — absence of enforceable limits on treatment of captured enemies and civilians — is disputed: humanitarian law institutions attest it remains live (asymmetric warfare produces constant pressure to erode protections); security practitioners attest it has shifted (the founding problem of unregulated warfare was solved in 1949, and new problems of asymmetric conflict require flexibility). This disagreement is the core of the reading contest. The mandatrophy test examines whether the constraint's survival depends on whether its mandate still functions. At t0 (1949), the mandate was unambiguous and clearly live: states had just completed industrial warfare with minimal humanitarian constraints. At t75 (contemporary), the mandate is contested: humanitarian law institutions argue the constraint prevents ongoing erosion; security practitioners argue the mandate has been superseded by asymmetric conflict dynamics. The constraint persists because the humanitarian law community enforces it institutionally (ICRC, treaty organs, advocacy networks, war-crimes tribunals), not primarily because the state-level parties view it as solving a live problem they share. This does NOT constitute mandatrophy resolution in the classical sense (mandate dead but constraint persists through inertia); rather, it reveals a contested mandate: the constraint is actively defended by one stakeholder seat (humanitarian law community) against competing mandates from other seats (security-maximization practitioners who argue asymmetric conflict creates a different, more pressing mandate). The classification as tangled rope captures this: coordination function (preventing race to the bottom) is real; extraction (asymmetric compliance costs) is real; active enforcement (institutional suppression of competing readings) is real. The divergence between claimed type and potential classification-as-snare-from-security-practitioner-seat is exactly the measurement the engine exists to take. Mandatrophy is not resolved; it is structurally unavoidable within the kernel contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_necessity_boundary,
    'Are absolute humanitarian minimum protections logically independent of adversary compliance, or does the coherence of the humanitarian law system structurally depend on reciprocal restraint from state actors?',
    'Empirical test: systematic analysis of conflict cases where states unilaterally maintained protections despite adversary violations. Examine whether those states'' long-term security outcomes, institutional stability, and conflict resolution prospects diverge from states that adopted conditional reciprocity. Examine also whether soldiers in humanitarian-law-compliant militaries experience lower casualty rates or higher morale than soldiers in security-maximization militaries.',
    'If absolute protections prove empirically decoupled from reciprocity (states that maintain ceilings achieve equal or better security outcomes), the humanitarian ceiling reading''s logical coherence is strengthened. If reciprocal violation cascades produce demonstrably worse outcomes for protection-maintaining states, the conditional reciprocity reading gains structural credibility. The answer reshapes which reading is tenable as a stable equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_necessity_boundary, empirical, 'Whether humanitarian protections can coherently persist absent reciprocity or whether the system requires reciprocal restraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (international institutional coercion) or internalized (military and governmental agents have fused humanitarian-law commitment into professional identity)?',
    'Observe post-institutional-pressure scenarios: if international pressure were removed (courts ceased prosecuting war crimes, coalitions stopped isolating violators), would suppression persist at comparable levels? Conduct interviews with military officers about the motivational sources of humanitarian compliance. Examine doctrine revision trajectories in militaries that have experienced loosening of institutional accountability.',
    'If suppression is primarily structural, the constraint is vulnerable to institutional collapse; if institutional authority erodes, compliance collapses. If suppression is primarily internalized, the constraint persists even absent external enforcement because military professionals have internalized the ceiling as a core institutional commitment. If mixed, the constraint has dual fragility: it depends on both institutional persistence and continued professional identity-fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of security rationales is structural or internalized in military professional identity.').

omega_variable(
    asymmetric_conflict_mandate_obsolescence,
    'Has asymmetric conflict (terrorism, insurgency, irregular forces) created a fundamentally different conflict environment that obsoletes the humanitarian ceiling reading''s founding mandate (preventing race-to-the-bottom in state-on-state warfare)?',
    'Examine whether the founding problem (unregulated state-on-state warfare producing humanitarian collapse) is the same problem contemporary conflicts present. Compare casualty ratios, civilian targeting, and humanitarian compliance rates in inter-state warfare (1949–1990) vs. asymmetric conflict (1990–present). If asymmetric conflict shows different dynamics (civilian targeting by non-state actors does not trigger cascade of state derogation in the pattern predicted by race-to-the-bottom theory), the mandate has shifted.',
    'If the founding problem is obsolete in asymmetric warfare, the humanitarian ceiling reading is maintaining a solution to a problem that no longer drives state behavior. Security-maximization readings gain structural plausibility. If the founding problem remains live (states still reason backward from security imperatives and erode protections when adversaries do), the ceiling reading''s mandate persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_conflict_mandate_obsolescence, empirical, 'Whether asymmetric conflict has created new dynamics that obsolete the humanitarian ceiling reading''s founding mandate.').

omega_variable(
    kernel_reading_alternative_framing,
    'Is the humanitarian ceiling reading a genuine alternative instantiation of the geneva_conventions_1949 kernel, or is it a meta-reading (a reading about how readings should be adjudicated) that functionally excludes the security-maximization and conditional-reciprocity readings from consideration?',
    'Examine whether the humanitarian ceiling reading''s authority structure permits the sibling readings to coexist as legitimate interpretations, or whether it treats them as illegitimate departures from the true kernel. If the reading''s authority structure (ICRC, treaty organs, humanitarian law doctrine) actively excludes the siblings as illegitimate, the reading is functioning as a meta-interpretation that suppresses competing readings rather than as one co-equal reading of the kernel.',
    'If the humanitarian ceiling reading functions as a meta-reading that suppresses siblings, its claim to be ''one reading among three'' is inaccurate; it is actually an enforcement regime for a single reading. This affects whether the constraint is properly classified as coexisting with siblings or as foreclosing them. If it functions as a co-equal reading, the coexists_with relation is accurate; if as a suppressive meta-reading, the foreclosure relation may be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether the humanitarian ceiling reading is genuinely one co-equal reading or a meta-reading that functionally excludes siblings.').

omega_variable(
    irregular_combatant_protection_ambiguity,
    'Can irregular combatants retain baseline humanitarian protections (humane treatment, fair trial) while the state denies them combatant immunity (the privilege to engage in hostilities without criminal liability)? Is this status coherent, or does denying combatant immunity entail logical permission to deny humanitarian protections?',
    'Examine treaty language and ICRC interpretations on the status of irregular combatants. Test whether states that maintain this dual status (protection without immunity) face internal doctrinal contradiction or whether the status is operationally sustainable. Examine actual prosecution patterns: are irregular combatants prosecuted under criminal law while receiving humanitarian protections, or does denial of combatant immunity cascade to denial of humanitarian status?',
    'If the dual status is coherent and sustainable, the humanitarian ceiling reading''s expansion of baseline protections to irregular combatants is logically sound. If the dual status is unstable and collapses either toward full POW status (granting immunity) or toward loss of humanitarian protection (denying immunity and protection together), the ceiling reading''s claim to protect irregular combatants is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irregular_combatant_protection_ambiguity, conceptual, 'Whether humanitarian protection can coherently persist for irregular combatants denied combatant immunity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 75, 0.18).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 75, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 45, 0.71).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% The constraint family geneva_conventions_1949 consists of three structurally distinct readings of the 1949 Conventions kernel, each instantiating different ε values and classification outcomes. humanitarian_ceiling_reading (this file): ε = 0.31 (low-to-moderate extraction serving coordination function), claimed tangled_rope, high suppression of security rationales, expansive protections. conditional_reciprocity_reading (sibling): ε ~ 0.45–0.55 (moderate extraction, protections degrade with adversary non-compliance), claimed rope or tangled_rope with reduced beneficiary set. security_maximization_reading (sibling): ε ~ 0.72+ (high extraction justified by operational necessity), claimed snare or scaffold with sunset clause, minimal beneficiary protections in asymmetric conflict. The three readings partition the geneva_conventions_1949 kernel along the axis of whether state compliance with humanitarian protections is conditioned on adversary reciprocity or maintained absolutely. They are neither alternative measurements of the same constraint (ε-invariance would forbid this) nor redundant restatements; they are distinct constraint-instances grounded in genuinely different interpretations of the kernel text and its authority. Each reading has its own beneficiary structure, its own authority grounding (humanitarian law institutions vs. national-security practitioners vs. pragmatist accommodationists), and its own sustainability dynamics. The family structure represents a contested kernel in which multiple parties hold incommensurable but coexisting readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
