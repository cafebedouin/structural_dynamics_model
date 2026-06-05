% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary: Live Birth Threshold Reading
 *   domain: moral_philosophy/historical_ethics/developmental_biology
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   personhood_boundary kernel. The birth-threshold reading holds that
 *   personhood begins categorically at live birth: any living human infant is
 *   a person with full homicide protection, regardless of cognitive capacity,
 *   relational capability, or developmental trajectory. This reading
 *   forecloses certain alternative framings (e.g., the developmental-capacity
 *   reading, which ties personhood to neurological emergence) while
 *   coexisting with others (e.g., the Spartan eugenic reading, which uses a
 *   different boundary). The constraint exhibits both genuine coordination
 *   value (the bright line simplifies parental authority and state
 *   administration) and asymmetric extraction (criminal liability is
 *   absolute, with no negotiation or capacity-dependent mitigation). The
 *   live-born infant experiences the constraint as a snare: total protection
 *   enforced coercively, with no exit option and no ability to consent.
 *   Parents experience tangled rope: the bright line coordinates their
 *   parental authority but extracts via criminal exposure for any killing.
 *   The state experiences rope: the boundary simplifies administrative and
 *   prosecutorial burden. The natural law tradition experiences piton: the
 *   doctrine is maintained through institutional inertia despite eroding
 *   empirical foundations (developmental biology has shown that 'birth' is
 *   not a neurological discontinuity). The analytical observer risks seeing a
 *   mountain (immutable natural law) but structural data reveals a false
 *   summit: identifiable beneficiaries (state apparatus, parental authority
 *   consolidation) contradict natural-law framing.
 *
 * KEY AGENTS:
 *   - Live-born infants: Primary victims (powerless/trapped) — experience the constraint as categorical protection via coercion; no exit or consent capacity
 *   - Parents and caregivers: Secondary victims and agents (moderate/constrained) — benefit from simplified parental authority; extract via criminal liability exposure for any killing
 *   - State legal apparatus: Primary beneficiary (institutional/arbitrage) — experiences bright-line rule as pure coordination; simplifies prosecution and eliminates difficult personhood-capacity adjudication
 *   - Natural law tradition: Institutional framework (institutional/arbitrage) — grounds legitimacy in doctrine; increasingly theatrical as empirical foundation (birth as neurological discontinuity) erodes
 *   - Developmental capacity alternative reading: Competing interpretation (analytical perspective) — holds that personhood emerges via neurological capacity rather than birth event; coexists with birth-threshold reading as live alternative
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent institutional boundary; false summit detection identifies beneficiaries contradicting natural-law claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.68).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, snare).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Live Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/developmental_biology").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'cb9f3d22-c298-4370-850a-d4ecd5623a89').
narrative_ontology:cs_kernel_codification('cb9f3d22-c298-4370-850a-d4ecd5623a89', formalized).
narrative_ontology:cs_authority_grounding('cb9f3d22-c298-4370-850a-d4ecd5623a89', lineage).
narrative_ontology:cs_interpretation_layer_present('cb9f3d22-c298-4370-850a-d4ecd5623a89').
narrative_ontology:cs_reading_relation('cb9f3d22-c298-4370-850a-d4ecd5623a89', personhood_boundary__developmental_capacity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb9f3d22-c298-4370-850a-d4ecd5623a89', personhood_boundary__spartan_eugenic_reading, forecloses).
narrative_ontology:cs_axiom('cb9f3d22-c298-4370-850a-d4ecd5623a89', foundational, categorical_personhood_at_birth).
narrative_ontology:cs_axiom_status(categorical_personhood_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('cb9f3d22-c298-4370-850a-d4ecd5623a89', categorical_personhood_at_birth, deontological).
narrative_ontology:cs_axiom('cb9f3d22-c298-4370-850a-d4ecd5623a89', foundational, birth_event_as_legal_boundary).
narrative_ontology:cs_axiom_status(birth_event_as_legal_boundary, holdable).
narrative_ontology:cs_axiom_grounding('cb9f3d22-c298-4370-850a-d4ecd5623a89', birth_event_as_legal_boundary, conventional).
narrative_ontology:cs_reference_frame('cb9f3d22-c298-4370-850a-d4ecd5623a89', natural_law_personhood_doctrine).
narrative_ontology:cs_drift_state('cb9f3d22-c298-4370-850a-d4ecd5623a89', contemporary_developmental_biology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cb9f3d22-c298-4370-850a-d4ecd5623a89', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, parental_authority_structures).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, state_monopoly_on_violence).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, live_born_infants).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, infants_with_severe_disabilities).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, marginal_agents_at_birth_boundary).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIVE-BORN INFANT (SNARE) — Categorical target of homicide law protection; cannot consent, cannot exit, cannot negotiate. Enters personhood status at birth via the bright-line rule regardless of cognitive capacity, viability, or relational capacity. Maximum extraction exposure: any agent can be prosecuted for killing; the infant's status is enforced coercively with no exit for the infant themselves. The constraint is experienced as pure protection-via-suppression — survival depends on external enforcement.
constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PARENT/CAREGIVER (TANGLED ROPE) — Benefits from the bright-line rule: it simplifies parental authority over infants without requiring proof of cognitive capacity or relational personhood. But the constraint also extracts via criminal liability: infanticide is homicide, non-negotiable, regardless of circumstances (severe disability, inability to care for newborn, cultural norms). The parent experiences coordination (the bright line simplifies decision-making authority) alongside asymmetric extraction (criminal exposure is absolute).
constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LEGAL APPARATUS (ROPE) — Experiences the birth threshold as pure coordination: the bright-line rule eliminates difficult case-by-case determinations of personhood capacity. Prosecutorial discretion, judicial resource allocation, and legislative burden all simplify when personhood is tied to birth rather than developmental capacity. The state's monopoly on legitimate violence is strengthened by the categorical boundary — no negotiation required. Arbitrage exit option: the state can shift the boundary or enforce it selectively.
constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATURAL LAW TRADITION (PITON) — Nominally grounds the birth threshold in natural or divine law ('life begins at birth' as cosmic fact). But the constraint is increasingly theatrical: developmental biology has revealed that 'birth' is an arbitrary marker (neurological differentiation occurs before birth, cognitive emergence occurs months after). The Natural Law reading persists through institutional inertia — seminary education, religious authority, inherited doctrine — even as the empirical foundation has eroded. Theater ratio is low because the justification rarely requires explicit defense; it is maintained through tradition and absence of contrary claims in institutional contexts.
constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (FALSE SUMMIT) — Risks classifying the birth threshold as an immutable natural law: 'birth is an objective fact of biology; personhood follows objective facts.' But the structural data reveals this as naturalization of a legal boundary. The birth threshold is contingent (different cultures have used different markers), extractive for marginal cases (severely disabled infants, borderline viability cases), and maintained by institutional enforcement, not by natural necessity. The false summit detector flags this: the identification of beneficiaries (state apparatus, parental authority structures) contradicts the mountain classification despite the natural-law framing. The constraint is constructed, not discovered.
constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__birth_threshold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, TR),
    TR >= 0.70.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The birth-threshold reading extracts from marginal cases (severely disabled infants, anencephalic infants, late-term viable infants with no relational personhood capacity). The bright-line rule provides no mitigation or case-by-case evaluation: live-born means person, means killing is homicide, no exceptions. Parents face criminal liability regardless of circumstances (severe disability, inability to provide care, cultural norms that dispute the reading). Extractiveness rises from 0.45 to 0.68 over the interval as developmental biology accumulates evidence that birth is not a neurological discontinuity, yet enforcement intensifies (state prosecution strengthens, traditions harden). Suppression (0.72): Very high. The constraint is maintained coercively: it is encoded in criminal law, enforced through prosecution and incarceration, and backed by state violence. There is no exit from the boundary (no negotiation with the state's personhood definition) and no meaningful escape route for parents (infanticide is homicide, prosecutable regardless of circumstances or consent). The bright-line format eliminates capacity-dependent defenses. Theater ratio (0.35): Low-to-moderate. The natural-law justification is not highly theatrical in domestic contexts — it operates through institutional transmission (seminary education, inherited doctrine, legislative precedent) rather than through explicit ritual performance. But cross-cultural comparison reveals theater: some traditions use quickening as the personhood boundary, others use survival tests, others use naming ceremonies. The 'natural' boundary is socially constructed yet presented as universal law. Theater is lower than in the piton perspective because the justification rarely requires explicit defense in monolithic legal systems.
 *
 * PERSPECTIVAL GAP:
 *   This constraint illustrates how the same institutional rule produces opposite experiences depending on structural position. The live-born infant's snare view (maximum coercion, no agency, pure protection) contrasts sharply with the state apparatus's rope view (coordination gain, no extraction experienced, efficient administration). The parent's tangled rope view occupies the middle: genuine coordination value (the bright line simplifies decision-making authority) alongside genuine extraction (absolute criminal liability, no mitigation). The natural law tradition's piton view reflects institutional degradation: the doctrine is maintained through tradition despite eroding empirical grounds (developmental biology shows birth is not a neurological discontinuity). The analytical observer's risk of false summit classification (seeing natural law where constructed boundary exists) highlights the framework's core diagnostic value: beneficiary identification contradicts natural-law framing, triggering reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain operates as follows: The live-born infant is identified as a victim (coercively protected, no exit capacity), trapped in the constraint (cannot negotiate personhood status, cannot exit childhood). This produces d = 0.95 (full target), f(d) ≈ 1.42 (high f), and χ = 0.68 × 1.42 × 1.0 = 0.96 (very high experienced extraction, reflected in snare classification). Parents are identified as partially beneficiary (simplified authority) and partially victim (criminal exposure): constrained exit (high cost to challenge the boundary), moderate power. d ≈ 0.65, f(d) ≈ 1.00, χ ≈ 0.68, reflected in tangled rope. The state apparatus is identified as primary beneficiary (arbitrage exit: can enforce or not), institutional power: d ≈ 0.05, f(d) ≈ -0.12 (negative, indicating net subsidy to the state), χ ≈ -0.08 (effective negative extraction), reflected in rope. The constraint extracts from trapped agents (infants) and constrained agents (parents) and subsidizes the institutional beneficiary (state). Suppression is high (0.72) and non-negotiable across all positions: it is a structural property, not scaled by context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurological_continuity_vs_birth_discontinuity,
    'Is the birth event a morally relevant discontinuity in personhood, or does neurological development represent a continuous trajectory across the birth boundary?',
    'Developmental neuroscience analysis: charting consciousness-associated neural structures (thalamocortical connectivity, corticospinal tract myelination, global workspace network formation) across gestational weeks 30–40 and postnatal weeks 1–8. If discontinuity is minimal, the birth threshold is arbitrary; if discontinuity is substantial, birth may be morally salient.',
    'If continuous: birth threshold is a legal fiction (forecloses natural-law grounding; classification shifts toward scaffold or snare). If discontinuous: personhood change at birth may be genuine (supports rope or mountain classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neurological_continuity_vs_birth_discontinuity, empirical, 'Whether neural development shows discontinuity at birth or continuity across the boundary').

omega_variable(
    extraction_asymmetry_for_severely_disabled_infants,
    'Does the bright-line birth-threshold rule extract disproportionately from parents of severely disabled or anencephalic live-born infants who have no capacity for relational personhood?',
    'Case analysis: prosecution rates, sentencing severity, and acquittal rates for infanticide involving severe congenital abnormality vs. healthy infants. Cultural variation: do jurisdictions with different personhood thresholds (e.g., some African traditions recognizing personhood after survival tests) show different victim patterns?',
    'If disproportionate extraction: the reading forecloses alternative readings based on capacity. If symmetric: the bright line serves all equally. High extraction asymmetry weakens the reading''s claim to natural-law universality and strengthens the snare classification for this victim subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_for_severely_disabled_infants, empirical, 'Whether bright-line birth threshold extracts disproportionately from parents of severely disabled infants').

omega_variable(
    kernel_identity_and_reading_foreclosure,
    'Does this reading''s core axiom (personhood begins at birth, categorically) logically foreclose the developmental_capacity_reading (personhood emerges via neurological capacity), or do they merely coexist as competing positions?',
    'Logical analysis: Can a single normative framework hold both ''personhood IS birth'' (categorical status) and ''personhood DEPENDS ON capacity'' (continuous emergence)? Or does affirming one require denying the other? Examine the Spartan eugenic reading: does it foreclose the birth-threshold reading (by placing a different boundary), or coexist?',
    'If foreclosure relation confirmed: the reading is stronger (eliminates alternatives logically). If coexistence: the reading is one option among live alternatives (weakens claims to universality). Clarifies whether the constraint is a reading of a contested kernel or an uncontested natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_and_reading_foreclosure, conceptual, 'Whether this reading logically forecloses or coexists with developmental capacity reading').

omega_variable(
    state_enforcement_selectivity,
    'Is state enforcement of the birth-threshold reading uniform across all infant categories, or does prosecutorial discretion effectively create enforcement gradations?',
    'Statistical analysis: prosecution rates for infanticide vs. homicide, by maternal age, socioeconomic status, reported intent, disability status of infant, and cultural/religious context. If prosecution is selective, de facto enforcement is softer than categorical law.',
    'If uniform enforcement: the reading''s snare classification is confirmed (categorical coercion with no exit). If selective: enforcement is softer than the written law suggests, weakening the snare classification and revealing a scaffold-like sunset mechanism (prosecutorial discretion serves as informal solar clause).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_selectivity, empirical, 'Whether state enforcement of birth-threshold reading is uniform or selective').

omega_variable(
    false_summit_candidate_identification,
    'Is this reading grounded in a genuine natural law about personhood emergence, or is it a constructed boundary that benefits state authority and parental power structures?',
    'Historical institutional analysis: trace the adoption of ''birth as personhood boundary'' across legal systems (common law, civil law, religious law traditions). Identify variation: when did the boundary shift from quickening to birth? Which jurisdictions use different markers? What material incentives (prosecutorial efficiency, state monopoly on violence, parental authority consolidation) benefit from the bright-line rule?',
    'If natural law: no beneficiaries should be identifiable (contradicts false summit signature). If constructed: beneficiary identification (state apparatus, parental authority) confirms false summit, triggering reclassification via engine signature detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate_identification, conceptual, 'Whether birth threshold is natural law or constructed boundary benefiting specific authorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birth_thresh_theater_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(birth_thresh_theater_t500, personhood_boundary__birth_threshold_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement(birth_thresh_theater_t1000, personhood_boundary__birth_threshold_reading, theater_ratio, 1000, 0.35).

% Extraction over time
narrative_ontology:measurement(birth_thresh_extract_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(birth_thresh_extract_t500, personhood_boundary__birth_threshold_reading, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(birth_thresh_extract_t1000, personhood_boundary__birth_threshold_reading, base_extractiveness, 1000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(birth_thresh_suppress_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(birth_thresh_suppress_t500, personhood_boundary__birth_threshold_reading, suppression_requirement, 500, 0.68).
narrative_ontology:measurement(birth_thresh_suppress_t1000, personhood_boundary__birth_threshold_reading, suppression_requirement, 1000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__developmental_capacity_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__spartan_eugenic_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, late_term_abortion_boundary).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, neonatal_euthanasia_legality).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into multiple constraint stories, each instantiating a different reading. The birth-threshold reading is one reading of this kernel; the developmental-capacity reading is another. They have different ε values (birth-threshold: 0.68, extraction-focused; developmental-capacity: likely lower, capacity-negotiated), different beneficiary/victim structures, and different classifications. Both are linked to the kernel; neither is the 'true' constraint. The engine's role is to track how each reading structures personhood claims and extract differently from marginal agents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
