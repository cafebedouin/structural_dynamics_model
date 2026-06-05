% ============================================================================
% CONSTRAINT STORY: thirteenth_amendment__badges_of_servitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thirteenth_amendment__badges_of_servitude_reading, []).

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
 *   constraint_id: thirteenth_amendment__badges_of_servitude_reading
 *   human_readable: Thirteenth Amendment Badges-of-Servitude Reading: Congressional Power to Reach Private Discrimination
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The badges-of-servitude reading of the Thirteenth Amendment represents
 *   one of two fundamental interpretations of the Amendment's scope and
 *   Congressional enforcement power. Under this reading, abolishing slavery
 *   requires not merely ending the legal institution of chattel slavery but
 *   eradicating the private discrimination and caste mechanisms that
 *   perpetuate slavery's residue. This reading grants Congress broad power to
 *   reach private discrimination (housing covenants, employment gatekeeping,
 *   credit exclusion, public accommodation segregation) as badges —
 *   disabilities and indignities that constitute the perpetuation of the
 *   abolished system. The constraint exhibits a perspectival structure that
 *   reveals deep doctrinal contestation: victims of race-based exclusion see
 *   snare (trapped in caste networks); private discriminators see tangled
 *   rope (threatened by Congressional reach); Congress sees rope
 *   (coordination mechanism); civil rights enforcers see piton (powerful tool
 *   maintained by inertia); an analytical observer risks seeing mountain
 *   (textual necessity), masking the fact that the formal_abolition_reading
 *   is equally textually defensible. The constraint's extractiveness (0.58)
 *   reflects that identifying and eliminating badges requires active
 *   Congressional enforcement mechanism and statutory intervention — moderate
 *   extraction from private discriminators who lose the ability to exclude,
 *   alongside benefit to discrimination victims. The suppression (0.68)
 *   captures the institutional resistance: courts have narrowed the reading
 *   significantly (Patterson v. McLean Credit Union, conservative
 *   reinterpretations), and political will to enforce the reading has waxed
 *   and waned across generations.
 *
 * KEY AGENTS:
 *   - Victims of Race-Based Exclusion: Primary beneficiary (powerless/trapped) — trapped within discrimination networks; depend on Congressional action under this reading to reach private discriminators
 *   - Private Discriminators and Caste Preservers: Primary victims (powerful/mobile) — face Congressional enforcement and loss of ability to exclude if this reading is enforced
 *   - Congress: Institutional actor (institutional/arbitrage) — gains expanded enforcement power under this reading; sees constraint as coordination mechanism for racial equality
 *   - Civil Rights Enforcement Community: Organized advocates (organized/constrained) — work within this reading's framework; constrained by political capacity and judicial narrowing
 *   - Formal-Abolition Adherents: Competing doctrinal position (analytical/analytical) — hold that the Thirteenth Amendment ended only the legal institution, not private discrimination
 *   - Supreme Court: Institutional arbiter (institutional/arbitrage) — decides whether this reading or formal_abolition_reading governs; has narrowed the reading through conservative interpretation (Patterson, Civil Rights Cases)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thirteenth_amendment__badges_of_servitude_reading, 0.58).
domain_priors:suppression_score(thirteenth_amendment__badges_of_servitude_reading, 0.68).
domain_priors:theater_ratio(thirteenth_amendment__badges_of_servitude_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thirteenth_amendment__badges_of_servitude_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(thirteenth_amendment__badges_of_servitude_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(thirteenth_amendment__badges_of_servitude_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thirteenth_amendment__badges_of_servitude_reading, tangled_rope).
narrative_ontology:human_readable(thirteenth_amendment__badges_of_servitude_reading, "Thirteenth Amendment Badges-of-Servitude Reading: Congressional Power to Reach Private Discrimination").
narrative_ontology:topic_domain(thirteenth_amendment__badges_of_servitude_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(thirteenth_amendment__badges_of_servitude_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(thirteenth_amendment__badges_of_servitude_reading, 'e72ebe48-de13-4958-aee8-0877c691124a').
narrative_ontology:cs_kernel_codification('e72ebe48-de13-4958-aee8-0877c691124a', fixed_text).
narrative_ontology:cs_authority_grounding('e72ebe48-de13-4958-aee8-0877c691124a', lineage).
narrative_ontology:cs_interpretation_layer_present('e72ebe48-de13-4958-aee8-0877c691124a').
narrative_ontology:cs_reading_relation('e72ebe48-de13-4958-aee8-0877c691124a', thirteenth_amendment__formal_abolition_reading, coexists_with).
narrative_ontology:cs_axiom('e72ebe48-de13-4958-aee8-0877c691124a', foundational, abolition_requires_eradicating_perpetuation_mechanisms).
narrative_ontology:cs_axiom_status(abolition_requires_eradicating_perpetuation_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('e72ebe48-de13-4958-aee8-0877c691124a', abolition_requires_eradicating_perpetuation_mechanisms, deontological).
narrative_ontology:cs_axiom('e72ebe48-de13-4958-aee8-0877c691124a', foundational, congressional_enforcement_power_extends_to_private_discrimination).
narrative_ontology:cs_axiom_status(congressional_enforcement_power_extends_to_private_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('e72ebe48-de13-4958-aee8-0877c691124a', congressional_enforcement_power_extends_to_private_discrimination, deontological).
narrative_ontology:cs_reference_frame('e72ebe48-de13-4958-aee8-0877c691124a', abolitionist_enforcement_comprehensive).
narrative_ontology:cs_drift_state('e72ebe48-de13-4958-aee8-0877c691124a', contemporary_conservative_retrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e72ebe48-de13-4958-aee8-0877c691124a', '').
narrative_ontology:cs_kernel_id(thirteenth_amendment__badges_of_servitude_reading, thirteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thirteenth_amendment__badges_of_servitude_reading, victims_of_race_based_exclusion).
narrative_ontology:constraint_victim(thirteenth_amendment__badges_of_servitude_reading, private_discriminators).
narrative_ontology:constraint_victim(thirteenth_amendment__badges_of_servitude_reading, institutional_preservers_of_caste_residue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIMS OF RACE-BASED EXCLUSION (SNARE) — Trapped within private discrimination networks (housing covenants, employment gatekeeping, credit denial, public accommodation exclusion) that perpetuate badges of servitude. No exit from discrimination without collective legal intervention. Bear full extraction cost: economic mobility blocked, dignity violated, institutional remedies unavailable absent Congressional action. Minimal coordination benefit — the constraint exists to extract, not to solve collective problems.
constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE DISCRIMINATORS (TANGLED ROPE) — Institutional actors (landlords, employers, merchants, lenders) who maintain caste-based exclusion. Experience the constraint as both coordination (enforcement of racial hierarchies through market mechanisms, social norms) and extraction (Congressional power threatens their ability to discriminate freely). Have mobility and agency but constrained by potential statutory reach. Extraction runs toward them as victims if Congress exercises this power; extraction runs from them if this reading is not enforced.
constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL AUTHORITY (ROPE) — Under the badges-of-servitude reading, Congress gains a powerful coordination mechanism: the power to identify and eliminate private discrimination that perpetuates slavery's residue. Low coercion overhead required from Congress's perspective — statutory language and remedial frameworks coordinate diverse private actors toward non-discrimination. Benefits from the institutional legitimacy this reading grants: expanded police power to address structural inequality. No suppression required from this perspective — willing compliance with anti-discrimination statutes is the coordination benefit.
constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / TEXTUAL READING (MOUNTAIN) — From a civilizational perspective, this classification appears as an unavoidable reading of the Thirteenth Amendment's plain language: 'Neither slavery nor involuntary servitude, except as a punishment for crime whereof the party shall have been duly convicted, shall exist within the United States.' The badges-of-servitude interpretation extracts an immutable structural feature: if slavery is abolished, its constituent mechanisms (the badges — disabilities, stereotypes, social stigma) cannot persist. The constraint appears as a logical necessity rather than a contingent institutional choice. However, structural data contradicts this mountain classification — the competing formal_abolition_reading is equally textually defensible, revealing that the mountain disguises a doctrinal contest.
constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CIVIL RIGHTS ENFORCEMENT (PITON) — Organized advocates (NAACP, civil rights litigators, voting rights organizations) working within this reading's framework. See the constraint as a powerful tool that has proven substantially performative: decades of statutory interpretation, court opinions reaffirming the reading (Jones v. Alfred H. Mayer Co., Runyon v. McCrary), and legislative language have established the doctrinal possibility. But actual enforcement remains constrained by political will, resource limitations, and conservative judicial narrowing. The reading persists through institutional inertia and doctrinal precedent, not because its enforcement machinery operates at full capacity. Theater ratio high because the reading's power is proclaimed but incompletely activated.
constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thirteenth_amendment__badges_of_servitude_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thirteenth_amendment__badges_of_servitude_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thirteenth_amendment__badges_of_servitude_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thirteenth_amendment__badges_of_servitude_reading, TR),
    TR >= 0.70.

:- end_tests(thirteenth_amendment__badges_of_servitude_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. This reading requires active statutory enforcement to identify and eliminate private discrimination. The extraction is moderate because the beneficiary set (victims of race-based exclusion) does not directly extract — rather, they are liberated from extraction. But from the perspective of private discriminators (victims under this reading), the constraint extracts their ability to exclude, which is substantive. The trajectory from 0.32 to 0.58 reflects historical pattern: early ratification debates (1865) produced lower measured extractiveness because the reading had not been institutionalized; mid-century enforcement (1950s-1970s) increased extractiveness as courts recognized the reading and Congress acted (Civil Rights Act 1964, Fair Housing Act 1968); contemporary period (1980s-present) shows elevated but stable extractiveness as conservative judges have narrowed the reading but not eliminated it. Suppression (0.68): High. Barriers to enforcement are substantial: political resistance from discriminating institutions, judicial retrenchment, resource limitations on enforcement agencies, and the distributed nature of private discrimination (harder to reach than state action). This is not suppression of victims trying to exit caste — it is suppression of the reading's enforcement mechanism. Theater ratio (0.42): Moderate. The reading is not primarily performative — doctrinal statements and statutory language have real enforcement consequences (housing integration, employment non-discrimination, voting rights). But the ratio is non-trivial because actual enforcement capacity is constrained relative to the breadth of the reading's stated power. The slight rise (0.28 to 0.42) reflects increasing gap between proclaimed power and actual enforcement as conservative judicial narrowing has accumulated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single legal doctrine produces radically different experienced classifications depending on observer position. The powerless victim sees snare — the constraint is the discrimination mechanism, not liberation from it; Congress's power under this reading is the only potential exit, which they cannot exercise alone. The powerful private discriminator sees tangled rope — they benefit from enforcement coordination (predictable rules instead of ad hoc discrimination) but experience significant extraction (loss of ability to exclude). Congress sees rope — a clean coordination mechanism for racial equality with minimal coercive overhead. Civil rights enforcers see piton — the reading persists but is increasingly performative as courts narrow it and enforcement capacity declines. The analytical observer risks seeing mountain — the reading appears as a logical necessity from the Amendment's text — but this masks the fact that the formal_abolition_reading is equally textually grounded, revealing that the mountain is a false summit (a contingent doctrinal choice naturalized as textual necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction differs by agent perspective. From the victims-of-exclusion perspective (powerless/trapped), the constraint's d is high (close to 1.0: these agents bear suppression without exit; they are targets of the discrimination this reading addresses). From the private-discriminators perspective (powerful/mobile), d is moderate-high (0.75-0.85: they face Congressional reach but retain significant agency and geographic arbitrage options — they can move operations, change practices at the margins, or lobby for exemptions). From Congress's perspective (institutional/arbitrage), d is low (around 0.10: Congress benefits from the reading as a coordination tool; it is not being extracted from). The engine derives these values from beneficiary/victim declarations and exit options; the commentary above reflects what those structural inputs represent. The piton perspective (organized/constrained) has d around 0.50: civil rights organizations see themselves as having partial agency (organized) but constrained capacity to enforce the reading fully.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the reading is genuinely hybrid: it contains both coordination content (Congressional mechanism for eradicating slavery's residue as a public good) and asymmetric extraction (from private discriminators who lose exclusion capacity). The tangled_rope classification holds because both elements are structurally necessary — remove the beneficiary side (victims of exclusion receiving protection) and the constraint becomes pure snare; remove the victim side (private discriminators facing enforcement) and it becomes pure rope. The classification stabilizes at tangled rope because Congress must actively enforce both components for the reading to cohere. The theater ratio (0.42) is not high enough to trigger piton gates — the reading is not primarily performative at its peak enforcement eras, though contemporary performance-to-function ratio has risen as courts have narrowed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_indeterminacy_badges_concept,
    'Does the Thirteenth Amendment''s language ''badges and incidents of slavery'' have a sufficiently determinate meaning to ground Congressional power, or is the concept inherently open-ended and thus subject to judicial retrenchment?',
    'Historical analysis of badges-of-servitude jurisprudence (Jones, Runyon, Patterson, Civil Rights Cases); comparison of accepted badges (literacy tests, housing discrimination, credit exclusion) against rejected badges (social stigma absent legal disability); identification of a principled boundary or its absence',
    'If determinate: the constraint stabilizes as tangled_rope with clearer victim/beneficiary boundaries. If indeterminate: the constraint drifts toward piton — the reading survives through doctrinal tradition but lacks stable enforcement mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_indeterminacy_badges_concept, conceptual, 'Whether ''badges and incidents'' has determinate meaning or remains open-ended').

omega_variable(
    private_reach_constitutionality_debate,
    'Can Congress constitutionally reach purely private discrimination under the Thirteenth Amendment, or does some state-action requirement persist despite the Amendment''s broad language?',
    'Comparative constitutional analysis: Thirteenth Amendment text vs. Fourteenth Amendment state-action limitation; Supreme Court doctrinal trajectory (from early ratification cases through Patterson v. McLean Credit Union and contemporary conservative opinions); identification of whether modern courts accept broad private reach or import unstated state-action limits',
    'If private reach is constitutional: extractiveness remains at 0.58 (stable enforcement mechanism). If state-action requirement persists: extractiveness drops to 0.35 (constraint becomes scaffolding with limited actual scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_reach_constitutionality_debate, empirical, 'Whether Congress can constitutionally reach private discrimination under the Thirteenth Amendment').

omega_variable(
    competing_textual_reading_foreclosure,
    'Does this reading''s core premise (slavery''s abolition reaches private discrimination perpetuating caste residue) logically foreclose the formal_abolition_reading''s core premise (Thirteenth Amendment ended only the legal institution, not social aftermath), or can a single legal framework coherently hold both?',
    'Formal doctrinal analysis: whether accepting ''badges of servitude'' Congressional power is incompatible with denying Congressional power over general social inequality; whether the two readings can coexist within originalist, living-constitution, or institutional textualist frameworks',
    'If forecloses: the relationship is competition for a single doctrinal slot — only one reading can govern Thirteenth Amendment interpretation in a coherent legal system. If coexists: the readings can fragment across different institutional levels or time periods. This omega determines the structure of the reading_relations field in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_textual_reading_foreclosure, conceptual, 'Whether badges-of-servitude and formal-abolition readings logically foreclose each other').

omega_variable(
    axiom_grounding_empirical_vs_normative,
    'Is the foundational axiom ''abolishing slavery requires eradicating mechanisms that perpetuate caste hierarchy'' grounded in empirically verifiable facts about what slavery''s residue consists of, or is it fundamentally a normative choice about what ''abolition'' means?',
    'Analysis of how courts and Congress have identified specific badges (literacy requirements, housing covenants, employment discrimination); assessment of whether these identifications are empirical descriptions of slavery''s mechanism or normative determinations of what counts as perpetuation',
    'If empirically grounded: the axiom is vulnerable to evidence-based challenge if caste residue mechanisms are shown not to derive from slavery. If normatively grounded: the axiom is deontological — not subject to foreclosure by empirical findings alone, but contestable on normative grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_grounding_empirical_vs_normative, conceptual, 'Whether the perpetuation axiom rests on empirical or normative grounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thirteenth_amendment__badges_of_servitude_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thir_tr_t0, thirteenth_amendment__badges_of_servitude_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(thir_tr_t40, thirteenth_amendment__badges_of_servitude_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(thir_tr_t80, thirteenth_amendment__badges_of_servitude_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(thir_be_t0, thirteenth_amendment__badges_of_servitude_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(thir_be_t40, thirteenth_amendment__badges_of_servitude_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(thir_be_t80, thirteenth_amendment__badges_of_servitude_reading, base_extractiveness, 80, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thirteenth_amendment__badges_of_servitude_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(thirteenth_amendment__badges_of_servitude_reading, thirteenth_amendment__formal_abolition_reading).

% DUAL FORMULATION NOTE:
% The Thirteenth Amendment kernel decomposes into two constraint stories with distinct extractiveness and beneficiary/victim structures. The badges-of-servitude reading (THIS constraint) models the interpretation under which Congress has broad power to reach private discrimination; the formal_abolition_reading models the competing interpretation limiting the Amendment to ending the legal institution. Network link: THIS reading influences the formal_abolition reading by occupying the doctrinal space and establishing jurisprudential precedent that the formal reading must accommodate or reject.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
