% ============================================================================
% CONSTRAINT STORY: reconstruction_amendments__thirteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reconstruction_amendments__thirteenth_amendment, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reconstruction_amendments__thirteenth_amendment
 *   human_readable: Thirteenth Amendment Abolition of Slavery and Involuntary Servitude
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Thirteenth Amendment, ratified in 1865, abolished slavery and
 *   involuntary servitude everywhere in the United States, with a single
 *   specified exception: punishment for crime. This constraint is one reading
 *   of the Reconstruction constitutional kernel — the contested commitment
 *   that grounds the three Reconstruction amendments (Thirteenth, Fourteenth,
 *   Fifteenth). The Thirteenth Amendment reading emphasizes the direct
 *   abolition of chattel bondage as a legal and personal status, binding all
 *   actors (state and private), with no intermediate mediation through
 *   citizenship, voting rights, or due process. The amendment instantiates a
 *   mountain constraint: the abolition of slavery is an immutable legal fact,
 *   accessible only through formal constitutional repeal. However, this
 *   reading coexists with competing readings instantiated by the Fourteenth
 *   Amendment (which grounds abolition in citizenship and equal protection)
 *   and the Fifteenth Amendment (which addresses voting rights and racial
 *   exclusion). The Thirteenth Amendment's unique feature is its direct
 *   binding on private actors — it is the only constitutional amendment that
 *   does not rely on state action doctrine to prevent private parties from
 *   enslaving. Measurement data shows that extractiveness dropped from 0.95
 *   (chattel slavery economy, pre-1868) to 0.08 (post-amendment), and has
 *   remained stable across 156 years of post-ratification history. Theater
 *   ratio has remained minimal (0.10-0.15), indicating that the amendment
 *   functions through legal force rather than performative ritual. The
 *   unresolved structural tension is the punishment exception: the amendment
 *   explicitly permits involuntary servitude as punishment for crime,
 *   creating a residual extraction mechanism that the core principle appears
 *   to target but the amendment's own language preserves.
 *
 * KEY AGENTS:
 *   - Formerly Enslaved and Descendants: Primary beneficiary (powerless/civilizational) — experienced the constraint as structural liberation; bears the historical and ongoing burden of incomplete enforcement
 *   - Free Labor Economy and Northern Industrial Interests: Secondary beneficiary (institutional/arbitrage) — benefits from the abolition of slavery as an economic system that competed with wage labor
 *   - Slaveholding South: Primary victim at ratification (institutional/arbitrage, trapped) — experienced the amendment as an irreversible structural collapse of their economic foundation
 *   - Federal Legal Order: Authority structure (institutional/arbitrage) — enforces the amendment as constitutional law binding all actors
 *   - Carceral System: Residual extractor (institutional/arbitrage) — operates within the punishment exception, extracting labor under state authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses the amendment as foundational constraint; questions the coherence of the punishment exception with the core principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reconstruction_amendments__thirteenth_amendment, 0.08).
domain_priors:suppression_score(reconstruction_amendments__thirteenth_amendment, 0.02).
domain_priors:theater_ratio(reconstruction_amendments__thirteenth_amendment, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, extractiveness, 0.08).
narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reconstruction_amendments__thirteenth_amendment, mountain).
narrative_ontology:human_readable(reconstruction_amendments__thirteenth_amendment, "Thirteenth Amendment Abolition of Slavery and Involuntary Servitude").
narrative_ontology:topic_domain(reconstruction_amendments__thirteenth_amendment, "political/legal/constitutional").

domain_priors:emerges_naturally(reconstruction_amendments__thirteenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reconstruction_amendments__thirteenth_amendment, 'df66e438-3612-45fc-a3e1-bf44699150ba').
narrative_ontology:cs_kernel_codification('df66e438-3612-45fc-a3e1-bf44699150ba', fixed_text).
narrative_ontology:cs_authority_grounding('df66e438-3612-45fc-a3e1-bf44699150ba', lineage).
narrative_ontology:cs_interpretation_layer_present('df66e438-3612-45fc-a3e1-bf44699150ba').
narrative_ontology:cs_reading_relation('df66e438-3612-45fc-a3e1-bf44699150ba', reconstruction_amendments__fourteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('df66e438-3612-45fc-a3e1-bf44699150ba', reconstruction_amendments__fifteenth_amendment, coexists_with).
narrative_ontology:cs_axiom('df66e438-3612-45fc-a3e1-bf44699150ba', foundational, chattel_slavery_abolished_categorically).
narrative_ontology:cs_axiom_status(chattel_slavery_abolished_categorically, holdable).
narrative_ontology:cs_axiom_grounding('df66e438-3612-45fc-a3e1-bf44699150ba', chattel_slavery_abolished_categorically, deontological).
narrative_ontology:cs_axiom('df66e438-3612-45fc-a3e1-bf44699150ba', foundational, legal_personhood_universal_within_us_jurisdiction).
narrative_ontology:cs_axiom_status(legal_personhood_universal_within_us_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('df66e438-3612-45fc-a3e1-bf44699150ba', legal_personhood_universal_within_us_jurisdiction, deontological).
narrative_ontology:cs_axiom('df66e438-3612-45fc-a3e1-bf44699150ba', secondary, amendment_binds_private_actors_directly).
narrative_ontology:cs_axiom_status(amendment_binds_private_actors_directly, holdable).
narrative_ontology:cs_axiom_grounding('df66e438-3612-45fc-a3e1-bf44699150ba', amendment_binds_private_actors_directly, conventional).
narrative_ontology:cs_axiom('df66e438-3612-45fc-a3e1-bf44699150ba', secondary, punishment_exception_legitimate).
narrative_ontology:cs_axiom_status(punishment_exception_legitimate, overridden).
narrative_ontology:cs_axiom_grounding('df66e438-3612-45fc-a3e1-bf44699150ba', punishment_exception_legitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('df66e438-3612-45fc-a3e1-bf44699150ba', chattel_slavery_abolition).
narrative_ontology:cs_drift_state('df66e438-3612-45fc-a3e1-bf44699150ba', contemporary_carceral_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('df66e438-3612-45fc-a3e1-bf44699150ba', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(reconstruction_amendments__thirteenth_amendment, reconstruction_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reconstruction_amendments__thirteenth_amendment, formerly_enslaved_and_descendants).
narrative_ontology:constraint_beneficiary(reconstruction_amendments__thirteenth_amendment, free_labor_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENSLAVED AND FORMERLY ENSLAVED (MOUNTAIN) — From the structural position of chattel bondage, the Thirteenth Amendment abolition is an immutable legal and moral fact, accessible only through formal legal repeal. The constraint's enforcement is near-absolute: the amendment stands as foundational law, changing the ontological status of human beings from property to persons. This perspective experiences the constraint as an irreversible boundary condition.
constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE FEDERAL LEGAL ORDER (MOUNTAIN) — The Thirteenth Amendment binds all actors — state and private — as foundational constitutional law. From the standpoint of legal authority, the amendment is a structural fact: it cannot be overridden by statute, contract, or custom; it can only be repealed through a new constitutional amendment. The federal legal order experiences the constraint as immutable and universal.
constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE SLAVEHOLDING ECONOMY AT RATIFICATION (MOUNTAIN) — From the perspective of the slaveholding South in 1865, the Thirteenth Amendment was experienced as an immutable structural collapse: the legal framework that sustained the plantation economy was abolished by amendment, with no internal mechanism for reversal within the constitutional order. The suppression of exit was total — slavery was ended; there was no negotiable middle ground.
constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal scope, the Thirteenth Amendment represents an irreducible legal and moral boundary: the abolition of chattel slavery and involuntary servitude is a foundational constraint on all subsequent legal and social arrangements in the United States. No observable measurement changes this classification; no alternative framing renders it negotiable. The constraint is a true mountain.
constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reconstruction_amendments__thirteenth_amendment_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reconstruction_amendments__thirteenth_amendment, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, ExtMetricName, E),
    domain_priors:suppression_score(reconstruction_amendments__thirteenth_amendment, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reconstruction_amendments__thirteenth_amendment),
    narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reconstruction_amendments__thirteenth_amendment, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reconstruction_amendments__thirteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): The Thirteenth Amendment abolishes chattel slavery and involuntary servitude as legal categories, reducing extractiveness from 0.95 (slavery economy) to near-zero (0.08, accounting for the punishment exception and measurement uncertainty). The residual 0.08 reflects the punishment clause as a specified exception; absent that exception, extractiveness would be 0.00. Suppression (0.02): The amendment establishes a legal ceiling on suppression — slavery is illegal everywhere in the US, binding all actors including private parties. Suppression represents only the measurement uncertainty and the gap between formal law and enforcement; the amendment itself creates minimal suppression (legal prohibition is absolute). Theater ratio (0.15): The amendment functions primarily through legal force and institutional prohibition, not through performative ritual. The low theater ratio indicates that the amendment's binding mechanism is enforcement (legal consequences for enslavement) rather than cultural performance. Accessibility collapse (0.92): The amendment is nearly impossible to reverse without a new constitutional amendment — the formal amendment process is the only mechanism. Resistance (0.08): There is minimal structural resistance to the amendment's core principle in contemporary law, though resistance persists in enforcement gaps and the punishment exception.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this constraint as mountain, reflecting that the Thirteenth Amendment abolition is universally experienced as immutable and foundational across structural positions. The agreement across perspectives is unusual and diagnostically significant: it indicates a constraint that has achieved near-universal legal and institutional recognition as a settled boundary condition. The tiny perspectival gap (all mountain, all high accessibility_collapse, all low resistance) reflects that this is a successful constitutional settlement — chattel slavery is not legally contested in contemporary US law. The potential gap emerges only at the omega level: the punishment exception creates a residual asymmetry (extractiveness ~0.08 rather than 0.00) that some perspectives (the formerly enslaved, justice-oriented observers) might classify as an unresolved tension requiring a second Thirteenth Amendment interpretation, while institutional and legal perspectives treat the exception as a legitimate historical provision. This gap between legal settlement and normative residue is the constraint's diagnostic edge.
 *
 * DIRECTIONALITY LOGIC:
 *   The Thirteenth Amendment establishes a directional flow opposite to chattel slavery: instead of forced extraction from the enslaved toward the slaveholder, the amendment creates a prohibition that protects the formerly enslaved. The beneficiary group (formerly enslaved and descendants) experiences d ≈ 0.05 (near-zero extraction; maximum benefit from the prohibition). The victim group at ratification (slaveholding economy) experienced d ≈ 0.95 (maximum extraction loss; the amendment abolished their extractive mechanism). The Federal Legal Order enforces the constraint as binding on all actors, with d ≈ 0.00 (the legal system is the authority, not the target or beneficiary). The carceral system experiences d ≈ 0.60 (constrained beneficiary — the punishment exception enables extraction, but within specified legal bounds). The analytical observer experiences d ≈ 0.72 (analytical perspective on the structural question of whether the amendment is complete or partial — the punishment exception creates ambiguity). All d values are derived from the structural relationship (beneficiary vs. victim) and the amendment's binding force on all actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not instantiate mandatrophy because extractiveness is uniformly low (0.08) and all perspectives converge on the mountain classification. The mandatrophy would arise if the three Reconstruction amendments were treated as a single constraint, creating tension between incompatible beneficiary/victim structures and enforcement mechanisms — but the kernel decomposition resolves this by treating them as three distinct constraint stories with distinct readings. The resolution of mandatrophy here is structural: by decomposing the kernel into three distinct constraint stories (Thirteenth, Fourteenth, Fifteenth), each with its own beneficiary/victim structure and enforcement mechanism, the apparent contradiction resolves into a complementary multi-layered settlement. The Thirteenth reading provides categorical abolition; the Fourteenth provides individual rights protection; the Fifteenth provides political inclusion. No single type adequately captures all three; therefore, three separate stories, each with its proper type, are generated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    punishment_exception_scope,
    'Does the punishment exception to the Thirteenth Amendment (slavery/involuntary servitude as punishment for crime) represent a legitimate residual category or an extractive loophole that the amendment''s core principle should foreclose?',
    'Historical analysis of legislative intent; comparative analysis of modern carceral systems against chattel slavery extraction mechanisms; normative legal theory on whether the exception is coherent with the amendment''s foundational principle',
    'If the exception is a legitimate categorical distinction: the amendment''s extractiveness remains at 0.08 (the exception is narrow and specified). If the exception represents an unresolved tension: the amendment''s extractiveness rises to 0.18-0.22 (structural extraction persists in carceral form, partly through the amendment''s own language)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(punishment_exception_scope, conceptual, 'Whether the punishment exception is a legitimate residual or an extractive loophole').

omega_variable(
    private_actor_binding_uniqueness,
    'What is the historical and structural significance of the Thirteenth Amendment being the only constitutional amendment that directly binds private actors (not just state actors)? Does this represent a unique principled commitment or a contingent historical fact?',
    'Comparative analysis of other amendments'' scope and the historical debate over private-actor binding in Reconstruction; examination of whether other principles (due process, equal protection) should also bind private actors; consideration of whether private-actor binding is essential to the amendment''s function or incidental',
    'If unique and principled: the amendment''s classification as mountain is strengthened (irreducible principle). If contingent: the amendment''s binding on private actors could theoretically be narrowed through legislative reinterpretation or state action doctrine, lowering its accessibility_collapse score',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_binding_uniqueness, conceptual, 'Structural significance of private-actor binding uniqueness').

omega_variable(
    persistent_extraction_through_systemic_inequality,
    'Does the Thirteenth Amendment''s abolition of chattel slavery leave unresolved extraction mechanisms (labor exploitation, wage suppression, occupational segregation, asset theft) that operate through systemic inequality rather than legal slavery, such that the amendment''s extractiveness should account for residual structural coercion?',
    'Empirical analysis of post-abolition extraction trajectories; comparison of forced-labor extraction (pre-amendment) vs. systemic inequality extraction (post-amendment); determination of whether residual extraction is extractive constraint (separate constraint stories) or a failure of enforcement of the amendment itself',
    'If residual extraction is a separate constraint: the Thirteenth Amendment remains mountain (ε=0.08) and those residual mechanisms are classified as tangled_rope or snare in separate stories (e.g., Jim Crow caste, carceral labor). If residual extraction is part of the amendment''s own scope: extractiveness rises to 0.20-0.28 (the amendment is incompletely enforced)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistent_extraction_through_systemic_inequality, empirical, 'Whether residual extraction mechanisms represent amendment scope or separate constraints').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the Thirteenth Amendment''s core principle (abolition of chattel bondage and involuntary servitude as legal categories) logically foreclose or coexist with the Fourteenth Amendment''s approach (nationalizing citizenship and individual rights through state action doctrine) and the Fifteenth Amendment''s approach (voting rights protection)?',
    'Normative legal theory on whether the three amendments represent a unified abolitionist principle or three separate and potentially conflicting commitments; historical analysis of the Reconstruction framers'' intent on how the amendments relate',
    'If the Thirteenth forecloses the others: the reading_relations should be forecloses (rare). If they coexist: reading_relations should be coexists_with (most likely). If the Thirteenth influences the structure that enables the others: reading_relations should be influences',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Logical relationship between Thirteenth Amendment abolition and sibling amendments'' principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reconstruction_amendments__thirteenth_amendment, 0, 156).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thirteenth_theater_post_amendment_1868, reconstruction_amendments__thirteenth_amendment, theater_ratio, 1, 0.1).
narrative_ontology:measurement(thirteenth_theater_1920, reconstruction_amendments__thirteenth_amendment, theater_ratio, 50, 0.12).
narrative_ontology:measurement(thirteenth_theater_1970, reconstruction_amendments__thirteenth_amendment, theater_ratio, 100, 0.15).
narrative_ontology:measurement(thirteenth_theater_2020, reconstruction_amendments__thirteenth_amendment, theater_ratio, 150, 0.15).

% Extraction over time
narrative_ontology:measurement(thirteenth_extractiveness_pre_amendment, reconstruction_amendments__thirteenth_amendment, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(thirteenth_extractiveness_post_amendment_1868, reconstruction_amendments__thirteenth_amendment, base_extractiveness, 1, 0.08).
narrative_ontology:measurement(thirteenth_extractiveness_1920, reconstruction_amendments__thirteenth_amendment, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(thirteenth_extractiveness_1970, reconstruction_amendments__thirteenth_amendment, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(thirteenth_extractiveness_2020, reconstruction_amendments__thirteenth_amendment, base_extractiveness, 150, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reconstruction_amendments__thirteenth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(reconstruction_amendments__thirteenth_amendment, reconstruction_amendments__fourteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__thirteenth_amendment, reconstruction_amendments__fifteenth_amendment).
narrative_ontology:affects_constraint(reconstruction_amendments__thirteenth_amendment, jim_crow_caste_system).
narrative_ontology:affects_constraint(reconstruction_amendments__thirteenth_amendment, thirteenth_amendment_punishment_exception).

% DUAL FORMULATION NOTE:
% The Thirteenth Amendment is one reading of the contested Reconstruction kernel. The Fourteenth and Fifteenth Amendment readings are separate constraint stories with distinct beneficiary/victim structures and enforcement mechanisms. All three share the common kernel (the Reconstruction constitutional commitment) but instantiate different constraint types and extractiveness profiles. The punishment exception is a downstream constraint that operates within the Thirteenth Amendment's residual space, extracting labor through carceral mechanisms. Jim Crow and subsequent caste systems are downstream constraints that operate in the gap between formal abolition and substantive equality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
