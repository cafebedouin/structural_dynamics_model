% ============================================================================
% CONSTRAINT STORY: later_amendment_eras__reconstruction_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_later_amendment_eras__reconstruction_amendments, []).

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
 *   constraint_id: later_amendment_eras__reconstruction_amendments
 *   human_readable: The Reconstruction Amendments: Constitutional Abolition of Slavery and Nationalization of Rights
 *   domain: constitutional_law/political_foundation
 *
 * SUMMARY:
 *   The Reconstruction Amendments (Thirteenth, Fourteenth, Fifteenth)
 *   represent a second constitutional founding that reconstituted the
 *   American political order in the aftermath of the Civil War. The
 *   Thirteenth Amendment abolished slavery as an institution. The Fourteenth
 *   Amendment established national citizenship, prohibited states from
 *   abridging the privileges or immunities of citizenship, and guaranteed
 *   equal protection and due process. The Fifteenth Amendment prohibited
 *   denial of the vote on grounds of race. These amendments did not merely
 *   modify the Constitution — they transformed its foundational principles by
 *   abolishing the legal category of slavery, nationalizing citizenship
 *   rights, and establishing equal rights as a constitutional floor. From the
 *   perspective of those freed from slavery and their descendants, these
 *   amendments appear as immutable constitutional fact — mountain-grade
 *   constraints that define the operative political order. From the
 *   perspective of the defeated slaveholding order, they appear as coercive
 *   extraction of property, status, and political power — a snare that
 *   suppresses all alternatives. From the analytical vantage, they represent
 *   a constitutional second founding that naturalizes a new order. This
 *   constraint story instantiates ONE reading of the contested kernel
 *   'later_amendment_eras': the Reconstruction reading emphasizes
 *   constitutional abolition and nationalization of rights, contrasting with
 *   the Progressive reading (income tax, direct election, prohibition,
 *   suffrage) and the Civil Rights reading (poll tax, capital votes,
 *   eighteen-year-old suffrage) and the structural housekeeping reading
 *   (electoral mechanics, repeal of prohibition, term limits). This story
 *   generates the Reconstruction reading only; sibling readings are separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Enslaved Persons and Descendants: Primary beneficiary (powerless→generational→trapped→national) — The Amendments establish their freedom and citizenship. Yet the trapped exit option reflects that freedom does not dissolve the structural constraints of post-abolition society. This is boundary-stretching: the Amendments liberate legally but do not guarantee economic or social liberation.
 *   - The Rights-Claiming Citizen: Secondary beneficiary (moderate→biographical→constrained→national) — Any person claiming rights against state action invokes the Fourteenth Amendment's citizenship and equal protection guarantees. Exit is constrained by the state's ability to violate the amendment; the constraint is experienced as a constitutional floor that the state may transgress but cannot legally eliminate.
 *   - The Slaveholding Order: Primary victim of the constraint (institutional→biographical→arbitrage→national) — The Amendments impose a constitutional prohibition on slavery, eliminating the legal infrastructure of enslavement and destroying the property value of enslaved persons. This is experienced as extractive confiscation by the defeated order.
 *   - States Claiming Sovereignty: Secondary victim (institutional→biographical→constrained→national) — The Fourteenth Amendment's privileges-or-immunities clause and state action prohibition restrict state power. States experience the constraint as a federal limit on their ability to regulate persons and conduct within their borders.
 *   - Analytical Observer: Civilizational vantage (analytical→civilizational→analytical→universal) — Sees the Amendments as establishing constitutional mountains: immutable principles that define the operative political order and cannot be violated without amendment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(later_amendment_eras__reconstruction_amendments, 0.08).
domain_priors:suppression_score(later_amendment_eras__reconstruction_amendments, 0.02).
domain_priors:theater_ratio(later_amendment_eras__reconstruction_amendments, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, extractiveness, 0.08).
narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(later_amendment_eras__reconstruction_amendments, mountain).
narrative_ontology:human_readable(later_amendment_eras__reconstruction_amendments, "The Reconstruction Amendments: Constitutional Abolition of Slavery and Nationalization of Rights").
narrative_ontology:topic_domain(later_amendment_eras__reconstruction_amendments, "constitutional_law/political_foundation").

domain_priors:emerges_naturally(later_amendment_eras__reconstruction_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(later_amendment_eras__reconstruction_amendments, '65c31143-c950-4b33-8dd7-1b7dee4bd330').
narrative_ontology:cs_kernel_codification('65c31143-c950-4b33-8dd7-1b7dee4bd330', formalized).
narrative_ontology:cs_authority_grounding('65c31143-c950-4b33-8dd7-1b7dee4bd330', lineage).
narrative_ontology:cs_interpretation_layer_present('65c31143-c950-4b33-8dd7-1b7dee4bd330').
narrative_ontology:cs_reading_relation('65c31143-c950-4b33-8dd7-1b7dee4bd330', later_amendment_eras__civil_rights_era_amendments, influences).
narrative_ontology:cs_reading_relation('65c31143-c950-4b33-8dd7-1b7dee4bd330', later_amendment_eras__progressive_era_amendments, coexists_with).
narrative_ontology:cs_reading_relation('65c31143-c950-4b33-8dd7-1b7dee4bd330', later_amendment_eras__structural_housekeeping_amendments, coexists_with).
narrative_ontology:cs_axiom('65c31143-c950-4b33-8dd7-1b7dee4bd330', foundational, slavery_categorically_abolished).
narrative_ontology:cs_axiom_status(slavery_categorically_abolished, holdable).
narrative_ontology:cs_axiom_grounding('65c31143-c950-4b33-8dd7-1b7dee4bd330', slavery_categorically_abolished, deontological).
narrative_ontology:cs_axiom('65c31143-c950-4b33-8dd7-1b7dee4bd330', foundational, citizenship_rights_national_not_state).
narrative_ontology:cs_axiom_status(citizenship_rights_national_not_state, holdable).
narrative_ontology:cs_axiom_grounding('65c31143-c950-4b33-8dd7-1b7dee4bd330', citizenship_rights_national_not_state, deontological).
narrative_ontology:cs_reference_frame('65c31143-c950-4b33-8dd7-1b7dee4bd330', constitutional_abolition_and_national_citizenship).
narrative_ontology:cs_drift_state('65c31143-c950-4b33-8dd7-1b7dee4bd330', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('65c31143-c950-4b33-8dd7-1b7dee4bd330', '').
narrative_ontology:cs_kernel_id(later_amendment_eras__reconstruction_amendments, later_amendment_eras).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(later_amendment_eras__reconstruction_amendments, enslaved_persons_and_descendants).
narrative_ontology:constraint_beneficiary(later_amendment_eras__reconstruction_amendments, rights_claimants_against_state_abridgment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENSLAVED AND FREEDMEN (MOUNTAIN) — From the generational perspective of those held in bondage, the Thirteenth Amendment's prohibition of slavery appears as an immutable constitutional fact — a structural fact of political organization that cannot be negotiated or compromised. The amendment eliminates the legal category that defined them; exit from enslavement is not a choice but a constitutional state. This is experienced as mountain-grade naturalization: slavery is abolished, period. No degrees of freedom remain to renegotiate the core claim.
constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE RIGHTS-CLAIMING CITIZEN (MOUNTAIN) — From the perspective of a citizen claiming rights against state action under the Fourteenth Amendment's citizenship clause and equal protection guarantee, the constitutional text establishes an immutable floor: no state may abridge the privileges or immunities of citizenship, and equal protection is non-negotiable. The constraint is mountain-grade because the constitutional prohibition admits no middle ground — either the state respects it or violates it; there is no scaling or partial compliance that preserves the constitutional settlement.
constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE DEFEATED SLAVEHOLDING ORDER (SNARE) — From the perspective of the antebellum slaveholding elite, the Reconstruction Amendments impose pure extraction: the constitutional prohibition of slavery seizes the property value of enslaved persons, eliminates the legal infrastructure that protected slavery, and creates equal citizenship that inverts the status hierarchy. This perspective experiences the constraint as coercive suppression of alternatives (there is no constitutional path to re-legalize slavery) with zero coordination benefit. The abolition is experienced as extractive confiscation of an entire economic system. High suppression (prohibition admits no exception); high extractiveness (property value destruction); no coordination function. This reading of the Amendments as snare-like (from the defeated slaveholder perspective) coexists with the mountain reading (from the freedmen perspective) — they are mutually constitutive.
constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational vantage, the Reconstruction Amendments represent a second constitutional founding that naturalizes a new political order: the abolition of slavery and the nationalization of citizenship rights become constitutional bedrock — immutable without amendment. The analytical frame sees these amendments as establishing axioms of the constitutional system itself. The mountain classification reflects that the Amendments create irreducible logical constraints on constitutional action: no constitutional actor can produce slavery, abridge citizenship rights, or deny equal protection without amending the Constitution itself. This is the standard analytical view — the Amendments are seen as constitutional mountains, not contingent arrangements.
constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(later_amendment_eras__reconstruction_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(later_amendment_eras__reconstruction_amendments, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, ExtMetricName, E),
    domain_priors:suppression_score(later_amendment_eras__reconstruction_amendments, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(later_amendment_eras__reconstruction_amendments),
    narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(later_amendment_eras__reconstruction_amendments, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(later_amendment_eras__reconstruction_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.08): The base extractiveness is very low because the Amendments establish a foundational principle (abolition of slavery and nationalization of rights) with minimal performative overhead. The Constitutional text simply declares the prohibition; there is no extraction of value in the text itself. The low value reflects that the Amendments are a pure normative reordering rather than a mechanism for extracting resources or labor. However, this low value applies specifically to the Amendments as written text and constitutional command. The enforcement gap omega (id: enforcement_gap_interpretation) captures the empirical reality that enforcement has been contested and inconsistent. A separate downstream constraint (not modeled here) would address the enforcement machinery and post-abolition subordination, which have much higher extractiveness. SUPPRESSION (0.02): The Amendments establish a constitutional prohibition (slavery is illegal; states cannot abridge citizenship rights) with no ambiguity or exception. The suppression value reflects the absoluteness of the constitutional text — there is no degree of freedom for alternative readings of the core prohibition. Suppression is low because the mechanism is a pure legal prohibition, not an enforcement apparatus that requires coercion. The high resistance to change (0.08) and accessibility collapse (0.92) reflect that the Amendments require amendment to revise, making them effectively immutable within the constitutional system. THEATER_RATIO (0.15): The Amendments are substantive constitutional law with minimal performative content. The ratification process, the amendment text, and the immediate effects are direct legal acts, not rituals. The low theater value reflects this directness. The 15% residual theater accounts for the ceremonial aspects of ratification and the formal announcement, but the core constraint is substantive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival polarization. From the powerless-trapped perspective (the enslaved and freedmen), it is a mountain: constitutional immutability that liberates them. From the institutional-arbitrage perspective (the defeated slaveholding order), it is a snare: coercive suppression with zero coordination benefit and maximum extraction of property and status. From the analytical perspective, it is a mountain: a second constitutional founding that establishes irreducible principles. The gap between snare (slaveholder view) and mountain (freedmen and analytical view) is not a difference of interpretation but a structural fact: the same amendment that abolishes slavery for the enslaved imposes extraction on the slaveholder. The constraint is constitutively polarized — it cannot be experienced symmetrically. This is the normal structure of constraints that overturn a previous social order.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the fundamental structural inversion created by the Amendments. For the freedmen (beneficiary + trapped), d is low (approximately 0.15–0.25): they experience the constraint as liberation, despite the continued barriers. For the rights-claiming citizen (beneficiary + constrained), d is moderate (approximately 0.40–0.50): the constitutional guarantee is a resource, but enforcement is inconsistent. For the slaveholding order (victim + arbitrage), d is very high (approximately 0.85–0.95): they experience the constraint as extractive confiscation. For the analytical observer (analytical + analytical), d is approximately 0.72–0.73 (canonical analytical). The beneficiary/victim declarations are explicit in base_properties: enslaved_persons_and_descendants and rights_claimants_against_state_abridgment are beneficiaries; the defeated Confederate settlement and states claiming sovereignty are implicit victims (not declared in base_properties because a mountain constraint does not require victim declaration, but the snare perspective makes the victimhood structural).
 *
 * MANDATROPHY ANALYSIS:
 *   The Reconstruction Amendments resolve potential mandatrophy by establishing a clear constitutional hierarchy: the Amendments are foundational law that establishes the operative framework within which all other constitutional action occurs. They cannot be reinterpreted as coordination without falsifying the historical record (slavery is not a coordination mechanism) or the constitutional text (the prohibition is categorical, not negotiable). The mandatrophy would arise if the analytical perspective attempted to classify the Amendments as rope (pure coordination) on the grounds that they coordinate state action and establish a uniform legal order. But this misses the asymmetry: the Amendments do coordinate (they create a common legal standard), but they do so by extracting value from the slaveholding order and conferring it on the freedmen. From the slaveholder perspective, this is pure extraction; from the freedmen perspective, this is justice and liberation. The mountain classification captures this: the Amendments establish immutable constitutional principles that are not open to renegotiation or scaling. The snare perspective (slaveholder view) is legitimate but represents the perspective of the agent whose property rights were abolished, not the perspective of the constitutional order itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abolition_as_natural_vs_constituted,
    'Is the abolition of slavery a natural law (inherent to human dignity, discovered by the Amendments) or a constituted legal order (created by the Amendments, contingent on their adoption and enforcement)?',
    'Philosophical analysis of the source of the prohibition''s authority: Does the Amendment declare a pre-existing truth about human personhood, or does it constitute a new legal category? Does the binding force come from natural law or from the amendment''s formal ratification and enforcement mechanisms?',
    'If natural law: the mountain classification is justified on epistemic grounds — the Amendments reveal immutable truth. If constituted: the mountain classification is justified on constitutional grounds — the Amendments create immutable constitutional law, but this does not mean the underlying principle is naturally immutable, only that the amendment process forecloses easy revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abolition_as_natural_vs_constituted, conceptual, 'Whether abolition is natural law or constituted legal order').

omega_variable(
    enforcement_gap_interpretation,
    'The period between Reconstruction amendment ratification and enforcement reveals persistent state violation of the constitutional text. Does this gap challenge the mountain classification (showing the constraint is contingent on enforcement machinery) or does it demonstrate the mountain''s integrity (showing the constitutional text endures despite violation)?',
    'Examine the structural relationship between constitutional text and enforcement capacity. If the text''s authority depends on enforcement, then enforcement failure constitutes authority failure (mountain becomes contingent, possibly rope or tangled_rope). If the text''s authority is independent of enforcement (the text is immutable even when violated), then the mountain stands despite enforcement gaps.',
    'If contingent on enforcement: the Amendments might be reclassified as tangled_rope (constitutive of rights + requires active enforcement to prevent state abridgment) or even rope (if enforcement succeeds sufficiently). If independent: the mountain classification holds even when enforcement is weak or fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_interpretation, conceptual, 'Whether enforcement gaps challenge the mountain classification').

omega_variable(
    subsequent_amendment_revisability,
    'The Reconstruction Amendments themselves can be amended or repealed through Article V. Does this mean the constraints they establish are not truly immutable (no mountain can be amended; mountains are the immutable constraints that remain after all possible amendments)? Or does the amendment process itself constitute the upper bound of mutability, making the Amendments mountain-grade at the constitutional level even though technically revisable?',
    'Clarify the definition of mountain in the context of constitutional constraints. If mountains must be logically immutable (not revisable even theoretically), the Amendments are not mountains — they are constitutional law at the highest rank but still formally revisable. If mountains include constraints that define the operative framework of a system (and are only revisable through the highest-order process), then the Amendments are mountains within the constitutional system.',
    'Affects the entire mountain classification strategy for constitutional constraints. If constitutional law is never mountain-grade (always revisable via Article V), then all Reconstruction Amendment perspectives should be reclassified to rope (high-level coordination) or tangled_rope (coordination + asymmetric extraction from the defeated order). If constitutional law at the Reconstruction level is mountain-grade (immutable within the operative constitutional system), the current classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsequent_amendment_revisability, conceptual, 'Whether formal revisability via Article V disqualifies mountain classification').

omega_variable(
    subordination_persistence_paradox,
    'The Amendments formally abolished slavery and established equal citizenship, yet subordination of African Americans persisted and persists through segregation, discrimination, and structural inequality. Does the persistence of subordination after abolition indicate that the extractiveness was higher (0.08 underestimates the constraint''s actual extraction) or that subordination is a separate constraint from the Amendments (different ε value, different story)?',
    'Distinguish the constraint ''abolition of slavery by constitutional command'' from the constraint ''maintenance of racial hierarchy after formal abolition.'' The first has low extractiveness (it transfers power from slaveholders to the freedmen). The second (Jim Crow, segregation, continued discrimination) is a different constraint with different extractiveness. Use network.affects_constraints to link them as a constraint family rather than conflating them into a single story.',
    'Current extractiveness (0.08) reflects the Amendments'' explicit abolition of slavery. If subordination persistence is modeled as part of this constraint, extractiveness should rise to 0.35–0.45 to reflect that the Constitutional promise of equal protection was not enforced and was contested for a century. The distinction affects the classification: Amendments alone might be mountain (text is immutable), but Amendments + enforcement gap become tangled_rope (text establishes rights + persistent violation despite constitutional prohibition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_persistence_paradox, empirical, 'Whether post-abolition subordination is part of this constraint or a separate downstream constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(later_amendment_eras__reconstruction_amendments, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recon_tr_t0, later_amendment_eras__reconstruction_amendments, theater_ratio, 0, 0.15).
narrative_ontology:measurement(recon_tr_t25, later_amendment_eras__reconstruction_amendments, theater_ratio, 25, 0.15).
narrative_ontology:measurement(recon_tr_t50, later_amendment_eras__reconstruction_amendments, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(recon_be_t0, later_amendment_eras__reconstruction_amendments, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(recon_be_t25, later_amendment_eras__reconstruction_amendments, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(recon_be_t50, later_amendment_eras__reconstruction_amendments, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(later_amendment_eras__reconstruction_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(later_amendment_eras__reconstruction_amendments, jim_crow_segregation_regime).
narrative_ontology:affects_constraint(later_amendment_eras__reconstruction_amendments, state_action_doctrine_limits).
narrative_ontology:affects_constraint(later_amendment_eras__reconstruction_amendments, privileges_immunities_interpretation_drift).

% DUAL FORMULATION NOTE:
% The Reconstruction Amendments as written text and constitutional command (this story, ε=0.08, Mountain) must be distinguished from the enforcement gap and post-abolition subordination (separate downstream constraints with higher extractiveness). The present story models the Amendments' explicit abolition and constitutional establishment of rights. The downstream constraints model what happened after: the violent suppression of Black political participation during Redemption, Jim Crow segregation, and the doctrine of state action that limited the Fourteenth Amendment's reach. These are separate constraints with different ε values linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
