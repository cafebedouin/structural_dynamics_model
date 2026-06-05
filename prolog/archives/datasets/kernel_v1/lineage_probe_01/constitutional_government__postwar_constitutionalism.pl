% ============================================================================
% CONSTRAINT STORY: constitutional_government__postwar_constitutionalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_government__postwar_constitutionalism, []).

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
 *   constraint_id: constitutional_government__postwar_constitutionalism
 *   human_readable: Constitutional Government as Postwar Rights-Anchored Reconstruction
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The postwar constitutionalism reading construes constitutional government
 *   as a designed reaction to catastrophic regime failure. After the collapse
 *   of a predatory state apparatus (the classical postwar scenario: Weimar to
 *   Bonn 1949, Vichy to Fifth Republic 1958, fascist Spain to 1978),
 *   successor regimes anchor governance in a written constitution that
 *   explicitly protects individual rights and includes 'militant democracy'
 *   or 'eternity clauses' — provisions designed to make the prior regime type
 *   structurally impossible. The West German Basic Law (Grundgesetz)
 *   exemplifies this reading: Article 1 protects human dignity as inviolable;
 *   Articles 1–19 entrench fundamental rights; Article 21(2) permits
 *   dissolution of parties that threaten constitutional democracy; Article
 *   79(3) declares amendments to the federal system, the constitutional state
 *   principle, or human dignity itself void even if passed by supermajority.
 *   This reading competes with three sibling readings of the same kernel
 *   ('constitutional government'): ancient constitutionalism sees limitation
 *   through mixed orders; revolutionary constitutionalism sees a founding act
 *   of first-principles constitution-making; westminster evolution sees
 *   accumulation of statute and convention. The postwar reading is distinct
 *   in its reactive structure — it is designed specifically to prevent
 *   something, not to establish something. The backward-directed suppression
 *   (preventing the prior regime) is the core mechanism. Extractiveness is
 *   low because the constraint's purpose is protective, not extractive — it
 *   expands rights rather than concentrating them. Victims are movements that
 *   structurally resemble the proscribed regime type; they are not victims of
 *   extraction but of deliberate exclusion from constitutional protection.
 *   Theater ratio rises over time as the historical threat recedes and the
 *   ritual invocations of 'never again' become increasingly performative.
 *
 * KEY AGENTS:
 *   - Protected Individuals: Primary beneficiary (powerless/constrained) — rights-holders whose dignity and fundamental freedoms are constitutionally entrenched and judicially enforced
 *   - Minority Rights Holders: Secondary beneficiary (powerless/constrained) — groups historically targeted by the proscribed regime type; beneficiaries of constitutional protections against majoritarian elimination
 *   - Movements Resembling the Proscribed Regime: Victim set (moderate/constrained) — political movements structurally similar to the defeated regime type; face militant-democracy clauses and eternity-clause foreclosure from constitutional amendment
 *   - Constitutional Court / Judicial Guardian: Institutional actor (institutional/arbitrage) — enforces rights protections and polices constitutional boundaries; derives legitimate authority from guardian role rather than extractive position
 *   - Democratic Political Actors: Secondary beneficiary (institutional/arbitrage) — mainstream parties benefit from rule-of-law stabilization and from the militant-democracy apparatus that excludes regime-threatening competitors
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — examines whether postwar constitutionalism naturalizes a contingent institutional choice as a structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_government__postwar_constitutionalism, 0.18).
domain_priors:suppression_score(constitutional_government__postwar_constitutionalism, 0.35).
domain_priors:theater_ratio(constitutional_government__postwar_constitutionalism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_government__postwar_constitutionalism, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_government__postwar_constitutionalism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_government__postwar_constitutionalism, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_government__postwar_constitutionalism, rope).
narrative_ontology:human_readable(constitutional_government__postwar_constitutionalism, "Constitutional Government as Postwar Rights-Anchored Reconstruction").
narrative_ontology:topic_domain(constitutional_government__postwar_constitutionalism, "political/legal/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_government__postwar_constitutionalism, '73451b97-b5be-4d64-83ea-c0210f169938').
narrative_ontology:cs_kernel_codification('73451b97-b5be-4d64-83ea-c0210f169938', formalized).
narrative_ontology:cs_authority_grounding('73451b97-b5be-4d64-83ea-c0210f169938', lineage).
narrative_ontology:cs_interpretation_layer_present('73451b97-b5be-4d64-83ea-c0210f169938').
narrative_ontology:cs_reading_relation('73451b97-b5be-4d64-83ea-c0210f169938', constitutional_government__ancient_constitutionalism, coexists_with).
narrative_ontology:cs_reading_relation('73451b97-b5be-4d64-83ea-c0210f169938', constitutional_government__revolutionary_constitutionalism, influences).
narrative_ontology:cs_reading_relation('73451b97-b5be-4d64-83ea-c0210f169938', constitutional_government__westminster_evolution, coexists_with).
narrative_ontology:cs_axiom('73451b97-b5be-4d64-83ea-c0210f169938', foundational, catastrophic_regime_failure_requires_rights_reconstruction).
narrative_ontology:cs_axiom_status(catastrophic_regime_failure_requires_rights_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('73451b97-b5be-4d64-83ea-c0210f169938', catastrophic_regime_failure_requires_rights_reconstruction, deontological).
narrative_ontology:cs_axiom('73451b97-b5be-4d64-83ea-c0210f169938', foundational, proscribed_regime_type_must_be_structurally_impossible).
narrative_ontology:cs_axiom_status(proscribed_regime_type_must_be_structurally_impossible, holdable).
narrative_ontology:cs_axiom_grounding('73451b97-b5be-4d64-83ea-c0210f169938', proscribed_regime_type_must_be_structurally_impossible, deontological).
narrative_ontology:cs_axiom('73451b97-b5be-4d64-83ea-c0210f169938', secondary, written_constitution_enables_backward_suppression).
narrative_ontology:cs_axiom_status(written_constitution_enables_backward_suppression, holdable).
narrative_ontology:cs_axiom_grounding('73451b97-b5be-4d64-83ea-c0210f169938', written_constitution_enables_backward_suppression, instrumental).
narrative_ontology:cs_reference_frame('73451b97-b5be-4d64-83ea-c0210f169938', postwar_reconstruction_after_totalitarian_collapse).
narrative_ontology:cs_drift_state('73451b97-b5be-4d64-83ea-c0210f169938', contemporary_post_first_generation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73451b97-b5be-4d64-83ea-c0210f169938', '').
narrative_ontology:cs_kernel_id(constitutional_government__postwar_constitutionalism, constitutional_government).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_government__postwar_constitutionalism, protected_individuals).
narrative_ontology:constraint_beneficiary(constitutional_government__postwar_constitutionalism, minority_rights_holders).
narrative_ontology:constraint_victim(constitutional_government__postwar_constitutionalism, movements_resembling_proscribed_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the standpoint of ordinary citizens whose rights are constitutionally protected after a regime collapse, the postwar constitution appears as pure coordination: it solves the collective action problem of preventing a return to the prior abuses through legally entrenched protections. The constraint enables exit from political domination via judicial recourse, not extraction toward it. Low theater — the rights mechanism is functionally operative.
constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From the standpoint of political movements that structurally resemble the defeated regime (militarism, totalitarian coordination, majoritarian elimination of minorities), the postwar constitution appears as extraction plus coordination: it genuinely solves coordination problems (stable governance, rule of law) but does so by deliberately foreclosing certain regime types through militant-democracy and eternity clauses. These actors experience suppression (constitutional barriers to organizing their movement), yet also benefit from the rule-of-law stability the constitution enables. The relationship is asymmetric: extraction aimed backward at a proscribed regime form.
constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the standpoint of the institution charged with policing constitutional compliance (constitutional court, supreme judicial authority), the postwar constitution appears as pure coordination. The judicially-enforced rights mechanism enables this institution to resolve disputes and prevent regime regression through lawful authority rather than political struggle. Low extraction — the institution's power derives from its legitimate role as constitutional arbiter, not from capturing surplus. High arbitrage: the court can leverage its constitutional role to influence policy across domains.
constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational/theatrical perspective, the postwar constitution increasingly becomes performative as time passes since the catastrophe. The eternity clauses and militant-democracy provisions (e.g., BVerfG protection of the democratic constitutional order) persist as ritual invocations of the proscribed regime type, even as the concrete structural threat recedes. Theater ratio rises as enforcement becomes less about preventing actual regime return and more about maintaining the symbolic commitment to 'never again.' The piton classification reflects the degradation of functional prevention into institutional theater.
constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% From a universal/analytical perspective, the postwar constitution might appear as an immutable natural law: after catastrophic regime failure, some form of rights-anchored reconstruction is structurally necessary. The analogy would be to thermodynamic limits — you cannot return to the prior regime type without first reconstructing the legal and normative order. This perspective risks naturalizing what is actually a contingent institutional choice. The false-summit test: does this constraint have identifiable beneficiaries and a designed backward-suppression mechanism? Yes — the individual and the institutional machinery that enforces rights. This suggests the mountain is a false summit: what appears as structural necessity is actually an installed institutional apparatus designed by postwar constitutionalists.
constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_government__postwar_constitutionalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_government__postwar_constitutionalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_government__postwar_constitutionalism, TR),
    TR >= 0.70.

:- end_tests(constitutional_government__postwar_constitutionalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint's primary function is protective (expanding rights, preventing regime return), not extractive. Rights protections benefit individuals without creating corresponding extraction flows toward institutional concentrators. The backward-directed suppression (militia clauses) is enforcement of deliberate limitation, not extraction from victims. However, extractiveness is not zero: the constitutional court gains authority and interpretive power from its guardian role, creating some institutional benefit. Suppression (0.35): Moderate. The constraint deliberately forecloses certain regime types through militant-democracy provisions and eternity clauses, creating legal barriers to organizing movements that resemble the proscribed regime. However, suppression is not severe because the mechanism is transparent, judicially reviewable, and grounded in explicit constitutional text rather than hidden coercion. The proscribed movement type can in principle organize if it renounces the outlawed structural form. Theater ratio (0.40): Moderate, rising over time. Initially (t=0, value 0.25), the constitutional protections are functionally operative — the threat of regime regression is concrete, and enforcement is genuinely preventive. As generational distance increases (t=15-30), the ritualistic invocation of 'never again' begins to exceed the structural threat level, and the constraint's functional force declines relative to its symbolic performance. Measurements show theater increasing from 0.25 to 0.40 across 30 years, reflecting this degradation. Rope classification: The constraint solves the coordination problem of preventing regime return (genuine coordination function) without pure extraction mechanisms. The backward-directed suppression is aimed at the proscribed regime type, not at ordinary political competition. The beneficiary (protected individual) and the institutional guardian (constitutional court) both experience genuine coordination rather than extraction.
 *
 * PERSPECTIVAL GAP:
 *   The postwar constitutionalism reading produces a significant perspectival gap between ordinary rights-holders (who see pure coordination), proscribed regime types (who see mixed coordination + targeted extraction), the constitutional court (who sees pure coordination + authority), and the civilizational observer (who risks naturalizing the constraint as a structural necessity rather than a designed institutional apparatus). The gap reveals that the constraint's classification depends entirely on the observer's relationship to the proscribed regime type: beneficiaries see rope; excluded movements see tangled_rope; institutional guardians see rope; theatrical observers at historical distance see piton degradation. None of these perspectives is more 'correct' than the others — the presheaf of classifications IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the backward-directed suppression and the rights-protection benefit. Protected individuals (beneficiaries + constrained exit) experience low or negative d: they benefit from the constraint without bearing extraction costs, producing d ≈ 0.15–0.25. Movements resembling the proscribed regime (victims + constrained exit) experience moderate d: they bear suppression costs (excluded from constitutional protection) but also benefit from rule-of-law stability, producing d ≈ 0.55–0.65. The constitutional court (beneficiary + arbitrage exit) experiences very low d: it derives authority and leverage from its guardian role without extracting from other actors, producing d ≈ 0.10. The piton perspective at civilizational distance risks high d if it misunderstands theater as extraction, but the actual directionality reflects that the constraint does not target this perspective — it targets the proscribed regime type.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_type_recession,
    'As generational distance from the catastrophe increases, does the threat of regime regression structurally diminish, or does the proscribed regime type pose a permanent structural threat requiring eternal constitutional vigilance?',
    'Longitudinal analysis of militant-democracy doctrine across 50+ years post-reconstruction; tracking of actual regime-restoration attempts; correlation between threat salience and constitutional enforcement intensity',
    'If threat genuinely recesses: the piton perspective is validated — the constraint becomes primarily performative. If threat is permanent: the rope perspective is validated — the constraint must remain functionally operative. If the answer changes by country: the reading is context-dependent (affecting the universality of the mountain perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_type_recession, empirical, 'Whether the proscribed regime threat structurally recedes with historical distance').

omega_variable(
    reading_kernel_contest,
    'Which reading of ''constitutional government'' is normatively and structurally correct: ancient (balanced orders), postwar (rights-anchored reconstruction), revolutionary (foundational rupture), or westminster (evolutionary accumulation)?',
    'No empirical resolution — this is a kernel contest. The question itself is the site of constitutional dispute. Different nations and constitutional traditions adopt different readings. The postwar reading is one live option among four; none forecloses the others within their respective constitutional frameworks.',
    'If postwar is adopted as canonical: the other readings become sibling options available to other constitutional systems, not competitors. If one reading forecloses others: that is the foreclosure claim being tested (this omega documents that foreclosure is NOT the relationship structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which kernel reading is normatively correct (kernel contest: no empirical resolution)').

omega_variable(
    eternity_clause_enforceability,
    'Are eternity clauses (constitutional provisions that declare themselves unamendable) legally binding constraints or merely aspirational commitments that subsequent generations can in principle override?',
    'Comparative constitutional law analysis: jurisdictions that have tested eternity clause enforcement (Germany BVerfG, Italy Constitutional Court); identification of cases where elected bodies attempted override and courts enforced the eternity commitment',
    'If eternity clauses are legally binding: suppression (0.35) is too low — the constraint''s backward-directed suppression is structurally entrenched and enforced, raising suppression toward 0.55+. If eternity clauses are merely aspirational: suppression is correct — later generations can amend them, reducing the constraint''s structural force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternity_clause_enforceability, empirical, 'Whether eternity clauses are legally binding or merely aspirational').

omega_variable(
    beneficiary_victim_symmetry,
    'Does the postwar constitution genuinely coordinate between protected individuals and the proscribed regime type, or does it impose pure extraction on movements that resemble the defeated regime?',
    'Analysis of whether exclusion from constitutional protection (via militant-democracy clauses) is presented as temporary suspensive condition or permanent categorical bar; examination of whether the excluded movement can in principle demonstrate commitment to constitutional order and regain protection',
    'If coordination: the tangled-rope perspective is correct — there is genuine mutual benefit from the rule-of-law framework even for the proscribed movement type. If pure extraction: the classification should shift from tangled_rope to snare for this perspective; extractiveness should rise toward 0.55+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_symmetry, conceptual, 'Whether postwar constitution coordinates or imposes extraction on proscribed regime types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_government__postwar_constitutionalism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constpostwar_tr_t0, constitutional_government__postwar_constitutionalism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(constpostwar_tr_t15, constitutional_government__postwar_constitutionalism, theater_ratio, 15, 0.35).
narrative_ontology:measurement(constpostwar_tr_t30, constitutional_government__postwar_constitutionalism, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(constpostwar_be_t0, constitutional_government__postwar_constitutionalism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(constpostwar_be_t15, constitutional_government__postwar_constitutionalism, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(constpostwar_be_t30, constitutional_government__postwar_constitutionalism, base_extractiveness, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_government__postwar_constitutionalism, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_government__postwar_constitutionalism, constitutional_government__ancient_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__postwar_constitutionalism, constitutional_government__revolutionary_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__postwar_constitutionalism, constitutional_government__westminster_evolution).
narrative_ontology:affects_constraint(constitutional_government__postwar_constitutionalism, militant_democracy_doctrine).
narrative_ontology:affects_constraint(constitutional_government__postwar_constitutionalism, eternity_clause_enforceability).

% DUAL FORMULATION NOTE:
% The postwar constitutionalism reading is one constraint among four readings of the same kernel. All four stories share the same label ('constitutional government') but instantiate different claims with different ε values, beneficiary/victim structures, and suppression mechanisms. The postwar reading (this story) has ε=0.18, beneficiary=protected_individual, victim=proscribed_regime_type, suppression=backward-directed. The sibling stories have different ε values and structural data. Linked via network.affects_constraints: the postwar reading influences (but does not foreclose) the revolutionary reading by establishing a precedent that written constitutions can protect rights; it coexists with the ancient reading as different constitutional traditions adopt different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
