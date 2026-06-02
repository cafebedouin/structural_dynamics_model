% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collectivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collectivist_reading, []).

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
 *   constraint_id: second_amendment_text__collectivist_reading
 *   human_readable: Second Amendment as Collective Militia Right (Collectivist Reading)
 *   domain: constitutional_law/political_philosophy/gun_policy
 *
 * SUMMARY:
 *   The collectivist reading of the Second Amendment holds that the operative
 *   clause ('the right of the people to keep and bear Arms, shall not be
 *   infringed') is grammatically and semantically restricted by the prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State'). Under this reading, the Second Amendment protects a right
 *   tied to militia organization and state regulatory authority, not an
 *   unrestricted individual right to personal armament. This reading was the
 *   dominant judicial interpretation from 1791 through 2007 (United States v.
 *   Miller, 1939, and subsequent precedent). The collectivist reading
 *   supports broad state regulatory authority to license, register, restrict
 *   categories of weapons, and impose conditions on civilian armament. The
 *   core mechanism is coordination: the state coordinates militia readiness
 *   and public safety through regulatory power over civilian arms. This is
 *   experienced as pure coordination by state authorities and public safety
 *   apparatus, as mixed coordination-extraction by citizens seeking
 *   unrestricted armament, and as a grammatically immutable textual fact by
 *   analytical observers reading the historical syntax. The constraint family
 *   includes the individualist reading (which sees an unrestricted personal
 *   right) and a hybrid scope-limitation reading (which acknowledges both
 *   militia and self-defense purposes but accepts categorical restrictions).
 *   Each reading instantiates a different constraint with different
 *   extractiveness values and beneficiary/victim configurations. This story
 *   instantiates ONLY the collectivist reading.
 *
 * KEY AGENTS:
 *   - State Regulatory Authority: Institutional beneficiary (institutional/arbitrage) — gains constitutional authority to regulate civilian arms through militia necessity framing
 *   - State Militia Apparatus: Institutional beneficiary (institutional/arbitrage) — constitutionally grounded organization of state military force; civilian arms regulation supports militia readiness
 *   - Public Safety Apparatus (police, fire, emergency services): Institutional beneficiary (institutional/arbitrage) — gains regulatory coordination mechanism for public safety
 *   - Citizens Seeking Individual Armament: Moderate-power population (moderate/constrained) — experience the constraint as both coordinating public safety and restricting personal armament access
 *   - Militia-Aligned Movements: Organized population (organized/constrained) — experience the reading as denying their constitutional right; constrained by regulatory authority but capable of political action
 *   - Gun Control Advocacy Coalition: Organized agents (organized/constrained) — use collectivist reading to justify specific regulations; require continuous legal and political maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collectivist_reading, 0.28).
domain_priors:suppression_score(second_amendment_text__collectivist_reading, 0.35).
domain_priors:theater_ratio(second_amendment_text__collectivist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collectivist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_text__collectivist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collectivist_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collectivist_reading, rope).
narrative_ontology:human_readable(second_amendment_text__collectivist_reading, "Second Amendment as Collective Militia Right (Collectivist Reading)").
narrative_ontology:topic_domain(second_amendment_text__collectivist_reading, "constitutional_law/political_philosophy/gun_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collectivist_reading, 'kernel_reading_sa_collectivist_v1').
narrative_ontology:cs_kernel_codification('kernel_reading_sa_collectivist_v1', fixed_text).
narrative_ontology:cs_authority_grounding('kernel_reading_sa_collectivist_v1', lineage).
narrative_ontology:cs_interpretation_layer_present('kernel_reading_sa_collectivist_v1').
narrative_ontology:cs_reading_relation('kernel_reading_sa_collectivist_v1', second_amendment_text__individualist_reading, coexists_with).
narrative_ontology:cs_reading_relation('kernel_reading_sa_collectivist_v1', second_amendment_text__hybrid_scope_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('kernel_reading_sa_collectivist_v1', foundational, militia_necessity_grammatically_restrictive).
narrative_ontology:cs_axiom_status(militia_necessity_grammatically_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('kernel_reading_sa_collectivist_v1', militia_necessity_grammatically_restrictive, empirically_contingent).
narrative_ontology:cs_axiom('kernel_reading_sa_collectivist_v1', foundational, state_regulatory_authority_constitutionally_grounded).
narrative_ontology:cs_axiom_status(state_regulatory_authority_constitutionally_grounded, holdable).
narrative_ontology:cs_axiom_grounding('kernel_reading_sa_collectivist_v1', state_regulatory_authority_constitutionally_grounded, deontological).
narrative_ontology:cs_reference_frame('kernel_reading_sa_collectivist_v1', federalist_militia_state_model).
narrative_ontology:cs_drift_state('kernel_reading_sa_collectivist_v1', contemporary_national_guard_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('kernel_reading_sa_collectivist_v1', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collectivist_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collectivist_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(second_amendment_text__collectivist_reading, state_militia_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collectivist_reading, public_safety_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE REGULATORY AUTHORITY (ROPE) — The collectivist reading preserves state power to regulate arms and organize militia. This authority experiences the constraint as pure coordination: the Second Amendment, properly read, coordinates the right to bear arms with state militia organization and state power to regulate civilian armament. No extraction — the state benefits from legitimate regulatory authority grounded in constitutional text.
constraint_indexing:constraint_classification(second_amendment_text__collectivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: CITIZENS SEEKING INDIVIDUAL ARMAMENT (TANGLED ROPE) — Citizens who view the collectivist reading as restricting their Second Amendment rights experience both coordination and extraction. The reading coordinates public safety and militia readiness (genuine beneficiary function) while constraining access to arms for self-defense or political expression (extraction). The constraint is surmountable through political/constitutional amendment but imposes real costs on this population.
constraint_indexing:constraint_classification(second_amendment_text__collectivist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC SAFETY APPARATUS (ROPE) — Fire departments, police forces, and emergency services benefit from the collectivist reading's coordination function: state power to regulate arms enables public safety infrastructure without competition from unregulated private armament. This is pure coordination with no extraction — the constraint solves a genuine collective action problem around emergency response and public order.
constraint_indexing:constraint_classification(second_amendment_text__collectivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER — TEXTUAL GRAMMAR (MOUNTAIN) — From a purely linguistic/textual analytical stance, the collectivist reading claims the Second Amendment's opening clause ('A well regulated Militia, being necessary to the security of a free State') grammatically modifies and restricts the operative clause ('the right of the people to keep and bear Arms, shall not be infringed'). The textual structure itself, parsed as a semantic unit, appears immutable — this is what the historical grammar says. However, this risks naturalizing a contested interpretive choice as a grammatical fact.
constraint_indexing:constraint_classification(second_amendment_text__collectivist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: GUN CONTROL ADVOCACY COALITION (SCAFFOLD) — Organized advocates for gun control regulation (Brady Campaign, Everytown, etc.) see the collectivist reading as a temporary solution to constitutional constraint on regulatory authority. The reading provides doctrinal legitimacy for narrow, specific regulations (background checks, assault weapon bans, licensing) but requires continuous legal defense and political work. The sunset logic is implicit: if the individualist reading gains decisional authority (5-10 year horizon), this regulatory architecture collapses and requires reconstruction.
constraint_indexing:constraint_classification(second_amendment_text__collectivist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collectivist_reading_tests).
:- end_tests(second_amendment_text__collectivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The collectivist reading coordinates genuine public goods (militia readiness, public safety) without substantial coercive overhead. The constraint does restrict personal armament access compared to an unrestricted baseline, but this is the intended regulatory function, not an extractive side effect. The reading does not require continuous suppression — it is voluntarily accepted by large populations (urban residents, public safety advocates) and resisted by others (rural populations, libertarian constituencies). The constraint is legally enforced but not socially illegitimate on its face. Suppression (0.35): Moderate. State enforcement mechanisms (licensing, registration, background checks, categorical prohibitions) create material barriers to unrestricted armament, but these barriers are legible and contestable through political channels. The suppression is institutionalized (formalized in law) rather than diffuse. Theater ratio (0.42): Moderate. The collectivist reading involves some performative elements (licensing procedures that may not substantially reduce criminal access; media focus on dramatic weapons rather than common methods of harm), but the core regulatory mechanism (state authority over militia and civilian arms) is functionally grounded. Theater has increased over time as urban policy has emphasized symbolic prohibitions (assault weapon categories) alongside functional background check infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory authority sees coordination (Rope) — the reading legitimizes militia organization and public safety authority. Citizens seeking unrestricted armament see extraction (Tangled Rope, moving toward Snare in high-enforcement contexts) — they experience the constraint as legitimizing restrictions on their constitutional reading. Public safety apparatus sees coordination (Rope) — no extraction from their perspective. The gun control advocacy coalition sees a temporary solution (Scaffold) — the reading provides doctrinal support but requires continuous political maintenance and is vulnerable to reinterpretation if the individualist reading gains decisional authority. The analytical observer reading textual grammar risks seeing an immutable natural law (Mountain) but is actually reading a contested interpretive choice. The perspectival gap reveals that the collectivist reading's coherence depends on accepting the grammatical modification claim and the militia necessity connection — both of which are empirically contested.
 *
 * DIRECTIONALITY LOGIC:
 *   The collectivist reading's beneficiaries are state regulatory authority, militia apparatus, and public safety coordination. These agents derive structural power from the reading — their authority is constitutionally grounded. Exit options are arbitrage (they can shift regulatory approaches without exiting the constraint). Directionality (d) is low: beneficiaries with arbitrage options experience low effective extraction. Citizens seeking individual armament are neither pure beneficiaries nor pure victims — they benefit from public safety coordination but bear costs from armament restrictions. Their exit options are constrained (they can advocate for constitutional amendment or reinterpretation, but this requires sustained political effort). Directionality is moderate-high: constrained agents with mixed benefits/costs experience moderate extraction from their perspective. The analytical observer is neither beneficiary nor victim — they observe the structure. The constraints derive d from declarative structural data (beneficiary/victim groups and exit options), not from abstract observer status.
 *
 * MANDATROPHY ANALYSIS:
 *   The collectivist reading resolves the mandatrophy (the tension between coordination and extraction) by declaring the extraction apparent rather than structural. From the state's perspective, the constraint is pure coordination. From the citizen seeking unrestricted armament's perspective, the constraint is mixed coordination-extraction. These are not contradictory — they reflect different structural positions relative to the same constraint. The mandatrophy is resolved by recognizing that the 'extraction' (restriction of armament) is the intentional regulatory mechanism, not a hidden side effect. The collectivist reading must show that this regulatory mechanism is justified by the genuine coordination need (militia readiness, public safety). If the militia necessity becomes obsolete (modern state militia independent of civilian armament), the reading loses its coordination justification and may reclassify as pure regulation or extraction. The T17 abductive trigger (mandatrophy drift detection) would flag this if the extractiveness rises over time — but the measurements show stable extractiveness (0.15 → 0.28 and plateau), suggesting the reading's coordination function remains stable despite empirical changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_modification_syntax,
    'Does the opening clause of the Second Amendment grammatically restrict (''prefatory clause'') the operative clause, or does it provide context without restriction (''statement of purpose'')?',
    'Comparative analysis of 18th-century English legal grammar in similar compound-clause structures; assessment by specialist linguists in historical syntax; examination of other founding-era documents with parallel syntactic patterns',
    'If grammatically restrictive: collectivist reading''s textual foundation is solid, supporting low epsilon. If purpose-statement but not restrictive: the operative clause retains independent force, supporting higher epsilon and stronger individualist reading claim. This is the primary axis of interpretive disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_modification_syntax, empirical, 'Whether opening clause grammatically restricts or contextualizes operative clause').

omega_variable(
    militia_necessity_conditional,
    'Is the militia necessity conditional (the right exists only insofar as militia is necessary) or conjunctive (the right and militia organization are both necessary to security, but the right is not restricted to militia context)?',
    'Historical intent evidence from ratification debates and founding-era commentary; analysis of how the militia necessity framing was deployed in state constitutions and legal writing; examination of whether founding authors distinguished militia right from self-defense or other personal armament purposes',
    'If conditional: supports collectivist reading and justifies broad state regulatory authority. If conjunctive: creates space for non-militia personal armament, supporting hybrid or individualist readings and constraining state regulatory authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_necessity_conditional, empirical, 'Whether militia necessity is conditional on or conjunctive with personal armament right').

omega_variable(
    original_understanding_scope,
    'What specific armaments and regulatory mechanisms were understood to fall under legitimate state militia organization and civilian regulation in 1789-1791?',
    'Historical analysis of founding-era militia statutes, militia readiness regulations, and civilian gun ownership patterns; examination of what weapons were in common use and how states regulated them; analysis of founding-era understanding of ''regulation''',
    'If founding understanding permitted broad state licensing and categorical prohibitions: collectivist reading gains historical grounding. If understanding was narrow (limited to militia-specific mandates): collectivist reading becomes a modern expansion that requires accepting evolutionary constitutional interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_understanding_scope, empirical, 'Founding-era understanding of state militia regulation scope').

omega_variable(
    twentieth_century_militia_obsolescence,
    'Has the state militia apparatus (National Guard, state military forces) become functionally independent of civilian-owned armament, making the militia necessity clause obsolete?',
    'Historical analysis of National Guard professionalization and independence from civilian gun ownership (1903 onwards); assessment of whether modern state security depends on civilian armament reserves; comparison with other democracies'' militia structures and gun policy',
    'If militia functionally obsolete: the collective militia rationale loses force, weakening the collectivist reading''s coordination function. The constraint''s extractiveness may rise (becomes less coordination, more pure regulation). If militia retains reserve function: collectivist reading retains its empirical grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(twentieth_century_militia_obsolescence, empirical, 'Whether modern state militia remains dependent on civilian armament').

omega_variable(
    reading_jurisdiction_foreclosure,
    'Does the collectivist reading logically foreclose the individualist reading within a single coherent constitutional framework, or can both coexist as competing but non-contradictory interpretations?',
    'Formal logical analysis of each reading''s core premises; assessment of whether a constitutional framework could accommodate both readings (e.g., through scope narrowing or tiering); examination of historical cases where courts held competing readings simultaneously in different jurisdictions or contexts',
    'If foreclosing: the readings cannot coexist; one must be rejected for the framework to be coherent. If coexisting: both readings are live positions, and the constraint family exhibits pure `coexists_with` relations. This determines whether the kernel exhibits genuine disagreement or logical incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_jurisdiction_foreclosure, conceptual, 'Whether collectivist and individualist readings are logically incompatible or merely competing interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collectivist_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_coll_theater_1791, second_amendment_text__collectivist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sa_coll_theater_1935, second_amendment_text__collectivist_reading, theater_ratio, 145, 0.35).
narrative_ontology:measurement(sa_coll_theater_2025, second_amendment_text__collectivist_reading, theater_ratio, 235, 0.42).

% Extraction over time
narrative_ontology:measurement(sa_coll_extractiveness_1791, second_amendment_text__collectivist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sa_coll_extractiveness_1865, second_amendment_text__collectivist_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(sa_coll_extractiveness_1935, second_amendment_text__collectivist_reading, base_extractiveness, 145, 0.28).
narrative_ontology:measurement(sa_coll_extractiveness_1965, second_amendment_text__collectivist_reading, base_extractiveness, 175, 0.28).
narrative_ontology:measurement(sa_coll_extractiveness_2010, second_amendment_text__collectivist_reading, base_extractiveness, 220, 0.28).
narrative_ontology:measurement(sa_coll_extractiveness_2025, second_amendment_text__collectivist_reading, base_extractiveness, 235, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collectivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collectivist_reading, second_amendment_text__individualist_reading).
narrative_ontology:affects_constraint(second_amendment_text__collectivist_reading, second_amendment_text__hybrid_scope_limitation_reading).
narrative_ontology:affects_constraint(second_amendment_text__collectivist_reading, gun_licensing_regulatory_authority).
narrative_ontology:affects_constraint(second_amendment_text__collectivist_reading, assault_weapon_categorical_prohibition).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into three reading constraints with different epsilon values and beneficiary/victim structures. The collectivist reading (this story) supports low extractiveness and strong regulatory authority. The individualist reading supports high extractiveness perception by regulated parties and strong personal liberty claims. The hybrid reading occupies intermediate ground. Each reading is a separate constraint story linked via network.affects_constraints. The constraint family also affects downstream regulatory constraints (gun licensing, categorical prohibitions) which inherit their doctrinal and normative legitimacy from the reading that prevails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__collectivist_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
