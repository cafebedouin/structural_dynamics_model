% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right — Civic Republican Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The civic republican reading of the Second Amendment grounds the arms
 *   right not in individual property or self-defense but in the structural
 *   necessity of armed citizenship for republican self-governance. On this
 *   reading, the right to bear arms exists to enable the citizenry to
 *   maintain their role as the ultimate check against tyranny—the 'well
 *   regulated Militia' is not a militia organization controlled by the state,
 *   but the armed body of citizens themselves, organized and trained
 *   according to republican principles. The constraint is neither a pure
 *   individual liberty (the libertarian reading) nor a pure state power (the
 *   collective-right reading), but a hybrid structure binding individual
 *   right to civic duty. Citizens gain access to arms in exchange for militia
 *   obligation and training. The state gains legitimacy from the distributed
 *   armed capacity but must constrain itself to honor the civic republican
 *   compact. This reading emphasizes that the Founders understood bearing
 *   arms as inseparable from militia service and republican participation—a
 *   duty-bearing right, not a bare entitlement. The constraint exhibits
 *   Tangled Rope structure because it simultaneously coordinates republican
 *   defense and extracts militia service from citizens. It also shows Snare
 *   structure from the regulatory authority's perspective (a check against
 *   its monopoly on force) and Scaffold structure from the view of liberal
 *   constitutionalists who see the civic duty requirement as a sunset clause
 *   being eroded by technology and professionalization.
 *
 * KEY AGENTS:
 *   - Armed Citizen-Militia Members: Primary beneficiary (moderate/constrained) — gain the right and the civic role; bear the duty obligation and training requirements
 *   - Disarmed Populations: Secondary victim (powerless/trapped) — excluded from armed citizenship; denied both right and civic participation mechanism
 *   - Regulatory Authority (Government/State): Dual victim and beneficiary (institutional/constrained) — theoretically benefits from militia check on tyranny; experiences constraint on monopoly of force
 *   - Republican State (Institutional Framework): Beneficiary (institutional/constrained) — depends on armed citizenry for republican vigilance; must maintain self-limitation to honor civic compact
 *   - Armed Establishment (Military & Law Enforcement): Powerful actor (powerful/constrained) — experiences militia as theoretical check; maintains practical monopoly through professionalization and technology
 *   - Liberal Constitutionalist Coalition: Organized reformer (organized/mobile) — views civic duty requirement as sunset clause being superseded by professional standing armies and constitutional drift
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating contingent historical reading as natural law of republics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right — Civic Republican Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '75ece277-dc66-416b-bea7-53472c334b1b').
narrative_ontology:cs_kernel_codification('75ece277-dc66-416b-bea7-53472c334b1b', fixed_text).
narrative_ontology:cs_authority_grounding('75ece277-dc66-416b-bea7-53472c334b1b', lineage).
narrative_ontology:cs_interpretation_layer_present('75ece277-dc66-416b-bea7-53472c334b1b').
narrative_ontology:cs_reading_relation('75ece277-dc66-416b-bea7-53472c334b1b', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('75ece277-dc66-416b-bea7-53472c334b1b', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('75ece277-dc66-416b-bea7-53472c334b1b', foundational, armed_citizenship_republican_necessity).
narrative_ontology:cs_axiom_status(armed_citizenship_republican_necessity, holdable).
narrative_ontology:cs_axiom_grounding('75ece277-dc66-416b-bea7-53472c334b1b', armed_citizenship_republican_necessity, conventional).
narrative_ontology:cs_axiom('75ece277-dc66-416b-bea7-53472c334b1b', foundational, militia_duty_constitutionally_binding).
narrative_ontology:cs_axiom_status(militia_duty_constitutionally_binding, holdable).
narrative_ontology:cs_axiom_grounding('75ece277-dc66-416b-bea7-53472c334b1b', militia_duty_constitutionally_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('75ece277-dc66-416b-bea7-53472c334b1b', constitutional_civic_militia_compact).
narrative_ontology:cs_drift_state('75ece277-dc66-416b-bea7-53472c334b1b', contemporary_professional_military_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75ece277-dc66-416b-bea7-53472c334b1b', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, armed_citizen_militia).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, regulatory_authority_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISARMED CITIZEN (ROPE) — Individual agent without access to arms sees the civic republican framework as pure coordination: the right protects collective capacity to resist tyranny in which they participate, even without personal weaponry. No individual extraction experienced; the constraint coordinates militia participation. However, from biographical horizon, the disarmed person is structurally trapped and cannot realize the coordination benefit — they are excluded from the armed citizenship class the right protects.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARMED CITIZEN-MILITIA MEMBER (TANGLED ROPE) — Primary beneficiary of the right. Experiences genuine coordination function: militia membership enables collective defense and political participation. Simultaneously experiences extraction through training requirements, militia duty obligations, and potential conscription. The constraint is hybrid — grants both right and duty. Exit is constrained by citizenship and obligation to collective defense.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (SNARE) — Government authority that must defend itself against the armed citizenry while claiming to represent the people. From this perspective, an armed population creates asymmetric extraction: citizens retain capacity to overthrow authority while authority claims legitimacy through the civic republican framework ('we are the people'). The regulatory authority experiences the constraint as a snare limiting its monopoly on force, though framed as republican principle.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUBLICAN STATE (TANGLED ROPE) — The state itself (as institutional embodiment of civic republican theory) benefits from the armed citizenry in principle: armed militia theoretically guards against tyranny and oppression. Yet the state simultaneously requires capacity to enforce law and prevent violence. This creates the core tangled-rope structure: genuine coordination function (armed populace maintains republican vigilance) embedded in asymmetric extraction (state must constrain the very armed capacity it theoretically depends on). Generational horizon reveals the sustainable equilibrium: each generation renegotiates the training/qualification requirements that bind the right to civic duty.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ARMED ESTABLISHMENT (TANGLED ROPE) — Organized armed forces (military, law enforcement) experience the civic republican constraint as both coordination and extraction. Coordination: citizen militia theoretically prevents monopoly on force. Extraction: militia capability threatens the establishment's capacity to execute state policy unopposed. Powerful actors are constrained but not trapped — they can adapt tactics, professionalize, and renegotiate the training/qualification boundary. The establishment has significant agency in determining what 'militia' means operationally.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LIBERAL CONSTITUTIONALIST COALITION (SCAFFOLD) — Organized reformers (academic theorists, policy advocates) see the civic republican reading as a temporary equilibrium being superseded by either individual-right libertarianism or collective-right progressivism. They perceive the constraint as scaffolding: the militia clause and civic duty requirement are sunset structures. As professional standing armies mature and firearms technology separates from militias (civilians cannot operate modern military hardware), the civic republican reading loses force. The coalition has exit options through constitutional reinterpretation and policy reform.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From the civilizational horizon, the civic republican framework appears as an immutable structural fact: any republic requires some mechanism to prevent tyranny; some form of distributed armed capacity serves this function; therefore the right to bear arms is a natural feature of republican government structure. This perspective risks naturalizing what is actually a contestable historical-institutional arrangement. The engine will assess whether this is a false summit.
constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_arms_right__civic_republican_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The civic republican reading avoids the high extraction of a pure snare because it emphasizes genuine coordination function—the militia right serves a collective good (republican defense). Yet extraction exists: citizens are obligated to militia service and training, with limited exit. The duty obligation is real extraction from the armed populace. Compared to the individual-right reading (which would show lower extractiveness because the duty is peripheral) or the collective-right reading (which would show higher extractiveness because the state controls the militia), the civic republican reading falls in the middle. Suppression (0.42): Moderate. The framework constrains regulatory authority (suppression of state power) while simultaneously constraining armed citizens (training, duty, qualification requirements). The suppression is bidirectional and partially self-enforcing through civic identity rather than purely coercive. The constraint is stable when citizens internalize the duty as civic virtue; it becomes more repressive when that internalization fails. Theater ratio (0.48): Moderate-Low. The civic republican framework has real functional content (militia training, civic participation mechanisms) but also performative elements (constitutional rhetoric about militia that bears little relation to modern standing armies). The ratio would be lower (0.30) if militia training were genuinely mandatory and integrated into civic life; it is higher (0.60+) in contexts where militia duty is vestigial. The measurement shows gradual theater increase (0.35→0.48) reflecting the declining practical role of militias in modern professional standing armies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival variance. The armed citizen sees Tangled Rope—genuine coordination benefit (participation in republican defense) mixed with real duty extraction. The disarmed person sees Rope (pure coordination of a collective good they cannot access) or Snare (excluded from the right entirely). The regulatory authority sees Snare (constrained from monopoly force). The republican state sees Tangled Rope (benefits from militia principle; constrained by actual armed citizenry). The armed establishment sees Tangled Rope (theory requires militia; practice requires maintaining control). The liberal constitutionalist sees Scaffold (duty requirement is sunset by technology and professional armies). The analytical observer at civilizational scale risks seeing Mountain (necessary structure of any republic) but the engine's false-summit detector will flag this as naturalization of a contested reading. The perspectival gap reveals that the civic republican reading depends on maintaining symmetry between right and duty, benefit and constraint—a balance that erodes as technology, professionalization, and constitutional drift push toward individual-right or collective-right dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to this specific constraint. Armed citizens benefit from the right but bear the duty—they are partial beneficiaries, placed at d ≈ 0.40 (symmetric between right gain and duty cost). The regulatory authority is placed at d ≈ 0.65 (primarily victimized by the check on monopoly force, though theoretically benefiting from republican legitimacy)—making them a net target despite the theoretical benefit. Disarmed populations are trapped victims, d ≈ 0.95, experiencing no benefit from a right they cannot exercise. The armed establishment is powerful and mostly avoids extraction through professionalization, d ≈ 0.35. The organized reformer coalition has mobile exit (through reinterpretation), d ≈ 0.25. The analytical observer at the civilizational horizon faces the false-summit risk: they may place d at 0.72 (observational) but the structural analysis reveals whether the reading naturalizes contingent arrangements. These derivations feed the sigmoid f(d), which then scales extractiveness for each perspective's chi calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   The civic republican reading avoids mandatrophy by explicitly incorporating both coordination and extraction into the constraint structure. This is not a case of mislabeled pure extraction (which would be snare) or missed extraction (which would be rope). The reading is analytically honest about the tangled-rope structure: citizens genuinely gain from the right (participation in republican defense) and genuinely bear extraction (militia duty and training). The constraint is mandatrophy-resolved because the reading accepts the hybrid nature rather than collapsing toward either pole. However, the reading faces existential pressure: if citizens increasingly see only the extraction (duty without meaningful participation), the reading collapses toward snare. If citizens increasingly see only the right (without duty or training requirements), it collapses toward individual-right libertarianism. The measurement trajectories show gradual theater increase and suppression increase—signals that the practical balance is shifting away from genuine coordination toward either performative duty (higher theater) or state control (higher suppression). A generational observer would note that the civic republican equilibrium is stable at the constitutional-principle level but increasingly unstable at the implementation level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_obligation_binding,
    'Are militia training and duty obligations constitutionally necessary to ground the right, or are they contingent regulatory preferences?',
    'Historical analysis of Founding-era militia statutes; comparative analysis of republics with and without mandatory militia duty; constitutional text analysis of whether ''well regulated Militia'' is condition precedent or descriptive context',
    'If necessary: civic republican reading is robust; training/qualification requirements are constitutionally defensible. If contingent: the reading depends on a particular historical moment''s militia structure; as technology and professionalization change, the reading''s foundation erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_obligation_binding, empirical, 'Whether militia duty is constitutionally necessary or contingent').

omega_variable(
    monopoly_breaking_threshold,
    'What threshold of armed-citizen capacity is sufficient to prevent tyranny without destabilizing law enforcement and order?',
    'Comparative historical analysis of republics with varying citizen-militia strength; measurement of coercive capacity gaps between state and distributed armed population; modeling of minority-rule breakage points',
    'If threshold is achievable: civic republican reading remains viable and extractiveness remains moderate (0.38). If threshold is unachievable or wildly unstable: either the reading is false (monopoly can never be broken without chaos) or extractiveness is higher (oppressive force is necessary to maintain both republic and order).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monopoly_breaking_threshold, empirical, 'Citizen-militia capacity threshold for tyranny prevention').

omega_variable(
    reading_collapse_vector,
    'Which direction does this civic republican reading collapse toward — individual-right libertarianism or collective-right progressivism — as historical conditions change?',
    'Longitudinal analysis of constitutional jurisprudence; tracking of which reading claims are winning empirical/cultural validation over 20-50 year periods; identification of structural pressures (technology change, urbanization, professional military maturation) that favor each sibling reading',
    'If collapses toward individual right: civic republican framing becomes window-dressing for libertarian property right; extractiveness rises (duty language becomes pure extraction). If collapses toward collective right: civic republican framing becomes window-dressing for state police power; extractiveness rises (militia becomes state militia, not citizenry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_collapse_vector, conceptual, 'Directional drift of civic republican reading toward sibling readings').

omega_variable(
    dual_obligation_paradox,
    'Can the same constitutional clause grant both an individual right and enforce individual duty without one dominating the other?',
    'Jurisprudential analysis of how courts have treated hybrid rights/duties in other contexts (jury duty + voting right; conscription + citizenship right); modeling of whether rights and duties can be symmetrical or if one always subsumes the other',
    'If truly dual: tangled rope classification holds; both beneficiary and victim relationships are structural. If duty dominates: reading collapses toward state power (collective right), extractiveness rises. If right dominates: reading collapses toward individual freedom (individual right), extractiveness falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_obligation_paradox, conceptual, 'Structural viability of symmetrical right-duty obligation').

omega_variable(
    false_summit_natural_law_claim,
    'Is the civic republican framework a natural law of republics, or a contingent historical reading of a contested text?',
    'Comparative constitutional analysis of how non-Anglo-American republics ground their arms regulations; examination of whether civic republican principles are empirically necessary or culturally contingent; analysis of whether the reading constrains state power or merely naturalizes a particular power allocation',
    'If natural law: mountain classification is appropriate; the reading cannot be overridden. If contingent: mountain classification fails (false summit); the reading is a beneficiary-protective interpretation masquerading as structural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Civic republican framework as natural law vs. contingent reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__civic_republican_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(seco_tr_t100, second_amendment_arms_right__civic_republican_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(seco_be_t100, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(seco_su_t100, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, resource_allocation).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, militia_duty_obligation).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, regulatory_authority_force_monopoly).

% DUAL FORMULATION NOTE:
% The 'Second Amendment arms right' kernel decomposes into three structurally distinct constraints, each instantiating a different reading with different ε values and beneficiary/victim structures. The civic republican reading (this story, ε=0.38) emphasizes dual benefit-duty structure; the individual-right reading (ε≈0.18) emphasizes liberty from constraint; the collective-right reading (ε≈0.55) emphasizes state regulatory authority. These are not observations of the same constraint from different angles—they are structurally different interpretations of a contested text. The civic republican reading's ε falls between the others, reflecting its hybrid coordination-extraction nature. All three readings are linked via network.affects_constraints to show that the constitutional interpretation contest is not abstract: different readings produce different allocation of benefit and burden, different duty structures, and different regulatory authority capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
