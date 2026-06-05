% ============================================================================
% CONSTRAINT STORY: overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overdetermined_composite_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: overdetermined_composite_reading
 *   human_readable: Dueling's Decline as Overdetermined Composite Collapse
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This reading claims that dueling's decline in the 19th century was
 *   causally overdetermined: multiple independent sufficient conditions
 *   (legal prohibition, institutional modernization, cultural shift, Civil
 *   War trauma) operated simultaneously and non-separably, each capable of
 *   producing the outcome independently but together creating a constraint
 *   collapse that no single causal story captures. The reading instantiates
 *   the tangled_rope type because the constraint exhibits both genuine
 *   coordination functions (the honor code coordinates status hierarchies and
 *   conflict resolution) and asymmetric extraction (dueling practitioners are
 *   trapped between social death and legal jeopardy; aristocratic culture
 *   bears the cost of displacement). The multiple mechanisms create a tangled
 *   constraint because each mechanism is enforced through different
 *   institutions (law, culture, military, economy) with different
 *   beneficiaries: the state benefits from legal prohibition (monopoly on
 *   violence), bourgeois culture benefits from institutional modernization
 *   (market-based honor), cultural modernizers benefit from delegitimation
 *   campaigns. The overdetermined reading contrasts with two sibling
 *   readings: the 'contraction reading' (dueling simply contracted due to
 *   economic shifts in aristocratic wealth) and the 'institutional
 *   displacement reading' (dueling was displaced by modern administrative
 *   structures, with other mechanisms as consequences rather than causes).
 *   The overdetermined reading asserts that no single mechanism was
 *   sufficient — legal prohibition alone would not have worked without
 *   cultural shift; cultural shift alone would not have eliminated the
 *   practice without legal enforcement; institutional modernization would
 *   have been incomplete without trauma from Civil War revealing the
 *   obsolescence of aristocratic honor codes.
 *
 * KEY AGENTS:
 *   - Dueling Practitioners: Powerless agents (biographical horizon, trapped exit) — bear maximum extraction via impossible choice (social death or legal jeopardy). Primary victims of the constraint.
 *   - Aristocratic Honor Culture: Moderate institutional agent (generational horizon, constrained exit) — experiences the constraint as mixed (coordination function persists, but culture is marginalized by modernization). Secondary victim; also beneficiary through status coordination.
 *   - State Legal Apparatus: Powerful institutional agent (generational horizon, arbitrage exit) — primary beneficiary of legal prohibition mechanism; experiences constraint as pure coordination (monopoly on violence consolidated).
 *   - Bourgeois Honor System: Institutional agent (generational horizon, arbitrage exit) — primary beneficiary of institutional modernization mechanism; experiences constraint as pure coordination (market-based honor replaces lethal feuds).
 *   - Institutional Modernizers: Organized agents (biographical horizon, constrained exit) — active enforcers of multiple mechanisms; experience constraint as deliberate scaffolding with sunset clause (cultural transition requires generational timescale).
 *   - Civil War Trauma: Civilizational event (not an agent) — trauma revealed the obsolescence of aristocratic honor codes by demonstrating mass mobilization, industrial-scale violence, and the irrelevance of individual honor in total war. Accelerated cultural shift and institutional reform.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overdetermined_composite_reading, 0.52).
domain_priors:suppression_score(overdetermined_composite_reading, 0.48).
domain_priors:theater_ratio(overdetermined_composite_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overdetermined_composite_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(overdetermined_composite_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(overdetermined_composite_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(overdetermined_composite_reading, "Dueling's Decline as Overdetermined Composite Collapse").
narrative_ontology:topic_domain(overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(overdetermined_composite_reading, distributed).
narrative_ontology:cs_authority_grounding(overdetermined_composite_reading, distributed).
narrative_ontology:cs_kernel_id(overdetermined_composite_reading, dueling_disappearance_mechanism).
narrative_ontology:cs_reading_relation(overdetermined_composite_reading, contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation(overdetermined_composite_reading, institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom(overdetermined_composite_reading, foundational, multiple_mechanisms_non_separable).
narrative_ontology:cs_axiom_status(multiple_mechanisms_non_separable, holdable).
narrative_ontology:cs_axiom_grounding(overdetermined_composite_reading, multiple_mechanisms_non_separable, empirically_contingent).
narrative_ontology:cs_axiom(overdetermined_composite_reading, foundational, dueling_decline_contingent_not_inevitable).
narrative_ontology:cs_axiom_status(dueling_decline_contingent_not_inevitable, holdable).
narrative_ontology:cs_axiom_grounding(overdetermined_composite_reading, dueling_decline_contingent_not_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame(overdetermined_composite_reading, dueling_as_functional_status_system).
narrative_ontology:cs_drift_state(overdetermined_composite_reading, civil_war_aftermath, gap(axiom_overriding, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overdetermined_composite_reading, state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(overdetermined_composite_reading, bourgeois_honor_system).
narrative_ontology:constraint_beneficiary(overdetermined_composite_reading, institutional_modernizers).
narrative_ontology:constraint_victim(overdetermined_composite_reading, aristocratic_honor_culture).
narrative_ontology:constraint_victim(overdetermined_composite_reading, dueling_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUELING PRACTITIONER (SNARE) — Trapped in the honor code requiring lethal response to insult. Legal prohibition, cultural delegitimation, and institutional pressure converge simultaneously, offering no exit: refusing a challenge means social death; accepting means legal jeopardy or actual death. The multiple independent mechanisms combine to eliminate all viable choices. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(overdetermined_composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARISTOCRATIC HONOR CULTURE (TANGLED ROPE) — Benefits from the social coordination function of the honor code (clear status ranking, conflict resolution through formalized ritual, group cohesion) while bearing extraction via legal jeopardy and social marginalization as modernization advances. The constraint coordinates status hierarchies but simultaneously extracts through criminalization and cultural delegitimation. Multiple causal pathways converge, but the coordination function persists even as extraction intensifies.
constraint_indexing:constraint_classification(overdetermined_composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LEGAL APPARATUS (ROPE) — Pure coordination benefit: legal prohibition consolidates state monopoly on violence, a foundational modern institutional requirement. No extraction from this perspective — the state is solving a collective action problem (preventing private violence that undermines authority) through law. Exit available to the state via revising the prohibition (arbitrage), but revision is not in the state's interest.
constraint_indexing:constraint_classification(overdetermined_composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BOURGEOIS HONOR SYSTEM (ROPE) — Pure coordination benefit: displacement of aristocratic honor codes by bourgeois reputation mechanisms (contract law, commercial credit, professional credentials) is a coordination solution to new economic realities. The bourgeoisie benefits from institutional arrangements that make dueling obsolete because market-based honor requires legal stability, not lethal feuds. No extraction — the displacement is functional coordination shift.
constraint_indexing:constraint_classification(overdetermined_composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL MODERNIZERS (SCAFFOLD) — Organized agents (judges, legislators, social reformers) actively dismantle dueling through multiple simultaneous mechanisms: legal prohibition with enforcement, cultural campaigns delegitimizing honor violence, institutional reforms creating alternative status pathways. The constraint is scaffolding because enforcement is deliberately temporary (limited to generational timescale needed for cultural transition) and the multiple mechanisms are designed to sunset as cultural norms shift. Theater is low because the intent is functional obsolescence, not performative suppression.
constraint_indexing:constraint_classification(overdetermined_composite_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — Risk of naturalizing the overdetermined collapse as inevitable historical progress, masking the contingency of which mechanism dominated. The constraint degraded from active cultural practice (dueling as functional honor system) to vestigial legal framework (dueling laws maintained but unenforceable, cultural memory only) through multiple non-inevitable causal pathways. From far temporal distance, the transition appears natural and inevitable; close inspection reveals contingent institutional choices and cultural contests.
constraint_indexing:constraint_classification(overdetermined_composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overdetermined_composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overdetermined_composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overdetermined_composite_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overdetermined_composite_reading, TR),
    TR >= 0.70.

:- end_tests(overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The composite mechanism creates moderate extraction for dueling practitioners (trapped between legal and cultural alternatives) and moderate extraction for aristocratic culture (displaced but not eliminated). The value reflects that multiple mechanisms together create higher effective extraction than any single mechanism would produce alone — the combined legal, cultural, institutional, and trauma pressures make exit nearly impossible. The rising trajectory in measurements (0.32 → 0.58) reflects the temporal accumulation of mechanisms: early period (0 years) has legal prohibition only (low extractiveness); mid-period (10 years) adds institutional modernization and cultural campaign; late period (20 years) includes Civil War trauma aftermath, accelerating cultural shift. Suppression (0.48): Moderate. Suppression is not total because some aristocratic practitioners could and did adapt to bourgeois honor systems; some jurisdictions retained dueling laws longer than others; some cultural pockets maintained honor codes (e.g., military academies, university fraternities). The constraint suppresses alternatives but does not eliminate all exit routes. Theater ratio (0.35): Low-moderate. Unlike performative constraints, the overdetermined composite is functionally effective — legal prohibition is enforced, cultural delegitimation campaigns are widespread, institutional alternatives (courts, contracts, reputation) actually work. Theater is not minimal because cultural delegitimation includes performative elements (public executions of duelers, ceremonial denouncements), but the overall constraint is primarily functional rather than theatrical. The low theater suggests the scaffold classification is apt: enforcement is real and meant to sunset (not sustain performatively) as cultural norms shift.
 *
 * PERSPECTIVAL GAP:
 *   The overdetermined reading exhibits maximum perspectival gap because different agents experience fundamentally different constraint types from identical structural mechanisms. The dueling practitioner sees a snare (trapped, no viable exit). Aristocratic culture sees tangled rope (mixed coordination and displacement). The state sees rope (pure coordination benefit). Bourgeois culture sees rope (pure coordination benefit). Institutional modernizers see scaffold (deliberately temporary, sunset clause operative). The analytical observer risks seeing piton (naturalizing the outcome as inevitable). No single perspective is 'correct' — the presheaf over the constraint IS the reading. The gap reveals that 'dueling's decline' is not a single constraint but a composite phenomenon that different observer positions decompose differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this composite reading is ambiguous because the mechanism non-separability prevents assigning a single (d) value. Each mechanism produces different d values: legal prohibition (state institutional perspective: d ≈ 0.05, net beneficiary); cultural delegitimation (bourgeois cultural perspective: d ≈ 0.10, net beneficiary); institutional modernization (institutional modernizers: d ≈ 0.25, constrained beneficiary); Civil War trauma (civilizational perspective: d ≈ 0.70, imposed cost without agent control). The dueling practitioner experiences a composite d = 0.95 (trapped victim across all mechanisms). The ambiguity is intentional: declaring no directionality_overrides reflects that the overdetermined reading's structure is inherently non-decomposable. The engine will compute chi differently depending on which mechanism dominates in a given historical context or analysis frame — this is a feature, not a bug. It demonstrates that when mechanisms are non-separable, perspectival classification is more valid than metric reduction.
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermined reading resolves mandatrophy by asserting that dueling's decline is genuinely tangled_rope (mixed coordination and extraction) rather than pure extraction (snare) or pure coordination (rope). The mandatrophy — how to classify a constraint that coordinates status hierarchies while extracting from practitioners — is resolved by the non-separability thesis: because legal prohibition, cultural shift, institutional modernization, and Civil War trauma operated simultaneously and entangled, no single classification captures the phenomenon. Different mechanisms produced different types: legal prohibition alone is institutional coordination (rope); cultural shift is cultural displacement (tangled rope at best, snare for holdouts); institutional modernization is coordination (rope); Civil War trauma is externally imposed cost (snare/mountain). The tangled_rope classification reflects that the composite constraint genuinely exhibits both coordination (status hierarchy maintenance, conflict resolution through dueling) and extraction (practitioners trapped, culture displaced). Mandatrophy is resolved not by choosing one type but by recognizing that the composite mechanism is authentically mixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_separability,
    'Were the four mechanisms (legal prohibition, institutional modernization, cultural shift, Civil War trauma) causally separable or fundamentally entangled such that no single ε can be assigned?',
    'Counterfactual historical analysis: If only legal prohibition had occurred (no cultural shift), would dueling have survived underground? If only cultural shift (no legal prohibition), would formal dueling have persisted legally? Comparative analysis across jurisdictions with different mechanism combinations (e.g., France vs. Britain vs. Germany had different timings).',
    'If separable: each mechanism is a distinct constraint with its own ε; the composite reading is meta-analysis. If entangled: overdetermined reading is correct; no single mechanism''s ε is meaningful; the tuple (legal, cultural, institutional, trauma) constitutes the constraint. Extraction classification depends on which mechanism analysis privileges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_pathway_separability, conceptual, 'Whether the four collapse mechanisms are causally separable').

omega_variable(
    reading_identity_crisis,
    'Is the ''overdetermined composite reading'' actually describing ONE constraint or a constraint FAMILY?',
    'Structural test: If the four mechanisms (legal prohibition, cultural delegitimation, institutional modernization, Civil War trauma) have different beneficiaries, different victims, and different temporal dynamics, they are four separate constraints. If they have the same beneficiary set, victim set, and temporal signature across all four, they are one constraint viewed through multiple lenses.',
    'If family: decompose into four separate constraint stories (legal_prohibition_dueling, cultural_delegitimation_dueling, institutional_modernization_dueling, civil_war_trauma_dueling) and link via network.affects_constraints. If one constraint: the overdetermined reading stands, but the non-separability of mechanisms becomes a fundamental uncertainty (omega variable: causal_pathway_separability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_crisis, conceptual, 'Whether ''overdetermined composite'' is one constraint or a family').

omega_variable(
    victim_identification_ambiguity,
    'Who are the true victims of this constraint? Is it dueling practitioners (trapped in honor code), aristocratic culture (marginalized by modernization), or both?',
    'Historical narrative analysis: Did dueling practitioners experience the constraint as extraction (forced choice between social death and legal jeopardy)? Did aristocratic culture experience it as displacement rather than victimization (adaptation rather than harm)? Did some practitioners resist while others adapted?',
    'If practitioners are victims: snare classification from powerless perspective is correct. If aristocratic culture is victim: tangled_rope is correct (culture benefits from coordination, bears cost of displacement). Classification precision depends on victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, empirical, 'Identity of true victims in dueling decline').

omega_variable(
    false_summit_historical_inevitability,
    'Is dueling''s decline an immutable historical law (mountain) or a contingent institutional outcome that could have been otherwise?',
    'Comparative institutional analysis: Did all modernizing societies eliminate dueling, or did some (Iceland, parts of Latin America, Middle East) retain it? If elimination was universal, suggest immutability; if contingent, suggest false summit.',
    'If immutable: mountain classification appropriate (historical laws). If contingent: the tangled_rope reading is authentic, and the mountain classification risks naturalizing what was a contested institutional outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_historical_inevitability, empirical, 'Whether dueling decline is historically inevitable or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overdetermined_composite_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(over_tr_t0, overdetermined_composite_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(over_tr_t10, overdetermined_composite_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(over_tr_t20, overdetermined_composite_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(over_be_t0, overdetermined_composite_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(over_be_t10, overdetermined_composite_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(over_be_t20, overdetermined_composite_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overdetermined_composite_reading, identity_coordination).
narrative_ontology:affects_constraint(overdetermined_composite_reading, dueling_legal_prohibition).
narrative_ontology:affects_constraint(overdetermined_composite_reading, dueling_cultural_delegitimation).
narrative_ontology:affects_constraint(overdetermined_composite_reading, dueling_institutional_modernization).
narrative_ontology:affects_constraint(overdetermined_composite_reading, dueling_civil_war_trauma).

% DUAL FORMULATION NOTE:
% The overdetermined composite reading is a meta-constraint analyzing four mechanistically distinct constraints (legal, cultural, institutional, traumatic) that operate non-separably on the practice of dueling. Each mechanism should be authored as a separate constraint story with its own epsilon, beneficiary/victim structure, and temporal dynamics. This story is the family-level analysis showing how non-separable mechanisms compound to produce tangled_rope classification where any single mechanism alone would classify differently (some as rope, some as snare, some as scaffold). Network.affects_constraints links to the four component mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
