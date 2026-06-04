% ============================================================================
% CONSTRAINT STORY: establishment_clause__coercion_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_establishment_clause__coercion_test_reading, []).

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
 *   constraint_id: establishment_clause__coercion_test_reading
 *   human_readable: Establishment Clause: Coercion Test Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The Establishment Clause ('Congress shall make no law respecting an
 *   establishment of religion') is a constitutional kernel contested by four
 *   structurally distinct doctrinal readings. This constraint instantiates
 *   the coercion test reading: government violates the Establishment Clause
 *   only when it compels religious participation or support by force of law.
 *   This reading, championed by Justice Scalia and others, represents the
 *   narrowest suppression barrier and the most permissive standard for
 *   government religious expression. Under this reading, government
 *   endorsement of religion, religious references in law, tax support for
 *   religious organizations, and religious speech in public spaces are
 *   constitutional so long as participation is voluntary and no legal
 *   compulsion attaches. Only direct legal mandates (mandatory school prayer,
 *   compulsory religious oaths, tax funds directly appropriated to sectarian
 *   purposes) trigger violation. The coercion test emerged as a dominant
 *   interpretive frame in the 1990s (Lee v. Weisman, 1992) but remains
 *   contested against three sibling readings: (1) the endorsement test
 *   (whether a reasonable observer perceives government as sending a message
 *   that religion is favored), (2) the history/tradition reading (what the
 *   founding generation accepted cannot violate the clause), and (3) the
 *   Lemon test (secular purpose, primary effect, no excessive entanglement).
 *   The coercion test trades clarity of violation condition (explicit
 *   compulsion) for breadth of permitted government religious expression.
 *   This creates a tangled-rope structure: genuine coordination function
 *   (enabling government to draw on religious authority and resources without
 *   mandating participation) paired with asymmetric suppression of objector
 *   interests (objectors must clear the high evidentiary bar of proving
 *   compulsion; mere endorsement, financial support, or social pressure do
 *   not suffice). The constraint's extractiveness (0.52) reflects moderate
 *   but real extraction of objector interests to enable public religious
 *   expression.
 *
 * KEY AGENTS:
 *   - Compelled religious objectors (powerless/trapped) — victims bearing the high burden of proving compulsion before violation is recognized
 *   - Government institutions sponsoring religious expression (institutional/arbitrage) — beneficiaries experiencing the constraint as coordination enabling religious authority without mandate
 *   - Conscience-objecting taxpayers (moderate/constrained) — mixed position: support religious programs indirectly through taxation; organized exit paths exist but face political constraints
 *   - Secular advocacy organizations (organized/arbitrage) — fight the coercion standard as too narrow; see it as temporary framework with sunset via successful litigation
 *   - Religious organizations defending establishment (organized/constrained) — defend coercion test as favorable; benefit from government support; constrained by obligation to remain voluntary
 *   - Courts applying the doctrine (institutional/arbitrage) — maintain the coercion standard through doctrine while its operational meaning drifts, creating piton-like degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(establishment_clause__coercion_test_reading, 0.52).
domain_priors:suppression_score(establishment_clause__coercion_test_reading, 0.35).
domain_priors:theater_ratio(establishment_clause__coercion_test_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(establishment_clause__coercion_test_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(establishment_clause__coercion_test_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(establishment_clause__coercion_test_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(establishment_clause__coercion_test_reading, tangled_rope).
narrative_ontology:human_readable(establishment_clause__coercion_test_reading, "Establishment Clause: Coercion Test Reading").
narrative_ontology:topic_domain(establishment_clause__coercion_test_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(establishment_clause__coercion_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(establishment_clause__coercion_test_reading, 'e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8').
narrative_ontology:cs_kernel_codification('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', fixed_text).
narrative_ontology:cs_authority_grounding('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', lineage).
narrative_ontology:cs_interpretation_layer_present('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8').
narrative_ontology:cs_reading_relation('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', establishment_clause__endorsement_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', establishment_clause__history_tradition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', establishment_clause__lemon_test_reading, coexists_with).
narrative_ontology:cs_axiom('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', foundational, compulsion_is_sole_violation_trigger).
narrative_ontology:cs_axiom_status(compulsion_is_sole_violation_trigger, holdable).
narrative_ontology:cs_axiom_grounding('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', compulsion_is_sole_violation_trigger, deontological).
narrative_ontology:cs_axiom('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', foundational, voluntary_religious_expression_by_government_is_permitted).
narrative_ontology:cs_axiom_status(voluntary_religious_expression_by_government_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', voluntary_religious_expression_by_government_is_permitted, instrumental).
narrative_ontology:cs_reference_frame('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', minimal_coercion_establishment_boundary).
narrative_ontology:cs_drift_state('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', contemporary_2020, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e2d7fe18-1633-49bb-b4ac-5c0b6bc48dc8', '').
narrative_ontology:cs_kernel_id(establishment_clause__coercion_test_reading, establishment_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(establishment_clause__coercion_test_reading, public_religious_expression).
narrative_ontology:constraint_victim(establishment_clause__coercion_test_reading, religious_objectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPELLED OBJECTOR (SNARE) — An objector facing direct legal compulsion to participate in religious observance or support (e.g., mandatory prayer in public schools, tax support for sectarian institutions via direct mandate) experiences the constraint as pure extraction. Cannot exit without violating law. Coercion test makes this the primary violation case — maximum extraction from objector's position.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT AS BENEFICIARY (ROPE) — Government benefits from using religious authority to legitimize law or build social cohesion, but under the coercion test, the constraint is experienced as pure coordination: 'we can endorse or support religion only if we don't compel participation.' The constraint enables rather than restricts — government can fund religious organizations, reference religious values, or permit religious speech in public spaces without triggering violation. Net beneficiary, low extraction experienced.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSCIENCE-OBJECTING TAXPAYER (TANGLED ROPE) — A taxpayer who objects to supporting religious institutions or programs through taxation faces a mixed constraint. The coercion test narrows violation: if tax support is indirect (general funding that happens to reach religious entities) versus direct mandate, the test requires showing actual compulsion. The taxpayer has exit options (voting, organizing) but constrained by the political process. Moderate extraction — some subordination of conscience without full suppression of alternatives.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR ADVOCACY ORG (SCAFFOLD) — Organized secular groups see the coercion test as a temporary framework with an implicit sunset: as religious establishment (understood as direct compulsion) becomes less frequent, litigation becomes less necessary and the constraint decays. The test is designed to phase itself out by making compulsion the operative standard — successful litigation enforces that standard, making future violations rarer. Organized agents see clear victory conditions and enforcement mechanisms.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL SYSTEM (PITON) — Courts apply the coercion test as doctrine, but the test's operational meaning is increasingly theatrical. Proof of 'coercion' or 'compulsion' has become debatable (does social pressure count? do school prayer policies that permit but don't mandate count?). The enforcement ritual persists through institutional inertia — the coercion standard remains the stated doctrine — but its meaning drifts with each application, making enforcement increasingly decoupled from the test's original clarity. Piton classification reflects theater_ratio driven by doctrinal drift masking practice.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DEFENDING RELIGIOUS ORGS (TANGLED ROPE) — Organized religious groups arguing for the coercion test see it as favorable (narrower than endorsement test, broader than history/tradition test). They benefit from government support and religious expression in public spaces, but must accept the coordination function: 'we can have this support only if we don't compel participation.' They face constrained exit — organized agents with institutional power but limited by the doctrinal boundaries of the coercion test itself. Moderate extraction from their perspective because benefits are paired with the obligation to remain voluntary.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational analytical view, the coercion test is one competing reading of the Establishment Clause kernel. The constraint's extractiveness (0.52) reflects that the coercion standard coordinates public support for religion while suppressing some objections through the high barrier of proving 'compulsion.' The tangled rope classification is stable across the analytical frame — genuine coordination function (enabling religious expression without mandate) paired with asymmetric suppression (objectors must prove compulsion, a high evidentiary burden). This reading coexists with three other doctrinal readings, each with different ε values and different beneficiary/victim structures.
constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(establishment_clause__coercion_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(establishment_clause__coercion_test_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(establishment_clause__coercion_test_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(establishment_clause__coercion_test_reading, TR),
    TR >= 0.70.

:- end_tests(establishment_clause__coercion_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.52): Moderate-high, rising over the interval. The coercion test starts narrower in 1970 (ε=0.35) as courts apply it strictly, narrowly defining compulsion. By 2020 (ε=0.52), the test has broadened de facto through doctrinal drift — courts increasingly find 'coercion' in psychological/social pressure, systemic exclusion, and peer pressure (Lee v. Weisman's 'subtle coercive pressure' language). This rising trajectory reflects the test's own instability: stricter definition of compulsion would lower ε; looser definition raises it toward the endorsement test. The midpoint value (0.48 in 1995) reflects the coercion test's post-Lee v. Weisman consolidation as a major standard with moderate scope. SUPPRESSION (0.35): Low-moderate and stable. The coercion test explicitly narrows suppression by setting the violation threshold high — objectors must prove compulsion, not mere endorsement or financial entanglement. This represents intentional narrowing relative to alternatives (endorsement test would have suppression ~0.55, Lemon test ~0.60). Suppression remains at 0.35 rather than dropping to 0.20 because the requirement to prove compulsion itself suppresses objector remedies: merely showing you object to government religious expression is insufficient; the system requires proof of actual coercive effect. THEATER RATIO (0.58, rising): Moderate-high and increasing. The coercion test's application has become increasingly theatrical — courts invoke the coercion standard while de facto expanding what counts as 'compulsion,' creating gap between the stated doctrine (explicit legal mandate) and actual practice (social/psychological pressure now counts). The rising trajectory (0.42 → 0.58) reflects increasing doctrinal drift: the test persists through institutional inertia (stare decisis, canonical citation) while its operational meaning diverges from the original narrow standard. Theater ratio captures this degradation.
 *
 * PERSPECTIVAL GAP:
 *   The coercion test produces the full spectrum of DR classifications from different structural positions. Compelled objectors (trapped/powerless) see a snare — they cannot exit and must endure extraction of their conscience. Governments and religious organizations defending the test (institutional/arbitrage beneficiaries) see a rope — they coordinate on 'we can use religion without mandating it' without experiencing extraction. Conscience-objecting taxpayers (moderate/constrained) see tangled rope — some benefit from living in a religiously expressive society, some extraction from funding religion, no clean exit. Secular advocacy groups (organized arbitrage) see a scaffold with sunset — the test will phase itself out if litigants successfully enforce its coercion requirement. Courts (institutional arbitrage) see piton — the standard persists through doctrine but its meaning becomes increasingly detached from practice. The analytical observer sees tangled rope — stable classification reflecting genuine coordination paired with genuine asymmetric suppression. These are not disagreements about facts; they are structural differences in how the constraint binds each agent.
 *
 * DIRECTIONALITY LOGIC:
 *   The coercion test's beneficiary structure (government religious expression) combined with victim structure (religious objectors) produces directionality (d) values that scale extractiveness differently across agent types. Government institutions with arbitrage options (can establish religion or not) experience low d (~0.15) → low effective extraction → rope classification. Religious objectors facing legal compulsion (trapped, no arbitrage) experience high d (~0.95) → high effective extraction → snare classification. Conscience-objecting taxpayers (constrained, partial exit through voting) experience moderate d (~0.65) → moderate effective extraction → tangled rope. Organized secular groups (constrained, collective action possible) experience moderate d (~0.55) → moderate effective extraction → scaffold/tangled rope. The perspectival spread derives from the structural fact that exit options distribute asymmetrically: beneficiaries have arbitrage (can use religion or not), victims have only trapped/constrained options (cannot avoid exposure or funding without cost). The coercion test amplifies this asymmetry by setting the violation threshold at explicit compulsion — objectors get recognition only when trapped, not when constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_definition_ambiguity,
    'What constitutes ''coercion'' or ''compulsion'' sufficient to violate the Establishment Clause under this reading: direct legal mandate only, or also psychological/social pressure and systemic exclusion?',
    'Case-by-case adjudication; comparison of outcomes where courts find compulsion (school prayer mandates) vs. where they deny compulsion (moments of silence, religious references in law) despite objector claims of social pressure. Empirical study of actual coercive effect (dropout rates, discrimination incidents) vs. doctrinal holdings.',
    'If ''coercion'' requires only direct legal mandate: suppression drops to 0.20, extractiveness drops to 0.30 (rope). If ''coercion'' includes social/systemic pressure: suppression rises to 0.65, extractiveness rises to 0.68 (snare). Classification outcome highly sensitive to this boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Definition of coercion threshold separating direct compulsion from social pressure').

omega_variable(
    sibling_reading_foreclosure,
    'Does the coercion test foreclose or merely compete with the endorsement test, history/tradition test, and Lemon test?',
    'Logical analysis: does commitment to ''only coercion matters'' logically entail rejection of ''endorsement matters'' or ''Lemon test applies''? Courts have applied coercion test alongside endorsement test in same opinions (e.g., Lee v. Weisman). This suggests coexistence rather than foreclosure. If a court adopts coercion as the sole standard and rejects endorsement/Lemon/history grounds explicitly, foreclosure is demonstrated.',
    'If coercion forecloses siblings: reading_relations should include forecloses entries. If coercion coexists: reading_relations should use coexists_with. Current empirical reality: coexistence (multiple standards applied in parallel), suggesting coexists_with is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether coercion test logically rules out sibling doctrinal standards').

omega_variable(
    tacit_compulsion_in_civic_space,
    'In a pluralist democracy with public funds and compulsory taxation, does government funding or endorsement of religion create tacit compulsion for objectors independent of explicit legal mandate?',
    'Empirical: measure objector exit costs — political organizing burden, relocation costs, dissent penalties, career/social sanctions. Normative: does moral complicity in funding religion through taxation constitute compulsion in the relevant sense? Jurisprudential: how do courts define the objector''s baseline entitlement (tax exemption, veto power, or mere standing to litigate)?',
    'If tacit compulsion is real: coercion reading collapses toward endorsement test (suppression/extractiveness rise). If tacit compulsion is not coercion in the relevant sense: coercion reading remains narrowest, suppression/extractiveness stay low. This omega captures the reading''s core contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_compulsion_in_civic_space, preference, 'Whether civic participation in taxing/funding religious programs creates tacit compulsion').

omega_variable(
    false_summit_natural_law_risk,
    'Is the coercion threshold (compulsion triggers violation) a natural feature of constitutional interpretation, or a contingent doctrinal choice that benefits government religious expression at the cost of objector interests?',
    'Historical: trace coercion test origins (Lee v. Weisman, Kennedy''s concurrence). Institutional: identify beneficiaries of coercion standard vs. alternatives. Structural: compare ε values under coercion test (0.52) vs. endorsement test (estimated 0.68) vs. history/tradition test (estimated 0.32). If coercion test selectively benefits religious expression, it is contingent doctrine, not natural law.',
    'If natural law: mountain classification might apply at civilizational scope. If contingent: false summit signature fires (beneficiaries declared, FSM evaluation occurs). Current structure: beneficiaries declared, omegas present, FSM will fire; constraint treated as doctrinal reading, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether coercion test is a natural limit on interpretation or a contingent doctrinal choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(establishment_clause__coercion_test_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(estab_coerce_theater_1970, establishment_clause__coercion_test_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(estab_coerce_theater_1995, establishment_clause__coercion_test_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(estab_coerce_theater_2020, establishment_clause__coercion_test_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(estab_coerce_extract_1970, establishment_clause__coercion_test_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(estab_coerce_extract_1995, establishment_clause__coercion_test_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(estab_coerce_extract_2020, establishment_clause__coercion_test_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(estab_coerce_supp_1970, establishment_clause__coercion_test_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(estab_coerce_supp_1995, establishment_clause__coercion_test_reading, suppression_requirement, 25, 0.33).
narrative_ontology:measurement(estab_coerce_supp_2020, establishment_clause__coercion_test_reading, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(establishment_clause__coercion_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(establishment_clause__coercion_test_reading, establishment_clause__endorsement_test_reading).
narrative_ontology:affects_constraint(establishment_clause__coercion_test_reading, establishment_clause__history_tradition_reading).
narrative_ontology:affects_constraint(establishment_clause__coercion_test_reading, establishment_clause__lemon_test_reading).

% DUAL FORMULATION NOTE:
% The Establishment Clause kernel generates four constraint stories with distinct ε values, beneficiary/victim structures, and suppression profiles. The coercion test reading (this constraint, ε=0.52) is the narrowest suppression, most permissive standard. Endorsement test reading (estimated ε=0.68) has broader suppression. History/tradition reading (estimated ε=0.32) has narrowest suppression among the four. Lemon test reading (estimated ε=0.55) has moderate suppression. These are not the same constraint viewed from different angles — they are structurally distinct doctrinal claims with different verification conditions, different beneficiary/victim sets, and different judicial outcomes. They form a constraint family linked by common kernel (the Establishment Clause text) but differentiated by reading (interpretation method). Network edges capture doctrinal influence: coercion test narrows the baseline; endorsement test broadens it; history/tradition provides alternative axis; Lemon test provides functional test. All four remain live options in contemporary jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(establishment_clause__coercion_test_reading, institutional, 0.12).
constraint_indexing:directionality_override(establishment_clause__coercion_test_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
