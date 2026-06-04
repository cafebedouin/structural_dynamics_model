% ============================================================================
% CONSTRAINT STORY: amendment_history__asylum_compromise_1993
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_history__asylum_compromise_1993, []).

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
 *   constraint_id: amendment_history__asylum_compromise_1993
 *   human_readable: 1993 Asylum Compromise: Safe Third Country Rule and Article 16 Narrowing
 *   domain: constitutional_law/migration_policy
 *
 * SUMMARY:
 *   The 1993 asylum compromise represents a pivotal narrowing of Article 16's
 *   founding guarantee of asylum for politically persecuted persons. The
 *   Basic Law's original Article 16 (1949) contained an unqualified
 *   individual right to asylum. This reading instantiates one constitutional
 *   interpretation of how that right was transformed in 1993: the
 *   safe-third-country rule reframed asylum as a right conditioned on route,
 *   not on substantive claim. An asylum applicant arriving via a country
 *   designated as 'safe' was deemed to have exhausted their remedy in that
 *   transit state and could not access Article 16 protection in the receiving
 *   state. The rule solved a real coordination problem — managing the surge
 *   in asylum applications following 1989 German reunification and Eastern
 *   European migration — but it did so by suppressing the constitutional
 *   guarantee itself. The constraint exhibits the core tangled_rope
 *   signature: genuine coordination function (managing state capacity and
 *   burden-sharing) coexists with asymmetric extraction (claimants bear the
 *   cost of route-based exclusion; protection is conditional on compliance
 *   with a rule not derived from the original text). The theater ratio (0.35)
 *   reflects that while safe-third-country determinations are made and
 *   enforced, the underlying legal logic is moderately performative —
 *   designations of 'safety' are sometimes decoupled from actual asylum
 *   infrastructure in the transit states.
 *
 * KEY AGENTS:
 *   - Asylum Claimants Routed via Safe Transit States: Primary victims (powerless/trapped) — routed out of Article 16 by border application of safe-third-country rule
 *   - Migration Control Policy Framework: Primary beneficiary (institutional/arbitrage) — designed to reduce claim volume and operationalize burden-sharing among receiving states
 *   - Receiving State (e.g., Federal Republic): Institutional actor (moderate/constrained) — experiences both coordination benefit (capacity management) and political cost (legitimacy gap between constitutional text and practice)
 *   - Constitutional Article 16 Guarantee: Abstract victim (analytical/analytical) — the scope of the guarantee is narrowed; the right persists in text but is suppressed in application
 *   - Human Rights Organization: Organized challenger (organized/constrained) — advocates for expanded asylum access; has legal and advocacy capacity but faces institutional barriers
 *   - Transit States (e.g., Poland, Czech Republic): Secondary institutional actors (institutional/constrained) — designated as 'safe' without necessarily having requested or consented to that role; bear externalized processing load
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_history__asylum_compromise_1993, 0.58).
domain_priors:suppression_score(amendment_history__asylum_compromise_1993, 0.68).
domain_priors:theater_ratio(amendment_history__asylum_compromise_1993, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_history__asylum_compromise_1993, extractiveness, 0.58).
narrative_ontology:constraint_metric(amendment_history__asylum_compromise_1993, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(amendment_history__asylum_compromise_1993, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_history__asylum_compromise_1993, tangled_rope).
narrative_ontology:human_readable(amendment_history__asylum_compromise_1993, "1993 Asylum Compromise: Safe Third Country Rule and Article 16 Narrowing").
narrative_ontology:topic_domain(amendment_history__asylum_compromise_1993, "constitutional_law/migration_policy").

domain_priors:requires_active_enforcement(amendment_history__asylum_compromise_1993).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_history__asylum_compromise_1993, '1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f').
narrative_ontology:cs_kernel_codification('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', formalized).
narrative_ontology:cs_authority_grounding('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', lineage).
narrative_ontology:cs_interpretation_layer_present('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f').
narrative_ontology:cs_reading_relation('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', amendment_history__debt_brake_2009, coexists_with).
narrative_ontology:cs_reading_relation('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', amendment_history__emergency_acts_1968, influences).
narrative_ontology:cs_reading_relation('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', amendment_history__rearmament_1956, coexists_with).
narrative_ontology:cs_reading_relation('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', amendment_history__reunification_amendments_1990, influences).
narrative_ontology:cs_axiom('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', foundational, asylum_right_route_conditional).
narrative_ontology:cs_axiom_status(asylum_right_route_conditional, holdable).
narrative_ontology:cs_axiom_grounding('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', asylum_right_route_conditional, conventional).
narrative_ontology:cs_axiom('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', foundational, state_capacity_trumps_text_guarantee).
narrative_ontology:cs_axiom_status(state_capacity_trumps_text_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', state_capacity_trumps_text_guarantee, instrumental).
narrative_ontology:cs_reference_frame('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', unconditional_article_16_guarantee).
narrative_ontology:cs_drift_state('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', post_1993_safe_third_country_implementation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('1e4ad1e8-a4ad-4932-bc2f-2cc2a7cb633f', '').
narrative_ontology:cs_kernel_id(amendment_history__asylum_compromise_1993, amendment_history).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_history__asylum_compromise_1993, migration_control_policy).
narrative_ontology:constraint_beneficiary(amendment_history__asylum_compromise_1993, receiving_state_capacity).
narrative_ontology:constraint_victim(amendment_history__asylum_compromise_1993, asylum_claimants_routed_via_safe_transit).
narrative_ontology:constraint_victim(amendment_history__asylum_compromise_1993, constitutional_asylum_guarantee_scope).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM CLAIMANT (SNARE) — Trapped by geography and legal fiction. The safe-third-country rule forecloses access to Article 16 protection by routing the claimant through a nominally 'safe' transit state, regardless of that state's actual asylum infrastructure or willingness to process claims. No exit from the constraint; maximum extraction — protection is conditioned on route, not on need or legal standing.
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: RECEIVING STATE (TANGLED ROPE) — Faces genuine coordination problem (managing asylum intake capacity and public integration) alongside extractive suppression of the constitutional guarantee. The state benefits from reduced caseload but also incurs costs of maintaining fictional 'safety' determinations and managing the legitimacy gap between constitutional promise and legal practice. Mixed experience: real coordination function (capacity management) with embedded asymmetric extraction (claimants bear costs of route-based exclusion).
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MIGRATION CONTROL FRAMEWORK (ROPE) — Pure beneficiary from the 1993 compromise. The safe-third-country rule operationalizes a coordination mechanism for burden-sharing among receiving states while appearing to preserve Article 16. The framework experiences the constraint as coordination: the rule solves the collective action problem of limiting claims on state resources. Low experienced extraction because the framework designed the mechanism to produce this outcome.
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HUMAN RIGHTS ORGANIZATION (TANGLED ROPE) — Experiences the constraint as an extraction mechanism (narrowing of constitutional guarantee) layered onto a genuine coordination problem (state capacity management). The organization has agency (advocacy capacity, legal standing to challenge rules) but faces systemic barriers (institutional inertia of migration policy, political costs of expanding asylum). Mixed experience: genuine interest in coordination (protection systems do require resources and planning) coexists with opposition to the particular extraction mechanism (route-based exclusion that violates the spirit of Article 16).
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / REALPOLITIK VIEW (MOUNTAIN) — From a civilizational perspective, all asylum systems must solve the problem of finite state capacity meeting potentially unlimited claims. The safe-third-country rule appears as an immutable structural feature of how asylum can be managed at all. However, this risks false-summit naturalization: the rule is a political choice (made in 1993 under specific conditions), not a law of nature. The analytical observer must recognize that other constitutional readings — unconditional asylum, burden-sharing without route-based exclusion, regional protection — are structurally possible but politically foreclosed.
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ARTICLE 16 CONSTITUTIONAL TEXT (PITON) — The constitutional guarantee persists in text ('politically persecuted persons shall have the right of asylum') but its function has degraded through the safe-third-country interpretation. The text remains because its removal would require explicit constitutional amendment (politically difficult); the rule persists through interpretive inertia and the institutional convenience of border enforcement. Theater ratio is moderate (the rule is not purely performative — safe-third-country determinations are actually made and enforced) but the gap between text and practice is substantial. This is piton: a degraded constitutional commitment sustained by institutional momentum rather than active reaffirmation.
constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_history__asylum_compromise_1993_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_history__asylum_compromise_1993, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amendment_history__asylum_compromise_1993, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amendment_history__asylum_compromise_1993, TR),
    TR >= 0.70.

:- end_tests(amendment_history__asylum_compromise_1993_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The safe-third-country rule extracts by narrowing access to a constitutional right. The extraction is not maximal (snare-level 0.66+) because the rule solves a genuine coordination problem — asylum systems do require resource allocation and claimant management — and some claimants can still access protection if they establish they cannot safely access transit-state processing. But the extraction is significant because it conditions the right on route in a way the original Article 16 did not. The extractiveness has increased over the interval as the rule was fully implemented and enforcement machinery matured (0.42 → 0.58). Suppression (0.68): High. The safe-third-country rule suppresses alternatives: claimants cannot refuse routing through designated transit states and then claim direct Article 16 protection; states cannot process claims from individuals who have transited through designated 'safe' states. The suppression is not total (0.75+) because court challenges and individual assessments remain possible, and the rule itself is subject to constitutional interpretation (though rarely challenged successfully on Article 16 grounds). Suppression requirement (enforcement machinery needed) rises from 0.50 to 0.68 as the rule matures — more enforcement infrastructure is needed to sustain the fiction that all designated transit states are genuinely 'safe.' Theater ratio (0.35): Moderate. The rule is not primarily performative (it is actually enforced at borders; determinations are made; people are turned away). But it is partially performative in that the safety designations do not always correspond to actual asylum infrastructure or processing capacity in the transit states. The theater is moderate rather than low because the rule's coherence depends on the designation system, which has elements of ritual (annual designation reviews, statutory criteria that may not track reality).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The claimant sees a snare (pure extraction with no exit). The beneficiary migration-control framework sees a rope (coordination mechanism for burden-sharing). The receiving state sees tangled rope (genuine coordination problem with embedded asymmetry). The human rights organization sees tangled rope with emphasis on extraction; the state sees tangled rope with emphasis on coordination. The analytical observer at the civilizational level risks seeing a mountain (inherent limits to state capacity and asylum claims) but the structural data reveal this as false-summit naturalization — the safe-third-country rule was a political choice in 1993, not a discovery of natural law. The article 16 text itself (piton perspective) shows degradation: the guarantee persists but is functionally suppressed through interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) derives from the base extractiveness (ε=0.58) scaled by the agent's structural position via the sigmoid directionality function f(d). The powerless claimant routed via safe-third-country has high d (close to 1.0: victim + trapped exit) → high f(d) → high χ experienced. The institutional migration control framework has low d (close to 0.0: beneficiary + arbitrage exit) → low/negative f(d) → low χ experienced (they see this as coordination, not extraction). The receiving state has moderate d reflecting mixed benefit/burden → moderate f(d) → moderate χ (genuine coordination plus extraction costs). The analytical observer's mountain reading at civilizational scope risks masking the political origins of the rule: viewing state capacity limits as natural law rather than as contingent historical conditions in 1993 (reunification, Eastern European migration, political pressure to appear 'tough'). The piton reading reflects that Article 16's text persists (the guarantee is still written) but its function has degraded through safe-third-country interpretation applied without explicit amendment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safe_third_country_fiction_efficacy,
    'Do the transit states designated as ''safe'' actually provide functionally equivalent asylum processing and protection, or is the safety designation a legal fiction designed to suppress claims?',
    'Empirical audit of asylum recognition rates, processing times, and protection standards in transit states; comparison of claimants'' outcomes routed via rule vs. those who reach direct application',
    'If genuinely safe: constraint is pure coordination (Rope from claimant perspective if exit via transit is viable). If fictional: constraint is pure extraction (Snare confirmed; suppression of constitutional guarantee confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safe_third_country_fiction_efficacy, empirical, 'Whether safe-third-country designations reflect actual functional equivalence or are legal fiction').

omega_variable(
    state_capacity_vs_policy_choice,
    'Is the safe-third-country rule a response to genuine material limits on state capacity to process asylum claims, or a political choice to suppress claims despite available capacity?',
    'Comparison of state resources allocated to asylum processing before and after 1993; correlation between policy changes and actual capacity constraints vs. political pressure; counterfactual modeling of processing timelines if all claimants accessed direct application',
    'If capacity-driven: constraint is primary coordination function (tangled_rope classification stable). If politically-driven suppression: constraint is primary extraction masquerading as coordination (reclassify to snare at multiple perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_policy_choice, empirical, 'Whether safe-third-country rule responds to material capacity limits or political choice').

omega_variable(
    reading_alternative_framings,
    'What alternative constitutional readings of Article 16 are foreclosed by the 1993 compromise reading, and what axioms ground each reading''s legitimacy claim?',
    'Examination of constitutional debate 1990-1993; identification of rejected proposals (unconditional asylum, regional burden-sharing mechanisms, supranational coordination); analysis of each proposal''s normative grounding',
    'This omega routes to cs_structure: each sibling reading will have its own axiom set, and this reading''s axioms distinguish its particular normative claim. This is the committer-frame documentation: the 1993 reading forecloses or influences (not coexists with) the alternatives, depending on their logical relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_alternative_framings, conceptual, 'Alternative constitutional readings of Article 16 and their logical relationships to the 1993 compromise').

omega_variable(
    theater_ratio_stability,
    'Is the moderate theater ratio (0.35) stable, or does the constraint exhibit increasing theater as safe-third-country determinations become more formulaic and detached from actual safety assessment?',
    'Longitudinal analysis of court challenges to safe-third-country designations; tracking of policy documentation quality; comparison of rejected vs. approved applications at borders vs. in asylum offices',
    'If theater increases over time (reclassify toward piton). If decreases: administrative tightening suggests strengthening of coordination function or move toward pure snare (depending on outcome direction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_stability, empirical, 'Temporal stability of theater ratio in safe-third-country enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_history__asylum_compromise_1993, 1993, 2003).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(asylum_1993_be_t0, amendment_history__asylum_compromise_1993, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(asylum_1993_be_t3, amendment_history__asylum_compromise_1993, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(asylum_1993_be_t6, amendment_history__asylum_compromise_1993, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(asylum_1993_be_t10, amendment_history__asylum_compromise_1993, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(asylum_1993_su_t0, amendment_history__asylum_compromise_1993, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(asylum_1993_su_t5, amendment_history__asylum_compromise_1993, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(asylum_1993_su_t10, amendment_history__asylum_compromise_1993, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_history__asylum_compromise_1993, resource_allocation).
narrative_ontology:boltzmann_floor_override(amendment_history__asylum_compromise_1993, 0.22).
narrative_ontology:affects_constraint(amendment_history__asylum_compromise_1993, amendment_history__reunification_amendments_1990).
narrative_ontology:affects_constraint(amendment_history__asylum_compromise_1993, amendment_history__debt_brake_2009).

% DUAL FORMULATION NOTE:
% The 1993 asylum compromise is part of the amendment_history kernel family. This reading interprets the compromise as suppression of an unconditional right (extractiveness 0.58). Sibling readings of the same kernel interpret OTHER amendments with different structural impacts. All readings share the kernel (the Basic Law as stabilized constitutional commitment) but differ in how they read the amendment's transformation of that kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
