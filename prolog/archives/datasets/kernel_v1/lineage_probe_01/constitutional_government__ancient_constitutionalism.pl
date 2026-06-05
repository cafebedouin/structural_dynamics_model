% ============================================================================
% CONSTRAINT STORY: constitutional_government__ancient_constitutionalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_government__ancient_constitutionalism, []).

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
 *   constraint_id: constitutional_government__ancient_constitutionalism
 *   human_readable: Constitutional Government as Ancient Balanced Polity
 *   domain: political/legal
 *
 * SUMMARY:
 *   The ancient constitutionalism reading models constitutional government as
 *   the balanced polity, where limitation on absolute rule emerges from
 *   mixing the social orders — monarchy, aristocracy, and commons — such that
 *   each order checks the ambitions of the others. This reading predates
 *   written constitutions and roots constitutional limitation in structural
 *   rather than textual form. The constraint exhibits tangled rope
 *   characteristics at the level of the participating orders (each gains
 *   coordination benefit from the balance while suffering extraction through
 *   the necessity of concession) and snare characteristics from the
 *   perspective of the unenfranchised excluded from all three orders. The
 *   extraction is moderate and contested: whichever order currently holds the
 *   strongest position benefits; the others experience extraction in the form
 *   of constrained prerogatives; the excluded below all orders bear
 *   suppression without any countervailing benefit. Theater ratio is
 *   relatively low (0.35) because the mechanism is largely functional for
 *   those within it — the balance operates through genuine institutional
 *   checks rather than through pure performance. However, from the
 *   perspective of later constitutional traditions (written constitutions,
 *   rights anchoring, popular sovereignty), this ancient form appears
 *   increasingly theatrical as alternative legitimacy bases emerge.
 *
 * KEY AGENTS:
 *   - Monarchy: Organized institutional actor (power-bearer anchored in heredity and prerogative) — benefits from the mixed constitution's legitimacy conferral, constrained by need to negotiate with orders. Victim of extraction through limitation of prerogative.
 *   - Aristocracy: Organized institutional actor (power-bearer anchored in property and lineage) — benefits from protection against monarchical overreach and popular pressure, constrained by need to concede some authority. Victim of extraction through necessity to negotiate.
 *   - Commons (Enfranchised): Moderate organized actor (property-holding merchants and burgesses) — benefits from participation in the balance and protection against aristocratic monopoly, constrained by resource requirements and threat of exclusion. Victim of extraction through necessity to concede authority to both king and lords.
 *   - Unenfranchised Commons: Powerless excluded population (peasants, laborers, women, religious minorities depending on context) — excluded from the balancing mechanism entirely. Primary victim of suppression with no countervailing coordination benefit.
 *   - The Mixed Constitution as Institutional Form: The pattern itself (trilogy of orders, mutual veto, participation as legitimacy) — carries through epochs and contexts as a structural solution to the problem of concentrating power without absolute rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_government__ancient_constitutionalism, 0.38).
domain_priors:suppression_score(constitutional_government__ancient_constitutionalism, 0.52).
domain_priors:theater_ratio(constitutional_government__ancient_constitutionalism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_government__ancient_constitutionalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_government__ancient_constitutionalism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_government__ancient_constitutionalism, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_government__ancient_constitutionalism, tangled_rope).
narrative_ontology:human_readable(constitutional_government__ancient_constitutionalism, "Constitutional Government as Ancient Balanced Polity").
narrative_ontology:topic_domain(constitutional_government__ancient_constitutionalism, "political/legal").

domain_priors:requires_active_enforcement(constitutional_government__ancient_constitutionalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_government__ancient_constitutionalism, 'aa91e866-69c4-4fef-a1a8-18bb5d7a275a').
narrative_ontology:cs_kernel_codification('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', formalized).
narrative_ontology:cs_authority_grounding('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', extraction).
narrative_ontology:cs_interpretation_layer_present('aa91e866-69c4-4fef-a1a8-18bb5d7a275a').
narrative_ontology:cs_reading_relation('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', constitutional_government__postwar_constitutionalism, influences).
narrative_ontology:cs_reading_relation('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', constitutional_government__revolutionary_constitutionalism, forecloses).
narrative_ontology:cs_reading_relation('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', constitutional_government__westminster_evolution, coexists_with).
narrative_ontology:cs_axiom('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', foundational, structural_order_necessity).
narrative_ontology:cs_axiom_status(structural_order_necessity, holdable).
narrative_ontology:cs_axiom_grounding('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', structural_order_necessity, deontological).
narrative_ontology:cs_axiom('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', foundational, written_instruments_insufficient).
narrative_ontology:cs_axiom_status(written_instruments_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', written_instruments_insufficient, deontological).
narrative_ontology:cs_reference_frame('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', structural_order_balance).
narrative_ontology:cs_drift_state('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', contemporary_postwar_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aa91e866-69c4-4fef-a1a8-18bb5d7a275a', '').
narrative_ontology:cs_kernel_id(constitutional_government__ancient_constitutionalism, constitutional_government).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_government__ancient_constitutionalism, dominant_order_of_the_moment).
narrative_ontology:constraint_victim(constitutional_government__ancient_constitutionalism, unenfranchised_commons).
narrative_ontology:constraint_victim(constitutional_government__ancient_constitutionalism, excluded_social_orders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENFRANCHISED COMMONS (SNARE) — Excluded from all three orders (aristocracy, monarchy, commons proper). The balance among the orders suppresses alternatives to the mixed constitution itself. No voice in the coordination mechanism; bears costs of suppression without sharing benefits. Maximum extraction from the powerless position.
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARISTOCRATIC ORDER (TANGLED ROPE) — Genuine coordination benefit: the mixed constitution prevents any single order (especially monarchy) from absolute rule. But also extraction: the aristocracy must concede some prerogatives to the king in exchange for participating in the balance. Constrained by the need to maintain the coalition against monarchical overreach. Experiences both coordination (protection against tyranny) and asymmetric extraction (conceding authority to the crown).
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONARCHY (TANGLED ROPE) — Genuine coordination benefit: the mixed constitution legitimates monarchical rule through the participation of the orders rather than through force alone. But also extraction: the king must negotiate with the orders rather than rule by prerogative. Constrained by the need to maintain the balance against aristocratic or popular coalitions. Experiences both coordination (legitimacy through participation) and extraction (limitation of prerogative).
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMONS (ENFRANCHISED) (TANGLED ROPE) — Moderate power derives from participation in the balancing mechanism. Genuine coordination benefit: protection against aristocratic monopoly. Extraction: the commons must concede authority to both king and aristocracy. Constrained by resource requirements for political organization and by the threat of exclusion if they push too hard against the balance.
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MIXED CONSTITUTION AS INSTITUTIONAL FRAMEWORK (ROPE) — Pure coordination function: the balancing mechanism solves the collective action problem of governing without absolute rule. The institution experiences the constraint as low-extraction coordination: each order maintains the balance because the alternative (dominion by a rival order) is worse. The institution itself is not victimized — it is the instrument through which the orders negotiate. Arbitrage exit option: the institution can always adjust the balance by shifting which orders hold which powers.
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-MEDIEVAL EUROPEAN POLITY MODELS (PITON) — From the vantage of later constitutional traditions (written constitutions, rights anchoring, popular sovereignty), the ancient mixed constitution appears as a degraded constraint: the theatrical maintenance of 'balance' masks an underlying struggle for dominance. The ritual invocation of mixed order checks becomes performative once alternative legitimacy bases (written law, rights, popular will) emerge. The ancient reading persists through institutional inertia — cited as precedent even as its functional mechanism atrophies.
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a trans-historical, trans-cultural perspective, the ancient mixed constitution appears as an expression of a fundamental structural law: concentration of power is inherently unstable, and the only sustainable solution is to fragment power among multiple centers that check one another. This reading treats the balance of orders as an immutable property of stable governance itself — a natural law of politics. However, the structural data contradicts the mountain classification: the beneficiary/victim declarations reveal that this 'law of nature' systematizes extraction from the unenfranchised while naturalizing the privilege of the orders.
constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_government__ancient_constitutionalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_government__ancient_constitutionalism, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_government__ancient_constitutionalism, TR),
    TR >= 0.70.

:- end_tests(constitutional_government__ancient_constitutionalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, contested among the orders. The mixed constitution delivers genuine coordination benefit for those within it — each order avoids dominance by the others through institutionalized mutual veto. However, this coordination comes at a cost: each order must limit its prerogatives and negotiate constantly. For the enfranchised orders, this is a fair trade (tangled rope from their perspective). For the unenfranchised entirely outside the mechanism, there is pure extraction with no coordination benefit (snare from their perspective). The moderate overall extractiveness reflects that the system genuinely coordinates for its participants but does so by excluding a large portion of the population. Suppression (0.52): Moderate-high, structural and enforceable. The mixed constitution actively suppresses alternatives to the three-order model: proposals for inclusion of new orders, expansion of commons participation, or shift to alternative forms (democracy, single-order rule, written constraint) are blocked by the established orders' collective interest in maintaining the balance. The suppression operates through both structural barriers (property requirements, exclusion from assembly) and through rhetorical naturalization (the three orders are presented as the only possible form of stable limitation). Theater ratio (0.35): Relatively low because the mechanism is substantially functional. The mutual veto of the orders operates through genuine institutional procedures (parliamentary sessions, royal prerogative, aristocratic council, commons petition) rather than pure ritual. However, from the perspective of post-medieval polities adopting written constitutions and rights anchoring, the ancient form becomes increasingly theatrical — the invocation of 'balanced orders' persists but the functional limitation has shifted to written text and explicit rights, making the order-balance appear performative.
 *
 * PERSPECTIVAL GAP:
 *   The ancient constitutionalism reading produces a distinctive perspectival profile. For the three participating orders, the constraint is tangled rope or rope — genuine coordination benefit mixed with extraction costs. For the monarchy specifically, the mixed constitution provides legitimacy (coordination benefit) alongside prerogative limitation (extraction). For the aristocracy and commons, the mechanism provides protection against dominance by the others (coordination) alongside the requirement to negotiate and concede (extraction). For the unenfranchised entirely outside the mechanism, the constraint is snare — pure suppression with no participation benefit. The analytical observer risks classifying the ancient constitution as a natural law (mountain) — treating the three-order balance as a universal principle of stable governance that cannot be otherwise. However, the structural data reveals this as a false summit: the beneficiaries (the enfranchised orders) and the victims (the excluded) are identifiable, and the 'natural necessity' of the three-order form naturalizes what is actually a contingent historical arrangement that benefits those within it while suppressing those outside. The piton perspective (degraded institutional form) emerges only when the constraint is observed from the vantage of later constitutional traditions that have adopted alternative forms of limitation (written text, rights, popular sovereignty) — at that historical point, the ritual invocation of order-balance becomes increasingly performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationships. For the monarchy and aristocracy as beneficiaries of the balancing mechanism, d is shifted downward by arbitrage-capable institutional exit — they can always adjust the balance by shifting which orders hold which powers, and they benefit from the arrangement. For the commons (enfranchised), d is moderate: they participate but are constrained by resource requirements and threat of exclusion if they push the balance too far. For the unenfranchised powerless, d is high (approaching 1.0): they are trapped by structural exclusion with no exit option, and they bear suppression costs with no coordination benefit. The constraint operates through enforcement of the exclusion boundaries — the orders collectively maintain the suppression of alternatives and of new entrants below all orders.
 *
 * MANDATROPHY ANALYSIS:
 *   The ancient constitutionalism reading avoids mandatrophy by clearly distinguishing between the tangled rope experienced by the participating orders (genuine coordination + extraction) and the snare experienced by the unenfranchised (pure suppression). The constraint is not mislabeled as pure coordination (that would be snare-disguised-as-rope) because the structural data explicitly declares victims: those excluded from the three orders. The analytical perspective that risks a false summit (treating the mixed constitution as an immutable natural law of governance) is identified as such — the structural data contradicts the mountain classification, and the false summit detector evaluates whether this is a genuine natural law or a naturalized institutional arrangement. The theater ratio is relatively low, indicating functional institutional mechanism rather than pure performance, which confirms the tangled rope classification for the participating orders rather than degradation to piton. The constraint is active and not yet degraded into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_vs_dominance_cycle,
    'Does the ancient mixed constitution prevent dominance of any single order, or does it merely obscure a cycling pattern in which orders take turns dominating while maintaining the rhetorical fiction of balance?',
    'Historical analysis of periods of alleged ''balanced'' rule: measurement of actual distribution of prerogative, resource allocation, and agenda-setting power across orders. Correlation between declared balance and observable concentration.',
    'If genuine balance: the constraint delivers real coordination benefit, and tangled_rope classification is stable across epochs. If cycling dominance: the constraint is a mechanism for legitimating each order''s turn at hegemony while preventing permanent opposition — extractiveness increases, classification shifts toward snare for the moment''s subordinate order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balance_vs_dominance_cycle, empirical, 'Whether mixed constitution prevents dominance or masks cycling hegemony').

omega_variable(
    alternative_stable_arrangements,
    'Are there stable polities that achieve limitation without the trichotomous balance of orders? If so, does the necessity claim for the mixed constitution collapse?',
    'Comparative institutional analysis: identify examples of stable limited government without all three orders present (e.g., Venice with oligarchic merchant republic, Poland with elected monarchy, Iceland with thing), and measure their stability/coerciveness profiles.',
    'If stable alternatives exist without the mixed order: the ancient reading''s claim to natural necessity weakens, and the constraint becomes a contingent historical form rather than a universal natural law. Reclassifies from mountain (in the analytical perspective) toward tangled_rope or snare (contingent institutional arrangement). If no stable alternatives: the analytical mountain perspective is epistemically warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_stable_arrangements, empirical, 'Existence of stable polities without mixed order structure').

omega_variable(
    order_definition_boundary,
    'What determines whether a group is one of the three orders or is excluded from the mixed constitution entirely? Is the boundary structural or epistemic?',
    'Textual and historical analysis: how are the three orders defined in foundational sources (Polybius, Cicero, Aristotle, medieval commentaries)? Do the definitions appeal to intrinsic properties (birth, property holdings, institutional role) or to the contingent recognition of the community? Are the boundaries stable or subject to contestation?',
    'If structural: the three-order limitation is the only possible form of balance, and alternatives are incoherent. If epistemic/contingent: the boundaries are subject to redefinition — new orders can be added, existing orders can merge or split, and this destabilizes the balancing mechanism. The constraint becomes more fragile and less naturally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(order_definition_boundary, conceptual, 'Whether order membership is structural or epistemically contingent').

omega_variable(
    written_vs_mixed_compatibility,
    'Does the ancient mixed constitution reading foreclose the possibility of written constitutional limitation, or can both coexist in a single framework?',
    'Historical and theoretical analysis: can a polity adopt a written constitution while retaining the mixed order structure? Do written limits strengthen or undermine the balancing mechanism? Are there historical examples of hybrid forms (mixed orders + written constraints)?',
    'If foreclose relation confirmed: the ancient reading and the written-constitution readings (revolutionary, postwar) are incompatible fundamental frameworks — one must prevail. If coexistence possible: the readings are orthogonal (one addresses power sources, the other addresses power limits), and both can hold in the same polity. This affects the classification of contemporary polities that claim both mixed orders and written constitutions (e.g., UK history, some European monarchies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(written_vs_mixed_compatibility, conceptual, 'Logical compatibility of mixed-order and written-constitution readings').

omega_variable(
    unenfranchised_suppression_mechanism,
    'How does the mixed constitution suppress the unenfranchised below all orders? Is the suppression structural (no mechanism for inclusion exists), internalized (the excluded accept the order as natural), or both?',
    'Historical analysis of excluded groups'' resistance and petitioning: do they challenge the mixed constitution itself, or do they demand inclusion within one of the existing orders? Does the suppression persist if the mechanism of exclusion is removed (e.g., property requirements dropped, religious tests abolished)? If suppression shifts to internalization rather than structural barrier, measure through rhetorical analysis of how the excluded legitimize the system.',
    'If purely structural: removal of barriers should reduce suppression significantly, suggesting the constraint is contingent on specific exclusion mechanisms. If internalized: the constraint persists even as structural barriers fall, suggesting deeper ideological capture or identity lock. Mixed mechanisms indicate a constraint that adapts its suppressive form as structural barriers erode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenfranchised_suppression_mechanism, empirical, 'Structural vs internalized suppression of the unenfranchised').

omega_variable(
    ancient_reading_kernel_contest,
    'This constraint is one reading of the constitutional_government kernel, instantiating the ancient_constitutionalism position. What is the precise relationship of this reading to its sibling readings (postwar_constitutionalism, revolutionary_constitutionalism, westminster_evolution)?',
    'Rule 4 application: populate cs_structure.reading_relations with explicit relation types (forecloses, coexists_with, influences) for each sibling. Populate cs_structure.axioms with the foundational normative claims that distinguish this reading from its siblings.',
    'Determines the logical structure of the kernel contest: which readings can coexist in a single polity, which are mutually exclusive, which influence but do not rule out one another. Affects how the engine evaluates contemporary polities claiming multiple constitutional traditions (e.g., ''our constitution is both ancient mixed order AND a postwar rights-charter'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ancient_reading_kernel_contest, conceptual, 'Logical structure of the constitutional_government kernel and relationship between ancient and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_government__ancient_constitutionalism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constgov_anc_tr_t0, constitutional_government__ancient_constitutionalism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(constgov_anc_tr_t3, constitutional_government__ancient_constitutionalism, theater_ratio, 3, 0.32).
narrative_ontology:measurement(constgov_anc_tr_t6, constitutional_government__ancient_constitutionalism, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(constgov_anc_be_t0, constitutional_government__ancient_constitutionalism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(constgov_anc_be_t3, constitutional_government__ancient_constitutionalism, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(constgov_anc_be_t6, constitutional_government__ancient_constitutionalism, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(constgov_anc_su_t0, constitutional_government__ancient_constitutionalism, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(constgov_anc_su_t3, constitutional_government__ancient_constitutionalism, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(constgov_anc_su_t6, constitutional_government__ancient_constitutionalism, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_government__ancient_constitutionalism, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_government__ancient_constitutionalism, constitutional_government__postwar_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__ancient_constitutionalism, constitutional_government__revolutionary_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__ancient_constitutionalism, constitutional_government__westminster_evolution).

% DUAL FORMULATION NOTE:
% Constitutional government as a kernel is decomposed into four constraint stories, one per reading. Each reading instantiates a different structural claim about what makes government constitutional. The ancient_constitutionalism reading emphasizes order-balance as the limitation mechanism. The postwar and revolutionary readings emphasize written instruments. The westminster reading emphasizes evolutionary accumulation of binding convention. These are not different views of the same constraint — they are different constraints derived from different interpretations of the kernel. Each has its own ε (moderate and contested for ancient; lower for postwar and revolutionary as they claim more complete written specification; variable for westminster depending on era). The network links show family membership: all four are siblings under the constitutional_government kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_government__ancient_constitutionalism, institutional, 0.35).
constraint_indexing:directionality_override(constitutional_government__ancient_constitutionalism, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
