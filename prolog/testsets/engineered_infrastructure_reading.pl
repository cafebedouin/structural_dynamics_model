% ============================================================================
% CONSTRAINT STORY: engineered_infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_engineered_infrastructure_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: engineered_infrastructure_reading
 *   human_readable: Markets as State-Engineered Infrastructure (Contested Reading)
 *   domain: political_economy/institutional_design/economic_ideology
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel: what IS a
 *   market? The engineered-infrastructure reading claims that markets are not
 *   natural spontaneous orders but political artifacts requiring continuous
 *   legal and regulatory maintenance. Property rules, contract enforcement,
 *   currency stability, bankruptcy law, labor standards, environmental
 *   limits, and intellectual property protection are NOT the absence of
 *   intervention — they ARE intervention. 'Deregulation' does not reduce
 *   state role; it re-engineers markets to benefit different coalitions. The
 *   constraint exhibits high theater ratio (0.64): the academic, policy, and
 *   popular discourse represents markets as natural or self-organizing,
 *   obscuring the active state maintenance beneath. Extractiveness (0.52) is
 *   moderate-high: beneficiary coalitions (incumbent firms, regulatory
 *   experts, financial intermediaries) extract value from rule-making power;
 *   excluded populations and alternative economic models bear suppression
 *   costs. The sibling readings (spontaneous_order_reading and
 *   beneficiary_maintenance_reading) present competing framings of the same
 *   kernel: are markets spontaneous equilibria that emerge naturally unless
 *   corrupted, or are they deliberately maintained to extract value? This
 *   reading rejects both: markets are political choices, and the key insight
 *   is recognizing which choices benefit whom.
 *
 * KEY AGENTS:
 *   - Regulatory Coalition (institutional/arbitrage): Legislators, regulators, central banks, trade associations — design and maintain market rules; arbitrage across jurisdictions; primary beneficiary
 *   - Incumbent Market Participants (institutional/arbitrage): Established firms, financial incumbents, large corporations — navigate rules efficiently; capture regulatory processes; benefit from barriers to entry
 *   - Excluded Economic Actors (powerless/trapped): Informal laborers, unbanked populations, workers in non-credentialed fields, small producers below regulatory threshold — face insurmountable barriers to formalized market participation
 *   - Small Domestic Producers (moderate/constrained): Entrepreneurs, small businesses, artisans — experience mixed benefit/cost; genuine need for property/contract enforcement; high compliance burden
 *   - Alternative Economic Movement (organized/constrained): Cooperatives, gift economies, mutual aid networks, commons-based production — organized resistance; must operate within hostile rule frameworks; face suppression despite providing coordination functions
 *   - Academic Economics Establishment (institutional/arbitrage): Economists, business schools, policy researchers — institutional interest in treating markets as natural; theater ratio high; theoretical edifice contradicts structural evidence
 *   - Analytical Observer (analytical/analytical): Sees full structure across all perspectives; risks naturalizing contingent arrangements; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(engineered_infrastructure_reading, 0.52).
domain_priors:suppression_score(engineered_infrastructure_reading, 0.58).
domain_priors:theater_ratio(engineered_infrastructure_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(engineered_infrastructure_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(engineered_infrastructure_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(engineered_infrastructure_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(engineered_infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(engineered_infrastructure_reading, "Markets as State-Engineered Infrastructure (Contested Reading)").
narrative_ontology:topic_domain(engineered_infrastructure_reading, "political_economy/institutional_design/economic_ideology").

domain_priors:requires_active_enforcement(engineered_infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(engineered_infrastructure_reading, 'fe5be6b4-15dc-4928-96e2-1c71260f5fa3').
narrative_ontology:cs_created_at('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', '').
narrative_ontology:cs_kernel_codification('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', distributed).
narrative_ontology:cs_authority_grounding('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', lineage).
narrative_ontology:cs_interpretation_layer_present('fe5be6b4-15dc-4928-96e2-1c71260f5fa3').
narrative_ontology:cs_kernel_id(engineered_infrastructure_reading, market_as_natural_default).
narrative_ontology:cs_reading_relation('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', spontaneous_order_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', beneficiary_maintenance_reading, influences).
narrative_ontology:cs_axiom('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', foundational, markets_are_political_choices).
narrative_ontology:cs_axiom_status(markets_are_political_choices, holdable).
narrative_ontology:cs_axiom_grounding('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', markets_are_political_choices, conventional).
narrative_ontology:cs_axiom('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', foundational, deregulation_masks_reregulation).
narrative_ontology:cs_axiom_status(deregulation_masks_reregulation, holdable).
narrative_ontology:cs_axiom_grounding('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', deregulation_masks_reregulation, empirically_contingent).
narrative_ontology:cs_reference_frame('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', markets_as_engineered_systems).
narrative_ontology:cs_drift_state('fe5be6b4-15dc-4928-96e2-1c71260f5fa3', contemporary_financialization_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(engineered_infrastructure_reading, regulatory_coalition).
narrative_ontology:constraint_beneficiary(engineered_infrastructure_reading, incumbent_market_participants).
narrative_ontology:constraint_victim(engineered_infrastructure_reading, excluded_participants).
narrative_ontology:constraint_victim(engineered_infrastructure_reading, alternative_economic_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ECONOMIC ACTORS (SNARE) — Those locked out of formalized markets (informal laborers, unbanked populations, non-credentialed producers) experience market infrastructure as pure extraction. The state-engineered rules that define legitimate market participation become insurmountable barriers. No exit from the imposed formal system; complete suppression of alternative exchange mechanisms.
constraint_indexing:constraint_classification(engineered_infrastructure_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL DOMESTIC PRODUCERS (TANGLED ROPE) — Navigate regulatory compliance costs and licensing barriers (requires active enforcement), but also benefit from market infrastructure that enables scale, credit access, and contractual stability. Mixed experience: genuine coordination function (property law, contract enforcement, currency stability) layered with asymmetric extraction (compliance burden falls disproportionately on smaller actors).
constraint_indexing:constraint_classification(engineered_infrastructure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT MARKET PARTICIPANTS & REGULATORY COALITION (ROPE) — Institutional actors (established firms, financial regulators, central banks, trade associations) see market infrastructure as coordination mechanism. Their position: markets require rules, and they helped shape those rules. Net beneficiary through arbitrage — they can shop for favorable regulatory jurisdictions, influence rule changes, and navigate compliance efficiently. Experience the constraint as solving collective coordination problems.
constraint_indexing:constraint_classification(engineered_infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC ECONOMICS ESTABLISHMENT (PITON) — Largely treats markets as natural or spontaneous order phenomena; the engineered-infrastructure framing threatens disciplinary foundations. Theater ratio is high: extensive theoretical and empirical work on 'market efficiency' and 'deregulation' obscures the active state maintenance beneath the language. The academic discourse maintains a ritualized separation (market vs state) that the actual structural constraint violates. Piton classification: the theoretical edifice persists through institutional inertia despite structural evidence contradicting its core premises.
constraint_indexing:constraint_classification(engineered_infrastructure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE ECONOMIC MOVEMENT (TANGLED ROPE) — Organized resistance (cooperative economies, gift economies, mutual aid, commons-based production) experiences market infrastructure as both target and tool. They must operate within state-defined property and contract frameworks to build alternatives, but those frameworks are designed to preclude their expansion (suppression ≥ 0.58). Some coordination function exists at scale (alternative markets need some enforcement mechanism), but asymmetric extraction of movement energy into compliance with hostile rules.
constraint_indexing:constraint_classification(engineered_infrastructure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT CANDIDATE) — From maximum scope, one might argue that ALL economic systems require infrastructure maintenance, so 'engineering' is a feature of existence, not a choice. Markets are no more engineered than forests or families — they are natural equilibria that emerge from human coordination. This perspective treats the constraint as a mountain (unchangeable, inherent to economic systems as such). FALSE SUMMIT: the structural data contradicts this. Identifiable beneficiaries exist (regulatory coalition, incumbent participants). Rules change when political coalitions change. The 'natural' appearance is an effect of state maintenance, not evidence of immutability.
constraint_indexing:constraint_classification(engineered_infrastructure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(engineered_infrastructure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(engineered_infrastructure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(engineered_infrastructure_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(engineered_infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(engineered_infrastructure_reading, TR),
    TR >= 0.70.

:- end_tests(engineered_infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The engineered reading identifies moderate-high extraction from the regulatory coalition's ability to shape rules favoring incumbents. The measurement trajectory shows rising extractiveness from 0.38 (post-WWII consensus with stronger labor standards and financial regulation) to 0.52 (financialization peak with deregulation concentration), stabilizing at 0.50 post-crisis. This is not inevitable economic law — it reflects political coalition power. The reading treats this as contingent, not natural. Suppression (0.58): Significant barriers constrain alternatives: regulatory requirements for legitimate market participation, capital requirements enforced by state, legal prohibition of alternative payment systems, cultural/academic dismissal of non-market economies. But suppression is not total (perspective 5 shows organized resistance). Theater ratio (0.64): Rising from 0.55 to 0.68 as financial complexity obscures rule structures. Markets are presented as self-organizing, natural, and efficient; the active engineering is rendered invisible by complexity and dominant narrative. Disclosure pressure (policy, academic, movement critique) is lowering theater slightly (0.68→0.64) by making engineering visible. Claimed type (tangled_rope): Genuine coordination function (property law enables commerce, contract enforcement enables specialization) is inseparable from asymmetric extraction (regulatory design favors incumbents, barriers exclude alternatives). Requires active enforcement (securities regulators, property courts, labor inspectors maintain the rules). Cannot be classified as pure rope (no meaningful extraction) or pure snare (coordination function is real, not performative).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival variance. Beneficiary coalition (perspective 3) sees rope: markets solve coordination problems; rules are reasonable necessity. Excluded actors (perspective 1) see snare: barriers are total; no escape from imposed system; extraction is complete. Small producers (perspective 2) experience tangled rope: genuine need for infrastructure; high compliance cost. Alternative movement (perspective 5) experiences tangled rope differently: forced to operate within hostile rules; extracting their energy into compliance. Academic establishment (perspective 4) produces piton classification: theoretical edifice treating markets as natural persists despite contradictory evidence; theater ratio high. Analytical observer (perspective 6) risks false summit: treating market engineering as 'natural' because all systems require some structure. The gap reveals that the constraint's classification depends entirely on structural position — who controls rules vs who must comply.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Regulatory coalition (beneficiaries + arbitrage exit) derives low d (~0.15) → negative χ; they experience the constraint as enabling and low-cost. Excluded actors (victims + trapped exit) derive high d (~0.95) → high χ; they experience maximum extraction. Small producers (weak victim status + constrained exit) derive moderate d (~0.55) → moderate χ. Alternative movement (victims + constrained exit but organized power) derives d ~0.65 but with organized modifier reducing experienced χ from raw value. The academic establishment (no structural victim/beneficiary relationship, but institutional power + arbitrage) derives institutional canonical d (~0.00); however, the piton classification emerges from theater_ratio (≥0.70 gate) not from high χ. The analytical observer (analytical/analytical context) derives canonical d ~0.73 → moderate-high χ, placing them in moderate extraction experience despite their scope advantage. The reading treats d as contingent on rule configuration: change the rules, and d values shift.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by making visible that the choice between 'market is natural law' (mountain) and 'market is political artifact' (tangled_rope) is itself a political choice, not an empirical discovery. The false summit signature (mountain + beneficiaries) activates exactly because the analytical observer's perspective attempts to naturalize what the structural data shows is contingent. The mandatrophy resolution is: both readings are defensible within their respective authority frameworks, but the engineered-infrastructure reading makes visible what the natural-law reading obscures — the beneficiary coalition whose interests are served by treating engineering as nature. No single classification is 'correct'; the presheaf over the observation site includes all six types, and their distribution reveals structural asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the ''market'' a discovered natural equilibrium that must be continuously re-engineered to maintain, or is it a political invention continually reimagined by coalitions?',
    'Historical analysis of market rule changes correlated with political coalition shifts. Counterfactual: what market rules would exist if a different political coalition held power? Examine cases of rule reversal (e.g., antitrust enforcement cycles, financial regulation shifts post-crisis) for evidence of path-dependence vs natural law.',
    'If natural equilibrium repeatedly corrupted: beneficiaries (perspective 3) and analytical observer correct; classification approaches mountain. If political invention: engineered reading correct; classification is tangled_rope. The difference is whether rule changes follow political logic or economic logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Market as natural equilibrium vs political invention').

omega_variable(
    beneficiary_identification_boundary,
    'Who exactly counts as the ''regulatory coalition'' benefiting from market infrastructure? Is it a stable bloc or shifting coalition?',
    'Document which actors systematically lobby for which rule changes. Track campaign finance flows, regulatory agency revolving doors, trade association positioning. Identify counterfactual: would incumbent firms support current rules if they lost arbitrage power?',
    'If stable bloc with permanent interest: beneficiary classification is clean; extraction mechanism is stable. If shifting coalition: the constraint itself may oscillate between types as coalitions shift. Affects confidence in tangled_rope vs snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_boundary, empirical, 'Stability and composition of regulatory beneficiary coalition').

omega_variable(
    alternative_system_viability,
    'Could non-state-engineered economic coordination systems (peer-to-peer, commons-based, gift economies) operate at global scale without state infrastructure, or is state infrastructure genuinely necessary?',
    'Study existing alternative systems at varying scales (local mutual aid, cooperative networks, blockchain-based exchange). Identify failure modes: coordination problems that state infrastructure solves vs those it artificially creates. Examine whether state withdrawal increases or decreases transaction costs for participants.',
    'If state infrastructure is necessary: the constraint functions as coordination (rope, tangled_rope from some perspectives) with no genuine alternative. If state infrastructure artificially constrains alternatives: the constraint is more extractive (snare, piton degradation of alternative systems) than structural necessity justifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_viability, empirical, 'Necessity vs contingency of state-engineered market infrastructure').

omega_variable(
    deregulation_rhetorical_function,
    'Does ''deregulation'' rhetoric mask re-regulation favoring different beneficiaries, or does it accurately describe genuine reduction in market engineering?',
    'Detailed rule-change analysis of claimed ''deregulation'' episodes (1980s-2000s financial deregulation, labor regulation shifts). Count rule removals vs rule additions. Categorize removals: do they reduce state intervention or shift it to different enforcement mechanisms (market surveillance, private enforcement, soft law)? Examine which beneficiary coalition benefits from each specific removal.',
    'If deregulation masks re-regulation: the tangled_rope classification stands; suppression shifts rather than decreases; theater ratio remains high. If deregulation is genuine but creates market failures requiring re-intervention: the classification shifts toward temporary scaffold (rules emerge then collapse then re-emerge) rather than stable tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deregulation_rhetorical_function, empirical, 'Whether deregulation masks re-regulation to different beneficiaries').

omega_variable(
    suppression_mechanism_specificity,
    'Is suppression of alternative economic models (score: 0.58) primarily structural (regulatory prohibition) or behavioral (cultural marginalization, economic inability to compete)?',
    'Decompose suppression into legal barriers (explicit prohibitions), financial barriers (access to capital, credit, scale economies), and epistemic barriers (cultural dismissal, academic discounting). Measure: what happens to alternative systems if legal barriers are removed while financial/epistemic barriers remain?',
    'If structural suppression dominates: removing regulations should enable alternatives; the constraint is largely extractive. If behavioral/epistemic suppression dominates: legal removal is insufficient; the constraint is partly internalized ideology (identity_locked at systems level); requires different intervention logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_specificity, empirical, 'Suppression mechanism decomposition: structural vs behavioral').

omega_variable(
    false_summit_natural_law_test,
    'Does the analytical observer''s mountain classification rest on the premise that engineering is universal (all systems are engineered) and therefore not a meaningful distinction?',
    'Test whether the natural law claim survives counterfactual: if current market rules were replaced with radically different rules (e.g., gift-based allocation, reputation-based credit, commons property), would the mountain perspective still claim this is ''natural engineering'' or would it reveal that specific rule choices are contingent? Does the claim that markets are natural depend on observing THIS particular market architecture?',
    'If mountain claim is contingent on specific rules: false summit confirmed; beneficiaries exist; constraint is tangled_rope. If mountain claim holds across all possible market architectures: genuine natural law regarding economic coordination requirements (compression of choice space); mountain is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_test, conceptual, 'Whether natural law claim depends on observing specific market architecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(engineered_infrastructure_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_visible_regulation, engineered_infrastructure_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_t3_invisible_hand_narrative, engineered_infrastructure_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(theater_t6_complexity_obscures_rules, engineered_infrastructure_reading, theater_ratio, 6, 0.68).
narrative_ontology:measurement(theater_t9_disclosure_pressure, engineered_infrastructure_reading, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_post_war_consensus, engineered_infrastructure_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(extractiveness_t3_neoliberal_shift, engineered_infrastructure_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(extractiveness_t6_financialization_peak, engineered_infrastructure_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(extractiveness_t9_post_crisis_stabilization, engineered_infrastructure_reading, base_extractiveness, 9, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(engineered_infrastructure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(engineered_infrastructure_reading, spontaneous_order_reading).
narrative_ontology:affects_constraint(engineered_infrastructure_reading, beneficiary_maintenance_reading).
narrative_ontology:affects_constraint(engineered_infrastructure_reading, property_rule_enforcement).
narrative_ontology:affects_constraint(engineered_infrastructure_reading, contract_law_stability).
narrative_ontology:affects_constraint(engineered_infrastructure_reading, monetary_system_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family with two sibling readings. All three reading constraints (engineered_infrastructure_reading, spontaneous_order_reading, beneficiary_maintenance_reading) describe the same structural phenomenon — market rule-making — but from different authority framings. They should NOT be merged into one constraint with 'measurement basis' parameters. Each reading is a distinct constraint with its own ε, its own beneficiary/victim structure, and its own classification perspectives. The family is linked via network.affects_constraints. Downstream constraints (property_rule_enforcement, contract_law_stability, monetary_system_maintenance) are specific mechanisms instantiating whichever reading dominates — they are causally dependent on which reading's authority grounding is active.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
