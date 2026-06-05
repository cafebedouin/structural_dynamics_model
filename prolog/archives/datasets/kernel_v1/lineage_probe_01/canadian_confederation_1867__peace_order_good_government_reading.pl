% ============================================================================
% CONSTRAINT STORY: canadian_confederation_1867__peace_order_good_government_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_confederation_1867__peace_order_good_government_reading, []).

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
 *   constraint_id: canadian_confederation_1867__peace_order_good_government_reading
 *   human_readable: POGG Centralization (1867 Act Peace Order Good Government Reading)
 *   domain: constitutional/legal/federalism
 *
 * SUMMARY:
 *   The 1867 British North America Act established the Canadian federation
 *   explicitly as a counter to American secession. The framers, having
 *   witnessed the American Civil War (1861-1865), designed a centralized
 *   dominion structure with three interlocking mechanisms: the Peace, Order,
 *   and Good Government clause (POGG) as a residuary power for the central
 *   government; the federal disallowance power (allowing Ottawa to veto
 *   provincial legislation); and an exhaustive enumeration of provincial
 *   powers leaving all unallocated authority to the centre. This reading
 *   instantiates the constitutional text as written: a designed asymmetry in
 *   which provincial legislatures are trapped in enumerated competencies
 *   while the centre retains open-ended residuary authority. The constraint
 *   is a tangled_rope because the 1867 design genuinely coordinated a
 *   multinational federation (addressing the coordination problem of uniting
 *   British North America) while simultaneously extracting provincial
 *   autonomy for the benefit of central dominion power. This reading contests
 *   the 'notwithstanding clause' reading (which emphasizes parliamentary
 *   sovereignty preserved in s.33) and the 'patriation 1982' reading (which
 *   treats the constitution as completed by patriation over Quebec's
 *   dissent). The POGG reading focuses on the founding moment's structural
 *   choice: centralization by design, suppression of state-sovereignty
 *   federalism via enumerated-residue architecture, and the Privy Council's
 *   subsequent rebalancing (1897-1926) as evidence that the original
 *   constraint was indeed extractive enough to require juridical correction.
 *
 * KEY AGENTS:
 *   - Central Government Dominion: Primary beneficiary (institutional/arbitrage). Designed to hold POGG, disallowance, and residue. Experiences the constraint as coordination — managing a federation, directing national policy. Extraction runs entirely toward the centre.
 *   - Provincial Legislatures: Primary victims (powerless/trapped until 1926). Bound by enumerated powers; subject to disallowance; trapped in the text. Cannot exit the federation or amend the constitution unilaterally. Bear maximum extraction from the constraint.
 *   - British Privy Council (1867-1926): Secondary institutional actor (institutional/constrained). Bound by the text they interpret but positioned to rebalance it. Their jurisprudence gradually shifts power downward to provinces, signaling that the original tilt was extractive.
 *   - State Sovereignty Federalism (Victim Concept): Abstract victim. The POGG reading suppresses any federalism model based on coordinate state sovereignty (as in the U.S. model). This reading prefers delegated-powers federalism from a supreme centre.
 *   - Rebalancing Coalition (1897-1926): Organized actors (organized/mobile). Privy Council judges, federal-provincial conference participants, legal scholars, regional premiers gradually displace the original tilt through interpretive rebalancing.
 *   - Analytical Observer: Civilizational (analytical/analytical). Risks naturalizing the 1867 design as inevitable federalism rather than contingent institutional choice designed against secession.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_confederation_1867__peace_order_good_government_reading, 0.58).
domain_priors:suppression_score(canadian_confederation_1867__peace_order_good_government_reading, 0.72).
domain_priors:theater_ratio(canadian_confederation_1867__peace_order_good_government_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_confederation_1867__peace_order_good_government_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(canadian_confederation_1867__peace_order_good_government_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(canadian_confederation_1867__peace_order_good_government_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_confederation_1867__peace_order_good_government_reading, tangled_rope).
narrative_ontology:human_readable(canadian_confederation_1867__peace_order_good_government_reading, "POGG Centralization (1867 Act Peace Order Good Government Reading)").
narrative_ontology:topic_domain(canadian_confederation_1867__peace_order_good_government_reading, "constitutional/legal/federalism").

domain_priors:requires_active_enforcement(canadian_confederation_1867__peace_order_good_government_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(canadian_confederation_1867__peace_order_good_government_reading, 'a6ed0894-92cc-4324-8ffd-0872f7667a19').
narrative_ontology:cs_kernel_codification('a6ed0894-92cc-4324-8ffd-0872f7667a19', formalized).
narrative_ontology:cs_authority_grounding('a6ed0894-92cc-4324-8ffd-0872f7667a19', extraction).
narrative_ontology:cs_interpretation_layer_present('a6ed0894-92cc-4324-8ffd-0872f7667a19').
narrative_ontology:cs_reading_relation('a6ed0894-92cc-4324-8ffd-0872f7667a19', canadian_confederation_1867__notwithstanding_clause_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6ed0894-92cc-4324-8ffd-0872f7667a19', canadian_confederation_1867__patriation_1982_reading, influences).
narrative_ontology:cs_axiom('a6ed0894-92cc-4324-8ffd-0872f7667a19', foundational, centralized_dominion_supremacy).
narrative_ontology:cs_axiom_status(centralized_dominion_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a6ed0894-92cc-4324-8ffd-0872f7667a19', centralized_dominion_supremacy, conventional).
narrative_ontology:cs_axiom('a6ed0894-92cc-4324-8ffd-0872f7667a19', foundational, residuary_authority_to_centre).
narrative_ontology:cs_axiom_status(residuary_authority_to_centre, overridden).
narrative_ontology:cs_axiom_grounding('a6ed0894-92cc-4324-8ffd-0872f7667a19', residuary_authority_to_centre, empirically_contingent).
narrative_ontology:cs_reference_frame('a6ed0894-92cc-4324-8ffd-0872f7667a19', centralized_dominion_federation_1867).
narrative_ontology:cs_drift_state('a6ed0894-92cc-4324-8ffd-0872f7667a19', post_privy_council_rebalancing_1926, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a6ed0894-92cc-4324-8ffd-0872f7667a19', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(canadian_confederation_1867__peace_order_good_government_reading, canadian_confederation_1867).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_confederation_1867__peace_order_good_government_reading, central_government_dominion).
narrative_ontology:constraint_victim(canadian_confederation_1867__peace_order_good_government_reading, provincial_legislatures).
narrative_ontology:constraint_victim(canadian_confederation_1867__peace_order_good_government_reading, state_sovereignty_federalism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL LEGISLATURE POST-1867 (SNARE) — Bound by the constitutional text (POGG, disallowance power, residuary clause all favor the centre). Provinces cannot exit the federation; cannot amend the constitution unilaterally; face disallowance of legislation touching interprovincial matters, trade, or anything claimed under POGG. The extraction is maximum: the constraint is baked into the founding document and enforced through judicial review + executive disallowance. No alternatives available within the framework.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BRITISH PRIVY COUNCIL (TANGLED ROPE) — Constrained by the constitutional text they must interpret, but also benefits from the interpretive role granted to them: the text is ambiguous enough to permit rebalancing toward provincial autonomy. The Privy Council experiences the constraint as both coordination (they must resolve federal disputes) and extraction (they are locked into the role of final arbiter, cannot escape the disputes). Their later jurisprudence (1926 onward) shows them rebalancing power downward to provinces — signaling that even the Privy Council felt the original tilt was extractive.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT DOMINION (ROPE) — Primary beneficiary. POGG gives the centre a residuary clause that swallows all unallocated powers; disallowance gives the centre veto over provincial legislation; the 1867 design hands primacy to Ottawa by structural intent. The centre experiences the constraint as pure coordination: managing a federation, resolving disputes, directing national policy. Extraction runs entirely toward the centre — they have exit options (the constitution is theirs to interpret via their appointed Privy Council judges) and benefits are asymmetric.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REBALANCING COALITION (SCAFFOLD) — Organized actors (Privy Council judges, federal-provincial conference participants, legal scholars, regional premiers) gradually displaced the original 1867 tilt through interpretive rebalancing and doctrinal innovation. By 1926 (Balfour Declaration), the constraint had been substantially restructured: provinces gained de facto primacy over property and civil rights, POGG was narrowed to emergencies, disallowance fell into disuse. This perspective sees the 1867 constraint as a scaffold — temporary architectural support for Dominion centrality — with a sunset in judicial practice and convention (1897-1926 period). The coalition had exit options (reinterpretation, disuse of disallowance) and could reshape the framework.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: 1867 TEXT AS PERFORMATIVE ARTIFACT (PITON) — From the post-1982 patriation view, the 1867 centralization text is sustained through theatrical deference rather than functional application. Courts cite POGG but interpret it narrowly (emergency doctrine only). Disallowance is historically invoked but never used post-1939. The residuary clause is formally the centre's but provincially-jealous jurisprudence has gutted it. The 1867 centralizing architecture persists in the text as a legitimating artifact, not a functioning constraint — theater_ratio high, actual exercise of the original powers minimal. The Privy Council's rebalancing (1926-1982) transformed the text into performance.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL FEDERALISM / FALSE SUMMIT CANDIDATE (MOUNTAIN) — From a civilizational/universal perspective, the tension between central and provincial power is an inherent structural feature of federated governance: any system must distribute sovereignty, and the choice is always contested. This reading naturalizes the 1867 design as reflecting inevitable federalism principles. However, the structural data contradicts the mountain classification: the 1867 Act beneficiaries (central government), victims (provincial legislatures), and suppression mechanisms (constitutional text + disallowance + POGG + residue to Ottawa) all point to a contingent institutional design, not a natural law. The engine's false summit detector will flag this as naturalization of a constructed power asymmetry.
constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_confederation_1867__peace_order_good_government_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_confederation_1867__peace_order_good_government_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_confederation_1867__peace_order_good_government_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_confederation_1867__peace_order_good_government_reading, TR),
    TR >= 0.70.

:- end_tests(canadian_confederation_1867__peace_order_good_government_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.58 (declining from 0.72 in 1867 to 0.42 by 1926). The POGG reading instantiates the constraint as written in 1867: the centre benefits structurally from residuary authority, disallowance, and open-ended POGG. But the measurement trajectory shows that the Privy Council's interpretive rebalancing reduced the centre's effective extractiveness over 59 years. The residue value (0.58 at the contemporary measurement point, 1926) reflects a hybrid state: the text remains centralist, but judicial practice has narrowed POGG to emergencies, disallowance has fallen into disuse, and property-and-civil-rights jurisprudence has given provinces substantive autonomy. Suppression: 0.72 (declining from 0.85 to 0.55). The 1867 constraint suppresses alternative federalism models (state-sovereignty federalism, coordinate federalism) through constitutional text and the threat of disallowance and POGG invocation. Suppression is highest at 1867 when disallowance is used and POGG is broadly interpreted. By 1926, disuse of disallowance and narrow POGG interpretation have lowered the effective suppression — provinces have carved out de facto autonomy. Theater ratio: 0.35 (rising from 0.08 to 0.35). At 1867, the constraint is almost entirely functional: POGG and disallowance are genuinely enforceable and frequently invoked. By 1926, the constraint becomes increasingly performative: the text still reads as centralizing, but courts interpret it narrowly and disallowance is obsolete by convention. The rising theater reflects the gradual transformation of the 1867 mechanism into a largely symbolic structure.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the central government (Rope perspective) and provincial legislatures (Snare perspective). Both experience the same textual constraint, but their structural positions produce opposite classifications. The centre experiences POGG, disallowance, and residue as coordination mechanisms for managing a multinational federation — they see efficiency gains and policy capacity (Rope). Provinces experience the same mechanisms as extraction — loss of autonomy, legal subordination, loss of exit options (Snare). The Privy Council perspective (Tangled Rope) is intermediate: they must coordinate federal disputes but are themselves trapped in an interpretive role they cannot fully escape. The scaffold perspective (1897-1926 rebalancing coalition) sees the original constraint as temporary — a structural support system being gradually dismantled through interpretive rebalancing and disuse of disallowance. The piton perspective (1982+) observes that the 1867 centralizing text persists as a performative artifact: it is cited for legitimacy but rarely applied in its original form. The analytical observer at the civilizational scale risks seeing an immutable federalism principle (Mountain) when the historical record shows a contingent design being deliberately rebalanced by the Privy Council and later by convention.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d) varies by perspective. The central government (institutional/arbitrage beneficiary) derives d ≈ 0.10-0.15 (low d, negative f(d)) because they experience benefit and have exit options (they can choose not to invoke disallowance, not to interpret POGG broadly). Provincial legislatures (powerless/trapped victims) derive d ≈ 0.95 (high d, f(d) ≈ 1.42) because they have no exit and bear the constraint's full cost. The Privy Council (institutional/constrained) derives d ≈ 0.55-0.65 (moderate d) because they are bound by the text but can reinterpret it — they are neither pure beneficiaries nor pure victims, but interpreters with limited agency within the framework. The engine's chi calculation scales extractiveness by these directionality values: chi = 0.58 × f(d) × σ(national) ≈ 0.58 × 0.65 × 1.0 ≈ 0.38 for the moderately-positioned Privy Council, but chi = 0.58 × 1.42 × 1.0 ≈ 0.82 for trapped provincial legislatures at the 1867 moment. The perspectival gap emerges: the beneficiary sees Rope (low chi), the victim sees Snare (high chi), the interpreter sees Tangled Rope (mixed chi), and the civilizational observer risks seeing Mountain (naturalization).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint resolves mandatrophy by clarifying that the 1867 Act is a contested kernel with multiple structurally distinct readings. The POGG reading instantiates one committed interpretation: centralization by design, suppression of state-sovereignty federalism, extraction of provincial autonomy for the benefit of the centre. This reading is the historical design moment. The notwithstanding clause reading (s.33) emphasizes parliamentary sovereignty preserved despite the centralizing text. The patriation 1982 reading treats the constitution as completed by patriation, potentially displacing the 1867 centralization logic. These are not perspectival variations on one constraint — they are three different readings of a contested kernel, each instantiating different foundational axioms about constitutional authority. The POGG reading asserts that the 1867 text is the supreme law binding provinces; the notwithstanding clause reading asserts that legislatures retain fundamental sovereignty despite Charter constraints; the patriation reading asserts that the 1982 patriation and Charter completion supersede the 1867 framework. These axioms coexist in different institutional traditions: the POGG reading dominates in early Canadian jurisprudence (1867-1926), the patriation reading dominates in contemporary constitutional law (1982+), and the notwithstanding clause reading represents the parliamentary sovereignty tradition running through both. The mandatrophy is resolved by recognizing that no single type is 'correct' — rather, different readings correspond to different constitutional moments and different authorized interpreters (1867 = founding fathers' intent; 1926 = Privy Council rebalancing; 1982 = patriation completion). The POGG reading's tangled_rope classification is stable for the 1867-1926 period; the scaffold classification of the rebalancing process (1897-1926) shows how the constraint evolved; and the piton classification (1982+) reflects that the original centralizing text has become performative in the post-patriation context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pogg_scope_semantics,
    'What is the semantic scope of ''Peace, Order, and Good Government''? Is POGG a residuary clause (all powers not allocated to provinces) or a narrowly-confined emergency power?',
    'Constitutional interpretation via case law evolution: Privy Council cases 1867-1926 vs post-1926 jurisprudence. Analyze which powers the courts attributed to POGG in early period vs late period.',
    'If residuary: extractiveness remains high (centre has unlimited reserve power). If emergency-only: extractiveness drops to 0.35-0.40 (centre''s residuary ambition is forestalled). Classification remains tangled_rope either way but chi values differ materially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pogg_scope_semantics, empirical, 'Semantic scope of POGG residuary power').

omega_variable(
    disallowance_enforcement_credibility,
    'Was disallowance power ever genuinely enforceable, or was it a structural threat rather than a functional mechanism?',
    'Historical record: count disallowance exercises post-1867; identify the last use; analyze why disallowance fell into disuse despite textual availability. Distinguish structural power (capacity to exercise) from effective power (willingness + capacity).',
    'If structurally credible but underused: suppression remains high (0.72) — the threat is the suppressive mechanism. If never credible: suppression should be lower (0.50-0.60) — the text is performative from the start. Current assessment assumes threat credibility; resolution would refine suppression metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disallowance_enforcement_credibility, empirical, 'Whether disallowance power was functionally enforceable or merely textual').

omega_variable(
    id_1867_design_intent_vs_outcome,
    'Did the 1867 framers genuinely intend POGG + disallowance + residue to create lasting central supremacy, or was the tilt a negotiation artifact expected to be rebalanced?',
    'Historical analysis of Confederation debates, Fathers of Confederation statements, and contemporaneous legal commentary. Cross-reference with whether any framers anticipated judicial rebalancing.',
    'If design-intent was supreme centre: the 1867 constraint is honestly extractive (what it was built to do). If tilt was negotiation artifact: the constraint is more properly scaffold (temporary support structure) and the Privy Council rebalancing (1897-1926) was reading the framers'' reserve into the text. Affects classification boundary between tangled_rope (current) and scaffold (alternative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_1867_design_intent_vs_outcome, empirical, 'Design intent of 1867 centralization mechanisms').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the contested 1867 Act kernel. The three sibling readings (POGG, notwithstanding_clause, patriation_1982) instantiate different fundamental commitments about what the constitutional text authorizes. Which reading captures the founding''s actual normative structure?',
    'Jurisprudential analysis: track which axioms each reading privileges. Examine whether the axiomatic commitments are logically foreclosed by each other or merely coexist in different institutional traditions. Test via counterfactual: could the 1867 framers have intended simultaneous support for all three readings?',
    'If readings foreclose each other: the kernel admits only one reading per constitutional moment (1867 vs 1982). If readings coexist: the kernel is genuinely ambiguous and different parties can hold different readings simultaneously. Current analysis assumes coexistence with later rebalancing; if foreclosure is discovered, the constitutional evolution was discontinuous (1867 POGG vs 1982 patriation are different constitutions, not rebalancings of one).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical structure of contest between 1867 POGG and later constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_confederation_1867__peace_order_good_government_reading, 0, 115).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pogg_theater_1867, canadian_confederation_1867__peace_order_good_government_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(pogg_theater_1897, canadian_confederation_1867__peace_order_good_government_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(pogg_theater_1926, canadian_confederation_1867__peace_order_good_government_reading, theater_ratio, 59, 0.35).

% Extraction over time
narrative_ontology:measurement(pogg_extractiveness_1867, canadian_confederation_1867__peace_order_good_government_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(pogg_extractiveness_1897, canadian_confederation_1867__peace_order_good_government_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(pogg_extractiveness_1926, canadian_confederation_1867__peace_order_good_government_reading, base_extractiveness, 59, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pogg_suppression_1867, canadian_confederation_1867__peace_order_good_government_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(pogg_suppression_1897, canadian_confederation_1867__peace_order_good_government_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(pogg_suppression_1926, canadian_confederation_1867__peace_order_good_government_reading, suppression_requirement, 59, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_confederation_1867__peace_order_good_government_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(canadian_confederation_1867__peace_order_good_government_reading, canadian_confederation_1867__notwithstanding_clause_reading).
narrative_ontology:affects_constraint(canadian_confederation_1867__peace_order_good_government_reading, canadian_confederation_1867__patriation_1982_reading).

% DUAL FORMULATION NOTE:
% The 1867 Act is a contested kernel with three structurally distinct readings. The POGG reading instantiates the founding moment's commitment to centralized dominion structure. The notwithstanding clause reading emphasizes parliamentary sovereignty preserved via s.33. The patriation reading treats 1982 as constitutional completion. All three readings affect the same legal text but instantiate different foundational axioms about constitutional authority. Each reading is a separate constraint story (three files total), linked via network.affects_constraints to show structural interdependence. The POGG reading's tangled_rope classification (0.58 extractiveness, 0.72 suppression, requires_active_enforcement) reflects the 1867 design; the measurement trajectory shows Privy Council rebalancing reducing extractiveness and suppression by 1926. The rebalancing itself is documented through the scaffold perspective (organized actors 1897-1926 gradually displacing the original tilt). The piton perspective (1982+) reflects that the POGG centralizing text persists as performance rather than function in the post-patriation context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
