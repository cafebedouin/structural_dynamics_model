% ============================================================================
% CONSTRAINT STORY: reserved_powers_amendments__tenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reserved_powers_amendments__tenth_amendment, []).

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
 *   constraint_id: reserved_powers_amendments__tenth_amendment
 *   human_readable: Tenth Amendment Reserved Powers Constraint
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Tenth Amendment reserves undelegated powers to the states and the
 *   people, functioning as the structural anchor of constitutional
 *   federalism. This constraint instantiates ONE READING of the contested
 *   kernel `reserved_powers_amendments`, specifically the tenth_amendment
 *   reading that centers state regulatory sovereignty and limits direct
 *   federal commandeering of state machinery. The sibling reading
 *   (ninth_amendment) emphasizes unenumerated rights retained by the people;
 *   the two readings coexist within contemporary constitutional doctrine but
 *   create structural tension when state-reserved powers conflict with
 *   fundamental unenumerated rights. The Tenth Amendment constraint exhibits
 *   the full range of DR classifications: state governments perceive
 *   coordination (Rope); federal uniform policy objectives perceive
 *   extraction (Snare); powerful large states perceive mixed
 *   coordination-extraction (Tangled Rope); intergovernmental reformers
 *   perceive a temporary arrangement with sunset potential (Scaffold); Tenth
 *   Amendment jurisprudence itself functions largely theatrically as an
 *   interpretation principle rather than an operative gate on federal power
 *   (Piton); and civilizational analysis risks naturalizing a contingent
 *   institutional settlement as immutable law (false-summit Mountain). The
 *   constraint's theater ratio (0.65) reflects that Tenth Amendment doctrine
 *   structures rhetorical debates and provides grounds for litigation, but
 *   actual federal-state power allocation is determined through political
 *   negotiation, conditional grants, and commerce clause interpretation
 *   rather than by the amendment's text. The extractiveness has increased
 *   over the interval (0.22 → 0.38) as federal administrative state has
 *   expanded relative to enumerated powers doctrine: conditional preemption
 *   and conditional grants have become primary mechanisms, subtly shifting
 *   the burden of policy implementation from federal to state machinery while
 *   federal standards are set nationally.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiary (institutional/arbitrage) — benefit from reserved regulatory powers, negotiating leverage in federal-state relations, control of local policy variation
 *   - Federal Uniform Policy Objectives: Primary victim (powerless/trapped) — abstract policy goals (clean air, civil rights, interstate commerce standards) trapped by state gatekeeping, unable to achieve coordinated implementation without negotiation or conditional incentives
 *   - Large States (CA, NY, TX): Powerful beneficiary (powerful/constrained) — benefit from reserved powers but also bear enforcement costs, coordinate interstate agreements, manage regulatory complexity; constrained by federal preemption doctrines and supremacy clause
 *   - Small States: Moderate beneficiary (moderate/constrained) — benefit from reserved powers and political influence (disproportionate in Senate), but face market pressures from larger states; constrained by free-rider incentives and regulatory arbitrage competition
 *   - National Uniform Schemes: Abstract victim (powerless/trapped) — environmental protection, civil rights enforcement, interstate commerce standardization; cannot organize politically or exit from state coordination barriers
 *   - Intergovernmental Reform Coalition: Organized actors (organized/mobile) — state legislatures, governors' associations, public interest law groups; see traditional Tenth Amendment federalism as obsolete; advocate for reformed boundaries with sunset clauses on emergency federal authority
 *   - Tenth Amendment Jurisprudence: Institutional actor (institutional/arbitrage) — Supreme Court doctrine, constitutional law scholarship; maintains the interpretive framework; derives benefit from continued relevance (career, legitimacy); functions largely theatrically given commerce clause expansion
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — sees federalism structure as architecturally necessary, risks naturalizing contingent historical arrangements as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reserved_powers_amendments__tenth_amendment, 0.38).
domain_priors:suppression_score(reserved_powers_amendments__tenth_amendment, 0.52).
domain_priors:theater_ratio(reserved_powers_amendments__tenth_amendment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reserved_powers_amendments__tenth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(reserved_powers_amendments__tenth_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reserved_powers_amendments__tenth_amendment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reserved_powers_amendments__tenth_amendment, tangled_rope).
narrative_ontology:human_readable(reserved_powers_amendments__tenth_amendment, "Tenth Amendment Reserved Powers Constraint").
narrative_ontology:topic_domain(reserved_powers_amendments__tenth_amendment, "political/legal/constitutional").

domain_priors:requires_active_enforcement(reserved_powers_amendments__tenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reserved_powers_amendments__tenth_amendment, '1af39cea-9fa2-4cf3-8b94-ca158280bc01').
narrative_ontology:cs_kernel_codification('1af39cea-9fa2-4cf3-8b94-ca158280bc01', fixed_text).
narrative_ontology:cs_authority_grounding('1af39cea-9fa2-4cf3-8b94-ca158280bc01', lineage).
narrative_ontology:cs_interpretation_layer_present('1af39cea-9fa2-4cf3-8b94-ca158280bc01').
narrative_ontology:cs_reading_relation('1af39cea-9fa2-4cf3-8b94-ca158280bc01', reserved_powers_amendments__ninth_amendment, coexists_with).
narrative_ontology:cs_axiom('1af39cea-9fa2-4cf3-8b94-ca158280bc01', foundational, enumerated_powers_constraint).
narrative_ontology:cs_axiom_status(enumerated_powers_constraint, holdable).
narrative_ontology:cs_axiom_grounding('1af39cea-9fa2-4cf3-8b94-ca158280bc01', enumerated_powers_constraint, conventional).
narrative_ontology:cs_axiom('1af39cea-9fa2-4cf3-8b94-ca158280bc01', foundational, anti_commandeering_principle).
narrative_ontology:cs_axiom_status(anti_commandeering_principle, holdable).
narrative_ontology:cs_axiom_grounding('1af39cea-9fa2-4cf3-8b94-ca158280bc01', anti_commandeering_principle, deontological).
narrative_ontology:cs_reference_frame('1af39cea-9fa2-4cf3-8b94-ca158280bc01', enumerated_federal_powers_with_state_residuum).
narrative_ontology:cs_drift_state('1af39cea-9fa2-4cf3-8b94-ca158280bc01', contemporary_post_wickard_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1af39cea-9fa2-4cf3-8b94-ca158280bc01', '').
narrative_ontology:cs_kernel_id(reserved_powers_amendments__tenth_amendment, reserved_powers_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reserved_powers_amendments__tenth_amendment, state_governments).
narrative_ontology:constraint_beneficiary(reserved_powers_amendments__tenth_amendment, regulatory_diversity).
narrative_ontology:constraint_victim(reserved_powers_amendments__tenth_amendment, uniform_national_policy).
narrative_ontology:constraint_victim(reserved_powers_amendments__tenth_amendment, federal_administrative_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIFORM FEDERAL SCHEME (SNARE) — National policy objectives (clean air, interstate commerce standards, civil rights enforcement) face structural barriers to uniform implementation. The Tenth Amendment constraint suppresses federal commandeering pathways, forcing reliance on incentives, conditional grants, or negotiated federalism. Trapped without exit: cannot bypass state gatekeeping; cannot organize politically as an abstract policy goal. Maximum extraction of the federal administrative objective toward state coordination costs.
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENTS (ROPE) — Benefit from reserved powers; experience the constraint as coordination mechanism protecting their regulatory sovereignty. Exit options exist (national preemption via commerce clause, supremacy clause workarounds); states with arbitrage capacity can negotiate favorable conditional grant terms. Experiences constraint as enabling legitimate local variation and political accountability, not as coercion. Net beneficiary with agency.
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE STATES (TANGLED ROPE) — Powerful states (CA, NY, TX) benefit from reserved powers but also bear enforcement costs: they must coordinate with federal agencies, negotiate interstate agreements, and manage regulatory complexity. They have constrained but real options (market exit via migration, regulatory arbitrage). Experience genuine coordination benefits (policy autonomy) mixed with extraction costs (federal unfunded mandates, compliance burdens). Chi balances genuine function with asymmetric cost distribution.
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERGOVERNMENTAL REFORM COALITION (SCAFFOLD) — Organized actors (National Governors Association, state legislative conferences, public interest law groups) see the Tenth Amendment as a temporary coordination mechanism now obsolete for interstate externalities and national crises. They advocate for conditional preemption, federal standard-setting with state implementation flexibility, and sunset clauses on emergency federal authority. Low effective extraction because organized agents perceive an exit path: reformed federalism with clearer boundaries and time limits.
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TENTH AMENDMENT JURISPRUDENCE (PITON) — The Tenth Amendment functions largely as a statement of structural principle rather than an operative enforcement mechanism. Federal courts rarely strike down federal legislation as exceeding enumerated powers (post-Wickard/Gonzales). The amendment's constraining power is theatrical: it grounds rhetoric in federalism debates, but actual coordination is achieved through political negotiation, conditional grants, and supremacy clause litigation. Theater ratio reflects that the doctrinal constraint is mostly symbolic while real allocation occurs elsewhere.
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, federalism itself (the structural division of sovereignty between unified and constituent units) is a necessary institutional form for large diverse polities. The Tenth Amendment merely codifies this immutable structural requirement: any large republic must distribute power to subunits or face collapse. The constraint appears unchangeable because its function is architecturally necessary. This perspective risks false summitry: what appears natural may be a constructed institutional arrangement serving identifiable beneficiaries (state political classes).
constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reserved_powers_amendments__tenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reserved_powers_amendments__tenth_amendment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reserved_powers_amendments__tenth_amendment, TR),
    TR >= 0.70.

:- end_tests(reserved_powers_amendments__tenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Tenth Amendment creates genuine coordination benefits for state governments (regulatory autonomy, political accountability), but also imposes extraction costs on uniform national policy objectives. The constraint forces federal actors to pay for state cooperation (through conditional grants, incentives, negotiation) rather than directly implementing policy. Base extractiveness is moderate because (1) the constraint has genuine coordination content (prevents federal overreach, preserves political diversity), and (2) the extraction of uniform-policy objectives is only partial — federal actors have workarounds (conditional grants, commerce clause expansion, agency regulation via private delegatees). Suppression (0.52): Moderate-high. Significant barriers exist to federal policy uniformity: the necessity of state legislative cooperation, anti-commandeering doctrine preventing direct conscription of state machinery, political diffusion of state power (small states have Senate representation leverage). But suppression is not total — federal preemption is available (commerce clause has expanded massively), conditional grants provide leverage, and some policy areas have achieved de facto federal dominance (civil rights via Fourteenth Amendment, environmental regulation via Clean Air Act). Theater ratio (0.65): Moderate-high. Tenth Amendment doctrine functions largely as interpretive rhetorical structure rather than operative gate. Post-Wickard (1942) and Gonzales v. Raich (2005), federal commerce power is effectively boundless; reserved powers doctrine provides grounds for litigation and debate but rarely blocks federal legislation. The constraint's actual force operates through political economy (state coalition power, conditional grants bargaining) rather than through doctrinal limits. Theater has increased over the interval as federal administrative state expanded: the amendment's constraining power has shifted from doctrinal gate to rhetorical anchor.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence between beneficiaries and victims. State governments (beneficiaries with arbitrage) experience the constraint as coordination protecting political autonomy: they see the Tenth Amendment as enabling legitimate policy variation and local democratic accountability. Federal uniform policy objectives (victims trapped without exit) experience the constraint as pure extraction: they must subsidize state cooperation through conditional grants, negotiate implementation, and accept regulatory variation that undermines national standards. Large states (powerful constrained beneficiaries) experience mixed effects: they benefit from regulatory autonomy but bear administrative burden and face federal preemption threats. The intergovernmental reform coalition (organized with mobile exit) experiences the constraint as temporary and renegotiable: they see it not as law but as a political settlement awaiting transformation. Tenth Amendment jurisprudence (institutional with arbitrage) experiences the constraint as rhetorical framework: maintains doctrine but acknowledges that real allocation is political. The analytical observer risks false summitry by naturalizing what is a constructed institutional settlement as an immutable architectural necessity. The perspectival gap reveals that the constraint's legitimacy depends entirely on the observer's structural position: beneficiaries invoke federalism principle; victims invoke commerce clause or Fourteenth Amendment counterprinciples; observers with analytical distance see the whole system as contingent and renegotiable.
 *
 * DIRECTIONALITY LOGIC:
 *   The Tenth Amendment constraint distributes directionality differentially across agents based on their structural position. State governments are beneficiaries with arbitrage capacity (d ≈ 0.15): they benefit from reserved powers and can negotiate favorable federal-state terms. Their exit option (arbitrage) is genuine — they can pressure Congress, shape conditional grants bargaining, or use regulatory variation as market-making. Federal uniform policy objectives are victims without exit (d ≈ 0.95): they face structural barriers (state gatekeeping, anti-commandeering, federalism rhetoric) and cannot organize politically or bypass state coordination. Powerful large states are constrained beneficiaries (d ≈ 0.40): they benefit from reserved powers but face enforcement costs and federal preemption threats; their exit is constrained but real (regulatory arbitrage, market power). Small states are moderate beneficiaries (d ≈ 0.20): benefit from reserved powers and disproportionate Senate influence, but structurally limited by market pressures and free-rider incentives. The intergovernmental reform coalition is organized with mobile exit (d ≈ 0.50): they see traditional federalism as negotiable, advocate for reformed boundaries, and have capacity to shift the institutional structure through coordinated state action. Tenth Amendment jurisprudence is institutional with arbitrage capacity (d ≈ 0.10): derives legitimacy from maintaining constitutional doctrine; has exit option through reinterpretation (commerce clause expansion demonstrates this capacity). Analytical observer uses canonical directionality for analytical power (d ≈ 0.72): sees the constraint structure but risks naturalizing contingent arrangement as necessary law.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tenth Amendment constraint resolves the mandatrophy by clarifying that the classification depends on temporal horizon and exit options. At immediate/biographical timescale, state governments perceive genuine coordination (Rope) enabling policy autonomy. Federal policy objectives perceive snare-level extraction. At generational timescale, the constraint exhibits degradation (Piton): Tenth Amendment doctrine has become increasingly theatrical as commerce clause interpretation has expanded federal power; the amendment provides rhetorical grounding for federalism debates but rarely blocks federal legislation. At civilizational timescale, the analytical observer perceives the constraint as architecturally necessary (Mountain), but this risks false summitry — the measurement data shows rising theater ratio and increasing extractiveness, suggesting that what appears natural is actually a degrading institutional arrangement serving state political classes. The mandatrophy is resolved by acknowledging that no single type captures the full structure: the constraint is genuinely coordinative (state autonomy is real), genuinely extractive (federal policy uniformity is suppressed), genuinely theatrical (doctrine outpaced by practice), and potentially contingent (federalism could be restructured through political reform). The presheaf of perspectives is the accurate representation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_reach_boundary,
    'What determines the boundary between delegated federal powers and reserved state powers? Is it fixed textually (enumerated powers) or fluid (expansible via interstate commerce and necessary-and-proper doctrines)?',
    'Longitudinal analysis of Supreme Court interpretation: frequency of preemption rulings; scope of commerce clause doctrine; correlation between economic integration and federal authority claims',
    'If boundary is fixed: Tenth Amendment is a genuine structural constraint (Mountain). If boundary is fluid: Tenth Amendment is a negotiable institutional arrangement (Tangled Rope or lower). Current jurisprudence shows fluid boundary (Wickard, Gonzales); claims of structural fixity appear false-summitry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_reach_boundary, conceptual, 'Textual fixity vs. fluid interpretation of delegated/reserved boundary').

omega_variable(
    commandeering_doctrine_alternative,
    'Does the anti-commandeering doctrine (barring direct federal conscription of state administrative machinery) represent a genuine coordination principle or a contingent political compromise that emerges and recedes with political alignments?',
    'Historical analysis of commandeering doctrine adoption (Garcia 1985, New York v. United States 1992, Printz v. United States 1997); correlation with state political power and federal revenue incentives; tracking of doctrine application across administrations with different federalism commitments',
    'If genuine principle: beneficiary structure is stable and justified. If contingent: beneficiary structure shifts with political conditions; constraint''s extractiveness becomes sensitive to federal fiscal capacity and state coalition power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commandeering_doctrine_alternative, empirical, 'Whether anti-commandeering is principled or contingent political arrangement').

omega_variable(
    conditional_preemption_sufficiency,
    'Can conditional federal grants (conditional on state adoption of federal standards) achieve uniform national policy objectives while preserving Tenth Amendment reserved powers? Or does conditioning render the reserve illusory by making state ''choice'' economically coercive?',
    'Comparative policy analysis across domains (environmental, labor, welfare): tracking of state adoption rates under conditional grants; measurement of opt-out capacity (whether states can refuse without fiscal catastrophe); identification of threshold spending levels where conditioning becomes de facto commandeering',
    'If conditioning preserves reserve: Tenth Amendment remains structurally operative (Rope/Tangled Rope). If conditioning renders reserve illusory: constraint is degraded (Piton) or false-summitry is exposed (Mountain claims are violated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_preemption_sufficiency, empirical, 'Whether conditional grants preserve or nullify reserved powers').

omega_variable(
    reading_contest_ninth_tenth,
    'Does the Tenth Amendment''s reservation of powers to the states foreclose the Ninth Amendment''s declaration of unenumerated rights retained by the people? Or can both coexist?',
    'Constitutional interpretation: analysis of whether state-reserved powers include authority to restrict unenumerated rights; comparison of judicial doctrine treating fundamental rights (Fourteenth Amendment due process) against state regulatory power; tracking of cases where Ninth and Tenth amendments pull in opposite directions',
    'If forecloses: only one reading can be operative; fundamental rights doctrine must prioritize either unenumerated rights (Ninth) or state autonomy (Tenth). If coexists: both readings remain live; tension is managed through balancing doctrines (rational basis, strict scrutiny) rather than logical exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_ninth_tenth, conceptual, 'Logical relationship between Tenth Amendment (state powers) and Ninth Amendment (unenumerated rights)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reserved_powers_amendments__tenth_amendment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rese_tr_t0, reserved_powers_amendments__tenth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rese_tr_t50, reserved_powers_amendments__tenth_amendment, theater_ratio, 50, 0.55).
narrative_ontology:measurement(rese_tr_t100, reserved_powers_amendments__tenth_amendment, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(rese_be_t0, reserved_powers_amendments__tenth_amendment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rese_be_t50, reserved_powers_amendments__tenth_amendment, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(rese_be_t100, reserved_powers_amendments__tenth_amendment, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(rese_su_t0, reserved_powers_amendments__tenth_amendment, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rese_su_t50, reserved_powers_amendments__tenth_amendment, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(rese_su_t100, reserved_powers_amendments__tenth_amendment, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reserved_powers_amendments__tenth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(reserved_powers_amendments__tenth_amendment, reserved_powers_amendments__ninth_amendment).
narrative_ontology:affects_constraint(reserved_powers_amendments__tenth_amendment, commerce_clause_supremacy).
narrative_ontology:affects_constraint(reserved_powers_amendments__tenth_amendment, anti_commandeering_doctrine).

% DUAL FORMULATION NOTE:
% The Tenth Amendment constraint is one reading (tenth_amendment) of the contested kernel reserved_powers_amendments. The sibling reading ninth_amendment focuses on unenumerated individual rights rather than reserved governmental powers, creating a structural distinction in beneficiary/victim sets and extractiveness. The two readings coexist in constitutional doctrine — both are operatively live — but create tension when state-reserved regulatory powers (Tenth reading) conflict with unenumerated fundamental rights (Ninth reading). Each reading gets its own constraint story with its own ε, beneficiary/victim structure, and measurement trajectory. They are linked via network.affects_constraints to indicate constitutional-doctrinal coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reserved_powers_amendments__tenth_amendment, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
