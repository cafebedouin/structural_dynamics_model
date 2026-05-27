% ============================================================================
% CONSTRAINT STORY: partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_partition_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: partition_reading
 *   human_readable: Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   UN Resolution 181 (1948) partitioned the British Mandate territory into
 *   Jewish and Arab states, establishing the partition reading as the
 *   legitimacy ground for territorial claims in the Levant. This constraint
 *   is ONE reading of the contested kernel of territorial legitimacy. The
 *   kernel has three major sibling readings: (1) security_necessity_reading —
 *   legitimacy derives from military necessity and defensible borders, not
 *   partition boundaries; (2) indigenous_continuity_reading — legitimacy
 *   derives from historical demographic and cultural continuity, not
 *   international partition mechanics. This story instantiates ONLY
 *   partition_reading: both Israeli and Palestinian states are legitimate
 *   within recognized borders; settlements beyond 1967 lines are illegitimate
 *   by definition; two-state solution is structurally possible. The
 *   constraint exhibits high theater ratio (0.68) because the partition
 *   apparatus (UN Trusteeship, Mandate system) has atrophied while the
 *   partition boundary persists through institutional inertia and legal
 *   doctrine. The extractiveness trajectory (0.35 → 0.58 over 75 years)
 *   reflects accumulating displacement, settlement extraction, and the gap
 *   between partition legitimacy claims and actual security/coordination
 *   outcomes. The constraint is Tangled Rope because it performs genuine
 *   coordination (establishes recognized borders enabling diplomacy) while
 *   extracting through displacement and the foreclosure of non-aligned
 *   territorial claims.
 *
 * KEY AGENTS:
 *   - Displaced Palestinian Refugees: Primary victims (powerless/trapped) — legally foreclosed from territorial return and property restitution by the partition boundary
 *   - Non-Partition Territorial Claimants: Secondary victims (powerless/trapped) — historical or indigenous claims that fall outside the partition boundaries are rendered illegitimate by partition reading
 *   - Israeli and Palestinian States: Beneficiaries and secondary victims (moderate/constrained) — gain sovereign territory and recognition within partition; constrained by inability to revise boundary; bear enforcement and compliance costs
 *   - UN and Post-Colonial International Order: Primary beneficiaries (institutional/arbitrage) — partition model provides coordination solution and legal framework for state recognition; architects of the legitimacy reading
 *   - Settlement Movement and Territorial Maximalists: Organized extractors (organized/constrained) — benefit from state recognition within partition while extracting through territorial expansion; suppressed by international law prohibition on boundary revision
 *   - Partition Implementation Machinery: Institutional actor in decay (institutional/arbitrage) — UN Trusteeship, Mandate system, colonial administration have atrophied; partition boundary persists through legal doctrine and institutional inertia (piton trajectory)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(partition_reading, 0.58).
domain_priors:suppression_score(partition_reading, 0.62).
domain_priors:theater_ratio(partition_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(partition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(partition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(partition_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(partition_reading, tangled_rope).
narrative_ontology:human_readable(partition_reading, "Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)").
narrative_ontology:topic_domain(partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(partition_reading, 'e24dbdb3-cfa2-4b2c-bd58-abdd4c1e215a').
narrative_ontology:cs_created_at('e24dbdb3-cfa2-4b2c-bd58-abdd4c1e215a', '').
narrative_ontology:cs_kernel_codification('e24dbdb3-cfa2-4b2c-bd58-abdd4c1e215a', fixed_text).
narrative_ontology:cs_authority_grounding('e24dbdb3-cfa2-4b2c-bd58-abdd4c1e215a', lineage).
narrative_ontology:cs_interpretation_layer_present('e24dbdb3-cfa2-4b2c-bd58-abdd4c1e215a').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(partition_reading, partition_framework_architects).
narrative_ontology:constraint_beneficiary(partition_reading, states_recognized_within_partition_boundaries).
narrative_ontology:constraint_victim(partition_reading, displaced_populations).
narrative_ontology:constraint_victim(partition_reading, territorial_claimants_outside_partition_boundaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED POPULATIONS (SNARE) — Trapped by the partition boundary which legally forecloses return and property restitution. The constraint extracts displacement costs (refugee status, family separation, property loss) without offering exit. Maximum suppression: the partition framework is enforced through legal doctrine and state military power; no alternative framework for claim-making is available within the partition reading.
constraint_indexing:constraint_classification(partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TERRITORIAL CLAIMANTS OUTSIDE PARTITION (SNARE) — Groups with historical territorial claims that do not align with the partition boundary are legally excluded. The partition reading forecloses their claims by design: the boundary is presented as the legitimate ground of territorial right, making non-aligned claims ipso facto illegitimate. Trapped at generational scale — the partition becomes settled international law, and challenging it requires rewriting the foundation.
constraint_indexing:constraint_classification(partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE ACTORS WITHIN PARTITION (TANGLED ROPE) — Both Israeli and Palestinian states are legitimate within the partition boundaries; both also experience extraction through the enforcement costs and the constraint that their territorial rights are bounded. Constrained by international law that permits no boundary revision without consensus. Benefits from state recognition and sovereign territory within the partition; bears costs of enforcement, settlement disputes, and the forfeiture of claims beyond the boundary. Active enforcement required to maintain the partition boundary against revisionist claims.
constraint_indexing:constraint_classification(partition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PARTITION FRAMEWORK ARCHITECTS (ROPE) — The UN system and international legal order benefit from the partition model as a coordination solution: it reduces the claim-space to a bounded set of recognized borders, enabling diplomacy and international relations. The constraint appears as pure coordination from this perspective — establishing agreed boundaries enables peaceful coexistence. Beneficiary with arbitrage: can exit by proposing alternative legitimacy frameworks (security, indigenous continuity, hybrid systems), but current partition reading is institutionally entrenched.
constraint_indexing:constraint_classification(partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TERRITORIAL MAXIMALISTS (TANGLED ROPE) — Organized agents (settlement movements, revisionist state actors) experience the partition boundary as a coordination constraint they oppose, yet also benefit from the legal framework (statehood within the boundary) while extracting through territorial expansion. Active enforcement of the constraint against their preferred expansion; suppressed exit options (cannot legally claim territory beyond the boundary without violating international law). Extraction flows toward states with military capacity to enforce territorial revision de facto.
constraint_indexing:constraint_classification(partition_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: PARTITION IMPLEMENTATION MACHINERY (PITON) — The machinery that enforces the 1948 partition (UN Trusteeship, Mandatory Power structures, post-colonial state apparatus) has substantially atrophied. The partition boundary persists through institutional inertia and legal doctrine, not through active enforcement of the original partition mechanism. Theater ratio high: the partition is invoked as legitimate ground for borders, but the actual enforcement mechanism (UN Trusteeship, League of Nations Mandate system) has dissolved. The constraint is maintained through narrative commitment to the partition reading, not through the structures that originally implemented it.
constraint_indexing:constraint_classification(partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the partition reading naturalizes the boundary as inevitable: given competing claims and the impossibility of territorial overlap, partition is presented as the only rational solution. This perspective risks treating a contingent historical choice (the 1948 partition plan) as a natural law of political geography. The engine will identify this as a false summit: the partition has identifiable beneficiaries and victims, revealing that the 'inevitability' framing is a rhetorical naturalization of a contested institutional arrangement.
constraint_indexing:constraint_classification(partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(partition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(partition_reading, TR),
    TR >= 0.70.

:- end_tests(partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The partition constraint extracts through displacement (0 → 750,000+ refugees), property expropriation, foreclosure of alternative claims, and the concentration of legitimate territorial rights in post-partition states. The extractiveness is not maximal (not 0.72+) because the partition reading does perform genuine coordination: it enables diplomatic engagement and reduces the claim-space to a bounded set. The measurement trajectory (0.35 → 0.58) reflects accumulating extraction over 75 years as settlements, military control, and legal doctrine entrench the partition boundary. Suppression (0.62): High. Multiple suppression mechanisms operate: (a) military enforcement of territorial borders by recognized states; (b) legal doctrine that forecloses non-partition claims; (c) demographic barriers to return (refugee camps, property loss); (d) international law prohibition on territorial revision. The suppression is active and multivalent — it operates through coercion and ideology. Victims have no legitimate exit within the partition reading: they cannot revise the boundary, cannot claim territory outside it, and cannot file claims through the partition framework. Theater Ratio (0.68): High and rising. The partition machinery that originally implemented the boundary (UN Trusteeship, Mandate administration, colonial state apparatus) has substantially atrophied. The boundary now persists through: (a) military enforcement by recognized states; (b) invocation of UN Resolution 181 as legitimacy ground; (c) institutional inertia in international law. The theater is increasing over time (0.45 → 0.73) because the original implementation structures have dissolved while the boundary persists on the basis of legal doctrine and institutional commitment to the partition reading.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Displaced populations experience snare classification (no exit, no coordination benefit, pure extraction). UN and international legal framework experience rope classification (coordination solution, minimal coercion, legitimate boundary-setting). Palestinian/Israeli states experience tangled rope (genuine coordination through recognized statehood, but extraction through boundary constraint and enforcement costs). Settlement movement experiences tangled rope (organized with constrained exit, both benefiting from state recognition and extracting through territorial expansion). The historical implementation machinery experiences piton (the boundary persists through institutional inertia, not through active enforcement of the original partition mechanism). The civilizational analytical observer risks mountain classification (naturalizing the partition as inevitable, treating competing territorial claims as an unsolvable problem that partition uniquely solves) — but the engine will identify this as false summit because beneficiaries exist (UN, post-partition states) and the partition is a contingent historical choice, not a natural law. The perspectival gap reflects the genuine structural conflict: the partition reading enables some coordination while extracting from displaced populations by design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the structural position of each agent relative to the partition constraint. Displaced refugees: d ≈ 0.95 (full targets of extraction) — they bear displacement costs while receiving no benefits from partition; trapped exit yields maximum f(d) ≈ 1.42. UN/International Order: d ≈ 0.05 (full beneficiaries) — they benefit from the partition as a coordination mechanism and legitimacy ground; arbitrage exit yields negative f(d) ≈ -0.12. Palestinian/Israeli States: d ≈ 0.55 (symmetric) — they gain sovereign territory and recognition (beneficiary side) while constrained by inability to revise boundary and enforcement costs (victim side); constrained exit yields f(d) ≈ 0.75. Settlement Movement: d ≈ 0.60 (victim-leaning) — they are organized actors seeking territorial expansion against the constraint's boundary prohibition, yet benefit from the state recognition the partition provides; constrained exit yields f(d) ≈ 0.85. The perspectival gap between displaced refugees (snare, high d, high χ) and partition architects (rope, low d, low/negative χ) reveals the extraction flow: from powerless targets toward institutional beneficiaries. The partition boundary is not neutral — it allocates rights to partition-framework architects and constrains non-aligned claimants.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL CONSTRAINT RESOLUTION: The mandatrophy (the ambiguity between coordination and extraction) is resolved by recognizing that the partition constraint performs BOTH functions. The coordination function is real: establishing recognized borders enables diplomacy and reduces the infinitely large claim-space to a bounded set. The extraction function is real: the boundary forecloses non-partition claims and displaces populations who fall outside it. The partition reading is not 'really' coordination pretending to be extraction, or vice versa. It is genuinely hybrid (tangled rope). The mandatrophy resolves when we recognize that the reading itself (partition as legitimacy ground) is the source of both coordination and extraction. Alternative readings (security_necessity, indigenous_continuity) would perform different balances of coordination and extraction, yielding different ε values and different victim sets. The partition reading's legitimacy claim naturalizes its boundary as inevitable, which is the false summit mechanism: naturalizing a contingent institutional choice as a law of political geography. The falseness becomes visible when we ask: 'Does partition solve coordination better than alternatives? Or does it simply allocate legitimacy (and extraction) to the parties who authored the partition reading?' The answer is: both. Partition does enable coordination within its framework AND it extracts through the boundary it imposes. The mandatrophy dissolves when we treat this as Tangled Rope (not Rope pretending to be Snare, or vice versa) and recognize that the legitimacy framework itself is the object being classified, not an external judge of other constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_legitimacy_kernel_contest,
    'Does legitimacy derive from the partition boundary itself (partition_reading), from security necessity (security_necessity_reading), or from indigenous/historical continuity (indigenous_continuity_reading)? What is the kernel — the contested commitment that grounds the reading?',
    'This omega documents the committer structure. The kernel is territorial_legitimacy. This constraint instantiates partition_reading: legitimate territory = land within UN-recognized borders post-1948 partition. Sibling readings: (a) security_necessity_reading: legitimate territory = land required for military defense; (b) indigenous_continuity_reading: legitimate territory = land with historical/demographic continuity to a people. These are structurally distinct constraints with different ε values, victim sets, and beneficiary classes. Only partition_reading is generated here.',
    'If security_necessity_reading is adopted instead: extraction mechanism shifts from displacement (partition) to military control (security buffer); victims change from displaced persons to populations in security zones. If indigenous_continuity_reading: extraction mechanism shifts to demographic continuity tests; victims include populations deemed non-continuous. Each reading produces a different constraint_type and different suppression signature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_legitimacy_kernel_contest, conceptual, 'Which kernel reading legitimizes territorial claims: partition boundary, security necessity, or indigenous continuity').

omega_variable(
    partition_false_summit_ambiguity,
    'Is the partition boundary (UN Resolution 181) a natural/inevitable outcome of competing territorial claims, or a contingent institutional arrangement that benefits framework architects (UN, post-colonial states) and harms displaced populations?',
    'Comparative historical analysis: if partition was inevitable given the claim-space, then similar partitions should emerge in structurally similar contexts (Cyprus, Ireland, India/Pakistan). If partition reflects contingent choice, then alternative legitimacy frameworks (federation, security arrangements, demography-based, historical continuity) should be structurally viable. The naturalness test: can the boundary be revised without collapsing the legitimacy framework? If revision requires abandoning the partition reading entirely, the boundary is a constitutive choice, not inevitable.',
    'False summit detection: if beneficiaries are present (UN architects, post-partition states) and contingency is confirmed, the engine reclassifies from mountain to tangled_rope or snare. This reveals the naturalization mechanism: the partition reading covers contingent extraction (displacement, territorial expropriation) with inevitability rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_false_summit_ambiguity, empirical, 'Whether partition boundaries are inevitable natural outcomes or contingent institutional choices').

omega_variable(
    suppression_mechanism_coercion_vs_ideology,
    'Does the partition constraint''s suppression operate primarily through military/legal coercion (territorial borders enforced by state apparatus) or through ideological entrenchment (the partition reading is internalized as inevitable/legitimate)?',
    'Behavioral tests: suppression mechanism is primarily coercive if revision is prevented by military barriers, legal prohibition, or economic penalty. Suppression mechanism is primarily ideological if suppression persists after structural barriers are removed (e.g., refugees who could materially return but have internalized the partition reading as legitimate). Measure through: (a) gap between legal/military barriers and expressed preference for territorial return; (b) rhetoric analysis of partition legitimacy; (c) post-war/post-conflict dynamics where military barriers are removed but partition persists.',
    'If suppression is primarily coercive, the constraint is structurally contingent — removing the coercive apparatus changes classification. If suppression is primarily ideological, the constraint has internalized binding even after coercive removal — more robust to structural change, closer to a rope (accepted coordination) than a snare (imposed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_coercion_vs_ideology, empirical, 'Whether partition suppression operates through coercion or ideological entrenchment').

omega_variable(
    two_state_sufficiency,
    'Does the partition reading''s legitimacy framework (two sovereign states within bounded borders) actually solve the coordination problem it claims to solve, or does the boundary itself create new coordination failures?',
    'Measure coordination function through: (a) reduction in territorial dispute intensity post-partition (does establishing the boundary reduce claims?); (b) capacity for diplomatic engagement within the partition framework (can states within the boundary coordinate?); (c) emergence of new disputes over boundary interpretation (settlement legality, water rights, airspace). If partitioning reduces overall dispute intensity and enables coordination, the rope classification is warranted. If partitioning creates new disputes and prevents coordination, the extraction mechanism is dominant.',
    'If two-state partition solves coordination: constraint remains tangled_rope (coordination + extraction). If partition creates new disputes: constraint moves toward snare (extraction dominates, coordination function atrophies). Theater ratio reflects this: if the partition machinery persists despite coordination failure, theater increases (piton trajectory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_sufficiency, empirical, 'Whether partition legitimacy framework enables or prevents coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(partition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(part_theater_1948, partition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(part_theater_1973, partition_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(part_theater_1998, partition_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement(part_theater_2023, partition_reading, theater_ratio, 75, 0.73).

% Extraction over time
narrative_ontology:measurement(part_extract_1948, partition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(part_extract_1973, partition_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(part_extract_1998, partition_reading, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(part_extract_2023, partition_reading, base_extractiveness, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(partition_reading, security_necessity_reading).
narrative_ontology:affects_constraint(partition_reading, indigenous_continuity_reading).
narrative_ontology:affects_constraint(partition_reading, settlement_territorial_expansion).
narrative_ontology:affects_constraint(partition_reading, refugee_repatriation_foreclosure).

% DUAL FORMULATION NOTE:
% The territorial legitimacy kernel (kernel_id: territorial_legitimacy) has three structurally distinct readings, each producing a different constraint with different ε values, victim sets, and beneficiary structures. partition_reading (this file) instantiates legitimacy via the UN 1948 partition boundary. security_necessity_reading instantiates legitimacy via military defensibility and security buffer logic — produces different victims (populations in security zones) and higher suppression. indigenous_continuity_reading instantiates legitimacy via demographic and historical continuity — produces different victims (populations deemed non-continuous) and different extraction mechanisms (demographic tests rather than boundary enforcement). Each reading is a separate constraint with its own ε-invariant classification. They are linked via network.affects_constraints because revising the legitimacy reading (shifting from partition to security or continuity) changes the constraint structure of all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(partition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
