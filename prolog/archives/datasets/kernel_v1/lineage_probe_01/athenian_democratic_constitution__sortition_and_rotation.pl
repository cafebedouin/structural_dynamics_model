% ============================================================================
% CONSTRAINT STORY: athenian_democratic_constitution__sortition_and_rotation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_athenian_sortition_rotation, []).

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
 *   constraint_id: athenian_democratic_constitution__sortition_and_rotation
 *   human_readable: Athenian Democratic Sortition and Rotation
 *   domain: political/historical
 *
 * SUMMARY:
 *   Athens' sortition-and-rotation mechanism is one reading of a contested
 *   constitutional kernel. This reading instantiates the claim that democracy
 *   is fundamentally about equal access to office through randomization, on
 *   the theory that election is aristocratic (favoring the rhetorical and
 *   wealthy) and chance alone preserves equal dignity. The constraint
 *   operates at the structural level of office-holding: magistrates (archons,
 *   generals, treasurers) are filled by lot and serve one-year terms,
 *   preventing any individual or family from establishing a permanent power
 *   base. The mechanism generates six distinct perspectival readings from the
 *   same constitutional structure: ordinary citizens experience it as
 *   coordinating their equal access; the assembly experiences it as
 *   protecting democratic principle; the excluded (slaves, metics, women)
 *   experience it as the mechanism through which democratic equality is built
 *   on their exclusion; aristocratic elites experience it as suppressing
 *   their electoral advantage; technical expertise victims experience it as a
 *   mixed coordination-suppression hybrid; and the civilizational analytical
 *   observer risks naturalizing it as the only true democracy. The
 *   constraint's extractiveness (0.22) reflects that while the mechanism does
 *   suppress aristocratic office-holding and professional expertise claims,
 *   the suppression is the point of the system rather than a parasitic
 *   extraction. The beneficiary (ordinary citizens and democratic principle)
 *   is also the authority making the constraint, not a separate extractive
 *   agent. Suppression (0.35) reflects the enforcement required to prevent
 *   aristocratic capture of the lottery mechanism and to exclude non-citizens
 *   entirely. Theater ratio (0.48) reflects that the lottery mechanism has
 *   genuine function but also contains performative elements (the ritual of
 *   drawing lots, the ceremonial equality it enacts, the documentary
 *   procedures that mask network effects).
 *
 * KEY AGENTS:
 *   - Ordinary Citizens: Primary beneficiary (moderate/mobile) — gain equal access to office and civic participation through sortition
 *   - Democratic Assembly (Ekklesia): Collective beneficiary (organized/constrained) — the assembled citizen body that maintains sortition as democratic principle
 *   - Excluded Groups (Slaves, Metics, Women): Structural victims (powerless/trapped) — excluded from lottery entirely; bear costs of democratic equality for citizens
 *   - Aristocratic Elite: Secondary agent (powerful/constrained) — lose electoral advantage through randomization but can work within lottery mechanism
 *   - Technical Expertise (Generals, Treasurers, Architects): Tertiary agent (powerful/constrained) — suppressed by randomization but required for functional governance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing this reading as the only true democracy, creating false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(athenian_democratic_constitution__sortition_and_rotation, 0.22).
domain_priors:suppression_score(athenian_democratic_constitution__sortition_and_rotation, 0.35).
domain_priors:theater_ratio(athenian_democratic_constitution__sortition_and_rotation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(athenian_democratic_constitution__sortition_and_rotation, extractiveness, 0.22).
narrative_ontology:constraint_metric(athenian_democratic_constitution__sortition_and_rotation, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(athenian_democratic_constitution__sortition_and_rotation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(athenian_democratic_constitution__sortition_and_rotation, rope).
narrative_ontology:human_readable(athenian_democratic_constitution__sortition_and_rotation, "Athenian Democratic Sortition and Rotation").
narrative_ontology:topic_domain(athenian_democratic_constitution__sortition_and_rotation, "political/historical").

domain_priors:requires_active_enforcement(athenian_democratic_constitution__sortition_and_rotation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(athenian_democratic_constitution__sortition_and_rotation, 'a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a').
narrative_ontology:cs_kernel_codification('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', formalized).
narrative_ontology:cs_authority_grounding('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', lineage).
narrative_ontology:cs_interpretation_layer_present('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a').
narrative_ontology:cs_reading_relation('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', athenian_democratic_constitution__accountability_machinery, coexists_with).
narrative_ontology:cs_reading_relation('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', athenian_democratic_constitution__assembly_supremacy, coexists_with).
narrative_ontology:cs_reading_relation('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', athenian_democratic_constitution__exclusionary_base, influences).
narrative_ontology:cs_axiom('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', foundational, election_is_aristocratic).
narrative_ontology:cs_axiom_status(election_is_aristocratic, holdable).
narrative_ontology:cs_axiom_grounding('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', election_is_aristocratic, deontological).
narrative_ontology:cs_axiom('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', foundational, randomization_defeats_domination).
narrative_ontology:cs_axiom_status(randomization_defeats_domination, holdable).
narrative_ontology:cs_axiom_grounding('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', randomization_defeats_domination, instrumental).
narrative_ontology:cs_axiom('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', secondary, rotation_prevents_entrenchment).
narrative_ontology:cs_axiom_status(rotation_prevents_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', rotation_prevents_entrenchment, instrumental).
narrative_ontology:cs_reference_frame('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', democratic_equality_through_chance).
narrative_ontology:cs_drift_state('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', classical_period_end, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a0819f1e-5bfb-4f55-bf12-0fc9fc6a874a', '').
narrative_ontology:cs_kernel_id(athenian_democratic_constitution__sortition_and_rotation, athenian_democratic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__sortition_and_rotation, ordinary_citizens).
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__sortition_and_rotation, democratic_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (ROPE) — Selected by lot to hold office, the citizen experiences the constraint as coordinating equal access to political participation. Low suppression because rotation ensures exit after one year. Exit options are real (mobile) — the citizen leaves office regardless of performance. Coordination is genuine: sortition solves the problem of preventing any one person from dominating office. No hidden extraction — the citizen bears costs of office-holding but also receives the civic opportunity.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: DEMOCRATIC ASSEMBLY (ROPE) — Collectively, the citizen body benefits from sortition as a coordination mechanism that maintains equal access and prevents aristocratic capture of office. The assembly experiences the constraint as protecting democratic equality through randomization. Constrained exit: the assembly could theoretically abandon sortition, but doing so would require overcoming the legitimacy claim that sortition embodies. Low effective extraction because the assembly is also the beneficiary — no parasitic agent extracting from the system.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE EXCLUDED (SNARE) — Slaves, metics (foreign residents), and women are structurally excluded from the lottery entirely. For these agents, sortition appears not as democratic coordination but as the mechanism through which democratic equality is built on their exclusion and dispossession. High suppression: no legal path to participation. No exit: status is legally fixed. Trapped and bearing the cost of democratic equality for citizens. This reading instantiates the exclusionary_base sibling — it is the same constitutional structure viewed from the perspective of those constituted as non-citizens.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ARISTOCRATIC ELITE (PITON) — Formerly powerful through inherited wealth and reputation, the aristocratic agent experiences sortition as suppressing their electoral advantage. Their traditional path to office (persuading voters through rhetoric and patronage) is defeated by random selection. Theater ratio high: the aristocracy may capture the lottery mechanism itself through wealth-enabled networks and symbolic capital, even though formal selection is random. Their effective exit is constrained: they cannot easily abandon democratic citizenship, but they can work within the constraint to dominate the selection pool of candidates from whom lots are drawn. Piton classification reflects degradation of their aristocratic office-seeking mechanism, now requiring theatrical manipulation of the random process rather than direct electoral victory.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: EXPERTISE VICTIM / TECHNICAL OFFICE (TANGLED ROPE) — Certain offices (generals, treasurers, architects) require genuine expertise. Sortition creates genuine coordination problems when technical competence is randomized out of office. The expert bears suppressed authority (cannot claim privileged decision-making based on knowledge) and constrained exit (must work within democratic constraints even when unqualified officers are selected). However, genuine coordination function remains: the democracy must still coordinate war, finance, and building. Active enforcement required: scrutiny procedures (dokimasia) examine officers' fitness; audits (euthynai) hold them accountable. Extraction exists (the expertise is suppressed and the expert cannot claim special authority) but so does coordination (the system maintains both democratic equality and functional governance). This is tangled rope because the suppression of expertise is the point (democracy defeats aristocratic claim to superior knowledge) but incompetence creates real costs that the system must then coordinate around.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, this perspective risks naturalizing the reading as immutable democratic principle: 'democracy IS equal access, which necessarily requires lot-drawing.' This universalized view treats sortition as an inescapable logical consequence of democratic equality. However, the base properties contradict mountain classification: extractiveness (0.22) and suppression (0.35) are not minimal enough for a genuine natural law. The analytical observer's mountain classification is a false summit, revealing that the normative claim 'only randomization is truly democratic' is a contingent constitutional choice, not a law of nature. Sibling readings (accountability_machinery, assembly_supremacy, exclusionary_base) show that Athens' democracy worked through multiple mechanisms, not sortition alone.
constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(athenian_democratic_constitution__sortition_and_rotation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(athenian_democratic_constitution__sortition_and_rotation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(athenian_democratic_constitution__sortition_and_rotation, TR),
    TR >= 0.70.

:- end_tests(athenian_democratic_constitution__sortition_and_rotation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22, increasing from 0.18 to 0.26 over the interval): Low-moderate. The constraint extracts from certain agents (suppresses aristocratic electoral advantage, suppresses expertise claims) but does not extract toward a parasitic beneficiary — the extraction flows toward the democratic principle and ordinary citizens, who are the authority maintaining the constraint. The rising trend reflects increasing pressure on the mechanism from aristocratic capture attempts and expertise suppression costs over time. Suppression (0.35, increasing from 0.30 to 0.40): Moderate. The mechanism requires active suppression of alternative paths: anti-aristocratic enforcement (preventing wealth-enabled network capture), anti-expertise claims (overriding knowledge-based authority), anti-exclusion-erosion (maintaining citizen-only lottery). The rising trend reflects increasing enforcement burden as aristocratic attempts to capture the lottery grow more sophisticated. Theater ratio (0.48, increasing from 0.38 to 0.48): Moderate, rising toward 0.50. The lottery has genuine function (randomization actually prevents aristocratic office monopoly) but also contains performative elements: the ceremonial drawing of lots, the documentary procedures (pinakia, Bronze tablet records), the ritual equality-statements. Theater is rising as the symbolic content of sortition (the public ritual of democracy) becomes increasingly important to legitimacy, even as the actual randomization mechanism faces growing pressure from sophisticated capture attempts.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the ordinary citizen (Rope) and the excluded groups (Snare). Both perspectives view the same constitutional mechanism. From the citizen's perspective, sortition is a genuine coordination mechanism that solves the democratic problem of preventing aristocratic domination. From the excluded perspective (slaves, metics, women), the lottery is the mechanism through which democratic equality for the few is constructed on the exclusion and dispossession of the many. The citizen experiences equal access; the excluded experience legal disability. A secondary gap appears between the assembly (Rope) and the expertise perspective (Tangled Rope). Both recognize that sortition is genuine coordination, but the expertise perspective sees a hidden suppression cost: technical incompetence is randomized into office, creating governance failures that require audits and accountability procedures to manage. The citizen assembly sees this as an acceptable coordination cost; the expertise perspective sees it as a real extraction mechanism (suppressed authority, constrained exit, forced participation in inferior governance). The aristocratic perspective (Piton) sees its own degradation: the aristocracy's former office-holding mechanism (electoral rhetoric, patronage networks) is defeated by randomization, leaving only theatrical manipulation of the lottery itself. The analytical observer's mountain perspective is a false summit: it naturalizes this reading as the only true democracy, missing that other readings (accountability, assembly, exclusion) are equally structural features. The false summit reveals the kernel contest: there is no single 'real' Athenian constitution, but rather a contested bundle of mechanisms (sortition, accountability, assembly, exclusion) through which the structure of democratic domination is maintained.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's relationship to the extraction flow. Ordinary citizens are net beneficiaries (low d, around 0.10): they benefit from equal access through sortition; their suppressed alternatives (exclusive electoral advantage) are exactly what the mechanism prevents. The assembly is a collective beneficiary (low d, around 0.15): it maintains sortition as protecting democratic principle; exit would require abandoning the legitimacy claim that sortition embodies. The excluded groups are full targets (high d, approaching 1.0): they bear the structural cost of democratic equality without receiving its benefits; suppression is total (legal exclusion from citizenship). The aristocratic elite are partial targets but with arbitrage options (d around 0.50-0.60): they lose electoral advantage but retain wealth and network capacity to work within the lottery mechanism; they have constrained exit (cannot leave citizenship easily but can accumulate symbolic capital). The expertise perspective is a target of suppression but with some coordination function (d around 0.55-0.65): expertise is suppressed (cannot claim special authority) but the system still requires technical competence (via audits and accountability, not electoral selection). The analytical observer (d around 0.72) sits outside the constraint as an observer, perceiving its universal structure. The chiastic pattern (beneficiary low-d, target high-d) is clean for the ordinary citizen and excluded groups; it becomes ambiguous for secondary agents (aristocracy, expertise) who both lose some privilege and gain some alternative authority within the constraint. This ambiguity in d-space is why their perspectives are piton and tangled_rope rather than pure rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy at the high level (extractiveness < 0.46). However, it exhibits a lower-level mandatrophy at the secondary level: the expertise perspective sees genuine coordination (the democracy must still govern) but also genuine suppression (expertise cannot claim special authority). Is sortition a mechanism coordinating democratic equality against aristocratic capture, or is it a mechanism suppressing expert knowledge in favor of democratic ideology? The answer is: both simultaneously. The tangled rope classification resolves this by holding both functions at once. The false summit at the analytical level (mountain perspective) is a different kind of mandatrophy: the risk of naturalizing a contingent reading as necessity. The sibling readings show that Athens maintained democracy through accountability machinery, assembly supremacy, and exclusion just as much as through sortition. No single mechanism is the real constitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aristocratic_capture_of_lottery,
    'Can the aristocratic elite effectively capture the sortition mechanism through wealth-enabled network effects and symbolic capital, even though formal selection is random?',
    'Historical analysis of actual archonates and magistracies drawn by lot: correlation between social class of selected officers and wealth/family prominence; examination of who served multiple offices despite randomization',
    'If high capture: sortition becomes theater masking aristocratic office-holding. Extractiveness rises to 0.45+, reclassifies as tangled_rope or snare for aristocratic perspective. If low capture: sortition genuinely defeats electoral advantage. Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aristocratic_capture_of_lottery, empirical, 'Extent of aristocratic network capture of the lottery mechanism').

omega_variable(
    excluded_status_as_constitutional_feature,
    'Is the exclusion of slaves, metics, and women a contingent feature of Athenian democracy or logically entailed by the sortition mechanism itself?',
    'Comparative analysis: whether sortition requires exclusion, or whether exclusion is a separate political choice. Thought experiment: would expanding citizenship to all residents and subjecting all to sortition change the sortition mechanism''s democratic character?',
    'If contingent: exclusionary_base is a separate reading, snare perspective (Perspective 3) is correct. If entailed: sortition-and-rotation logically implies exclusion; the readings are interdependent. Affects whether exclusionary_base is truly a sibling reading or a necessary component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_status_as_constitutional_feature, conceptual, 'Logical relationship between sortition and exclusion').

omega_variable(
    expertise_suppression_cost,
    'What is the actual governance cost of randomizing technical expertise out of office? Do audits and scrutiny procedures (dokimasia, euthynai) successfully mitigate incompetence?',
    'Historical analysis of major governance failures (military disasters, fiscal crises) correlated with officer quality and selection method; comparison of outcomes under sortition vs. elected technical magistrates in same time periods',
    'If audits fail and costs are high: extractiveness rises to 0.35+, snare classification for expertise perspective. If audits work: tangled_rope holds. Affects whether sortition is a pure coordination mechanism (rope) or a mixed mechanism with hidden suppression costs (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_suppression_cost, empirical, 'Governance cost of expertise suppression through sortition').

omega_variable(
    rotation_term_length_optimality,
    'Is one-year rotation the optimal term length for democratic equality, or does it create unnecessary churn that favors short-term thinking or incumbent capture?',
    'Comparative analysis of outcomes under one-year rotation vs. multi-year sortition in theoretical models or later democratic systems; measurement of policy continuity and incompetence rates across rotation lengths',
    'If shorter is better: extractiveness decreases (less time for office-holder to entrench). If longer is better: current one-year mechanism suppresses continuity and learning, raising hidden costs. Affects theater_ratio (if churn is performative ritual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rotation_term_length_optimality, empirical, 'Optimal rotation term length for democratic function').

omega_variable(
    sortition_as_reading_vs_natural_law,
    'Is sortition-and-rotation a contingent constitutional reading (one way to instantiate democratic equality among multiple valid ways) or a necessary logical consequence of democracy itself?',
    'Comparison with other democratic constitutions: do non-sortition democracies achieve similar equality? What do competing readings (accountability_machinery, assembly_supremacy) accomplish that sortition does not? Can a democracy be fully democratic without sortition?',
    'If contingent reading: mountain perspective (Perspective 6) is false summit; sibling readings are genuine alternatives. If necessary: sortition is the democratic principle; other readings are secondary mechanisms. Affects the fundamental framing of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sortition_as_reading_vs_natural_law, conceptual, 'Contingency vs. necessity of sortition as democratic principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(athenian_democratic_constitution__sortition_and_rotation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ath_sort_tr_t0, athenian_democratic_constitution__sortition_and_rotation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ath_sort_tr_t50, athenian_democratic_constitution__sortition_and_rotation, theater_ratio, 50, 0.45).
narrative_ontology:measurement(ath_sort_tr_t100, athenian_democratic_constitution__sortition_and_rotation, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(ath_sort_be_t0, athenian_democratic_constitution__sortition_and_rotation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ath_sort_be_t50, athenian_democratic_constitution__sortition_and_rotation, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(ath_sort_be_t100, athenian_democratic_constitution__sortition_and_rotation, base_extractiveness, 100, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(ath_sort_su_t0, athenian_democratic_constitution__sortition_and_rotation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ath_sort_su_t50, athenian_democratic_constitution__sortition_and_rotation, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(ath_sort_su_t100, athenian_democratic_constitution__sortition_and_rotation, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(athenian_democratic_constitution__sortition_and_rotation, identity_coordination).
narrative_ontology:affects_constraint(athenian_democratic_constitution__sortition_and_rotation, athenian_democratic_constitution__accountability_machinery).
narrative_ontology:affects_constraint(athenian_democratic_constitution__sortition_and_rotation, athenian_democratic_constitution__assembly_supremacy).
narrative_ontology:affects_constraint(athenian_democratic_constitution__sortition_and_rotation, athenian_democratic_constitution__exclusionary_base).

% DUAL FORMULATION NOTE:
% Sortition is one reading of the contested kernel 'athenian_democratic_constitution'. The other readings (accountability_machinery, assembly_supremacy, exclusionary_base) are separate constraint stories with their own constraint IDs and base properties. Sortition has extractiveness 0.22 (suppression of aristocratic advantage, but beneficiary is democratic principle not parasitic agent). Accountability has extractiveness centered on liability and oversight. Assembly supremacy has extractiveness centered on direct decision-making authority. Exclusionary base has extractiveness centered on structural exclusion. These form a constraint family through network.affects_constraints, not through combination. Each reading is a separate ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
