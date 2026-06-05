% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria (Hybrid Reading): Objective Criteria + Normative Legitimacy
 *   domain: international_law/state_theory/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) established four objective criteria for
 *   statehood: defined territory, permanent population, effective government,
 *   and capacity to enter into relations with other states. For 40+ years,
 *   the declaratory reading dominated: a political entity meeting these
 *   objective criteria was a state, regardless of legitimacy. The hybrid
 *   reading emerged in the 1960s-70s, as decolonization created new states
 *   with diverse governance models. Liberal democracies began conditioning
 *   recognition on normative criteria — democratic legitimacy, human rights
 *   compliance, non-aggression, minority rights protection. The hybrid
 *   reading holds that BOTH objective criteria (Montevideo) AND normative
 *   legitimacy are required for recognition. This creates a structural
 *   constraint: political entities (secessionist movements, non-democratic
 *   regimes, post-conflict states) can meet objective criteria but fail
 *   normative gates controlled by liberal democracies. The constraint's
 *   extractiveness (0.52) reflects that it genuinely coordinates
 *   international legal order (Rope function) while enabling liberal states
 *   to extract legitimacy and power projection capacity through selective
 *   recognition (Snare function from secessionist perspective). Theater ratio
 *   (0.65) reflects that recognition debates invoke normative criteria
 *   (democracy, human rights) extensively, but actual recognition follows
 *   geopolitical interest with selective application of standards.
 *
 * KEY AGENTS:
 *   - Liberal Democratic States: Institutional gatekeepers (institutional/arbitrage) — control recognition machinery; benefit from normative criteria that legitimize strategic recognition decisions; experience constraint as empowering coordination mechanism
 *   - Non-Liberal Secessionist Movements: Primary victims (powerless/trapped) — meet objective criteria, control territory and population, but cannot achieve recognition because they lack liberal credentials; permanently excluded from international legal personality
 *   - Post-Colonial Weak States: Secondary victims (moderate/constrained) — emerged via decolonization with diverse governance models; now constrained by recognition conditionality and policy autonomy restrictions tied to liberal-state approval
 *   - Humanitarian Intervention Coalition: Organized beneficiaries (organized/mobile) — NGOs, regional courts, ICC gain legal framework for intervention under normative statehood criteria; experience constraint as authorizing mechanism for humanitarian action
 *   - Regional Integration Bodies: Organized moderates (organized/constrained) — AU, ASEAN, MERCOSUR use hybrid reading to drive internal normative convergence; see constraint as scaffolding for regional democratization
 *   - International Legal System: Institutional artifact (institutional/arbitrage) — maintains performative adherence to objective Montevideo criteria while actual practice follows normative hybrid reading; degraded through inertia (piton perspective)
 *   - Analytical Observer: Detached analyst (analytical/analytical) — risks naturalizing the hybrid reading as objective requirement rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.52).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.58).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria (Hybrid Reading): Objective Criteria + Normative Legitimacy").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/state_theory/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'f8cf536f-314d-4d5d-b71f-b2355036a0b4').
narrative_ontology:cs_kernel_codification('f8cf536f-314d-4d5d-b71f-b2355036a0b4', formalized).
narrative_ontology:cs_authority_grounding('f8cf536f-314d-4d5d-b71f-b2355036a0b4', extraction).
narrative_ontology:cs_interpretation_layer_present('f8cf536f-314d-4d5d-b71f-b2355036a0b4').
narrative_ontology:cs_reading_relation('f8cf536f-314d-4d5d-b71f-b2355036a0b4', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8cf536f-314d-4d5d-b71f-b2355036a0b4', montevideo_statehood_criteria__constitutive_reading, influences).
narrative_ontology:cs_axiom('f8cf536f-314d-4d5d-b71f-b2355036a0b4', foundational, democratic_legitimacy_necessary_for_statehood).
narrative_ontology:cs_axiom_status(democratic_legitimacy_necessary_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('f8cf536f-314d-4d5d-b71f-b2355036a0b4', democratic_legitimacy_necessary_for_statehood, deontological).
narrative_ontology:cs_axiom('f8cf536f-314d-4d5d-b71f-b2355036a0b4', foundational, human_rights_compliance_necessary_for_statehood).
narrative_ontology:cs_axiom_status(human_rights_compliance_necessary_for_statehood, overridden).
narrative_ontology:cs_axiom_grounding('f8cf536f-314d-4d5d-b71f-b2355036a0b4', human_rights_compliance_necessary_for_statehood, empirically_contingent).
narrative_ontology:cs_reference_frame('f8cf536f-314d-4d5d-b71f-b2355036a0b4', liberal_normative_legitimacy_framework).
narrative_ontology:cs_drift_state('f8cf536f-314d-4d5d-b71f-b2355036a0b4', contemporary_selective_application_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f8cf536f-314d-4d5d-b71f-b2355036a0b4', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, post_colonial_weak_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_under_non_democratic_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-LIBERAL SECESSIONIST MOVEMENT (SNARE) — Movement has territorial control, effective government, and popular support within its territory but cannot achieve international recognition because hybrid reading permits liberal democracies to deny recognition on normative grounds (regime type, human rights record). Trapped: no path to sovereignty that bypasses liberal-state gatekeepers; no exit from the constraint. Pure extraction — the movement bears the cost (international isolation, resource denial) while liberal states extract legitimacy through selective recognition.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POST-COLONIAL WEAK STATE (TANGLED ROPE) — State meets objective Montevideo criteria (territory, government, capacity to enter treaties) and emerged via decolonization process, but constrained by dependence on liberal-state recognition for treaty participation, loan access, and trade agreements. Hybrid reading permits normative challenges to legitimacy on democratic governance grounds. Mixed experience: genuine coordination function (international legal personality) coexists with asymmetric extraction (constrained policy autonomy through recognition conditionality). Some agency through coalition-building; significant barriers through dependency.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LIBERAL DEMOCRATIC STATE (ROPE) — Institutional actor with arbitrage options (can choose recognition as strategic tool; can withhold recognition without cost). Hybrid reading provides normative justification for recognition decisions: can deny recognition to non-liberal regimes on democratic governance grounds, can grant recognition to liberal movements. Experiences the constraint as coordination mechanism — provides international legal order while maintaining discretion. Net beneficiary: extracts legitimacy and diplomatic leverage through selective recognition.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMANITARIAN INTERVENTION COALITION (TANGLED ROPE) — Organized actors (NGOs, regional courts, ICC) gain legal framework for intervention under hybrid reading's normative criteria. Sees genuine coordination function (international human rights norms) but also extracted authority: framework permits state intervention in internal affairs on normative grounds, enabling power consolidation disguised as humanitarian concern. Mobile: can exit by rejecting normative criteria or shifting to declaratory reading. Mixed extraction: some genuine coordination (human rights standards do prevent abuses); asymmetric application (powerful states intervene under humanitarian cover; weak states cannot).
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL INTEGRATION PROJECT (SCAFFOLD) — Regional bodies use hybrid reading to legitimize internal normative standards (AU democracy requirements, ASEAN human rights pressure) with sunset logic: frameworks are intended to drive convergence toward liberal-democratic standards, with recognition delayed until convergence occurs. Constrained: members must reform or face exclusion. Theater moderate: regional bodies genuinely aim for democratic integration (not purely performative) but enforcement is often theatrical (symbolic pressure without coercive mechanisms). Sunset embedded: the constraint is meant to dissolve as regional members converge on liberal-democratic norms.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SYSTEM (PITON) — Treaty-based statehood system itself has become inert. Original Montevideo criteria (objective: territory, government, capacity for treaty relations, independence) provided self-executing recognition mechanism. Hybrid reading added normative overlay but without formalized enforcement institutions. Result: legal system maintains performative adherence to Montevideo while actual recognition practice follows power politics and normative coalitions. Theater ratio high: frequent invocations of statehood criteria, human rights standards, democratic legitimacy in recognition debates; actual recognition driven by geopolitical interest. Theater persists through institutional inertia — replacing Montevideo would require new treaty consensus impossible to achieve.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED NORMATIVITY VIEW (MOUNTAIN) — From civilizational perspective, some normative gate on statehood appears inherent to the concept: a political community that systematically violates human rights or threatens neighbors cannot be legitimate 'state' (only tyranny, rogue actor). Normativity appears baked into the concept itself. However, this naturalization is itself the constraint's mechanism — the hybrid reading succeeds by making normative criteria appear objective.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hybrid reading creates asymmetric extraction: liberal democracies extract legitimacy and geopolitical leverage through selective recognition while non-liberal entities face recognition denial without recourse. However, extractiveness is not maximal (not snare-level ≥0.66) because genuinely functional coordination occurs — international legal order depends on recognition mechanisms, and normative criteria do reduce certain abuses. The extraction is real but layered atop coordination function. Measurement trajectory shows rising extractiveness (0.38 → 0.52) as liberal-state application of normative criteria becomes increasingly strategic and selective over time. Suppression (0.58): Moderate-high. Barriers to recognition include: normative gatekeeping (liberal-state discretion), resource penalties (trade, investment, treaty participation), diplomatic isolation, and for secessionists the core barrier — no institutional path to convince liberal states that non-liberal governance can be legitimate. Suppression rises over interval (0.52 → 0.58) as liberal democracies institutionalize normative criteria into conditionality frameworks (IMF governance requirements, EU accession standards). Theater ratio (0.65): Moderate-high. Recognition debates extensively invoke normative language (democracy, human rights, rule of law) but allocation of recognition follows geopolitical interest. Criteria are applied selectively: similar governance records produce opposite recognition outcomes depending on geostrategic alignment. Theater rises (0.48 → 0.65) as normative rhetoric becomes more elaborate and institutionalized while actual consistency declines.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading produces maximum perspectival divergence. The secessionist movement sees pure extraction (Snare) — trapped by criteria they cannot meet, bearing isolation cost for regime-type sin. The weak post-colonial state sees mixed coordination-extraction (Tangled Rope) — genuine legal personality function, but constrained by conditionality. The liberal democratic state sees pure coordination (Rope) — the constraint empowers them to maintain order through selective recognition. The humanitarian coalition sees mixed function (Tangled Rope) — genuine human rights enforcement coexists with extraction of intervention authority. The regional integration body sees temporary coordination (Scaffold) — driving convergence toward liberal norms with sunset logic. The international legal system sees degraded ritual (Piton) — maintains performative adherence to objective Montevideo while actual practice follows normative hybrid reading. The analytical observer risks seeing immutable normativity (Mountain) — that legitimacy inherently requires liberal democratic governance. The perspectival gaps reveal the hybrid reading's structural mechanism: it appears as natural principle to beneficiaries, as extractive gatekeeping to victims, as degraded performance to the system maintaining it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) represent each agent's structural position relative to extraction flow. Secessionist movement: d ≈ 0.95 (trapped victim) — no exit options, bears full extraction cost. Weak post-colonial state: d ≈ 0.72 (mobile victim with dependency) — constrained mobility due to resource/treaty access dependency; high extraction but some agency through coalition-building. Liberal democratic state: d ≈ 0.15 (beneficiary with arbitrage) — arbitrage exit (can grant/deny recognition strategically); net flow of extracted legitimacy and leverage toward this agent. Humanitarian coalition: d ≈ 0.50 (mixed position) — both benefits from normative legal framework and extracted into power-projection logic; organized so mobility exists. Regional integration body: d ≈ 0.65 (organized victim) — constrained by need for member-state cooperation; moderate extraction from members who resist normative pressure. International legal system: d ≈ 0.20 (arbitrage beneficiary) — system maintains authority through hybrid reading even as actual practice degrades it. Analytical observer: d ≈ 0.72 (analytical victim) — cannot directly influence recognition but trapped by need to justify using conceptual framework that naturalizes extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by noting that this constraint simultaneously coordinates (establishes global legal order, prevents arbitrary recognition) and extracts (enables liberal states to weaponize recognition, excludes non-liberal entities). The coordination function is genuine — without recognition mechanisms, international relations dissolve into ad hoc power bargaining. The extraction function is equally genuine — normative criteria permit liberal states to deny recognition strategically while claiming principle. The hybrid reading does NOT resolve mandatrophy by proving one function dominates the other. Instead, it reveals that mandatrophy is inherent to the constraint's structure: normative gating on recognition enables BOTH order and hegemony, and cannot cleanly separate them. A pure-coordination reading (declaratory — objective criteria only) loses the order-enhancement function (normative pressure does improve some states' rights records). A pure-extraction reading (constitutive — whatever liberal states decide is statehood) loses the normative constraint on recognition power. The hybrid reading is the instantiation of irreducible dual function, which is why extractiveness (0.52) sits in the tangled-rope range rather than approaching snare (which would indicate extractive dominance) or rope (which would indicate coordination dominance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_epistemic_status,
    'Are democratic governance and human rights protection objective criteria (measurable, falsifiable standards) or normative judgments that cannot be objectively instantiated?',
    'Comparative analysis of how different recognition authorities operationalize ''democratic legitimacy'' and ''human rights compliance'' — assess variance in standards, selective application, temporal consistency, and whether criteria produce reproducible classifications',
    'If objective: hybrid reading is well-defined constraint with stable ε ≈ 0.52. If normative: ε depends on which liberal state is judging, producing vector-valued extractiveness. The reading''s legitimacy claim collapses if standards are subjective but presented as objective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_criteria_epistemic_status, conceptual, 'Whether normative statehood criteria can be objectively operationalized').

omega_variable(
    recognition_outcome_causality,
    'Does refusal of recognition actually cause non-liberal regimes to liberalize (normative constraint working as intended) or does it entrench authoritarianism and increase state capacity for extraction of subjects (suppression backfire)?',
    'Longitudinal analysis of post-recognition-denial trajectories: compare governance indicators (VDEM, V-Dem, Freedom House scores) for regimes denied recognition vs. similar regimes with recognition; track state repression intensity, civil society space, and institutional capacity across 5-20 year windows',
    'If liberalizing: hybrid reading is genuine coordination mechanism with justified normativity gate. If entrenching: mechanism is pure extraction (suppression raises as punishment for non-compliance); classification shifts toward snare. If neutral: constraint is scaffold with external pressures (not the recognition regime) driving convergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_outcome_causality, empirical, 'Whether recognition denial causally drives democratic convergence or entrenchment').

omega_variable(
    liberal_state_strategic_bias,
    'Do liberal democracies apply normative statehood criteria consistently (as universal principle) or selectively based on geopolitical alignment (as strategic tool)?',
    'Content analysis of recognition decisions and public justifications: catalog all recognition/non-recognition decisions over 20 years; code justifications as ''normative'' (human rights, democracy) vs ''strategic'' (geopolitical interest); assess correlation between coded normative reasons and voting patterns of major liberal states; identify cases where identical governance records produced opposite recognition outcomes',
    'If consistent: hybrid reading legitimately constrains recognition via principled criteria. If selective: extractiveness shifts upward (ε → 0.65-0.72); classification may shift to snare from liberal-state perspective (using normativity as cover for strategic extraction). Determines whether theater ratio is high (if selective) or moderate (if principled).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liberal_state_strategic_bias, empirical, 'Whether liberal states apply normative recognition criteria consistently or strategically').

omega_variable(
    sibling_reading_coexistence_structure,
    'Can the hybrid reading (objective + normative) coexist in the same legal framework with the declaratory reading (objective only) and constitutive reading (normative only), or does each reading foreclose its siblings?',
    'Institutional mapping of recognition authorities: identify which authorities operationalize which reading; assess whether mixed frameworks (one authority using declaratory, another using hybrid) produce legal contradictions or merely competing legitimacy claims; examine whether a single state applies different readings in different contexts (e.g., hybrid to delegitimize a target, declaratory when convenient)',
    'If coexist: each reading shapes different recognition coalitions; constraint family is a true multi-reading structure. If foreclose: one reading dominates; siblings are suppressed positions rather than live alternatives. Determines whether ''coexists_with'' is the correct reading_relations value or if ''influences'' better captures the causal dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_structure, conceptual, 'Logical structure of coexistence vs foreclosure among Montevideo readings').

omega_variable(
    post_colonial_identity_lock,
    'Do post-colonial states that have internalized liberal democratic norms remain genuinely mobile or are they identity-locked into liberal frameworks by the institutional inheritance of colonial law and donor-state conditionality?',
    'Institutional history analysis: trace legal system origins and donor-state policy requirements for post-colonial states; assess whether alternative governance models are intellectually available to post-colonial political elites or systematized as deviance; compare reform trajectories for states with vs without donor conditionality; evaluate whether ''choosing'' liberal democracy is structurally constrained choice or genuine preference',
    'If identity-locked: post-colonial state victim set expands; suppression shifts upward (internalized constraint requires less external enforcement); classification may shift from tangled_rope to snare. If mobile: states have genuine agency in governance choices; tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_identity_lock, conceptual, 'Whether post-colonial liberal adoption is autonomous or identity-locked institutional inheritance').

omega_variable(
    humanitarian_intervention_legitimacy_boundary,
    'At what threshold of internal rights violation does the hybrid reading permit external intervention? Is the boundary objective, subjective, or does it dissolve upon examination?',
    'Comparative case analysis of interventions justified under hybrid reading: identify stated thresholds (genocide? systematic torture? electoral fraud? denial of political participation?); catalog cases that met stated thresholds but received no intervention; catalog cases that received intervention below stated thresholds; assess whether threshold statements post-hoc justify geopolitically motivated decisions',
    'If objective and applied consistently: hybrid reading legitimately constrains unilateral intervention. If subjective or inconsistently applied: mechanism is extraction (selective intervention justified through normative language); ε may shift toward 0.70 (snare territory). Determines whether humanitarian intervention coalition experiences genuine coordination or cover for power projection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_intervention_legitimacy_boundary, empirical, 'Whether humanitarian intervention thresholds are objective or post-hoc rationalizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mvd_hybrid_theater_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mvd_hybrid_theater_t25, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(mvd_hybrid_theater_t50, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(mvd_hybrid_extract_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mvd_hybrid_extract_t25, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(mvd_hybrid_extract_t50, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mvd_hybrid_suppress_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mvd_hybrid_suppress_t25, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(mvd_hybrid_suppress_t50, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_authority).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, secession_right_legitimacy).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, post_colonial_state_sovereignty).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, liberal_hegemony_legitimation).

% DUAL FORMULATION NOTE:
% The Montevideo statehood criteria kernel decomposes into three readings with distinct ε values. The declaratory reading (ε ≈ 0.15, Rope — pure coordination without extractive gatekeeping) permits any entity meeting objective criteria to become a state regardless of regime type. The constitutive reading (ε ≈ 0.65, Snare — international community decides, enabling hegemonic choice-making) permits only entities the liberal majority recognizes, with no principled constraint. The hybrid reading (ε ≈ 0.52, Tangled Rope — objective criteria plus normative gatekeeping) balances coordination (normative pressure improves governance in some cases) with extraction (selective recognition enforcement of liberal standards). These are distinct constraints with different effects on downstream constraints: humanitarian intervention authority is enabled by hybrid and constitutive readings but constrained by declaratory reading; secession legitimacy is high under declaratory, low under constitutive/hybrid; post-colonial state sovereignty is preserved under declaratory, conditional under hybrid/constitutive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerful, 0.15).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
