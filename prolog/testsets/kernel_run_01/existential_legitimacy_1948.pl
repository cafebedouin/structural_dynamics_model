% ============================================================================
% CONSTRAINT STORY: existential_legitimacy_1948
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_existential_legitimacy_1948, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: existential_legitimacy_1948
 *   human_readable: 1948 Existential Legitimacy Claim (Israel)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The 1948 Israeli existential legitimacy claim represents a foundational
 *   constraint in territorial politics and represents one of the most
 *   analytically challenging cases for the Deferential Realism classification
 *   system. The constraint describes the legitimacy framework that Israeli
 *   state authority grounds in Holocaust remembrance, Jewish historical
 *   connection to the territory, and international law recognition (UN
 *   Partition Plan, Balfour Declaration). This claim has structured
 *   territorial, demographic, and political outcomes for 76 years
 *   (1948–2024). However, the constraint exhibits structural incoherence: the
 *   1948 state creation claim operates on an existential register (does
 *   Jewish sovereignty have a right to exist?), while the territorial
 *   occupation claim operates on a security/settlement register (does Israeli
 *   expansion within Palestinian territory serve legitimate state interest?).
 *   These operate on incommensurable temporal and conceptual registers,
 *   making single-framework resolution resistant. The source material
 *   suggests this may be two distinct kernels requiring separate constraint
 *   stories rather than one kernel read two ways. This story addresses only
 *   the 1948 existential legitimacy constraint; a parallel story addressing
 *   1967 territorial occupation would have different ε, different
 *   beneficiary/victim structure, and different temporal measurements. The
 *   1948 claim is the legitimacy foundation; the 1967 claim layers security
 *   justification onto that foundation. The analysis treats them as
 *   structurally distinct and focuses exclusively on the 1948 register.
 *
 * KEY AGENTS:
 *   - Israeli State Authority: Primary beneficiary (institutional/arbitrage) — the 1948 claim provides foundational legitimacy for state existence and territorial authority
 *   - Displaced Palestinian Populations: Primary victim (powerless/trapped) — bear full costs of displacement, legal status denial, and foreclosure of alternative territorial frameworks
 *   - Palestinian National Authority/Movement: Secondary victim (organized/constrained) — coordinates around territorial claim but faces asymmetric extraction through power disparity
 *   - Regional State System (Egypt, Jordan, Lebanon, Syria): Secondary victim (moderate/constrained) — coordinates around Palestinian recognition while bearing refugee and militarization costs
 *   - International Law / UN Framework: Institutional actor (institutional/arbitrage) — recognizes both Jewish right to sovereignty and Palestinian territorial rights without coherent resolution mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent 1948 institutional choices as inevitable laws of post-WWII state creation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(existential_legitimacy_1948, 0.68).
domain_priors:suppression_score(existential_legitimacy_1948, 0.72).
domain_priors:theater_ratio(existential_legitimacy_1948, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(existential_legitimacy_1948, extractiveness, 0.68).
narrative_ontology:constraint_metric(existential_legitimacy_1948, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(existential_legitimacy_1948, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(existential_legitimacy_1948, snare).
narrative_ontology:human_readable(existential_legitimacy_1948, "1948 Existential Legitimacy Claim (Israel)").
narrative_ontology:topic_domain(existential_legitimacy_1948, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(existential_legitimacy_1948).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(existential_legitimacy_1948, fixed_text).
narrative_ontology:cs_authority_grounding(existential_legitimacy_1948, lineage).
narrative_ontology:cs_interpretation_layer_present(existential_legitimacy_1948).
narrative_ontology:cs_reading_relation(existential_legitimacy_1948, palestinian_right_return_1948, forecloses).
narrative_ontology:cs_reading_relation(existential_legitimacy_1948, territorial_occupation_1967, influences).
narrative_ontology:cs_axiom(existential_legitimacy_1948, foundational, jewish_right_sovereignty_post_holocaust).
narrative_ontology:cs_axiom_status(jewish_right_sovereignty_post_holocaust, holdable).
narrative_ontology:cs_axiom(existential_legitimacy_1948, secondary, territorial_settlement_permanent_necessity).
narrative_ontology:cs_axiom_status(territorial_settlement_permanent_necessity, overridden).
narrative_ontology:cs_reference_frame(existential_legitimacy_1948, jewish_state_existence_legitimacy).
narrative_ontology:cs_drift_state(existential_legitimacy_1948, contemporary_settlement_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(existential_legitimacy_1948, israeli_state_institutional_authority).
narrative_ontology:constraint_victim(existential_legitimacy_1948, palestinian_territorial_claims).
narrative_ontology:constraint_victim(existential_legitimacy_1948, displaced_palestinian_populations).
narrative_ontology:constraint_victim(existential_legitimacy_1948, regional_territorial_settlement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN COMMUNITIES (SNARE) — Trapped by physical displacement, legal status denial, and absence of alternative territorial frameworks. No exit option available; all costs borne by this agent. The 1948 legitimacy claim forecloses Palestinian self-determination within the same territorial footprint. Maximum extraction relative to trapped status and biographical horizon.
constraint_indexing:constraint_classification(existential_legitimacy_1948, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN TERRITORIAL CLAIM / IDENTITY-LOCKED (SNARE) — The Palestinian claim to the territory is constituted through displacement narrative and national identity fusion. The claim cannot exit the territorial framework without dissolving Palestinian nationhood as culturally framed. Structurally mobile (other national frameworks exist) but identity-locked to this specific territorial claim. Biographical horizon shows rope-adjacent perception (recognizes constraint as changeable in principle); generational horizon shows snare (territorial claim feels immutable from within Palestinian identity frame).
constraint_indexing:constraint_classification(existential_legitimacy_1948, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI STATE AUTHORITY (ROPE) — Institutional beneficiary with arbitrage options. The 1948 claim provides foundational legitimacy for state existence and territorial authority. From the immediate/institutional perspective, this appears as coordination: the legitimacy claim solves the state-creation collective action problem. Net beneficiary; experiences the constraint as enabling rather than constraining. Arbitrage options available through diplomacy, regional settlement, security arrangements.
constraint_indexing:constraint_classification(existential_legitimacy_1948, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED PALESTINIAN RESISTANCE (TANGLED ROPE) — Organized agents (national authorities, civil society) face constrained exit: they coordinate around Palestinian statehood claim but also bear asymmetric extraction through military disparity, blockade, and settlement expansion. The constraint has coordination function (national unity around territorial claim) and extraction function (power asymmetry enables unilateral Israeli state expansion). Generational horizon shows mixed experience: genuine coordination within Palestinian framework, asymmetric extraction relative to Israeli institutional power.
constraint_indexing:constraint_classification(existential_legitimacy_1948, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGIONAL STATE SYSTEM (TANGLED ROPE) — Neighboring states (Egypt, Jordan, Lebanon, Syria) coordinate around recognition of Palestinian territorial claims while bearing extraction costs through refugee populations, military expenditure, and geopolitical constraint. The constraint coordinates regional opposition to Israeli expansion and coordinates refugee legal status; it extracts through requiring perpetual militarization and preventing alternative settlement arrangements.
constraint_indexing:constraint_classification(existential_legitimacy_1948, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — The civilizational/universal perspective risks viewing the 1948 legitimacy claim as a natural law: territorial occupation is described as inevitable given the Holocaust, Jewish historical claims, and international law protocols. From this view, the constraint appears immutable — a structural feature of post-WWII state creation. However, this perspective instantiates the false summit: the naturalization of a contingent historical event (1948 state declaration) and a specific institutional choice (Law of Return + settlement policy) as inevitable laws of territorial politics. The engine will detect this as FSM candidate.
constraint_indexing:constraint_classification(existential_legitimacy_1948, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(existential_legitimacy_1948_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(existential_legitimacy_1948, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(existential_legitimacy_1948, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(existential_legitimacy_1948, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(existential_legitimacy_1948_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The 1948 claim forecloses Palestinian territorial alternatives in the same geographic footprint, creating asymmetric benefits (Israeli state existence) and costs (Palestinian displacement, legal status denial). Measurement trajectory shows extractiveness increasing over the interval (0.52 → 0.68) as settlement expansion layers additional territorial claims onto the foundational 1948 legitimacy. The extractiveness is not at maximum (0.95) because the constraint does solve a genuine collective action problem (Israeli state creation and institutional coordination), which maintains some rope-like coordination function. Theater ratio (0.58): Moderate-high. The 1948 claim is presented with partial theatrical content. The historical narrative (Holocaust necessity, Jewish connection, international legal recognition) has genuine weight, but the claim's application to prevent Palestinian settlement and to justify territorial expansion beyond 1948 boundaries contains significant performative elements. Measurement trajectory shows theater increasing (0.38 → 0.62) as the temporal distance from 1948 grows and institutional justification layers thicken. Suppression (0.72): High. The constraint operates through substantial suppression mechanisms: forced displacement, legal status denial, military enforcement, settlement expansion, and discursive foreclosure of alternative Palestinian claims. Palestinian agents lack material, legal, and epistemic pathways to exit or contest the claim. The suppression is structural (not merely internalized), enforced through state institutions and military apparatus.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence, indicating high structural conflict. Israeli institutional perspective sees rope (coordination mechanism solving state creation problem). Palestinian trapped perspective sees snare (asymmetric extraction with no exit). Palestinian identity-locked perspective sees snare at generational horizon (identity constituted through territorial claim) but rope-adjacent possibility at biographical horizon if identity frame shifts. Regional state perspective sees tangled rope (coordination around Palestinian support, extraction through militarization burden). Organized Palestinian perspective sees tangled rope (genuine national coordination, asymmetric extraction from power disparity). The analytical perspective risks mountain classification (naturalizing contingency as inevitable necessity), but the structural data (beneficiaries present, high suppression, high extractiveness) contradicts the mountain gates and indicates false summit. The perspectival gaps reveal that no single type captures the constraint's structure — the presheaf over all observations is the accurate representation.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) for each agent is derived from their structural position relative to the constraint: beneficiary status (d → 0), victim status (d → 1), power level, and exit options. Israeli institutional authority benefits from the claim with arbitrage options (diplomacy, settlement negotiation) — derived d ≈ 0.08–0.15, producing negative effective extraction (constraint subsidizes this agent). Displaced Palestinian populations are victims with no exit (trapped) — derived d ≈ 0.95, producing maximum f(d) ≈ 1.42. Palestinian organized resistance has victim status but organized power enabling some coalition formation — derived d ≈ 0.65–0.75 (moderate victim position). Regional states are secondary victims with constrained options — derived d ≈ 0.60–0.70. The analytical observer analyzing across all positions sees the structure but operates in a frame that risks naturalizing the claim as inevitable — this instantiates the oracle gap (Theorem 4): the analyst's own civilizational/analytical context makes it difficult to perceive the contingency of the institutional choice underlying the claim. No directionality overrides are required; the derived values accurately reflect structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH DUAL-KERNEL HYPOTHESIS: This constraint resolves the mandatrophy through structural decomposition. The apparent contradiction (how can the claim be both natural law AND unjust extraction?) dissolves when recognizing that 1948 existential legitimacy and 1967 territorial occupation are two structurally distinct kernels. This story addresses only the 1948 kernel: state creation legitimacy. The 1948 claim (Jewish sovereignty's right to exist) has genuine coordination function and can coherently classify as Rope when viewed from the beneficiary's immediate/institutional perspective. However, when extended to justify permanent territorial control and settlement expansion, the claim becomes a Snare or Tangled Rope — this extension belongs to the 1967 occupation constraint, not the 1948 existential legitimacy constraint. The false summit detection occurs not because 1948 is illegitimate, but because the analytical observer conflates 1948 (state creation) with 1967 (occupation) and naturalizes both as inevitable. The engine's FSM signature correctly identifies beneficiaries (Israeli state authority) and triggers reclassification from mountain to snare/tangled_rope, revealing that the naturalness framing prevents examination of the structural choice to expand beyond 1948 boundaries. The mandatrophy is resolved by: (1) distinguishing two kernels, (2) recognizing that 1948 has genuine coordination function (rope-legitimate), (3) recognizing that extension to 1967+ territory becomes extraction (snare/tangled_rope-illegitimate), (4) noting that naturalizing both registers as inevitable is the analytical failure. The framework's ability to emit tangled_rope from multiple perspectives correctly captures the hybrid nature of the actual political situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_kernel_boundary,
    'Is the 1948 existential legitimacy claim a single kernel with competing readings, or two distinct kernels (1948 state creation vs. 1967 occupation) requiring separate constraint stories?',
    'Structural analysis: if the claims operate on the same temporal and conceptual register and could be unified by a single adjudicating authority, they are sibling readings of one kernel. If they operate on incommensurable registers (existential vs. territorial, state creation vs. settlement), they are distinct kernels requiring separate stories. Examine whether Israeli authority can coherently hold both 1948 justification (Jewish historical connection) and 1967 justification (security necessity) within a single legitimacy framework, or whether they require different epistemic commitments.',
    'If one kernel: this story must address both readings; sibling reading story required. If two kernels: this story addresses 1948 only; separate story for 1967 occupation constraint with different ε and different beneficiary/victim structure. Classification outcome depends on this decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_kernel_boundary, conceptual, 'Whether 1948 claim and 1967 occupation are one kernel or two').

omega_variable(
    extraction_vs_coordination_boundary,
    'To what extent does the 1948 legitimacy claim function as coordination mechanism (solving state creation collective action problem) versus extraction mechanism (preventing alternative Palestinian territorial solution)?',
    'Counterfactual analysis: absent the 1948 claim, what collective action problems remain unsolved? (True coordination.) What Palestinian alternatives become possible? (Extraction mechanism now visible.) Distinguish between the claim''s role in internal Israeli state-building (coordination) and its role in preventing Palestinian settlement (extraction). Measure by comparing Israeli institutional cohesion costs with Palestinian exit barrier costs.',
    'Higher coordination ratio: classification shifts toward Rope from some perspectives. Higher extraction ratio: classification shifts toward Snare. Boundary case (0.40–0.60): Tangled Rope confirmed. This omega directly determines whether the constraint is pure extraction or hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Ratio of coordination to extraction function in 1948 claim').

omega_variable(
    identity_lock_dissolution_cost,
    'What fraction of Palestinian resistance to alternative territorial settlements is structural (material barriers, power asymmetry) versus identity-constituted (Palestinian nationhood inherently linked to specific territorial claim)?',
    'Comparative analysis: Palestinian positions in settlements or diaspora that accept territorial alternatives vs. those that reject them. Track whether rejection is motivated by negotiation demand (structural exit cost) or by identity frame incompatibility. Examine Palestinian civic identity construction in non-territorial contexts (UAE, diaspora, post-conflict scenarios) to identify whether Palestinian identity can coherently exist absent this specific territorial claim.',
    'If primarily structural: all Palestinian perspectives should use `trapped` or `constrained`, not `identity_locked`. If primarily identity-constituted: `identity_locked` on biographical horizon reflects perceptual frame dependency, and rope-like classification becomes possible if identity frame shifts. This omega determines whether Palestinian perspectives are trapped (no solution) or identity-locked-to-rope (solution possible via identity reframing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_cost, empirical, 'Proportion of Palestinian resistance that is identity-locked vs. structural').

omega_variable(
    israeli_state_beneficiary_stability,
    'Does Israeli state institutional authority genuinely experience the 1948 legitimacy claim as enabling (rope-type coordination), or is the claim''s persistence driven by path dependency and cannot be reconsidered without existential threat?',
    'Analysis of Israeli institutional debate: can the 1948 claim be questioned within Israeli civil society without institutional rejection? Compare stability of claim in different institutional contexts (military, judiciary, legislative, civil society). Identify whether claim revision would require state reorganization or merely policy adjustment. Track institutional coherence costs of maintaining claim under counterfactual scenarios (e.g., if 1948 boundaries were accepted without settlement expansion, would Israeli state stability change?).',
    'If enabling (genuine rope): Israeli perspective is accurately classified as institutional/arbitrage/rope. If path-dependent (pseudo-rope): Israeli perspective is actually tangled_rope or snare depending on whether state requires constant enforcement (tangled_rope) or has lost choice capacity (snare). This determines whether Israeli beneficiary position is genuinely optional or locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(israeli_state_beneficiary_stability, empirical, 'Whether Israeli state genuinely experiences 1948 claim as enabling or as locked').

omega_variable(
    false_summit_natural_law_status,
    'Is the 1948 claim presented as natural law (historical inevitability, post-Holocaust necessity) a genuine structural immutability or a naturalized contingent institutional choice?',
    'Historical counterfactual: what specific institutional choices made by Israeli and international authorities in 1947-1950 were contingent (could have been chosen differently) versus which were structurally inevitable? Compare to other state creations (India/Pakistan, Germany post-WWII, Korea) to identify whether the Israeli case shows structural features unique to those states or merely contingent policy choices. Examine whether the claim''s ''naturalness'' derives from the actual structures of state creation or from retroactive narrative construction.',
    'If genuinely structural: mountain classification is accurate, FSM does not fire. If naturalized contingency: FSM fires, reclassification to tangled_rope or snare, revealing that the ''inevitable law of nature'' framing is constructed to prevent examination of beneficiary/victim structure. This omega determines the engine''s false summit detector output.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether 1948 necessity is structural or naturalized contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(existential_legitimacy_1948, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exist_theater_t0_state_creation, existential_legitimacy_1948, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exist_theater_t25_settlement_justification, existential_legitimacy_1948, theater_ratio, 25, 0.48).
narrative_ontology:measurement(exist_theater_t50_discourse_drift, existential_legitimacy_1948, theater_ratio, 50, 0.62).
narrative_ontology:measurement(exist_theater_t76_contemporary, existential_legitimacy_1948, theater_ratio, 76, 0.58).

% Extraction over time
narrative_ontology:measurement(exist_extractiveness_t0_1948, existential_legitimacy_1948, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(exist_extractiveness_t25_1967_escalation, existential_legitimacy_1948, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(exist_extractiveness_t50_settlement_expansion, existential_legitimacy_1948, base_extractiveness, 50, 0.71).
narrative_ontology:measurement(exist_extractiveness_t76_contemporary, existential_legitimacy_1948, base_extractiveness, 76, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(existential_legitimacy_1948, identity_coordination).
narrative_ontology:affects_constraint(existential_legitimacy_1948, territorial_occupation_1967).
narrative_ontology:affects_constraint(existential_legitimacy_1948, palestinian_displacement_nakba).
narrative_ontology:affects_constraint(existential_legitimacy_1948, settlement_expansion_mechanism).

% DUAL FORMULATION NOTE:
% The 1948 existential legitimacy claim is structurally upstream of the 1967 territorial occupation constraint. Both operate on the same territory but with different temporal registers and different ε values. This story addresses the state-creation legitimacy claim (ε=0.68, snare); a parallel story addresses the occupation claim with expected higher ε (0.75+) due to extended territorial expansion justified by security. The two constraints are linked because the 1948 claim provides the legitimacy foundation that subsequent occupation/settlement policies appeal to for justification. However, they are structurally distinct: the 1948 claim solves a genuine state-creation collective action problem (rope-like coordination function), while the 1967+ extension becomes pure extraction (snare/tangled_rope) if treated as permanent territorial claim. Decomposition is required because measuring the combined 1948+1967 claim produces internal contradiction in ε valuation (impossible to get single stable value). Separate them: 1948 state creation alone (this story), 1967 occupation alone (separate story). Network link enables the engine to model how legitimacy claims cascade: the 1948 foundation is cited to justify 1967 expansion, but the two operate on different registers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
