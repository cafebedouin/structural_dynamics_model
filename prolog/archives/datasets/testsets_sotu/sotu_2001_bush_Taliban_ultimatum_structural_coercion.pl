% ============================================================================
% CONSTRAINT STORY: sotu_2001_bush_Taliban_ultimatum_structural_coercion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2001_bush_Taliban_ultimatum_structural_coercion, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2001_bush_Taliban_ultimatum_structural_coercion
 *   human_readable: SOTU 2001 Bush Taliban Ultimatum: Structural Coercion via Military Threat
 *   domain: foreign_policy/counterterrorism/state_coercion
 *
 * SUMMARY:
 *   On September 20, 2001, President George W. Bush issued a public ultimatum
 *   to the Taliban regime during a Joint Session address to Congress. The
 *   demand was immediate and non-negotiable: surrender Osama bin Laden and
 *   all al Qaeda leaders for prosecution, close all training camps, provide
 *   the United States with full access to verify compliance, and return all
 *   al Qaeda financial assets within days. The constraint established that
 *   failure to comply would result in military invasion. This represents a
 *   structurally coercive constraint that collapses negotiation space and
 *   establishes military action as the enforcement default. The constraint
 *   benefits the U.S. counterterrorism apparatus and allied states through
 *   alignment of objectives and elimination of ambiguity; it imposes
 *   existential costs on the Taliban regime (loss of territorial control or
 *   regime collapse) and catastrophic costs on the Afghan civilian population
 *   (imminent war, displacement, infrastructure destruction). The
 *   constraint's theater ratio (0.35) is notably low — the demand is stark
 *   and unambiguous, leaving minimal room for performative negotiation or
 *   face-saving reinterpretation. The extractiveness (0.85) reflects the
 *   constraint's fundamental character: it offers the Taliban no viable exit
 *   path. Compliance means regime collapse via loss of state control;
 *   non-compliance triggers military action. Both paths destroy the Taliban's
 *   state capacity.
 *
 * KEY AGENTS:
 *   - United States Counterterrorism Apparatus: Primary beneficiary (institutional/arbitrage) — gains operational framework for military campaign, intelligence coordination, and enforcement authority
 *   - Allied States (UK, Australia, others): Secondary beneficiary (institutional/arbitrage) — gain alignment with U.S. counterterrorism objectives without bearing primary enforcement burden
 *   - Taliban Regime: Primary target/victim (powerless/trapped) — faces choice between regime collapse (compliance) or military invasion (non-compliance); no exit option
 *   - Afghanistan Civilian Population: Collateral victim (powerless/trapped) — trapped between Taliban's inability to comply and U.S. military response to non-compliance; bear extraction through imminent war
 *   - Pakistan State: Constrained secondary actor (moderate/constrained) — pressured to cease Taliban support while managing domestic Pashtun nationalist and Islamist constituencies
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as deliberately structured to foreclose negotiation and establish military enforcement as inevitable outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2001_bush_Taliban_ultimatum_structural_coercion, 0.85).
domain_priors:suppression_score(sotu_2001_bush_Taliban_ultimatum_structural_coercion, 0.92).
domain_priors:theater_ratio(sotu_2001_bush_Taliban_ultimatum_structural_coercion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2001_bush_Taliban_ultimatum_structural_coercion, extractiveness, 0.85).
narrative_ontology:constraint_metric(sotu_2001_bush_Taliban_ultimatum_structural_coercion, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(sotu_2001_bush_Taliban_ultimatum_structural_coercion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2001_bush_Taliban_ultimatum_structural_coercion, snare).
narrative_ontology:human_readable(sotu_2001_bush_Taliban_ultimatum_structural_coercion, "SOTU 2001 Bush Taliban Ultimatum: Structural Coercion via Military Threat").
narrative_ontology:topic_domain(sotu_2001_bush_Taliban_ultimatum_structural_coercion, "foreign_policy/counterterrorism/state_coercion").

domain_priors:requires_active_enforcement(sotu_2001_bush_Taliban_ultimatum_structural_coercion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2001_bush_Taliban_ultimatum_structural_coercion, united_states_counterterrorism_apparatus).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_Taliban_ultimatum_structural_coercion, allied_states_threatened_by_al_qaeda).
narrative_ontology:constraint_victim(sotu_2001_bush_Taliban_ultimatum_structural_coercion, taliban_regime_state_capacity).
narrative_ontology:constraint_victim(sotu_2001_bush_Taliban_ultimatum_structural_coercion, afghanistan_civilian_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TALIBAN REGIME (SNARE) — Faced with a take-it-or-face-war ultimatum with no path to renegotiation or compromise. Exit options eliminated: compliance means loss of state control over al Qaeda (political suicide within Pashtun nationalist base); non-compliance triggers military invasion with near-certainty. The constraint offers no exit, only a choice between two forms of regime destruction. Maximum suppression and extraction.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFGHANISTAN CIVILIAN POPULATION (SNARE) — Structurally trapped between the Taliban's inability to comply without regime collapse and the U.S. ultimatum that presupposes non-compliance will trigger military action. Civilians bear extraction through imminent war, displacement, and infrastructure destruction, with no exit option and no voice in the constraint's negotiation.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES COUNTERTERRORISM APPARATUS (ROPE) — Experiences the constraint as pure coordination: the ultimatum establishes the operational parameters for the counterterrorism campaign and aligns allied states around a unified demand structure. The constraint coordinates military planning, intelligence sharing, and alliance management with minimal perceived coercion overhead. Net beneficiary with arbitrage options: can escalate, negotiate, or declare victory through reframing.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED STATES (ROPE) — Perceive the constraint as coordination mechanism that aligns their counterterrorism objectives with U.S. military capacity. The ultimatum establishes shared demand (al Qaeda surrender) and shared enforcement (U.S. military action as default). These states benefit from the constraint without bearing direct extraction costs. Exit options include deferring to U.S. action or contributing token forces.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PAKISTAN STATE (TANGLED ROPE) — Constrained by U.S. pressure to cease support for Taliban while also facing domestic pressure from Pashtun nationalist and Islamist constituencies. Pakistan experiences both coordination (shared counterterrorism objectives with U.S.) and extraction (forced choice between U.S. alliance and domestic stability). Suppression is high due to resource constraints and political cost; exit options exist but at significant cost.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Sees the ultimatum as a structurally coercive constraint designed to eliminate negotiation pathways and establish military enforcement as the default mechanism. The constraint's architecture removes ambiguity and reduces the Taliban's degrees of freedom to near-zero. This is snare classification derived from suppression and extraction gates, not a false summit: the constraint genuinely operates through elimination of alternatives, not through natural law.
constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2001_bush_Taliban_ultimatum_structural_coercion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2001_bush_Taliban_ultimatum_structural_coercion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2001_bush_Taliban_ultimatum_structural_coercion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_2001_bush_Taliban_ultimatum_structural_coercion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. The ultimatum offers the Taliban regime no viable exit. Compliance requires immediate surrender of al Qaeda leadership and closure of training camps, which would constitute internal political suicide (loss of Pashtun nationalist base and Saudi financial support) and external political catastrophe (international humiliation and internal challenge to regime legitimacy). Non-compliance triggers military invasion with near-certainty given the public commitment and congressional authorization. The regime faces existential extraction through elimination of degrees of freedom. The extractiveness value reflects not the magnitude of physical harm but the structural elimination of negotiable options. Suppression (0.92): Extremely high. The constraint operates through simultaneous elimination of exit pathways: diplomatic channels are closed by the public ultimatum (no face-saving negotiation); internal power-sharing is threatened (compliance would trigger regime coup); external alliance options are eliminated (U.S. military capacity is overwhelming; no counterbalancing alliance exists). The Afghan civilian population faces suppression through proximity to imminent military action with no protection or exit pathway. Theater ratio (0.35): Low. The ultimatum is a starkly presented demand structure with minimal theatrical negotiation or ambiguity. The constraint does not rely on interpretive flexibility or performative face-saving — it establishes explicit conditions and explicit consequences. The low theater ratio reflects the constraint's operational character: it is designed to eliminate ambiguity and force binary choice, not to permit ongoing negotiation or reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The beneficiary (U.S. apparatus) sees coordination and alignment (Rope classification from institutional/arbitrage position). The victim (Taliban regime) sees pure extraction with no pathway (Snare classification from powerless/trapped position). The constrained secondary actor (Pakistan) sees mixed extraction and coordination (Tangled Rope classification from moderate/constrained position). The collateral victim (Afghan civilian population) sees pure extraction through imminent war (Snare classification from powerless/trapped position). The analytical observer recognizes the constraint's structural character: it is deliberately designed to eliminate negotiation space and establish military enforcement as inevitable. The constraint's architecture ensures that cooperation becomes impossible (tribal honor codes prohibit surrender of guests; Pashtun nationalism prohibits accepting foreign military diktat) while maintaining formal offer of negotiation, creating the appearance of choice while structurally foreclosing viable options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. The U.S. counterterrorism apparatus and allied states are beneficiaries with arbitrage exit options (can escalate, negotiate, or declare victory through reframing) — they derive low d values (approximately 0.05-0.15) indicating net benefit and high mobility. The Taliban regime is a victim with no exit options — d ≈ 0.95 (full target), producing maximum experienced extractiveness (f(d) ≈ 1.42). The Afghan civilian population also derives d ≈ 0.95 (powerless/trapped victims). Pakistan, as a constrained secondary actor with both coordination and extraction elements, derives d ≈ 0.60-0.70 (victim with some agency but significant barriers). The beneficiary-victim declarations (beneficiaries: U.S. counterterrorism apparatus, allied states; victims: Taliban regime state capacity, Afghan civilian population) establish the directional flow: extraction runs from the regime and civilian population toward the U.S.-allied coalition. The high suppression (0.92) enters the analysis through the constraint's structural design: by eliminating negotiation pathways, the constraint maximizes suppression independent of agent power level.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION GATES: Extractiveness ≥ 0.46 (0.85 ✓), Suppression ≥ 0.60 (0.92 ✓), Effective extraction χ ≥ 0.66 (computed as 0.85 × 1.42 × 1.0 ≈ 1.20, well above threshold ✓). The constraint unambiguously satisfies Snare classification. Mandatrophy is resolved through structural analysis: the constraint does not pretend to be anything other than coercive. Theater ratio (0.35) is low, indicating minimal performative negotiation masking the constraint's true function. The beneficiary-victim structure is transparent: the constraint benefits those with state capacity and military reach while imposing maximal costs on those without alternatives. No Mandatrophy ambiguity exists — the constraint's extractive character is its explicit purpose. The classification as Snare reflects the constraint's structural design: elimination of negotiation pathways, establishment of military enforcement as default, foreknowledge that the regime faces existential costs regardless of compliance. This is not a Tangled Rope (where genuine coordination benefits coexist with extraction) or a false Mountain (where extraction is naturalized as inevitable law). It is pure extraction mechanism, transparently designed to achieve military objectives through elimination of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taliban_internal_capacity_to_comply,
    'Did the Taliban regime possess sufficient internal control over al Qaeda leadership to materially comply with the surrender demand, or was the ultimatum predicated on an impossible condition?',
    'Post-facto analysis of Taliban-al Qaeda organizational structure (2001), leadership networks, and tribal authority patterns. Intelligence assessments from 2001 regarding regime''s actual capacity to apprehend and extradite bin Laden and al Qaeda leadership.',
    'If Taliban had compliance capacity: constraint is coercive but technically feasible; classification remains Snare but with acknowledgment that regime faced genuine choice (albeit catastrophic). If Taliban lacked capacity: ultimatum is structurally designed to fail, making military action inevitable; classification intensifies to pure extraction mechanism regardless of regime''s theoretical response options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taliban_internal_capacity_to_comply, empirical, 'Whether Taliban possessed internal capacity to comply with al Qaeda surrender demand').

omega_variable(
    path_dependent_escalation_inevitability,
    'Was military escalation a contingent consequence of the ultimatum structure, or a predetermined outcome that the ultimatum''s framing was designed to implement?',
    'Comparative counterfactual analysis: what would have occurred under alternative constraint structures (negotiated settlement, phased demands, third-party mediation)? Review of contemporaneous decision-making documents regarding escalation scenario planning.',
    'If escalation contingent: ultimatum is coercive Snare but not necessarily extractive conspiracy; impact judgment depends on whether alternative paths were foreclosed by design or circumstance. If escalation predetermined: ultimatum is theater masking predetermined military action; classification shifts toward identifying the constraint''s actual function (regime change / military projection) rather than stated objective (al Qaeda surrender).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependent_escalation_inevitability, empirical, 'Whether military escalation was contingent or predetermined under ultimatum structure').

omega_variable(
    extraction_beneficiary_definition,
    'Who is the actual primary beneficiary of the ultimatum constraint — counterterrorism objectives, regional geopolitical positioning, or institutional expansion of military/intelligence apparatus?',
    'Analysis of post-2001 outcomes: counterterrorism metric success vs regional strategic gains vs institutional budgets/authorities. Identification of which outcomes were primary design objectives vs secondary consequences.',
    'If counterterrorism primary: extraction flows to security apparatus as legitimate benefit for executing valid state function. If geopolitical positioning primary: extraction partially misdirected toward regional hegemony objectives. If institutional expansion primary: constraint functions partially as mechanism for capturing state resources and power, changing the beneficiary identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_definition, preference, 'Primary beneficiary identity and objective hierarchy of ultimatum constraint').

omega_variable(
    civilian_harm_suppression_mechanism,
    'Is the high suppression (0.92) attributable to the ultimatum''s structural design, or to the enforcement mechanism (military invasion) that follows from non-compliance?',
    'Decomposition of suppression sources: constraint-inherent (eliminates negotiation pathways) vs enforcement-dependent (military casualties/displacement). Counterfactual: would alternative constraint structures with same enforcement mechanism produce similar suppression?',
    'If suppression is constraint-inherent: the ultimatum itself is the suppressive mechanism; classification reflects the demand structure. If suppression is enforcement-dependent: the ultimatum is a coordination signal, but the military response carries the suppression cost; the constraint''s true classification may be rope (coordination) with the snare classification reflecting only the predicted enforcement outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_harm_suppression_mechanism, conceptual, 'Attribution of suppression to constraint structure vs enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2001_bush_Taliban_ultimatum_structural_coercion, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_taliban_theater_t0_prespeech, sotu_2001_bush_Taliban_ultimatum_structural_coercion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu_taliban_theater_t1_immediate, sotu_2001_bush_Taliban_ultimatum_structural_coercion, theater_ratio, 1, 0.35).
narrative_ontology:measurement(sotu_taliban_theater_t5_days, sotu_2001_bush_Taliban_ultimatum_structural_coercion, theater_ratio, 5, 0.28).

% Extraction over time
narrative_ontology:measurement(sotu_taliban_extractiveness_t0_prespeech, sotu_2001_bush_Taliban_ultimatum_structural_coercion, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(sotu_taliban_extractiveness_t1_immediate, sotu_2001_bush_Taliban_ultimatum_structural_coercion, base_extractiveness, 1, 0.85).
narrative_ontology:measurement(sotu_taliban_extractiveness_t5_days, sotu_2001_bush_Taliban_ultimatum_structural_coercion, base_extractiveness, 5, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2001_bush_Taliban_ultimatum_structural_coercion, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2001_bush_Taliban_ultimatum_structural_coercion, operation_enduring_freedom_military_enforcement).
narrative_ontology:affects_constraint(sotu_2001_bush_Taliban_ultimatum_structural_coercion, afghanistan_regime_change_structural_coercion).
narrative_ontology:affects_constraint(sotu_2001_bush_Taliban_ultimatum_structural_coercion, us_counter_terrorism_apparatus_expansion).

% DUAL FORMULATION NOTE:
% The SOTU 2001 ultimatum is upstream of Operation Enduring Freedom (military enforcement mechanism) and downstream of U.S. counterterrorism apparatus expansion post-9/11. The ultimatum represents the constraint structure that establishes the operational parameters for military escalation. The military invasion (OEF) is the enforcement mechanism that realizes the constraint's coercive intent when the Taliban regime fails to comply. Decomposition reflects ε-invariance principle: the ultimatum as negotiation structure has different extractiveness (0.85) than the subsequent military invasion, which shifts extraction mechanisms from structural coercion to direct kinetic enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2001_bush_Taliban_ultimatum_structural_coercion, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
