% ============================================================================
% CONSTRAINT STORY: sotu_1972_nixon_defense_below_human_resources
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1972_nixon_defense_below_human_resources, []).

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
 *   constraint_id: sotu_1972_nixon_defense_below_human_resources
 *   human_readable: Defense Spending Below Human Resources (1972 SOTU Threshold)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   In 1972, President Nixon's State of the Union address established a
 *   nominal constraint that defense spending must remain below human resource
 *   spending (social services, healthcare, education) for the first time in
 *   20 years. This represents a structural reallocation of budget priority:
 *   the constraint embeds a political claim (domestic welfare matters as much
 *   as military strength) into fiscal architecture rather than leaving the
 *   competition to annual congressional discretion. The mechanism is
 *   threshold-based: defense cannot exceed the human resources baseline
 *   without triggering implicit violation of stated national priorities. The
 *   constraint exhibits all six DR types from different perspectives,
 *   revealing how a single budget architecture can appear as coordination
 *   mechanism, extraction vehicle, degraded ritual, and natural law depending
 *   on the observer's structural position. The extractiveness value (0.52)
 *   reflects moderate asymmetry: military-industrial actors face real
 *   constraints on procurement and force structure, but can partially
 *   circumvent through baseline redefinition, threat inflation, and
 *   reclassification gaming. The theater ratio (0.65) shows that the
 *   constraint operates partly through actual budget reallocation and partly
 *   through performative priority signaling that allows both
 *   social-investment advocates and security hawks to claim victory.
 *
 * KEY AGENTS:
 *   - Defense Industry and Military Procurement: Primary victim (powerless/trapped in declining relative allocation; moderate/constrained for procurement complex) — faces extraction through spending ceiling with limited circumvention options
 *   - Civilian Populations and Social Service Sector: Primary beneficiary (institutional/arbitrage) — captures expanded resource allocation; experiences constraint as pure coordination without extraction
 *   - Congress and Legislative Branch: Organized/mobile — can amend constraint language, redefine baselines, create supplemental appropriations; experiences constraint as hybrid coordination and binding pressure
 *   - Military Personnel and Defense Sector Workers: Trapped within career path during relative contraction; experience maximum extraction without exit options
 *   - Cold War Strategic Doctrine Framework: Institutional/arbitrage — the operative doctrine persists independently of budget threshold; constraint is performative relative to actual strategic requirements
 *   - Analytical Observer (Civilizational): Risks naturalizing politically constructed constraint as immutable fiscal law of mature democracies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1972_nixon_defense_below_human_resources, 0.52).
domain_priors:suppression_score(sotu_1972_nixon_defense_below_human_resources, 0.48).
domain_priors:theater_ratio(sotu_1972_nixon_defense_below_human_resources, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1972_nixon_defense_below_human_resources, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1972_nixon_defense_below_human_resources, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1972_nixon_defense_below_human_resources, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1972_nixon_defense_below_human_resources, tangled_rope).
narrative_ontology:human_readable(sotu_1972_nixon_defense_below_human_resources, "Defense Spending Below Human Resources (1972 SOTU Threshold)").
narrative_ontology:topic_domain(sotu_1972_nixon_defense_below_human_resources, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1972_nixon_defense_below_human_resources).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_defense_below_human_resources, civilian_populations).
narrative_ontology:constraint_beneficiary(sotu_1972_nixon_defense_below_human_resources, social_service_sector).
narrative_ontology:constraint_victim(sotu_1972_nixon_defense_below_human_resources, military_procurement).
narrative_ontology:constraint_victim(sotu_1972_nixon_defense_below_human_resources, defense_contractors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENSE SECTOR (SNARE) — Trapped within military-industrial career paths during a structural spending contraction. Cannot exit without abandoning accumulated expertise and seniority. Faces maximum extraction through forced reallocation without compensation or transition support. The constraint locks them into a declining resource pool relative to civilian expansion.
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY PROCUREMENT COMPLEX (TANGLED ROPE) — Experiences extraction through spending ceiling but also coordination benefit from budgetary predictability and explicit priority ranking. Constrained by legislative architecture but can adapt through efficiency arguments and threat narratives. Asymmetric extraction constrained by ability to lobby and negotiate within defined budget envelope.
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOCIAL SERVICE BENEFICIARIES (ROPE) — Benefits directly from expanded resource allocation. Experiences constraint as pure coordination mechanism — establishes guaranteed floor for domestic investment without zero-sum annual battle. Net beneficiary with institutional flexibility (can redirect flows, adjust priorities, reallocate within expanded envelope).
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESS (TANGLED ROPE) — Organized actors with mobile exit options (can amend SOTU language, reclassify spending categories, redefine baselines). Experience constraint as hybrid: coordination benefit (removes annual zero-sum defense vs. human resources battle from floor debate) but also extraction (hands-tied against military-industrial lobby pressure to reclassify or redefine terms). Mobile because legislative discretion persists despite nominal constraint.
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COLD WAR STRATEGIC FRAMEWORK (PITON) — The constraint embeds a political statement (domestic welfare matters as much as military strength) into budget architecture, but the operative doctrine (nuclear deterrence requires spending level X, conventional forces require level Y) persists independently of the SOTU threshold. Theater ratio high because the constraint is performative relative to actual strategic requirements — allows administration to signal domestic priority shift while maintaining defense capacity through efficiency claims and threat inflation. The ritual persists because both hawks and social advocates can claim victory.
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical position, the constraint appears to enshrine an immutable structural truth: in mature democracies, welfare spending must eventually exceed military spending, reflecting demographic transition and resource scarcity. This view sees the threshold as capturing a natural law of fiscal architecture. However, beneficiary and victim declarations reveal this as false summit — the constraint is politically constructed to benefit specific actors (social service sectors) while extracting from others (military procurement). The 'natural law' framing naturalizes what is a deliberate policy choice.
constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1972_nixon_defense_below_human_resources_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1972_nixon_defense_below_human_resources, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1972_nixon_defense_below_human_resources, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1972_nixon_defense_below_human_resources, TR),
    TR >= 0.70.

:- end_tests(sotu_1972_nixon_defense_below_human_resources_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes real costs on military procurement (force structure planning constrained, acquisition delayed) but permits partial circumvention through baseline gaming, threat inflation, and reclassification. Extractiveness rises from 0.35 at initiation (new constraint, unclear enforcement) to 0.52 by 1982 (enforcement established through budget cycles) before oscillating downward during Cold War escalation periods (geopolitical pressure to circumvent). Suppression (0.48): Moderate. Barriers to circumvention exist (congressional coalition, public commitment to threshold) but are not total (legislative discretion persists, threat narratives provide escape valves, baseline definitions can be revised). Theater ratio (0.65): Moderate-high. The constraint operates partly through actual budget allocation but substantially through performative priority signaling. The increase from 0.40 (1972) to 0.65 (1982-1992) reflects growing reliance on threat narratives and rhetorical claims to justify spending patterns that nominally comply with the threshold while maintaining defense capacity. By 1992, the Cold War escalation provides cover for both increased defense spending (reframed as 'response to Soviet threat') and claims of social investment maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a sharp perspectival gap between beneficiaries and victims. Social service sectors see the constraint as enabling coordination (removing annual zero-sum battle, establishing guaranteed floor for domestic investment). Military procurement sees extraction (hard cap on budget regardless of strategic requirements). Military personnel see maximum extraction (trapped without exit). Congress sees hybrid constraint (can enforce or circumvent depending on political will). The Cold War strategic framework sees the constraint as irrelevant (doctrine determines spending, not budget threshold). The civilizational analytical observer risks seeing a natural law (mature democracies eventually prioritize welfare over military spending) when the structural data reveals deliberate political choice. This gap reveals that the constraint functions simultaneously as coordination mechanism (for beneficiaries with organized power), extraction vehicle (for victims with limited exit), degraded ritual (for doctrine-level actors for whom the constraint is performative), and false natural law (for observers who see budget architecture as reflecting immutable priorities).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the constraint's benefit and cost flow. Beneficiaries are civilian populations and social service sectors — they receive expanded allocation and experience the constraint as pure coordination (low d → negative or minimal χ). Victims are military procurement complex and defense workers — they bear extraction through constrained allocation (high d → high f(d) → high χ). The institutional positions differ in exit capacity: social service sectors have arbitrage options (can redirect within expanded envelope, can advocate for further expansion without facing extraction); military procurement is constrained (cannot easily relocate to civilian budget, cannot exit defense-sector path without career cost); military workers are trapped (accumulated expertise is defense-specific). The Congressional perspective is hybrid because legislative actors are both enforcers and circumventers — they have mobile exit options (can redefine baseline, create supplemental appropriations, reclassify spending) so their experienced extraction is lower than raw suppression would suggest. The piton classification emerges because the Cold War strategic doctrine (the operative framework determining what spending levels are 'necessary') is independent of the SOTU threshold — the doctrine persists, the constraint is grafted onto it, the constraint becomes performative relative to the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification captures the true hybrid structure: the constraint simultaneously coordinates domestic spending (removes annual budgetary competition, establishes stability) and extracts from military procurement (imposes real cost through spending ceiling). The mandatrophy is not 'which is it — coordination or extraction?' but 'for whom is it each?' The resolution requires understanding that the same constraint can appear as Rope (to beneficiaries with exit options), Snare (to trapped victims), Piton (to actors for whom it is performative), Mountain (to observers who naturalize it), and Tangled Rope (to the analytical engine that sees both coordination and extraction functions). The extractiveness value (0.52) is stable despite the perspectival multiplicity because extractiveness measures the constraint's structural properties (benefit/cost asymmetry, suppression level, enforcement difficulty), not its experiential properties from specific positions. The tangled_rope classification is correct because the constraint is genuinely hybrid: it solves a collective-action problem (defense-vs-human-resources annual battle is replaced by constitutional-level threshold) while imposing asymmetric costs (military loses relative priority without compensation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_definition_gaming,
    'What accounting mechanisms allow reclassification of defense spending as ''human resources'' or vice versa?',
    'Forensic budget analysis: track how military healthcare, military education (service academies), military housing, and nuclear weapons maintenance get classified across administrations post-1972. Identify reclassification patterns coinciding with spending pressure.',
    'If reclassification pathways are available: constraint becomes performative (theater_ratio rises toward 0.8+). If baseline definitions are locked: constraint is structurally binding and extraction is real (supports tangled_rope classification). Classification consequence: constraint moves toward Piton if gaming succeeds, remains Tangled Rope if baseline is enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_definition_gaming, empirical, 'Whether spending baselines can be gamed through reclassification').

omega_variable(
    enforcement_mechanism_clarity,
    'Is the constraint a hard legislative cap, a presidential policy statement, an informal norm, or a rhetorical commitment with no enforcement?',
    'Track instances where constraint is about to be violated: did violation occur without consequence (norm only), trigger legislative correction (soft constraint), or produce automatic budget cut (hard cap)? Historical record 1972-present.',
    'If hard cap: suppression and extraction real, classification as Tangled Rope or Snare stable. If norm only: constraint is performative theater, suppression is low (agents can circumvent), classification should move toward Piton. If rhetorical only: constraint is aspirational, extractiveness ≤ 0.25 (move toward Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_clarity, empirical, 'Whether constraint is enforced or merely rhetorical').

omega_variable(
    geopolitical_threat_inflation,
    'Does the constraint become circumvented through threat-narrative escalation that justifies ''emergency'' defense spending outside the baseline?',
    'Correlation analysis: instances of constraint pressure vs. subsequent threat assessments / emergency appropriations / supplemental funding. Timeline of Cold War crises and budget conflicts post-1972.',
    'If threat inflation is systematic: constraint is effectively weakened over time (extractiveness declines, theater_ratio rises). If threats are exogenous: constraint remains binding (supports Tangled Rope). Classification consequence: cycle of constraint-followed-by-circumvention-followed-by-recommitment suggests oscillating classification (Scaffold at initiation, Piton at degradation point, brief return to Tangled Rope during reform). Measurements should show this cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_threat_inflation, empirical, 'Whether geopolitical threat narratives circumvent the spending constraint').

omega_variable(
    coalition_durability,
    'Which political coalitions enforce the constraint, and how stable are they across administrations and electoral cycles?',
    'Congressional voting analysis on defense appropriations post-1972; track partisan splits on defense-vs-domestic spending; identify which administrations attempted to breach the constraint and which maintained it; measure coalition stability across 8-year and 20-year intervals.',
    'If coalition is durable (stable across administrations): constraint is structurally embedded (Tangled Rope stable, high enforcement). If coalition is fragile or shifts with administration: constraint is vulnerable to reclassification/circumvention (extractiveness declines, classification drifts toward Piton). If constraint is only durable under specific party control: it is a factional constraint, not a system-level one (revise beneficiary/victim framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_durability, empirical, 'Stability of political coalition maintaining the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1972_nixon_defense_below_human_resources, 1972, 1992).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(def_hr_tr_t0, sotu_1972_nixon_defense_below_human_resources, theater_ratio, 0, 0.4).
narrative_ontology:measurement(def_hr_tr_t5, sotu_1972_nixon_defense_below_human_resources, theater_ratio, 5, 0.58).
narrative_ontology:measurement(def_hr_tr_t10, sotu_1972_nixon_defense_below_human_resources, theater_ratio, 10, 0.65).
narrative_ontology:measurement(def_hr_tr_t15, sotu_1972_nixon_defense_below_human_resources, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(def_hr_be_t0, sotu_1972_nixon_defense_below_human_resources, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(def_hr_be_t5, sotu_1972_nixon_defense_below_human_resources, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(def_hr_be_t10, sotu_1972_nixon_defense_below_human_resources, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(def_hr_be_t15, sotu_1972_nixon_defense_below_human_resources, base_extractiveness, 15, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1972_nixon_defense_below_human_resources, resource_allocation).
narrative_ontology:affects_constraint(sotu_1972_nixon_defense_below_human_resources, cold_war_arms_race_escalation).
narrative_ontology:affects_constraint(sotu_1972_nixon_defense_below_human_resources, military_industrial_complex_rent_seeking).
narrative_ontology:affects_constraint(sotu_1972_nixon_defense_below_human_resources, social_safety_net_expansion_1970s).

% DUAL FORMULATION NOTE:
% The defense-below-human-resources threshold is downstream of Cold War strategic doctrine (which determines absolute spending levels) but structurally independent of specific military capabilities claims. The constraint redistributes a fixed-or-growing fiscal envelope between military and civilian uses; it does not determine the envelope size itself. Upstream constraints (arms race escalation, strategic doctrine requirements) can interact with the threshold through pressure to redefine baselines or create supplemental appropriations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1972_nixon_defense_below_human_resources, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
