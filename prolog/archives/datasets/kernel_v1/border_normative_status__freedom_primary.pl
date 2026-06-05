% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Enforcement as Rights Violation (Freedom Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   Under the freedom-primary reading, border enforcement constitutes a prima
 *   facie violation of a fundamental human right. The constraint operates by
 *   restricting access to territory based on origin, citizenship, or other
 *   immutable characteristics, thereby suppressing the freedom of movement
 *   that the reading treats as inviolable. Exclusion is permissible only
 *   under extraordinary justification — defined narrowly as imminent physical
 *   threat or genuine humanitarian emergency, not routine labor market
 *   protection or cultural preservation. From this reading's perspective, the
 *   current global border regime extracts significant value for native
 *   labor-protection coalitions, state bureaucratic apparatus, and
 *   nationalist elites while imposing maximum suppression on excluded
 *   migrants and displaced workers who bear the constraints of both border
 *   restrictions and labor market stratification. The constraint's theater
 *   ratio (0.65) reflects that much border enforcement rhetoric employs
 *   security and cultural-preservation narratives that mask labor-market and
 *   institutional-expansion functions. Migration threat narratives (invoked
 *   to justify enforcement escalation) serve as cover story for protectionist
 *   redistribution. The reading establishes that excluded migrants exit the
 *   victim set (their exclusion is not justified under freedom-primary
 *   axioms) while displaced domestic workers enter the victim set (bearing
 *   suppression costs from the labor-market distortion that border
 *   restrictions create).
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victim (powerless/trapped) — face insurmountable legal barriers, armed enforcement, visa denial; exercise of fundamental right is suppressed; no exit option
 *   - Displaced Domestic Workers: Secondary victim (moderate/constrained) — benefit from labor-supply constraints on wages but constrained from accessing higher-wage markets; bear suppression cost from both wage compression and immobility
 *   - Labor-Protectionist Coalition: Primary beneficiary (institutional/arbitrage) — unions, import-competing industries, wage-protection advocates; perceive border enforcement as solving labor-supply coordination problem; net beneficiary with high arbitrage exit options
 *   - State Bureaucratic Apparatus: Secondary beneficiary (institutional/constrained) — border enforcement agencies, immigration authorities; expand institutional authority and budgets through crisis narratives; embedded in state apparatus with constrained exit
 *   - Rights-Based Migration Coalition: Organized challenger (organized/mobile) — international human rights bodies, cosmopolitan advocates, subnational open-borders movements; see constraint as temporary institutional arrangement with sunset; possess agency and structural mobility
 *   - Analytical Observer: Cosmopolitan universalist (analytical/analytical) — views freedom of movement as fundamental right analogous to physical law; risks naturalizing a contestable normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.58).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Enforcement as Rights Violation (Freedom Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '24b7314a-9ce8-4889-b703-b9ac4dc98fa4').
narrative_ontology:cs_kernel_codification('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', distributed).
narrative_ontology:cs_authority_grounding('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', distributed).
narrative_ontology:cs_reading_relation('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', foundational, freedom_of_movement_is_fundamental).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', freedom_of_movement_is_fundamental, deontological).
narrative_ontology:cs_axiom('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', foundational, border_exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(border_exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', border_exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', cosmopolitan_universalism).
narrative_ontology:cs_drift_state('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('24b7314a-9ce8-4889-b703-b9ac4dc98fa4', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, native_labor_protectionists).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, state_bureaucratic_apparatus).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, aspirational_refugees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces insurmountable legal barriers to exit home territory; fundamental freedom is suppressed via armed borders, visa denials, and detention. No alternative pathway. Maximum experienced extraction: the constraint physically prevents the exercise of a claimed fundamental right. Bears full cost; no exit option; no meaningful benefit from the constraint.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED DOMESTIC WORKER (TANGLED ROPE) — Experiences border enforcement as both coordination mechanism (labor market stabilization, wage protection norms) and extraction (wages suppressed by labor supply reduction, constrained by inability to migrate to higher-wage markets). The constraint provides some protection from migration-induced wage pressure while simultaneously trapping workers in lower-wage regions. Mixed extraction and benefit; high suppression cost but some coordination function.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR-PROTECTIONIST COALITION (ROPE) — Unions, import-competing industries, and wage-protection advocates see border enforcement as legitimate coordination: managing labor supply stabilizes wages and working conditions for existing citizens. This perspective frames the constraint as solving a genuine collective action problem (wage pressure from global labor arbitrage). Net beneficiary position with arbitrage exit options; experiences low effective extraction because the coordination function is perceived as genuine.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE BUREAUCRATIC APPARATUS (TANGLED ROPE) — Border enforcement agencies experience the constraint as both coordination (maintaining territorial integrity, managing population flows, allocating public services) and extraction mechanism (agencies expand enforcement capacity, budgets, and institutional authority through crisis narratives around migration threat). Active enforcement requirement generates institutional dependency and rent-seeking; state apparatus benefits from crisis while also bearing coordination burden.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHTS-BASED MIGRATION COALITION (SCAFFOLD) — International human rights organizations, cosmopolitan advocates, and subnational open-borders movements see border restrictions as a temporary institutional arrangement with a sunset: transnational labor mobility, open-border zones (Schengen-like), refugee resettlement quotas, and climate migration frameworks are building alternative pathways. This perspective has agency and sees a structural exit path toward greater mobility freedoms. Theater low because the coalition operates through direct challenge to the constraint's legitimacy, not through accommodation.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist/cosmopolitan analytical frame, freedom of movement is a fundamental human right akin to physical laws: restrictions require extraordinary justification just as exceptions to conservation principles require extraordinary evidence. This perspective risks treating a contestable normative commitment (freedom primacy) as if it were an immutable principle. The engine will flag this as a potential false summit — the 'naturalness' of the freedom principle depends on accepting a specific reading of human rights, not on neutral observation.
constraint_indexing:constraint_classification(border_normative_status__freedom_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_normative_status__freedom_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_normative_status__freedom_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value for beneficiaries (labor protection, institutional expansion, nationalist political capital) while imposing severe suppression on excluded populations. The extractiveness is not as high as pure slavery (0.85+) because some agents (domestic workers, state apparatus) experience mixed benefits alongside extraction. Suppression (0.72): High. The constraint operates through armed enforcement, visa denial, detention, deportation — structural mechanisms that prevent exit with minimal alternatives. Suppression is not total (some migration pathways exist) but affects the vast majority of would-be migrants. Theater ratio (0.65): Moderate-high. Border enforcement and immigration policy employ substantial performance elements: security threat narratives that exceed actual risk, cultural-preservation rhetoric that masks labor-market protection, bureaucratic complexity that obscures redistributive functions. However, the underlying suppression mechanisms (armed forces, detention infrastructure) have genuine coercive function beyond pure theater. The constraint is snare-classified because: (1) extractiveness > 0.46, (2) suppression > 0.60, and (3) the primary mechanism (border enforcement) suppresses alternatives rather than coordinating multiple interests. Beneficiaries are few and concentrated (labor protectionists, state apparatus); victims are numerous and dispersed (excluded migrants, displaced workers).
 *
 * PERSPECTIVAL GAP:
 *   The readings generate fundamental perspectival disagreement on the constraint's legitimacy. Under freedom-primary, border enforcement is a rights violation requiring extraordinary justification (snare classification for the excluded migrant). Under qualified_sovereignty, border enforcement is legitimate governance requiring proportionality review (likely tangled_rope). Under sovereignty_primary, border enforcement is a foundational instrument of self-determination (likely rope). The same structural mechanism (border restriction) classifies as snare, tangled_rope, and rope depending on which reading anchors the perspective. The freedom-primary reading asserts that no agent at any power level can legitimately perceive the constraint as a coordination mechanism — it is extraction from all perspectives. The sovereignty_primary reading asserts that states perceive the constraint as coordination, not extraction. The qualified_sovereignty reading splits the difference: some restrictions are coordination, others are extraction, depending on proportionality. These are not merely different interpretations of shared facts; they rest on incommensurable normative commitments about the foundation of legitimacy (individual rights vs. state authority vs. balanced governance).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural relationship to the freedom-primary reading's normative framework. Excluded migrants experience maximum extraction (d ≈ 0.95) because they are total victims of a constraint that denies them a claimed fundamental right, with no exit option and no counter-benefit. Displaced workers experience high extraction (d ≈ 0.70) because they bear suppression from labor-market stratification, though some benefit from wage protection. Labor-protectionists experience low extraction (d ≈ 0.20) because they are beneficiaries with arbitrage exit options — they can relocate abroad or shift economic sectors if border restrictions relax, but they choose to stay because the restriction benefits them. State apparatus experiences moderate extraction (d ≈ 0.45) because it is a secondary beneficiary (institutional expansion) but also constrained by resource requirements and reputational costs of enforcement. The rights-based coalition experiences moderate extraction (d ≈ 0.55) because it is an organized challenger with agency and mobility but faces institutional resistance and budget constraints. The analytical observer experiences high extraction (d ≈ 0.75) because committing to the freedom-primary reading means viewing border enforcement as rights violation — full target of the constraint's suppressive function. Each d value feeds into the sigmoid f(d) to produce the effective extractiveness chi experienced by that agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy appears at the border between freedom-primary (snare) and qualified_sovereignty (tangled_rope or rope). The freedom-primary reading sees the constraint as pure extraction with no genuine coordination function: the labor-market benefit to unions is parasitic on rights violation. The qualified_sovereignty reading sees genuine coordination: border-regulated labor supply has real effects on wage stability and working conditions; this function can coexist with human rights obligations if proportionality is maintained. The engine's mandatrophy resolution distinguishes these as two readings of a single contested kernel, not two observations of a single constraint. The question 'which is correct?' is answered by saying 'both are defensible under their own axiom foundations, but they rest on incommensurable normative commitments.' The freedom-primary reading resolves the mandatrophy by asserting that only one normative foundation (freedom-primary) is legitimate; the other readings are sophistications that obscure rights violation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_versus_collective_self_determination,
    'Does individual freedom of movement logically entail that states cannot legitimately restrict membership through border control, or can collective self-determination coexist with individual movement rights?',
    'Philosophical analysis of competing normative foundations: cosmopolitan universalism vs. communitarianism vs. liberal contractarianism. Historical examination of whether open-border and closed-border systems have coexisted in theory without logical foreclosure.',
    'If freedom entails foreclosure of state self-determination: this reading forecloses the sovereignty_primary reading (Type I omega — binary outcome). If coexistence is possible: this reading coexists with sovereignty_primary, and the contest is empirical/practical rather than logical (Type II omega — contestable application).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_versus_collective_self_determination, conceptual, 'Logical relationship between freedom-of-movement and state self-determination principles').

omega_variable(
    extraordinary_justification_threshold,
    'What counts as ''extraordinary justification'' for border exclusion under this reading? Who adjudicates the threshold?',
    'Specification of justification criteria (genuine security threat, epidemiological emergency, capacity constraints, etc.) and specification of adjudicative authority (international courts, human rights bodies, democratic deliberation within receiving states). Empirical comparison of historical exclusion justifications against stated criteria.',
    'If threshold is very high (imminent physical threat only): most current border restrictions become unjustifiable (snare classification reinforced). If threshold is permissive (cultural preservation, fiscal sustainability): constraint shifts toward tangled_rope or qualified_sovereignty territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, empirical, 'Definition and adjudication of extraordinary-justification standard for border exclusion').

omega_variable(
    displaced_worker_causation_chain,
    'To what degree are domestic wage suppression and working-condition degradation causally attributable to border-restricted migration vs. other factors (automation, offshoring, union decline, macroeconomic policy)?',
    'Econometric analysis of wage impacts of migration restrictions; comparison of wage trends in high-restriction vs. low-restriction regimes controlling for other factors; natural experiments from policy changes (Schengen expansion, visa reforms).',
    'If border restrictions account for <10% of wage variance: domestic workers are not primary victims of this constraint (victim set contracts). If >30%: constraint''s extraction from domestic workers is structurally significant (snare classification for this agent reinforced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_causation_chain, empirical, 'Causal role of border restrictions in domestic wage suppression').

omega_variable(
    sovereignty_primary_logical_relation,
    'Does this reading (freedom primary) logically foreclose the sovereignty_primary reading (states have foundational authority to exclude), or do they represent incommensurable normative commitments that coexist in different political frameworks?',
    'Philosophical analysis of foundational premises: does freedom-of-movement entail that any border-based exclusion is illegitimate, such that sovereignty-based exclusion authority is conceptually incoherent? Or do the readings rest on different value orderings that can coexist?',
    'If forecast forecloses: reading_relations entry for sovereignty_primary should be ''forecloses''. If coexistent: reading_relations should be ''coexists_with''. This determines whether the kernel contest is a binary logical dispute or an ongoing contestation between incommensurable frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primary_logical_relation, conceptual, 'Logical foreclosure relation to sovereignty_primary reading').

omega_variable(
    qualified_sovereignty_incorporation,
    'Does the qualified_sovereignty reading (proportionality + human rights obligations) represent a middle-ground compromise incorporating this reading''s freedom commitment, or a fundamentally different framework that subordinates freedom to state discretion?',
    'Analysis of how qualified_sovereignty applies the freedom principle in practice: if it uses freedom of movement as a floor constraint on state discretion, it incorporates this reading''s core axiom (freedom is fundamental). If freedom is merely one factor among others, it subordinates this reading.',
    'If incorporation: reading_relations to qualified_sovereignty is ''influences'' (this reading''s axioms shape downstream constraints). If subordination: reading_relations is ''coexists_with'' (fundamental disagreement on axiom priority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualified_sovereignty_incorporation, conceptual, 'Whether qualified_sovereignty incorporates or subordinates freedom-primary axioms').

omega_variable(
    measuring_suppression_in_border_constraint,
    'Is border suppression best measured by enforcement intensity (guards, walls, visa rejections), by outcome (percentage of would-be migrants prevented), or by subjective impact (aspiration gaps among excluded populations)?',
    'Comparison of suppression metrics: enforcement infrastructure data vs. counterfactual migration modeling vs. survey data on migration aspiration and reported obstacles. Analysis of whether different measures produce concordant rankings across time periods and jurisdictions.',
    'If enforcement intensity dominates: suppression level is stable or rising (consistent with measurements showing persistent enforcement burden). If outcome dominates: suppression depends on migrant volume pressures (variable with economic cycles). If subjective impact dominates: suppression may rise even if enforcement relaxes (if aspirations rise faster).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measuring_suppression_in_border_constraint, empirical, 'Measurement basis for border constraint suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_freedom_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.5).
narrative_ontology:measurement(border_freedom_tr_t15, border_normative_status__freedom_primary, theater_ratio, 15, 0.58).
narrative_ontology:measurement(border_freedom_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(border_freedom_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(border_freedom_be_t15, border_normative_status__freedom_primary, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(border_freedom_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(border_freedom_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(border_freedom_su_t15, border_normative_status__freedom_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(border_freedom_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, resource_allocation).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, labor_market_segmentation_migration).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, refugee_status_determination).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel decomposes into three constraint stories: freedom_primary (this file, ε=0.58, snare), qualified_sovereignty (ε=0.45, tangled_rope), and sovereignty_primary (ε=0.30, rope). Each reading instantiates a different extractiveness value reflecting its different normative framework and its different assessment of what counts as extraction vs. coordination. The three stories form a constraint family linked by network edges. The freedom_primary reading has the highest extractiveness because it treats border enforcement as unjustified suppression; the sovereignty_primary reading has the lowest because it treats enforcement as legitimate coordination. The qualified_sovereignty reading falls between, treating some enforcement as justified and some as extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
