% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Combatant Status Definition (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   The state-centric definition of combatant status is a reading of the
 *   contested kernel: 'Who qualifies for prisoner-of-war protections and
 *   combatant immunity under international humanitarian law?' This reading
 *   instantiates the position that only formally organized state militaries
 *   meeting Article 4 Geneva Convention criteria (responsible command, fixed
 *   emblem, uniform, open carrying of arms) can qualify as lawful combatants
 *   entitled to POW protections. Non-state armed actors—insurgents,
 *   liberation movements, proxy forces, private military contractors
 *   operating for non-state entities—are categorically excluded from
 *   combatant status and therefore from POW immunity. Captured non-state
 *   fighters face criminal prosecution under domestic law rather than
 *   protected prisoner status under international humanitarian law. This
 *   reading coexists with two sibling readings: the
 *   national_liberation_reading, which argues that national liberation
 *   movements have status-granting authority equivalent to states under
 *   Article 1 of the Additional Protocols, and the
 *   functional_protection_reading, which argues that combatant status should
 *   track functional military characteristics rather than organizational
 *   origin. The state-centric reading is the dominant legal position in
 *   positive international law (reflected in 1949/1977 Geneva Conventions and
 *   their Additional Protocols), but its applicability has become
 *   increasingly strained as contemporary armed conflict has shifted toward
 *   non-state actors. The constraint exhibits the classic Tangled Rope
 *   pattern: it combines genuine coordination function (establishing clear
 *   rules for treatment of captured soldiers, preventing escalation through
 *   reciprocal protections) with asymmetric extraction (conferring
 *   humanitarian advantage on state militaries while denying it to non-state
 *   fighters). The extractiveness has increased over 75 years (0.45→0.64) as
 *   non-state conflicts have become the norm rather than the exception,
 *   making the categorical exclusion affect an ever-larger proportion of
 *   combatants. Theater ratio is low because the institutional structure is
 *   straightforward, not performative—but the functional coordination problem
 *   it was designed to solve (preventing escalation in interstate conflicts)
 *   has atrophied, making the institutional structure increasingly inert
 *   (piton perspective).
 *
 * KEY AGENTS:
 *   - State Militaries: Primary beneficiary (institutional/arbitrage) — capture combatant immunity and POW protections; negotiating position in wars of choice; reciprocal protections for captured soldiers in state-vs-state conflicts
 *   - Non-State Armed Groups: Primary victim (moderate/constrained) — face categorical exclusion from protections; fighters subject to domestic criminal prosecution; organizational legitimacy denied; some capacity to negotiate humanitarian agreements but constrained by lack of legal recognition
 *   - Non-State Combatants (Individual Fighters): Maximum-extraction victim (powerless/trapped) — no exit from status as non-combatant for criminal law purposes; no immunity for acts of war; face prosecution for acts that state soldiers perform with full legal immunity; no reciprocal protection if captured by adversary state
 *   - International Humanitarian Law Institutions (ICRC, ILC): Secondary beneficiary (moderate/constrained) — coordinate around state-recognized framework; maintain institutional compliance through state cooperation; constrained from expanding protections without losing state buy-in
 *   - Civilian Protection Commons: Victim (powerless/trapped) — broader humanitarian protection system depends on state compliance incentives; state combatant immunity creates quid pro quo for respecting civilian protections; non-state exclusion weakens the bargain and creates incentive structure favoring civilian targeting
 *   - Post-WWII International Legal Order: Institutional inertia (institutional/arbitrage) — maintains state-centric framework through institutional path dependence despite changing conflict patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.62).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.75).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Combatant Status Definition (State-Centric Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'fc44493f-d5b5-45cb-9d42-c923927fd7d9').
narrative_ontology:cs_kernel_codification('fc44493f-d5b5-45cb-9d42-c923927fd7d9', fixed_text).
narrative_ontology:cs_authority_grounding('fc44493f-d5b5-45cb-9d42-c923927fd7d9', extraction).
narrative_ontology:cs_interpretation_layer_present('fc44493f-d5b5-45cb-9d42-c923927fd7d9').
narrative_ontology:cs_reading_relation('fc44493f-d5b5-45cb-9d42-c923927fd7d9', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('fc44493f-d5b5-45cb-9d42-c923927fd7d9', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('fc44493f-d5b5-45cb-9d42-c923927fd7d9', foundational, state_organizational_monopoly).
narrative_ontology:cs_axiom_status(state_organizational_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('fc44493f-d5b5-45cb-9d42-c923927fd7d9', state_organizational_monopoly, conventional).
narrative_ontology:cs_axiom('fc44493f-d5b5-45cb-9d42-c923927fd7d9', foundational, formal_recognition_as_legitimacy).
narrative_ontology:cs_axiom_status(formal_recognition_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('fc44493f-d5b5-45cb-9d42-c923927fd7d9', formal_recognition_as_legitimacy, deontological).
narrative_ontology:cs_reference_frame('fc44493f-d5b5-45cb-9d42-c923927fd7d9', state_military_monopoly_on_legitimate_combatancy).
narrative_ontology:cs_drift_state('fc44493f-d5b5-45cb-9d42-c923927fd7d9', contemporary_non_state_conflict_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc44493f-d5b5-45cb-9d42-c923927fd7d9', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_war_departments).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, civilian_protection_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-STATE COMBATANT (SNARE) — Captured non-state fighter faces domestic prosecution for acts of war that state soldiers would enjoy POW immunity for. No exit from combatant status (identity trapped in armed resistance); faces full criminal liability. Maximum extraction: benefits of participation in armed struggle are negated by categorical exclusion from humanitarian protections. Suppression is maximal — the legal framework treats the fighter as criminal, not as protected combatant.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-STATE ARMED GROUP (TANGLED ROPE) — As an organizational actor, the group has some agency (can negotiate with states, seek recognition, form alliances) but faces constrained exit options (withdrawal from armed struggle risks loss of political legitimacy, territory, or constituency). Experiences mixed extraction: denied POW protections but may negotiate humanitarian agreements, gain de facto status, or work toward recognition. Moderate power; genuine coordination function around resource allocation and combat doctrine exists alongside extraction of fighters' uncompensated risk.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE MILITARY ESTABLISHMENT (ROPE) — State soldiers and war departments experience the constraint as pure coordination: the definition establishes clear status, predictable protections, reciprocal treatment of captured soldiers, and legal clarity for military operations. High institutional power; arbitrage options (can leverage treaty status, negotiate prisoner exchanges, leverage international reputation). Extraction flows FROM non-state actors TO state militaries — the definition creates asymmetric protection advantage.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN LAW INSTITUTIONS (TANGLED ROPE) — These actors benefit from the state-centric framework (it provides clear adjudication rules, stable state compliance incentives, institutional continuity) but face constrained exit from the definition (changing it requires state consensus). The IHL institutions coordinate protection standards while managing institutional dependency on state acceptance. The constraint serves their coordination function (keeping states in compliance with humanitarian norms) but also extracts their autonomy — they cannot substantially expand protections to non-state fighters without losing state cooperation.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-WWII INTERNATIONAL LEGAL ORDER (PITON) — The state-centric definition is now a vestigial institutional arrangement. It originated from the 1949 Geneva Conventions to prevent brutal treatment of captured soldiers in state-vs-state conflicts. But contemporary armed conflicts are overwhelmingly non-state (civil wars, insurgencies, proxy wars). The definition persists through institutional inertia — states maintain the framework because it privileges state power and provides clarity, even though its functional coordination purpose (preventing escalation in interstate conflicts) is largely irrelevant. Theater ratio is low (0.35) because the institutional structure is straightforward, not performative; but the functional coordination problem it was designed to solve has atrophied.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing analytical perspective, states are the only actors with sufficient organizational capacity, accountability mechanisms, and reciprocal treaty obligations to guarantee POW protections. The definition appears as a natural architectural limit: non-state actors cannot credibly commit to the legal reciprocities that POW status requires. However, this risks naturalizing what is actually a contingent institutional choice — the exclusion reflects state institutional advantage, not intrinsic incapacity. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(combatant_status_definition__state_centric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, TR),
    TR >= 0.70.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The state-centric definition creates measurable humanitarian advantage for state militaries while imposing criminal liability on non-state combatants. The extraction is not maximal (0.75+) because non-state groups retain some agency—they can negotiate humanitarian agreements, seek de facto recognition, form alliances with states—and the extraction is mediated through domestic legal systems rather than directly through the international framework. The value reflects that the definition confers categorical benefit on one class (states) while categorically denying it to another (non-state actors). Suppression (0.75): High. The framework actively suppresses alternatives: a captured non-state fighter has no legal recourse to POW status, no appeal to international humanitarian law, and faces criminal prosecution under domestic law where that law is controlled by the warring state or its allies. The suppression is structural—the definition itself closes off alternative status claims. Theater ratio (0.35): Low. The institutional structure is straightforward and functional (not performative). The Article 4 criteria are clearly stated; the distinction between combatants and civilians is explicit in the treaty text; compliance monitoring is relatively transparent. The low theater reflects that this is genuinely a coordination mechanism, not a theatrical performance—the architecture is clear even if its application has become contested. Rising extractiveness (0.45→0.64) over 75 years reflects the shift in conflict patterns: when inter-state war was the norm (1949), the definition served most combatants. As non-state conflicts became dominant (post-1975), the definition increasingly excludes combatants from protections, raising effective extraction.
 *
 * PERSPECTIVAL GAP:
 *   The most revealing perspectival gap lies between the beneficiary (state military) rope perspective and the primary victim (non-state combatant) snare perspective. For state soldiers, the constraint appears as pure coordination: it establishes clear status, reciprocal protections, legal clarity, predictable treatment of captured soldiers. The experience is positive—the soldier is protected. For non-state fighters, the same constraint appears as pure extraction: the definition denies legal status, exposes them to criminal prosecution for acts of war that state soldiers perform legally, and offers no reciprocal protection. The two perspectives describe the same mechanism serving opposite functions for two classes of actors. The analytical observer at civilizational timescale risks seeing a natural law (mountain): states are uniquely capable of credible humanitarian commitment due to their organizational scale and treaty obligations. But this naturalizes what is actually a contingent institutional choice—the exclusion reflects state power, not intrinsic state capacity. The functional_protection_reading (sibling reading) would reject this mountain view and classify as tangled_rope at analytical scale, arguing that functional military characteristics (organization, command, firepower) track humanitarian commitment better than formal state origin. The piton perspective reveals institutional inertia: the constraint persists through path dependence even as its functional coordination problem (preventing escalation in state-vs-state war) has become marginal to contemporary conflict patterns.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) value for each perspective is derived from structural position. State militaries are beneficiaries with high institutional power and arbitrage options (can leverage treaty status, negotiate reciprocal treatment, leverage international reputation) → d ≈ 0.05-0.15 (full beneficiary) → negative or very low f(d) → low experienced extraction. Non-state combatants are victims with powerless status and trapped exit options (no legal alternative, no organizational voice, subject to prosecution) → d ≈ 0.95 (full target) → f(d) ≈ 1.42 → maximum experienced extraction. Non-state armed groups are victims with moderate power and constrained exit options (can negotiate, seek recognition, but cannot claim legal status) → d ≈ 0.75-0.85 (strong target) → f(d) ≈ 1.15-1.28 → strong experienced extraction. IHL institutions are partial beneficiaries with moderate power and constrained exit (benefit from state cooperation; cannot unilaterally change definitions) → d ≈ 0.40-0.45 → f(d) ≈ 0.40-0.50 → moderate experienced extraction. The analytical observer operates at d ≈ 0.72 (asymmetry observer) → f(d) ≈ 1.15 with global scope → no inherent beneficiary/victim relationship, so canonical fallback applies. Scope modifier σ(S) = 1.2 (global): the constraint operates at global scope, amplifying effective extraction by 1.2x through χ = ε × f(d) × σ(S). A local exclusion (σ = 0.8) would dampen extraction; a universal principle (σ = 1.0) would be neutral. Global scope amplifies the distribution effect—the definition affects combatants across all jurisdictions uniformly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_state_capacity_for_compliance,
    'Can non-state armed groups credibly commit to the legal and organizational requirements of POW protections, or is state monopoly on such commitment a structural necessity?',
    'Empirical analysis of non-state compliance with humanitarian agreements (e.g., FARC, PKK, various Palestinian organizations, UNITA, Tamil Tigers, etc.); measurement of capture-and-treatment outcomes under state-recognized protocols vs. state-denied protocols; post-conflict transitional justice data on combatant treatment by non-state victors',
    'If non-state actors demonstrate capacity for compliance: the exclusion is choice, not necessity — classification shifts toward functional_protection reading. If non-state actors systematically violate humanitarian norms: exclusion appears structurally justified — state-centric reading strengthens. If compliance is conditional on recognition: the definition''s exclusion becomes self-fulfilling (denying recognition prevents the condition for compliance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_capacity_for_compliance, empirical, 'Whether non-state actors can credibly comply with POW protection obligations').

omega_variable(
    distributional_burden_of_categorical_exclusion,
    'What proportion of contemporary combatants are excluded from POW protections by the state-centric definition? Does the humanitarian burden of exclusion scale with the prevalence of non-state conflict?',
    'Quantitative analysis: proportion of casualties in non-state vs. state-vs-state conflicts (1949-present); longitudinal trend in conflict type; estimates of persons exposed to unprotected combatant status; comparison of humanitarian outcomes (casualty rates, treatment of prisoners, post-conflict trauma) in non-state vs. state conflicts',
    'If non-state conflicts represent <20% of combatants: original 1949 context preserved, exclusion may be structurally justified. If >60%: the definition excludes a majority of combatants from protections — the constraint shifts toward snare classification and undermines humanitarian coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_burden_of_categorical_exclusion, empirical, 'Scale of humanitarian burden from categorical exclusion of non-state combatants').

omega_variable(
    definition_kernel_vs_enforcement_mechanism,
    'Is the structural extraction located in the definition itself (which combatants qualify for status) or in the enforcement mechanism (whether states comply with protections for actors they do recognize)?',
    'Comparative analysis of combatant treatment: (a) non-state actors meeting functional criteria but denied status by definition; (b) state actors NOT meeting organizational criteria but granted status; (c) actors granted status but denied protections through enforcement failure; (d) actors denied status but receiving de facto humanitarian treatment through enforcement pressure',
    'If enforcement failure dominates: the definition is less extractive than it appears — the constraint is better classified as rope (coordination mechanism with implementation gaps). If the definition itself is the bottleneck: the constraint remains snare from the non-state perspective — exclusion is designed into the kernel, not merely into enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_kernel_vs_enforcement_mechanism, empirical, 'Whether extraction is in the definition itself or in enforcement of recognized status').

omega_variable(
    threshold_criteria_functionality,
    'Do the Article 4 criteria (organized command, fixed emblem, uniforms, open arms carrying) actually correlate with organizational capacity to commit to humanitarian norms, or are they proxy criteria that exclude actors for political reasons?',
    'Mapping of Article 4 compliance to: (a) organizational capacity for prisoner management; (b) prevalence of humanitarian violations; (c) state political interest in recognition; (d) functional military effectiveness. If criteria correlate weakly with stated purpose (humanitarian commitment) but strongly with state political interest: the criteria are proxy mechanisms.',
    'If criteria are functionally justified: the definition is legitimate coordination mechanism. If criteria are political exclusion mechanisms: the constraint is extraction cloaked in humanitarian language — classification shifts toward snare/tangled_rope from analytical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_criteria_functionality, empirical, 'Whether Article 4 criteria functionally correlate with humanitarian commitment capacity').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the 1949 Geneva Conventions'' combatant-status kernel, or a distinct constraint about state monopoly on legitimate armed force?',
    'This is a conceptual omega addressing the kernel boundaries themselves. The answer determines what counts as a sibling reading: if the kernel is ''who qualifies as a combatant for protection purposes'', then functional_protection_reading is a direct sibling (different answer to the same question). If the kernel is ''who has authority to legitimate armed force'', then national_liberation_reading is a direct sibling (different authority framework). If both kernels are active (legal definition + authority framework), then the three readings cover both. This omega resolves through conceptual analysis of legal tradition and political philosophy.',
    'This omega routes through cs_structure.reading_relations and axioms. It does not change classification but clarifies the committer structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'What kernel this reading is interpreting (combatant status vs. authority to use force)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combstat_theater_1949, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(combstat_theater_1989, combatant_status_definition__state_centric_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(combstat_theater_2024, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(combstat_epsilon_1949, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(combstat_epsilon_1969, combatant_status_definition__state_centric_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(combstat_epsilon_1989, combatant_status_definition__state_centric_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(combstat_epsilon_2024, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(combstat_suppression_1949, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(combstat_suppression_1989, combatant_status_definition__state_centric_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(combstat_suppression_2024, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, reciprocal_prisoner_exchange).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, civilian_protection_commons).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, internal_armed_conflict_status).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, private_military_contractor_liability).

% DUAL FORMULATION NOTE:
% Combatant status definition is a kernel with three distinct constraint stories representing readings of the same contested norm. The state-centric reading instantiates the position that only state militaries qualify. The national_liberation_reading and functional_protection_reading are separate constraints with different ε values and perspectival profiles. All three are linked via network.affects_constraints because they compete to define the same legal space. The state-centric reading is upstream of institutional path dependence; the functional_protection reading is downstream of humanitarian pressure to expand protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
