% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_individual_right, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Pre-existing Right Framework
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment asserts that the
 *   operative clause ('the right of the people to keep and bear Arms, shall
 *   not be infringed') establishes a pre-existing individual right to firearm
 *   possession, and that the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') states a purpose or
 *   rationale but does not limit the operative right's scope. This reading
 *   treats firearm possession as a personal liberty protected from state
 *   regulation, positions the militia clause as explanatory rather than
 *   restrictive, and shields private firearms markets from comprehensive
 *   state authority. The constraint structure combines genuine coordination
 *   (stable property rights, market predictability for manufacturers and
 *   distributors) with significant extraction (foreclosed public health
 *   regulation, suppressed alternatives, harm to those endangered by
 *   unrestricted access). This is one of three contested readings of the
 *   Second Amendment kernel; the other readings (militia-conditioned and
 *   insurrectionist) instantiate structurally different constraints with
 *   different beneficiary/victim sets and ε values. The individual-right
 *   reading produced the institutional shift following DC v. Heller (2008),
 *   which rejected prior precedent treating the Second Amendment as
 *   militia-conditional and established the individual-right framework as
 *   governing doctrine. The extractiveness and suppression metrics show
 *   accumulation over the 2008-2018 interval as lower courts applied Heller's
 *   framework to strike down increasingly broad regulations (magazine
 *   capacity limits, assault-weapon bans, permit requirements), expanding the
 *   protected domain of private possession.
 *
 * KEY AGENTS:
 *   - Firearm Manufacturers and Distributors: Primary institutional beneficiary (institutional/arbitrage) — shield from presumptive regulatory authority, market expansion, liability limitation
 *   - Gun Rights Coalitions: Organized beneficiary (organized/constrained) — constitutional framework for political mobilization, litigation strategy, norm-setting in favor of private ownership
 *   - Mass Shooting Victims and Families: Primary victim (powerless/trapped) — no exit from harm pathway; constitutional framework suppresses regulatory alternatives
 *   - Domestic Violence Victims with Firearm Access: Primary victim (powerless/trapped) — structural entrapment in high-lethality violence contexts; regulations blocked as infringements
 *   - Suicide Completers and Families: Victim (powerless/trapped) — firearm access dramatically increases lethality; safe-storage and access-restriction regulations foreclosed as infringements
 *   - Public Health Regulatory Capacity: Victim (institutional/trapped) — agencies lack authority to implement evidence-based interventions; extraction manifests as regulatory impotence
 *   - Gun Violence Prevention Advocacy: Organized but constrained victim (organized/constrained) — mobilizes evidence-based policy but faces constitutional bar on presumed-infringement regulations
 *   - Federal Judiciary: Institutional enforcer (institutional/arbitrage) — maintains the reading through precedent; piton perspective reflects institutional inertia and legitimacy doctrine of 'neutral' interpretation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contested institutional reading as discovered constitutional law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading: Pre-existing Right Framework").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, 'b399e982-281a-46a2-bd11-4c1e6bd8fe48').
narrative_ontology:cs_kernel_codification('b399e982-281a-46a2-bd11-4c1e6bd8fe48', fixed_text).
narrative_ontology:cs_authority_grounding('b399e982-281a-46a2-bd11-4c1e6bd8fe48', lineage).
narrative_ontology:cs_interpretation_layer_present('b399e982-281a-46a2-bd11-4c1e6bd8fe48').
narrative_ontology:cs_reading_relation('b399e982-281a-46a2-bd11-4c1e6bd8fe48', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_reading_relation('b399e982-281a-46a2-bd11-4c1e6bd8fe48', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('b399e982-281a-46a2-bd11-4c1e6bd8fe48', foundational, operative_clause_establishes_preexisting_right).
narrative_ontology:cs_axiom_status(operative_clause_establishes_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('b399e982-281a-46a2-bd11-4c1e6bd8fe48', operative_clause_establishes_preexisting_right, empirically_contingent).
narrative_ontology:cs_axiom('b399e982-281a-46a2-bd11-4c1e6bd8fe48', foundational, prefatory_clause_states_purpose_not_limit).
narrative_ontology:cs_axiom_status(prefatory_clause_states_purpose_not_limit, holdable).
narrative_ontology:cs_axiom_grounding('b399e982-281a-46a2-bd11-4c1e6bd8fe48', prefatory_clause_states_purpose_not_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('b399e982-281a-46a2-bd11-4c1e6bd8fe48', original_public_meaning_individual_right_framework).
narrative_ontology:cs_drift_state('b399e982-281a-46a2-bd11-4c1e6bd8fe48', post_heller_2008_institutional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b399e982-281a-46a2-bd11-4c1e6bd8fe48', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearm_manufacturers_and_distributors).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_coalitions).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_with_firearm_access).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_health_regulatory_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MASS SHOOTING AND DOMESTIC VIOLENCE VICTIMS (SNARE) — Structurally powerless. Cannot exit the harm pathway; bear full extraction cost through death, injury, and trauma. The individual-right reading shields the constraint mechanism (private possession without comprehensive regulation) from state correction. Maximum experienced extraction — no agency, no arbitrage, no organized coalition capacity sufficient to overcome constitutional shield.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEM (SNARE) — Trapped by constitutional constraint. Public health agencies lack authority to regulate private firearm access; evidence-based interventions (safe storage mandates, capacity restrictions, enhanced background checks) are foreclosed as presumptive infringements under this reading. Extraction manifests as regulatory impotence — the system bears costs (treating injuries, epidemiological tracking) without capacity to modify the risk factor. High suppression: constitutional doctrine suppresses alternative regulatory pathways.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GUN RIGHTS COALITIONS (TANGLED ROPE) — Constrained by litigation risks and political opposition, but also benefit from the constitutionally-secured market for private ownership and rights infrastructure. Experience genuine coordination function (mobilizing political coalition for rights defense) alongside extraction of political power and institutional resources from the regulatory domain. Moderate experienced extraction because the coalition has organized capacity and the constraint enables their core objective.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FIREARM MANUFACTURERS AND DISTRIBUTORS (ROPE) — Primary institutional beneficiary. The individual-right reading shields private markets from presumptive regulation; manufacturers experience the constraint as pure coordination of a protected commercial domain. Net beneficiary through constitutionally-secured access to buyers. Low or negative experienced extraction — the constraint subsidizes this actor. The coordination function is real: stable property rights in firearm sales, protection from liability expansion, market predictability.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN VIOLENCE PREVENTION ADVOCACY (TANGLED ROPE) — Organized but constrained by constitutional doctrine treating regulation as presumptive infringement. Experience both coordination (mobilizing evidence-based policy proposals) and extraction (political power drained to constitutional litigation rather than public health measures). The constraint forces the coalition to work within narrow constitutional boundaries, reducing their agency. Moderate experienced extraction despite organized status because the constitutional frame forecloses direct pathways.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL JUDICIARY AS INSTITUTIONAL ACTOR (PITON) — Maintains the individual-right reading through precedent enforcement and canon construction despite the interpretive contest remaining live. Theater is high (0.48 floor reflects the genuine coordinative and extractive functions, but the ritualistic quality of 'original public meaning' interpretation and originalist methodology operates partly as performance of scientific objectivity over contested normative claims). Judges experience the constraint as something they must enforce, not something they created — institutional inertia and the legitimacy doctrine of 'neutral interpretation' sustain the framework.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — This perspective risks treating the individual-right reading as a discovered natural law of constitutional interpretation: 'The text plainly establishes a pre-existing right; the prefatory clause cannot negate operative language.' From a civilizational view, this appears immutable — a logical fact about language and constitutional structure. However, the structural data contradicts the mountain classification: identifiable beneficiaries (manufacturers, gun-rights coalitions), victims (shooting victims, public health capacity), and active enforcement (litigation doctrine, canon construction) reveal this as a contingent institutional reading, not a natural law. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_boundary__individual_right_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. The individual-right reading establishes a constitutional shield against regulation of private firearm possession. For beneficiaries (manufacturers, gun-rights organizations), this produces stable extraction of regulatory value — regulations that would exist under alternative readings are foreclosed. For victims, extraction manifests as blocked public health interventions and accumulated harm in violence contexts. The increasing trajectory (0.32→0.45→0.58) reflects the institutional entrenchment of the individual-right reading through lower court application and the broadening of protected domains (striking down magazine limits, assault-weapon bans, permit schemes). Suppression (0.62): Moderate-high, increasing. The individual-right reading treats regulation as presumptively unconstitutional, requiring strict scrutiny. This creates high barriers to regulatory alternatives — states cannot implement evidence-based interventions (universal background checks, safe storage, permit requirements, extreme-risk protection orders) without constitutional challenge. The suppression mechanism operates partly explicitly (strict-scrutiny doctrine) and partly implicitly (regulatory agencies pre-emptively restrict their own actions fearing litigation). Rising trajectory reflects expanded doctrine coverage. Theater (0.48): Moderate. The reading has both genuine substantive content (property-rights protection, market stability) and performative elements (originalist methodology operates partly as performance of scientific objectivity, 'original public meaning' interpretation addresses contested historical facts through the lens of doctrinal conclusion). The theater is lower than piton-level because the coordination function is real — manufacturers do benefit from market stability and property rights protection — but the reading's grounding in 'discovered' constitutional meaning (rather than acknowledged policy choice) introduces performative dimension.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Beneficiaries (manufacturers, gun-rights coalitions) experience the constraint as pure coordination or beneficial rope — stable markets, predictable property rights, constitutional protection for their core objectives. Victims experience snare — trapped in harm pathways, constitutional foreclosure of regulatory alternatives, maximum extraction without exit capacity. The public health regulatory system experiences snare characterized by impotence — the system bears costs (treating injuries, epidemiological tracking) while foreclosed from modifying the risk source. Gun violence prevention advocates, though organized, experience constrained tangled rope — they can mobilize politically but face constitutional headwinds that foreclose direct regulatory pathways. The federal judiciary experiences piton — maintaining precedent through ritual of 'interpretation' while the reading remains institutionally contested. The critical gap: beneficiaries see the constraint as discovering pre-existing rights and protecting natural liberty, while victims see it as creating a constitutional entitlement to a harmful practice and suppressing public health authority. The analytical observer risks naturalizing this as immutable constitutional law (mountain) when the structural data reveals it as a contingent institutional reading with clear beneficiaries and victims — a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position: beneficiary vs. victim status, power level, and exit options. Manufacturers (beneficiary + institutional + arbitrage exit) derive d ≈ 0.05-0.15 (full or near-full beneficiary), producing negative or minimal f(d) and low/negative χ. Victims (trapped/powerless) derive d ≈ 0.95-1.00 (full target), producing maximum f(d) ≈ 1.42 and high χ. Organized coalitions (constrained + organized power) derive moderate d reflecting partial exit capacity and partial organizational power. The judicial perspective derives institutional d with consideration for the self-enforcing nature of precedent maintenance — they are not beneficiaries in the market sense but they do benefit institutionally from the precedent's stability and their role as interpreters. The directionality structure reveals why beneficiaries see rope while victims see snare: the same structural constraint produces opposite experienced extractiveness depending on whether you capture value from the shielded domain (manufacturers) or bear suppressed alternatives and accumulated harms (public health, victims).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between coordination and extraction by instantiating both genuinely. The individual-right reading DOES coordinate a stable market for private firearm ownership — this is not illusory or purely performative. Manufacturers benefit from predictable property rights; gun owners benefit from constitutional protection; the constraint enables a functioning firearms commerce. Simultaneously, the constraint EXTRACTS by suppressing regulatory alternatives — public health agencies cannot implement evidence-based interventions; victims cannot exit harm pathways; the regulatory domain is foreclosed. The mandatrophy is not 'is this coordination or extraction?' (answer: both) but 'who captures value from the coordination and who bears the extraction cost?' Beneficiaries experience the coordination; victims experience the extraction. The tangled-rope classification reflects this genuine hybridization: the constraint solves a real coordination problem (stable property rights, predictable rules) while embedding asymmetric extraction (foreclosed alternatives, suppressed harms). The theater ratio (0.48) is lower than snare or pure-extraction mechanisms because the coordination function is substantive, not merely performative window-dressing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_logical_relationship,
    'Does the prefatory clause ''A well regulated Militia, being necessary to the security of a free State'' grammatically and logically CONDITION the operative clause, or does it merely STATE A PURPOSE without limiting scope?',
    'Linguistic analysis: comparison with 18th-century constitutional texts and legal manuscripts to establish grammatical convention for prefatory-operative relationships; examination of whether the Second Amendment''s structure matches other prefatory-operative pairs (e.g., preamble to Constitution) where conditioning intent is clear vs. cases where prefatory statements do NOT condition operative rights.',
    'If conditioning relationship established: militia-conditioned reading prevails; extractiveness drops to ~0.35; victims set shrinks to regulatory overreach; beneficiary set becomes ''militia-regulated firearms access.'' If purpose-only relationship confirmed: individual-right reading prevails; extractiveness ~0.58; victims = those harmed by unrestricted access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_logical_relationship, empirical, 'Grammatical relationship between prefatory and operative clauses').

omega_variable(
    original_public_meaning_determination,
    'What did ''keep and bear Arms'' mean in 1791 public understanding: private individual possession for self-defense, or participation in state militia structures?',
    'Historical-textual analysis: examination of militia service records, state constitutions, founding-era legal commentary, and actual regulations contemporaneous with ratification; comparison of arms-bearing language in militia law vs. individual rights contexts; determination of whether private self-defense was contemplated as within the right or as separate from it.',
    'If private self-defense interpretation supported: individual-right reading anchored in historical legitimacy; classification as tangled_rope stable. If militia participation primary: individual-right reading loses historical grounding; becomes a modern doctrine layered atop older text; extractiveness classification becomes epistemically contingent rather than constitutionally anchored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_determination, empirical, 'Original public meaning of ''keep and bear Arms'' (1791)').

omega_variable(
    extraction_vs_coordination_function,
    'Is the market-shielding effect of the individual-right reading a genuine coordination mechanism (protecting stable property rights in firearms commerce) or a disguised extraction mechanism (rent-seeking by manufacturers, suppression of public health regulation)?',
    'Comparative regulatory analysis: measurement of public health outcomes under the individual-right regime vs. jurisdictions with conditioned-right interpretations; assessment of whether market stability produces innovations or merely protects existing producers; examination of whether private market for firearms serves coordination functions (sporting, self-defense, collection) that could not operate under alternative regimes.',
    'If coordination dominant: manufacturers'' rope perspective confirmed; chi for beneficiaries ~ 0.20-0.30. If extraction dominant: manufacturers'' perspective reclassifies to snare (beneficiary extracting from public health commons); chi for beneficiaries rises toward 0.70-0.85; constraint structure becomes purely exploitative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_function, empirical, 'Coordination vs. extraction function in firearms market shielding').

omega_variable(
    kernel_reading_contest_underdetermination,
    'Is the interpretive contest between individual-right, militia-conditioned, and insurrectionist readings a dispute ABOUT THE TEXT''S MEANING (resolvable by historical-linguistic evidence) or a contest ABOUT LEGITIMATE FRAMINGS of the same text (where multiple readings are defensible and the ''winner'' is determined by institutional power rather than textual facts)?',
    'Meta-constitutional analysis: examination of whether the three readings are mutually exclusive (only one can be true) or whether they represent distinct normative choices laid atop a genuinely ambiguous text; assessment of whether historical evidence favors one reading decisively or supports multiple readings; determination of whether the Supreme Court''s adoption of the individual-right reading reflects textual discovery or institutional choice to prioritize this framing.',
    'If dispute is textual: the individual-right reading is either correct (mountain/rope classification firm) or incorrect (false reading, classification collapses). If dispute is normative framing: all three readings are defensible; the individual-right reading''s institutional adoption is a choice, not a discovery; the constraint is better analyzed as a commitment-system outcome than as a constitutional fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_underdetermination, conceptual, 'Whether the reading contest is about textual meaning or normative framing').

omega_variable(
    false_summit_natural_law_risk,
    'Is the individual-right reading a discovered natural law of constitutional structure or a constructed institutional reading that benefits identifiable parties and could be reversed?',
    'Institutional-historical analysis: tracking the adoption of the individual-right reading in Supreme Court precedent (DC v. Heller 2008) and its subsequent invocation; assessment of whether the reading was always present in the constitutional text (discovered) or emerged as a new institutional commitment (constructed); measurement of whether the reading''s adoption correlates with shifts in beneficial interest groups or constitutional theory.',
    'If natural law: mountain classification appropriate; no false summit. If constructed: false_summit_mountain signature fires; constraint reclassifies through override chain to tangled_rope or snare; the ''discovered'' framing is exposed as naturalization of a policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Natural law vs. constructed reading status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 2008, 2018).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_right_theater_2008, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sa_ind_right_theater_2013, second_amendment_boundary__individual_right_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(sa_ind_right_theater_2018, second_amendment_boundary__individual_right_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(sa_ind_right_extractiveness_2008, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sa_ind_right_extractiveness_2013, second_amendment_boundary__individual_right_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sa_ind_right_extractiveness_2018, second_amendment_boundary__individual_right_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_ind_right_suppression_2008, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sa_ind_right_suppression_2013, second_amendment_boundary__individual_right_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(sa_ind_right_suppression_2018, second_amendment_boundary__individual_right_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearms_market_shielding).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, public_health_firearm_regulation_foreclosure).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, mass_shooting_victim_powerlessness).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into three distinct constraint stories, each with its own ε value and reading. The individual-right reading (this story, ε=0.58) treats the operative clause as establishing pre-existing individual right and the prefatory clause as purpose-statement. The militia-conditioned reading (ε=0.52) treats the prefatory clause as a substantive limit. The insurrectionist reading (ε=0.58) treats private possession as instrumental to resistance capacity. All three readings coexist as live institutional positions; none is logically foreclosed by the others. The structural decomposition is necessary because the readings differ in their victim sets, beneficiary structures, and suppression mechanisms, producing different ε values even though they address the same text. The individual-right reading creates downstream constraints on public health regulation and on victim capacity to exit harm pathways; these downstream constraints are structurally dependent on this reading's establishment as institutional doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__individual_right_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
