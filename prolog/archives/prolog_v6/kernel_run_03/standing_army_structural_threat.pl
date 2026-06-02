% ============================================================================
% CONSTRAINT STORY: standing_army_structural_threat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_standing_army_structural_threat, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: standing_army_structural_threat
 *   human_readable: Standing Army Structural Threat: The Militia Clause as Constitutional Control Mechanism
 *   domain: constitutional_law/political_theory/federalism
 *
 * SUMMARY:
 *   The Second Amendment militia clause operates as a contested
 *   constitutional kernel where identical text generates structurally
 *   distinct constraints depending on interpretive reading. The Framing
 *   intent was to limit federal military monopoly by protecting citizen
 *   militia capacity as a counterbalance to standing armies (which the
 *   Founders viewed as inherent threats to republican government). The
 *   constraint visible in 1791 was Rope — genuine coordination between
 *   federal defense and militia deterrence. Over 235 years, federal military
 *   consolidation through industrialization, centralization, and
 *   technological advancement transformed the constraint into Tangled Rope
 *   (federal benefit from standing army maintained alongside extraction from
 *   disarmed citizens) and from citizen perspective into Snare (military
 *   monopoly with no citizen counterbalance). The Heller decision (2008)
 *   resolved the prefatory-clause interpretive contest by holding that the
 *   Second Amendment protects individual arms rights independent of militia
 *   service — but this resolution paradoxically evacuates the militia
 *   clause's actual limiting function on federal power while maintaining its
 *   symbolic constitutional presence. The constraint now operates with high
 *   theater: the constitutional text persists as a performative invocation in
 *   gun-rights discourse, but its operative capacity to limit federal
 *   military dominance has been foreclosed by interpretive practice.
 *
 * KEY AGENTS:
 *   - Federal Executive & Standing Military: Primary beneficiary (institutional/arbitrage) — consolidates military power; experiences constraint as legitimate coordination mechanism
 *   - Professional Military Class: Secondary beneficiary (institutional/arbitrage) — derives career advancement and organizational autonomy from standing army structure
 *   - State Governments (Militia Authority): Primary victim (institutional/constrained) — retain nominal militia command but lost practical capacity; preempted by federal military superiority
 *   - Armed Citizenry as Counterbalance: Secondary victim (powerless/trapped) — disarmed by regulatory and legal barriers; lacks capacity for meaningful militia organization
 *   - Constitutional Textualists: Powerful interpreter (powerful/mobile) — can invoke Second Amendment text but face judicial precedent (Heller) foreclosing militia-as-limit reading
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing federal military monopoly as inevitable feature of modern state organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standing_army_structural_threat, 0.58).
domain_priors:suppression_score(standing_army_structural_threat, 0.62).
domain_priors:theater_ratio(standing_army_structural_threat, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standing_army_structural_threat, extractiveness, 0.58).
narrative_ontology:constraint_metric(standing_army_structural_threat, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(standing_army_structural_threat, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standing_army_structural_threat, tangled_rope).
narrative_ontology:human_readable(standing_army_structural_threat, "Standing Army Structural Threat: The Militia Clause as Constitutional Control Mechanism").
narrative_ontology:topic_domain(standing_army_structural_threat, "constitutional_law/political_theory/federalism").

domain_priors:requires_active_enforcement(standing_army_structural_threat).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(standing_army_structural_threat, 'e4c7b8eb-2966-4c5a-988f-198f446a4b14').
narrative_ontology:cs_created_at('e4c7b8eb-2966-4c5a-988f-198f446a4b14', '').
narrative_ontology:cs_kernel_codification('e4c7b8eb-2966-4c5a-988f-198f446a4b14', fixed_text).
narrative_ontology:cs_authority_grounding('e4c7b8eb-2966-4c5a-988f-198f446a4b14', lineage).
narrative_ontology:cs_interpretation_layer_present('e4c7b8eb-2966-4c5a-988f-198f446a4b14').
narrative_ontology:cs_reading_relation('e4c7b8eb-2966-4c5a-988f-198f446a4b14', militia_clause_as_limit_reading, forecloses).
narrative_ontology:cs_axiom('e4c7b8eb-2966-4c5a-988f-198f446a4b14', foundational, second_amendment_protects_individual_arms_rights).
narrative_ontology:cs_axiom_status(second_amendment_protects_individual_arms_rights, holdable).
narrative_ontology:cs_axiom_grounding('e4c7b8eb-2966-4c5a-988f-198f446a4b14', second_amendment_protects_individual_arms_rights, deontological).
narrative_ontology:cs_axiom('e4c7b8eb-2966-4c5a-988f-198f446a4b14', foundational, prefatory_clause_announces_not_restricts).
narrative_ontology:cs_axiom_status(prefatory_clause_announces_not_restricts, holdable).
narrative_ontology:cs_axiom_grounding('e4c7b8eb-2966-4c5a-988f-198f446a4b14', prefatory_clause_announces_not_restricts, empirically_contingent).
narrative_ontology:cs_axiom('e4c7b8eb-2966-4c5a-988f-198f446a4b14', secondary, militia_counterbalance_historically_obsolete).
narrative_ontology:cs_axiom_status(militia_counterbalance_historically_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('e4c7b8eb-2966-4c5a-988f-198f446a4b14', militia_counterbalance_historically_obsolete, empirically_contingent).
narrative_ontology:cs_reference_frame('e4c7b8eb-2966-4c5a-988f-198f446a4b14', heller_individual_right_framework).
narrative_ontology:cs_drift_state('e4c7b8eb-2966-4c5a-988f-198f446a4b14', contemporary_post_heller_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standing_army_structural_threat, federal_executive_military_apparatus).
narrative_ontology:constraint_beneficiary(standing_army_structural_threat, professional_military_class).
narrative_ontology:constraint_victim(standing_army_structural_threat, state_militia_capacity).
narrative_ontology:constraint_victim(standing_army_structural_threat, armed_citizenry_as_counterbalance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISARMED CITIZENRY (SNARE) — Citizens who cannot organize militia capacity face federal military monopoly with no structural exit. Trapped by legal prohibition, regulatory barriers, and resource concentration. Cannot exit the constraint; bears full cost of state military dominance without proportional defensive capacity.
constraint_indexing:constraint_classification(standing_army_structural_threat, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENTS AS MILITIA AUTHORITY (SNARE) — States retain nominal militia command but have lost practical capacity relative to federal standing army. Constrained by federal preemption, funding dependencies, and superior federal enforcement capacity. Cannot meaningfully exit; structurally subordinated to federal military.
constraint_indexing:constraint_classification(standing_army_structural_threat, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL EXECUTIVE & MILITARY (ROPE) — Primary beneficiary. Coordinates defense through centralized command structure. Experiences constraint as legitimate coordination: standing army solves collective action problem of national defense. Net beneficiary of militia clause interpretation that permits federal military monopoly.
constraint_indexing:constraint_classification(standing_army_structural_threat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ARMED CITIZEN-MILITIA ADVOCATES (TANGLED ROPE) — Powerful organized actors (militia movements, constitutional federalism advocates) see the constraint as hybrid: genuine coordination function (military defense) layered with asymmetric extraction (federal monopoly preventing citizen counter-organization). Mobile and powerful enough to contest legally, but suppressed by post-Heller interpretive consensus that decouples militia clause from operative clause. Mixed experience of coordination benefit and extraction.
constraint_indexing:constraint_classification(standing_army_structural_threat, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL TEXT AS LITERAL CONSTRAINT (PITON) — The plain-language reading of the Second Amendment ('the right of the people to keep and bear Arms, shall not be infringed') operates as a written institutional constraint, but its function has atrophied. Post-Heller, it is maintained through performative constitutional discourse (courts invoke it; scholars cite it; gun-rights advocates appeal to it) while its practical limiting function on federal military monopoly has been evacuated. Theater ratio high because the literal text persists but its operative constraint against standing-army dominance has been hollowed by interpretive practice.
constraint_indexing:constraint_classification(standing_army_structural_threat, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, standing armies are a natural and unchangeable feature of modern state organization. Professional military forces are inherent to security provision in complex societies — militia as counterbalance is an archaic constraint that cannot survive modern military technology and organizational complexity. This perspective naturalizes standing army dominance as an immutable structural law of state development. However, the structural data contradicts the mountain classification: identified beneficiaries (federal executive, military class) and victims (disarmed citizenry, state capacity) reveal this as a false summit — the constraint is contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(standing_army_structural_threat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(standing_army_structural_threat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(standing_army_structural_threat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(standing_army_structural_threat, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(standing_army_structural_threat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(standing_army_structural_threat, TR),
    TR >= 0.70.

:- end_tests(standing_army_structural_threat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial benefit to federal executive (military monopoly, reduced constraints on force projection) while imposing costs on disarmed citizenry (inability to organize militia counterbalance, reliance on federal military monopoly for security). The extraction is not total because the federal military does provide genuine collective defense — extractiveness is not at Snare levels (0.70+) because there is real coordination function. Suppression (0.62): High. Significant structural and legal barriers to citizen militia organization: federal regulatory preemption of arms, legal barriers to militia formation outside National Guard structure, resource asymmetry (federal military budgets dwarf civilian arms capacity), doctrinal suppression (post-Heller interpretive consensus that militia clause does not limit federal standing army). Theater ratio (0.68): High. Constitutional discourse around the Second Amendment is substantially performative — the text is repeatedly invoked in legal and political argumentation, but post-Heller the performative function (affirming gun rights) is decoupled from the operative function (limiting federal military dominance). Courts maintain the theatrical invocation; the actual limiting mechanism has been foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The federal military establishment sees Rope (legitimate coordination of national defense through standing army; militia clause is merely historical context for why armed civilians exist, not a limiting principle). State governments see Snare (nominally militia authority, functionally subordinated by federal military superiority, unable to exit). Disarmed citizens see Snare (trapped by legal barriers and resource asymmetry; no meaningful capacity to organize militia; bear full cost of federal military monopoly). Armed militia advocates see Tangled Rope (genuine coordination function of standing army is real, but federal monopoly extracts by preventing citizen counter-organization; legally mobile to contest in courts but suppressed by Heller precedent). The constitutional text itself sees Piton (the literal Second Amendment persists in performative constitutional discourse but its operative function — limiting federal standing army — has been evacuated by interpretive consensus). The civilizational observer sees Mountain (standing armies are inevitable feature of modern states, militia is obsolete, federal military dominance is natural law of statecraft). The false summit detector identifies this last perspective as naturalization: the constraint is not inevitable but contingent on interpretive choices (Heller decision, post-1933 administrative consolidation, post-1991 military dominance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by perspective. Federal executive and military establishment (beneficiaries with arbitrage options) experience low d — they benefit from the constraint and can easily exit (they control the military apparatus). From the institutional perspective, d ≈ 0.15, producing negative effective extraction via the sigmoid f(d). Disarmed citizens (victims, trapped) experience high d — they bear costs and cannot exit. From powerless/trapped perspective, d ≈ 0.95, producing f(d) ≈ 1.42, amplifying experienced extractiveness to maximum. State governments (constrained, moderate victim status) experience moderate d ≈ 0.65, producing f(d) ≈ 1.00, resulting in moderate experienced extraction. The perspectival gap (Rope for beneficiaries, Snare for victims, Tangled Rope for analytical observers) reflects these directionality differences. The analytical observer at d ≈ 0.72 experiences the constraint as a structural phenomenon rather than as extraction or benefit — the mountain perspective naturalizes what is contingently institutional.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that the constraint contains genuine coordination function (standing armies do provide collective defense) layered with genuine extraction (federal monopoly prevents citizen counter-organization, disarmed citizenry cannot exit, federal executive benefits disproportionately). The tangled_rope classification captures this hybrid structure. The mandatrophy temptation arises from the false mountain perspective: if standing armies are 'natural law,' then the constraint cannot be extraction, and the Second Amendment is merely performative. Resolution: the constraint is not natural law but contingently institutional. The Heller decision and post-1933 military consolidation created the current configuration; alternative historical paths were available. Extractiveness (0.58) reflects that while federal coordination of defense is real, the suppression of militia counterbalance and the prevention of citizen organization represent genuine extraction mechanisms. The measurement trajectory shows extraction accumulation: from 0.22 at ratification (militia clause functioned as real check on standing army) to 0.65 at 1933 (federal military consolidation via administrative expansion) to 0.58 at present (slight decrease because drone/cyber warfare makes citizen militia capacity structurally impossible, reducing the extraction mechanism's functional relevance — the extraction mechanism weakens not because it is voluntarily dismantled but because it becomes technologically irrelevant). Theater ratio trajectory shows performative content rising (from 0.15 in 1791 when militia clause had operative limiting function, to 0.68 at present when Second Amendment is invoked performatively in constitutional discourse but no longer limits federal military). This temporal signature confirms tangled_rope: the theater has risen while the core extraction mechanism has remained stable, indicating that the constraint increasingly relies on performative maintenance rather than structural enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_scope_determination,
    'Does the prefatory militia clause (''A well regulated Militia, being necessary to the security of a free State'') restrict the operative clause''s grant of individual arms rights, or merely announce the Framers'' purpose?',
    'Historical semantic analysis of 18th-century constitutional grammar; examination of parallel prefatory constructions in foundational documents; comparative analysis of court opinions pre- and post-Heller on militia clause functionality',
    'If prefatory clause is restrictive: Second Amendment protects only militia-serving arms ownership, federal standing army monopoly is constitutionally legitimate, constraint is legitimate Rope. If prefatory clause is merely annunciatory: Second Amendment protects individual arms rights independent of militia service, federal standing army dominance extracts against constitutional intent, constraint is Snare or Tangled Rope. Heller decided restrictive interpretation prevails, but scholarly contest persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_scope_determination, empirical, 'Whether the prefatory militia clause restricts or merely announces purpose for the operative clause').

omega_variable(
    militia_viability_in_modern_context,
    'Is a citizen-militia capable of counterbalancing a professional standing army technologically and organizationally feasible in the 21st century, or is the militia-as-check concept obsolete?',
    'Comparative analysis of asymmetric warfare outcomes (insurgent vs conventional forces); assessment of drone, cyber, and force-multiplier technologies accessible to organized civilians vs federal military; historical case studies of militia effectiveness against professional armies in asymmetric contexts',
    'If militia viability is real: the constraint represents genuine extraction (federal monopoly prevents functioning counterbalance), classification remains Snare/Tangled Rope from disarmed citizen perspective. If militia viability is obsolete: the constraint becomes performative (citizens cannot meaningfully resist standing army regardless of Second Amendment rights), classification shifts toward Piton (theater maintained but function evacuated). This drives the mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_viability_in_modern_context, empirical, 'Whether citizen militia can meaningfully counterbalance modern standing army').

omega_variable(
    heller_interpretive_foreclosure,
    'Has the Heller decision (2008) foreclosed the prefatory-clause-as-limit reading, or does the contest between restrictive and individual-right interpretations remain open within constitutional jurisprudence?',
    'Tracking of post-Heller Supreme Court docket for Second Amendment cases; analysis of circuit court divergence; measurement of scholarly consensus shift pre/post Heller; documentation of whether lower courts continue to cite militia-clause-as-limit arguments in gun regulation cases',
    'If Heller is legally foreclosing: the reading contest is resolved (for now) in favor of individual-right interpretation, federal military monopoly is judicially legitimated, alternative reading has no live constitutional status. If Heller is merely dominant but not foreclosing: both readings persist as live constitutional positions, the contest is genuine kernel dynamics, both readings can be authored as separate constraint stories. Affects whether this story is a kernel reading or a settled constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heller_interpretive_foreclosure, conceptual, 'Whether Heller forecloses the prefatory-clause-as-limit reading or contest remains open').

omega_variable(
    federal_executive_consolidation_trajectory,
    'Over the 230-year span from ratification to present, has the federal executive''s military capacity concentrated monotonically, or have counter-cycles of decentralization or state capacity buildout occurred?',
    'Historical measurement of federal vs state military budgets; tracking of National Guard federalization vs devolution cycles; analysis of militia capacity relative to standing army at key historical moments (1791, 1865, 1933, 1991, 2020)',
    'If concentration is monotonic: the extraction mechanism is structural and locked in by centuries of institutional path dependence, classification remains Snare/Tangled Rope. If concentration shows reversible cycles: the constraint''s extractiveness could be reduced through deliberate policy, classification may shift toward Scaffold if sunset mechanisms emerge. Affects the mandatrophy resolution and whether reform is plausible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_executive_consolidation_trajectory, empirical, 'Whether federal military consolidation is monotonic or cyclical').

omega_variable(
    second_amendment_as_natural_law_status,
    'Is the constitutionalized right to bear arms a genuine constraint limiting federal power, or is it a false natural law that naturalizes contingent federal military dominance by appeal to constitutional text?',
    'Comparative constitutional analysis: which democracies protect individual gun rights constitutionally vs legislatively? What explains variance? Does constitutional protection correlate with citizen capacity to resist state dominance, or with state legitimacy narratives? Examination of whether Second Amendment functions as operational limit on federal power or as performative invocation in legal discourse.',
    'If genuine constraint: federal military monopoly violates Second Amendment if correctly interpreted, problem is interpretive capture (Heller), not structural inevitability. If false natural law: the constraint is extraction justified by appeal to constitutional text, citizen disarmament is not inevitable but is naturalized through constitutional discourse. This drives the false summit detection — the mountain perspective naturalizes what is structurally contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_amendment_as_natural_law_status, conceptual, 'Whether Second Amendment functions as genuine limit on federal power or false natural law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standing_army_structural_threat, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(standing_army_theater_1791, standing_army_structural_threat, theater_ratio, 0, 0.15).
narrative_ontology:measurement(standing_army_theater_1848, standing_army_structural_threat, theater_ratio, 57, 0.28).
narrative_ontology:measurement(standing_army_theater_1880, standing_army_structural_threat, theater_ratio, 89, 0.42).
narrative_ontology:measurement(standing_army_theater_1933, standing_army_structural_threat, theater_ratio, 142, 0.65).
narrative_ontology:measurement(standing_army_theater_1991, standing_army_structural_threat, theater_ratio, 201, 0.68).
narrative_ontology:measurement(standing_army_theater_2026, standing_army_structural_threat, theater_ratio, 235, 0.68).

% Extraction over time
narrative_ontology:measurement(standing_army_extractiveness_1791, standing_army_structural_threat, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(standing_army_extractiveness_1848, standing_army_structural_threat, base_extractiveness, 57, 0.38).
narrative_ontology:measurement(standing_army_extractiveness_1880, standing_army_structural_threat, base_extractiveness, 89, 0.52).
narrative_ontology:measurement(standing_army_extractiveness_1933, standing_army_structural_threat, base_extractiveness, 142, 0.65).
narrative_ontology:measurement(standing_army_extractiveness_1991, standing_army_structural_threat, base_extractiveness, 201, 0.58).
narrative_ontology:measurement(standing_army_extractiveness_2026, standing_army_structural_threat, base_extractiveness, 235, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standing_army_structural_threat, enforcement_mechanism).
narrative_ontology:affects_constraint(standing_army_structural_threat, militia_clause_interpretive_contest).
narrative_ontology:affects_constraint(standing_army_structural_threat, federal_military_expansion).
narrative_ontology:affects_constraint(standing_army_structural_threat, second_amendment_individual_right).

% DUAL FORMULATION NOTE:
% The standing_army_structural_threat is the operational constraint produced by the individual-right reading of the Second Amendment. A separate constraint (militia_clause_as_limit_reading) would model the alternative reading where the prefatory clause limits the operative clause to militia-serving arms. These are two readings of the same kernel with different ε values and different beneficiary/victim structures. The individual-right reading (this story) produces moderate extraction (0.58, Tangled Rope) because federal military monopoly is unchecked. The militia-clause-as-limit reading would produce higher suppression and higher extraction from citizen perspective because the constraint would explicitly authorize federal military superiority. Both readings are historically live; Heller decided in favor of individual-right interpretation, but the contest remains conceptually unresolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(standing_army_structural_threat, institutional, 0.12).
constraint_indexing:directionality_override(standing_army_structural_threat, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
