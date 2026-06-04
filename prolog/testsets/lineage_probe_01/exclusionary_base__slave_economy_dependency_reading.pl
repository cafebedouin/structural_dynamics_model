% ============================================================================
% CONSTRAINT STORY: exclusionary_base__slave_economy_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusionary_base__slave_economy_dependency_reading, []).

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
 *   constraint_id: exclusionary_base__slave_economy_dependency_reading
 *   human_readable: Athenian Democracy's Structural Dependence on Slave Labor (Dependency Reading)
 *   domain: political/historical
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested kernel
 *   'exclusionary_base': the claim that Athenian democracy was structurally
 *   dependent on slavery, particularly the slave labor extracted in the
 *   Laurion silver mines and in household and craft production. The kernel
 *   itself is contested across three mutually-acknowledging readings: the
 *   citizen_privilege_reading (democracy was guarded by restricting
 *   citizenship to those of double descent), the imperial_tribute_reading
 *   (democracy ran on imperial hegemony and allied tribute), and this reading
 *   (democracy ran on slave labor). Each reading identifies a different
 *   material foundation for the democracy's institutional structure, and each
 *   partially forecloses or influences the others depending on historical
 *   periodization and emphasis. This story models only the slave economy
 *   dependency reading as a clean ε-invariant constraint, without averaging
 *   or hedging across sibling readings. The structural claim is clear: the
 *   democracy bought the citizen his assembly pay and jury leisure by
 *   extracting the labor of an enslaved majority in the mines above all. The
 *   extractiveness is maximal (0.92) because the constraint's function is
 *   pure transfer of labor value and leisure opportunity. Suppression is
 *   total (0.95) because the enslaved majority has zero exit options and zero
 *   recourse. Theater ratio is minimal (0.15) because the constraint does not
 *   function through performance — it functions through brute material
 *   extraction and legal force.
 *
 * KEY AGENTS:
 *   - Enslaved miners and laborers (especially Laurion silver mines): Primary victims (powerless/trapped) — bear maximum extraction; supply the silver that funds citizen pay; face mortality rates 3-5x general population
 *   - Enslaved domestic and craft workers: Secondary victims (powerless/trapped) — extract household labor and craft production; enable citizen leisure for assembly and jury service
 *   - Non-citizen free laborers (metics): Tertiary victims (moderate/constrained) — pay labor tax; excluded from political participation despite enabling it; benefit partially from public goods
 *   - Participating male citizen: Primary beneficiary (institutional/arbitrage) — receive assembly pay (misthos) and jury pay (dikasteria); purchase leisure time to attend assembly and democratic functions
 *   - Democratic institutions (Assembly, law enforcement, property system): Institutional beneficiary (institutional/arbitrage) — maintain slavery legally and structurally; enforce suppression; naturalize slavery as apolitical
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees the constraint as foundational extraction enabling the democracy's entire structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusionary_base__slave_economy_dependency_reading, 0.92).
domain_priors:suppression_score(exclusionary_base__slave_economy_dependency_reading, 0.95).
domain_priors:theater_ratio(exclusionary_base__slave_economy_dependency_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusionary_base__slave_economy_dependency_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(exclusionary_base__slave_economy_dependency_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(exclusionary_base__slave_economy_dependency_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusionary_base__slave_economy_dependency_reading, snare).
narrative_ontology:human_readable(exclusionary_base__slave_economy_dependency_reading, "Athenian Democracy's Structural Dependence on Slave Labor (Dependency Reading)").
narrative_ontology:topic_domain(exclusionary_base__slave_economy_dependency_reading, "political/historical").

domain_priors:requires_active_enforcement(exclusionary_base__slave_economy_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exclusionary_base__slave_economy_dependency_reading, '421b46ec-4a9e-4ce6-aa04-b59a92d91606').
narrative_ontology:cs_kernel_codification('421b46ec-4a9e-4ce6-aa04-b59a92d91606', formalized).
narrative_ontology:cs_authority_grounding('421b46ec-4a9e-4ce6-aa04-b59a92d91606', extraction).
narrative_ontology:cs_reading_relation('421b46ec-4a9e-4ce6-aa04-b59a92d91606', exclusionary_base__citizen_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('421b46ec-4a9e-4ce6-aa04-b59a92d91606', exclusionary_base__imperial_tribute_reading, coexists_with).
narrative_ontology:cs_axiom('421b46ec-4a9e-4ce6-aa04-b59a92d91606', foundational, slavery_is_foundational_extraction_axis).
narrative_ontology:cs_axiom_status(slavery_is_foundational_extraction_axis, holdable).
narrative_ontology:cs_axiom_grounding('421b46ec-4a9e-4ce6-aa04-b59a92d91606', slavery_is_foundational_extraction_axis, empirically_contingent).
narrative_ontology:cs_axiom('421b46ec-4a9e-4ce6-aa04-b59a92d91606', foundational, enslaved_majority_has_zero_political_standing).
narrative_ontology:cs_axiom_status(enslaved_majority_has_zero_political_standing, holdable).
narrative_ontology:cs_axiom_grounding('421b46ec-4a9e-4ce6-aa04-b59a92d91606', enslaved_majority_has_zero_political_standing, deontological).
narrative_ontology:cs_reference_frame('421b46ec-4a9e-4ce6-aa04-b59a92d91606', slave_labor_funded_democracy).
narrative_ontology:cs_drift_state('421b46ec-4a9e-4ce6-aa04-b59a92d91606', contemporary_late_athenian_period_5th_century_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('421b46ec-4a9e-4ce6-aa04-b59a92d91606', '').
narrative_ontology:cs_kernel_id(exclusionary_base__slave_economy_dependency_reading, exclusionary_base).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusionary_base__slave_economy_dependency_reading, participating_male_citizens).
narrative_ontology:constraint_victim(exclusionary_base__slave_economy_dependency_reading, enslaved_population).
narrative_ontology:constraint_victim(exclusionary_base__slave_economy_dependency_reading, non_citizen_laborers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED MINERS (SNARE) — Maximum experienced extraction. The enslaved majority, especially those in the Laurion silver mines, face complete structural immobility. The mines extract silver that funds citizen pay; labor is exhausting and fatal; escape is suppressed by force and legal death penalty. Exit is impossible — slavery is hereditary and enforced by Athenian law. No alternative exists within the political order. This is extraction in its purest form: the constraint exists entirely to transfer labor value and leisure time from the enslaved to the citizen.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ENSLAVED DOMESTIC AND CRAFT WORKERS (SNARE) — Similar structural immobility to miners, though with slightly higher survival rates and lower mortality. The constraint extracts their labor for household maintenance, craft production, and service. Slave ownership is dispersed (every citizen household contains enslaved persons), making suppression structural and distributed. No escape, no recourse, no counter-organization. The constraint is foundational to citizen leisure — without enslaved domestic labor, the citizen cannot afford the time to attend assembly or serve on juries.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: PARTICIPATING MALE CITIZEN (ROPE) — Experiences the constraint as pure coordination and benefit. The citizen receives assembly pay (misthos) funded by silver mines worked by enslaved labor. The citizen receives jury pay funded by the same source. The citizen's leisure time to participate in democracy is purchased by enslaved labor. From the citizen's perspective, this is coordination: the system solves the problem of how to fund mass political participation. The citizen does not experience this as extractive coercion — it is a legitimate institutional arrangement that makes their political rights possible. This is high beneficiary positioning: d ≈ 0.05, resulting in negative or minimal effective extraction from the citizen's view.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: NON-CITIZEN FREE LABORER / METIC (TANGLED ROPE) — Metics occupy a hybrid status: free but non-citizen, with legal protections but no political rights and high labor tax (metoikion). The constraint extracts their labor tax while denying them the political participation that citizen slaves make possible. However, metics also benefit from the stable legal order that the slave-funded infrastructure enables — roads, harbor, markets, security. Extraction is real but mixed with genuine public goods provision. Exit is constrained: leaving means abandoning accumulated property and trade networks, though it is technically possible (unlike chattel slavery). The metic experiences partial suppression (legal barriers to political participation) alongside constrained economic mobility.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: DEMOCRATIC INSTITUTIONAL APPARATUS (PITON) — The democratic apparatus presents itself as fundamentally about freedom, equality, and justice (isonomia, isegoria). Yet its operational function depends entirely on enslaved labor. The theater_ratio here is low (0.15) because the constraint's function is not performative — it is brutally material. However, the piton classification emerges from the institutional contradiction: democratic institutions must naturalize slavery as non-political (outside the scope of isonomia) while depending on it absolutely. The democratic rhetoric is increasingly strained by the 5th-century ethical pressures (sophists questioning slavery, tragedies staging enslaved perspectives). The institution maintains the constraint through this cognitive dissonance, not through active functional coherence. The enforcement apparatus is degraded by the gap between democratic principle and extractive practice.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the civilizational view, this constraint is pure extraction embedded in the foundational institutions of a political order. The democracy's claim to be a system of equal citizen rights depends entirely on the exclusion and domination of the enslaved majority. The constraint's function is not to solve a coordination problem — it is to create and maintain a structural inequality. The entire edifice of Athenian democracy is predicated on this extraction. The analytical observer classifies this as snare because: extractiveness is maximal (0.92), suppression is total (0.95), the constraint's only function is to transfer wealth and leisure time, and the beneficiary group (male citizens) actively suppresses alternatives (resistance ≤ 0.05 because suppression is nearly total). There is no coordination function that benefits any other group. This is extraction in its structural purity.
constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusionary_base__slave_economy_dependency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exclusionary_base__slave_economy_dependency_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exclusionary_base__slave_economy_dependency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exclusionary_base__slave_economy_dependency_reading, TR),
    TR >= 0.70.

:- end_tests(exclusionary_base__slave_economy_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92): Maximum. The constraint extracts the labor output of an enslaved population estimated at 15-30% of total Athenian population to fund the institutional structure of democracy. The silver mines alone produced enormous wealth: the Laurion mines output roughly 50-75 talents of silver per year in peak periods, directly funding citizen assembly pay and public expenditure. Household slavery extracted domestic labor that freed male citizens for political participation. Craft slavery produced goods. The constraint's sole function is to transfer labor value from the enslaved to the citizen. Suppression (0.95): Total. Exit options for the enslaved are zero: slavery is legal, hereditary, enforced by death penalty for escape. No alternatives exist within the political system — the enslaved cannot appeal to law, cannot own property, cannot transmit freedom to children. Suppression is structural (legal codes) and distributed (every citizen household participates in enforcement). Enforcement capacity is nearly complete. Theater ratio (0.15): Minimal. The constraint operates through direct material extraction and force, not through performance or rhetoric. The mines run; the slaves work; the silver is collected; the pay is distributed. Democratic rhetoric (about isonomia and freedom) exists, but the constraint itself is unperformed — it is brute extraction. Increasing theater ratio in later periods (to 0.22 by end of interval) reflects emerging ethical pressure (sophistic arguments, tragic drama staging enslaved perspectives) that forces the institutional apparatus to add rhetorical cover (claims that slavery is natural, or necessary, or outside the political community). This rising theater is a signature of ideological strain, not functional degradation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between victim and beneficiary is maximal. The enslaved see a pure snare with no exit (trapped exit, powerless agent, maximum extraction experienced). The citizen sees a rope of coordination and legitimate benefit (arbitrage exit, institutional agent, positive extraction flowing toward them). The analytical observer sees pure extraction (snare) when analyzing the system as a whole. The metic occupies the middle (tangled rope: some suppression, some benefit). The institutional apparatus experiences cognitive dissonance (piton: the rhetoric of freedom contradicts the practice of slavery, forcing performative cover). No common ground exists between the beneficiary's experience (legitimate participation) and the victim's experience (chattel slavery). The constraint is not perceived differently depending on context — it is functionally different for different agents. For the citizen, it is coordination. For the enslaved, it is extraction. The constraint CANNOT be classified the same way from both perspectives because the structural relationship is structurally asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's power level, exit options, and beneficiary/victim status. ENSLAVED AGENTS (perspectives 1-2): victims, trapped, powerless → d ≈ 0.95 (full target). Sigmoid f(d) ≈ 1.42 (maximum). CITIZEN BENEFICIARY (perspective 3): beneficiary, arbitrage, institutional → d ≈ 0.05 (full beneficiary). Sigmoid f(d) ≈ -0.12 (negative extraction, institution subsidized by constraint). METIC (perspective 4): neither pure beneficiary nor victim, constrained exit, moderate power → d ≈ 0.55 (slightly victim-leaning). Sigmoid f(d) ≈ 0.75 (moderate extraction). INSTITUTIONAL APPARATUS (perspective 5): complex: nominally arbitrage but increasingly constrained by ethical pressure → d ≈ 0.50 (symmetric stress). ANALYTICAL OBSERVER (perspective 6): observing from external position, seeing pure asymmetry → d ≈ 0.72 (observer derived value). No overrides are necessary; the structural data produces the directionalities directly. The beneficiary experiences subsidization (negative chi); the victims experience maximum extraction (positive chi approaching 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint is pure extraction (snare from analytical perspective, ε=0.92, χ≥0.66 at victim perspective). There is no mandatrophy tension because the constraint has no legitimate coordination function that could be mistaken for extraction. The citizen benefits, but the constraint's function is pure transfer — it does not solve a coordination problem for the enslaved or for the metic. It solves a resource problem for the citizen by forcibly extracting from the enslaved majority. The constraint is not tangled (there is no genuine coordination benefit for victims), not rope (there is no mutual benefit), not scaffold (it has no sunset clause and is not temporary). The classification as snare is stable across all agent perspectives. The mandatrophy (the paradox that extraction and coordination appear the same from outside) does not arise because the constraint is transparently extractive when analyzed from the victim's perspective. The citizen's experience (rope, benefit) is subordinate to the analytical observer's finding (snare, pure extraction). The constraint's mandate is extraction; there is no alternative function hiding beneath a coordination mask.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slave_economy_scale_ambiguity,
    'What proportion of Athenian economy and citizen leisure time was directly funded by enslaved labor vs. citizen-owned land, trade, or imperial tribute?',
    'Quantitative historical analysis: estimate slave population (15-30% of total population), proportion working in mines vs. household/craft, output of Laurion mines, calculation of citizen assembly pay budget, cross-reference with other revenue sources (tribute, citizen land productivity)',
    'If >70% of citizen leisure time budget comes from slave labor: constraint is purely extractive (snare, ε→1.0). If 30-70%: constraint is partially extractive but mixes multiple funding sources (tangled_rope likely). If <30%: slave labor was supplementary, not foundational (extraction drops, constraint reclassifies)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slave_economy_scale_ambiguity, empirical, 'Proportion of democratic funding from slave labor vs. other sources').

omega_variable(
    citizen_ideological_recognition_gap,
    'Did the participating citizen ideologically recognize that their leisure depended on slavery, or did they naturalize slavery as external to political community?',
    'Textual analysis: Athenian philosophical and political sources (Aristotle, Plato, Assembly speeches, comic drama). Search for: explicit acknowledgment that slavery funds democracy; naturalization of slavery as apolitical; sophistic or ethical questioning of slavery; citizenship rhetoric that brackets slavery from isonomia discourse',
    'If widespread recognition: citizens were consciously choosing extraction (agency raises d from beneficiary perspective, no classification change but changes interpretation of moral standing). If naturalization dominant: citizens believed slavery was outside political community (ideology maintains snare suppression, validates this reading). Impacts omega variable on identity_locked vs. constrained for citizen perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citizen_ideological_recognition_gap, conceptual, 'Whether citizens recognized or naturalized dependence on slavery').

omega_variable(
    alternative_funding_counterfactual,
    'Was mass citizen participation (assembly pay, jury pay, festival funding) structurally possible through non-slave revenue sources? Could the democracy have sustained the same participatory structure with metic labor, citizen income, or reduced public expenditure?',
    'Historical counterfactual: estimate required revenues for full democratic participation; calculate alternative revenue streams (increased metoikion, citizen liturgies, reduced scale, rationed participation by socioeconomic class); assess whether other Greek poleis achieved comparable participation with lower or non-slave labor dependence',
    'If alternatives existed and were rejected: suppression is active choice, not necessity (strengthens snare classification, raises mandatrophy concern). If alternatives were structurally impossible: constraint approaches mountain classification (necessary condition of democracy). If partial alternatives existed: constraint is tangled_rope (mix of necessary coordination and active extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_counterfactual, conceptual, 'Whether alternative funding structures could have sustained democratic participation').

omega_variable(
    reading_vs_imperial_tribute_dominance,
    'Does the slave economy reading or the imperial tribute reading better explain the material foundation of Athenian democracy? Did tribute or slave labor dominate the funding stream?',
    'Quantitative historical analysis: estimate total tribute revenue over the period; estimate total Laurion silver output; cross-reference with documented citizen pay budgets and temple construction; separate pre-empire (before 478 BCE) from empire period (478-404 BCE)',
    'If slave labor > tribute: this reading dominates; imperial tribute reading is secondary. If tribute > slave labor: readings coexist with different emphasis periods. If pre-empire slavery > post-empire tribute: readings sequence historically rather than coexist. Outcome determines whether reading_relations use ''coexists_with'' or ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_imperial_tribute_dominance, empirical, 'Relative material dominance of slave labor vs. imperial tribute in funding democracy').

omega_variable(
    natural_law_false_summit_candidate,
    'Is this constraint a reading of a socio-political kernel (contingent institutional arrangement), or does it claim to reveal a natural law about democracy''s material structure?',
    'Philosophical analysis: determine whether the constraint-as-authored claims that democracy *requires* slavery as a necessary condition (natural law reading, mountain candidate) or whether slavery was a *contingent choice* by Athenian institutions (kernel reading, snare). Check: does the axiom ground in necessity or choice?',
    'If natural law framing: false summit detector triggers because beneficiaries are declared (participating citizens benefit). Reclassification pathway opens. If explicitly contingent: snare classification stands. If ambiguous: omega documents the ambiguity as a reading contest within the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_false_summit_candidate, conceptual, 'Whether this reading naturalizes slavery as structural necessity or treats it as contingent choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusionary_base__slave_economy_dependency_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meas_theater_period_start_brutal_clarity, exclusionary_base__slave_economy_dependency_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(meas_theater_period_peak_minimal_theater, exclusionary_base__slave_economy_dependency_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(meas_theater_period_decline_rhetorical_cover, exclusionary_base__slave_economy_dependency_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(meas_extract_period_start_laurion_expansion, exclusionary_base__slave_economy_dependency_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(meas_extract_period_peak_silver_output, exclusionary_base__slave_economy_dependency_reading, base_extractiveness, 30, 0.92).
narrative_ontology:measurement(meas_extract_period_decline_war_losses, exclusionary_base__slave_economy_dependency_reading, base_extractiveness, 60, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(meas_suppress_period_start_law_enforcement, exclusionary_base__slave_economy_dependency_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(meas_suppress_period_peak_resistance_suppression, exclusionary_base__slave_economy_dependency_reading, suppression_requirement, 30, 0.95).
narrative_ontology:measurement(meas_suppress_period_decline_ethical_pressure, exclusionary_base__slave_economy_dependency_reading, suppression_requirement, 60, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusionary_base__slave_economy_dependency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exclusionary_base__slave_economy_dependency_reading, exclusionary_base__citizen_privilege_reading).
narrative_ontology:affects_constraint(exclusionary_base__slave_economy_dependency_reading, exclusionary_base__imperial_tribute_reading).

% DUAL FORMULATION NOTE:
% The exclusionary_base kernel admits three distinct constraint stories corresponding to three contested readings of what foundational extraction enabled Athenian democracy. This story models the slave economy reading: slavery in the mines and households funded citizen participation. Each sibling reading has its own constraint file with its own ε, its own beneficiary/victim structure, and its own perspectives. The readings coexist in historical discourse and each may be partially true (slavery AND tribute AND citizenship tightening all mattered), but they are structurally distinct claims about causal priority and constitute distinct constraints with distinct extractiveness values. This decomposition follows the ε-invariance principle: measuring the constraint one way (via slave labor output) gives ε≈0.92; measuring it another way (via tribute revenue) gives ε≈0.70 for the imperial reading; measuring it a third way (via citizenship restriction effects) gives ε≈0.55 for the privilege reading. Because ε differs across measurement frames, we have three constraints, not one. Each is linked via network.affects_constraints to show that these readings compete to explain the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exclusionary_base__slave_economy_dependency_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
