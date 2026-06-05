% ============================================================================
% CONSTRAINT STORY: autocratic_rule__hereditary_monarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autocratic_rule__hereditary_monarchy, []).

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
 *   constraint_id: autocratic_rule__hereditary_monarchy
 *   human_readable: Hereditary Monarchy as Autocratic Rule
 *   domain: political/comparative
 *
 * SUMMARY:
 *   Hereditary monarchy as a form of autocracy solves the succession problem
 *   by removing the throne from contestation through a rule of birth. Instead
 *   of every death of a ruler triggering a succession war (the fate of less
 *   institutionalized autocracies), hereditary succession establishes a fixed
 *   pathway: the eldest son, or the designated heir of the ruling line,
 *   automatically succeeds. This constraint is ONE READING of a contested
 *   kernel — the problem of how autocratic rule perpetuates itself.
 *   Alternative readings include military junta (officers as collective),
 *   party autocracy (organizational discipline), and personalist dictatorship
 *   (leader cult and institution destruction). The hereditary monarchy
 *   reading suppresses succession contests through the permanent force of
 *   birth rule, benefiting the dynasty and its court through stable revenue
 *   and predictable hierarchies while extracting from non-dynastic
 *   populations through taxation and compulsory service. The constraint
 *   exhibits tangled rope structure: genuine coordination of elite
 *   competition is embedded within asymmetric extraction from the subject
 *   population. The theater ratio increases over the dynasty's lifespan as
 *   the rule becomes ceremonial — coronation rituals, legitimacy narratives,
 *   and formal succession law increasingly sustain what force initially
 *   established.
 *
 * KEY AGENTS:
 *   - Dynastic Lineage: Primary beneficiary (institutional/arbitrage) — captured succession rule removes the threat of internal contestation; extracts surplus through permanent tax and service claims
 *   - Court Aristocracy: Secondary beneficiary (institutional/constrained) — benefits from stable rank hierarchy and tax-farming revenue; constrained by crown's extractive capacity and the successor's power to replace them
 *   - Non-Dynastic Talent: Primary victim (moderate/constrained) — barred from the throne by birth rule; some can rise within the hierarchy, but the path to supreme power is closed permanently
 *   - Excluded Factions: Primary victim (moderate/constrained) — rival military, merchant, or noble families that challenge the dynasty; face armed suppression and exclusion from rule
 *   - Peasant Base: Structural victim (powerless/trapped) — subject to the dynasty's extraction through taxation, conscription, and compulsory labor; no exit from the kingdom and no alternative authority
 *   - Institutional Memory: Keeper of theater (institutional/constrained) — religious authorities, legal scholars, ceremonial officials who maintain the legitimacy narratives; constrained to preserve the rule even as its enforcement capacity decays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autocratic_rule__hereditary_monarchy, 0.58).
domain_priors:suppression_score(autocratic_rule__hereditary_monarchy, 0.72).
domain_priors:theater_ratio(autocratic_rule__hereditary_monarchy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autocratic_rule__hereditary_monarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(autocratic_rule__hereditary_monarchy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(autocratic_rule__hereditary_monarchy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autocratic_rule__hereditary_monarchy, tangled_rope).
narrative_ontology:human_readable(autocratic_rule__hereditary_monarchy, "Hereditary Monarchy as Autocratic Rule").
narrative_ontology:topic_domain(autocratic_rule__hereditary_monarchy, "political/comparative").

domain_priors:requires_active_enforcement(autocratic_rule__hereditary_monarchy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autocratic_rule__hereditary_monarchy, 'd89f3c4a-a415-40fb-89f0-9a14d84731f5').
narrative_ontology:cs_kernel_codification('d89f3c4a-a415-40fb-89f0-9a14d84731f5', formalized).
narrative_ontology:cs_authority_grounding('d89f3c4a-a415-40fb-89f0-9a14d84731f5', lineage).
narrative_ontology:cs_interpretation_layer_present('d89f3c4a-a415-40fb-89f0-9a14d84731f5').
narrative_ontology:cs_reading_relation('d89f3c4a-a415-40fb-89f0-9a14d84731f5', autocratic_rule__military_junta, coexists_with).
narrative_ontology:cs_reading_relation('d89f3c4a-a415-40fb-89f0-9a14d84731f5', autocratic_rule__party_autocracy, coexists_with).
narrative_ontology:cs_reading_relation('d89f3c4a-a415-40fb-89f0-9a14d84731f5', autocratic_rule__personalist_dictatorship, coexists_with).
narrative_ontology:cs_axiom('d89f3c4a-a415-40fb-89f0-9a14d84731f5', foundational, birth_determines_succession_right).
narrative_ontology:cs_axiom_status(birth_determines_succession_right, holdable).
narrative_ontology:cs_axiom_grounding('d89f3c4a-a415-40fb-89f0-9a14d84731f5', birth_determines_succession_right, conventional).
narrative_ontology:cs_axiom('d89f3c4a-a415-40fb-89f0-9a14d84731f5', foundational, bloodline_legitimacy_trumps_merit).
narrative_ontology:cs_axiom_status(bloodline_legitimacy_trumps_merit, holdable).
narrative_ontology:cs_axiom_grounding('d89f3c4a-a415-40fb-89f0-9a14d84731f5', bloodline_legitimacy_trumps_merit, theological).
narrative_ontology:cs_reference_frame('d89f3c4a-a415-40fb-89f0-9a14d84731f5', stable_dynasty_succession).
narrative_ontology:cs_drift_state('d89f3c4a-a415-40fb-89f0-9a14d84731f5', contemporary_state_system, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d89f3c4a-a415-40fb-89f0-9a14d84731f5', '').
narrative_ontology:cs_kernel_id(autocratic_rule__hereditary_monarchy, autocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autocratic_rule__hereditary_monarchy, dynastic_lineage).
narrative_ontology:constraint_beneficiary(autocratic_rule__hereditary_monarchy, court_aristocracy).
narrative_ontology:constraint_victim(autocratic_rule__hereditary_monarchy, non_dynastic_talent).
narrative_ontology:constraint_victim(autocratic_rule__hereditary_monarchy, excluded_factions).
narrative_ontology:constraint_victim(autocratic_rule__hereditary_monarchy, peasant_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-DYNASTIC SUBJECT (SNARE) — The peasant or non-noble talent is trapped within the kingdom's borders and subject to the hereditary ruler's extraction. No alternative authority exists; no exit path leads to retained livelihood. The constraint extracts labor, taxes, and deference through permanent structural assignment by birth.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED FACTION (SNARE) — Military, merchant, or noble families excluded from succession experience the constraint as pure extraction. They bear the costs of the regime's military campaigns and revenue demands without access to the rule-making apparatus. Exit is constrained by property ties, family networks, and the border-patrol state. The hereditary succession rule actively suppresses their pathway to power, and the suppression is permanent.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURT ARISTOCRACY (TANGLED ROPE) — The nobility and military attached to the ruling dynasty experience genuine coordination benefits (stable rank hierarchy, predictable revenue from tax farming, security against lower-class revolt) alongside significant extraction (crown takes the surplus, heir can replace you). The constraint coordinates the elite's internal competition through a fixed rule (birth succession) while the crown extracts from both the coordination function and the extraction from below. Active enforcement of the birth rule is essential — without it, every reign becomes a succession war.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DYNASTIC LINEAGE (ROPE) — The ruling dynasty experiences the hereditary rule as pure coordination: it eliminates the succession war within every reign except those where the rule breaks. The dynasty's extraction of surplus from the realm is enabled by the rule, but the rule itself solves a genuine coordination problem (preventing the elite from tearing itself apart). The dynasty has arbitrage power — it can flee or negotiate terms with rivals if the regime destabilizes.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY (PITON) — The formalized succession law (primogeniture, agnatic rules, clear heir designation) persists through ceremony and ritual even when the underlying enforcement capacity decays. The rule's theater — coronation, oath-taking, legitimacy narratives — maintains the fiction of smooth succession even as factions plot. As enforcement mechanisms weaken (late regimes, weakened armies), the rule becomes increasingly performative; the theater sustains what force once did. High theater, degraded function.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the succession problem is inherent to autocratic rule: any rule system must solve the problem of peaceful power transition or collapse into succession war. Hereditary monarchy is one solution to an irreducible structural problem. This perspective risks naturalizing what is actually a contingent institutional arrangement — the reading will likely trigger false summit detection because identifiable beneficiaries (the dynasty) clearly benefit from treating the rule as inevitable.
constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autocratic_rule__hereditary_monarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autocratic_rule__hereditary_monarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autocratic_rule__hereditary_monarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(autocratic_rule__hereditary_monarchy, TR),
    TR >= 0.70.

:- end_tests(autocratic_rule__hereditary_monarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dynasty captures surplus through tax-farming permanence and extractive revenue collection. However, extraction is not maximal because the coordination function (elite stability) is genuine — the dynasty does solve a real problem (preventing succession wars within the realm), and this function generates some actual efficiency gain rather than pure transfer. The extractiveness trajectory shows slight accumulation over 50 years as the rule becomes more formalized and extractive mechanisms (taxation systems, feudal obligations) become more systematic. Suppression (0.72): High. The birth rule is maintained through active enforcement: military control of rival claimants, ideological indoctrination (the sacredness of the bloodline), legal punishment of succession challenges, and geographic isolation of threats. Suppression is structural — the regime must continuously work to prevent succession contestation because the underlying incentive (ambitious elites want power) never disappears. Theater ratio (0.65): Moderate-high and rising. Early in the dynasty's rule, succession is backed by real force (the founder's military defeated rivals; the heir is defended by loyal armies). As the dynasty matures and the succession rule becomes institutionalized, theater increases — coronation ceremonies, oath-taking, legitimacy narratives about blood and destiny increasingly sustain what force once did. At 50 years, the theater-to-force ratio is rising, indicating potential vulnerability as enforcement capacity might be weakening relative to ceremonial maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The dynasty experiences pure coordination (Rope) — the rule solves a genuine problem at low cost to them. The court aristocracy experiences tangled rope — they benefit from stable hierarchy but are constrained by the crown's extraction capacity. Excluded factions and non-dynastic talent experience snare — the birth rule permanently bars them from power and forces submission. The peasant base experiences snare with maximum extraction — permanently trapped, no exit, no voice. The institutional memory of succession law experiences piton — the rule is increasingly performed through ceremony as force decays. The analytical observer risks mountain (natural law) but structural evidence points to false summit (naturalized extraction). The perspectival gap is maximal: from the dynasty's view this is a low-extraction coordination mechanism; from the subject's view it is maximal extraction justified by hereditary accident.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's structural position relative to the constraint. The dynasty and court are beneficiaries with institutional power and arbitrage/constrained exit — they derive low or negative d values, experiencing the constraint as beneficial coordination. Non-dynastic actors are victims with constrained or trapped exit — they derive high d values, experiencing maximal extraction. The peasant base is trapped with powerless status — maximum d value, maximum experienced extraction. The analytical observer at the universal/civilizational level risks neutral d (0.50), which would classify the constraint as mountain if the underlying metrics supported it. However, the base metrics show clear beneficiary/victim structure (suppression=0.72, extractiveness=0.58), preventing the mountain classification. The false summit detection will note that treating succession-by-birth as an immutable law naturalizes a contingent institutional arrangement — the dynasty benefits from treating it as inevitable, and the suppression mechanisms prove the rule requires active enforcement rather than natural emergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy at the reading level. The kernel contest (hereditary monarchy vs. military junta vs. party autocracy vs. personalist dictatorship) represents competing solutions to the same political problem: how does autocratic rule perpetuate itself? Each reading offers a different answer, with different beneficiary structures and suppression mechanisms. Hereditary monarchy routes succession through the bloodline; military junta through officer consensus; party autocracy through organizational discipline; personalist dictatorship through leader cult and rival-institution destruction. The mandatrophy is resolved by recognizing that all four are live readings of the same kernel — they coexist in political discourse because different regimes instantiate different solutions. This constraint (the hereditary monarchy reading) has ε=0.58 and tangled_rope structure. Its sibling readings will have different ε values and structures reflecting their different mechanisms. The false summit risk emerges when the analytical observer tries to treat succession-by-birth as a law of nature rather than a contested institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    succession_stability_mechanism,
    'Does the hereditary rule actually prevent succession wars, or does it merely displace them to non-reigning years and create devastating wars when the line fails?',
    'Historical frequency analysis: succession wars in hereditary monarchies vs. military juntas vs. party autocracies; correlation between line stability and regime duration',
    'If hereditary rule is genuinely stabilizing: classification is Rope for the dynasty. If succession wars are equally frequent but concentrated in line-break years: the rule transfers rather than prevents conflict, and classification shifts toward Snare (suppression of normal succession conflict creates catastrophic wars when rules break).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(succession_stability_mechanism, empirical, 'Whether hereditary rule prevents succession wars or displaces them').

omega_variable(
    non_dynastic_talent_alternative_outlets,
    'What proportion of excluded talent can escape the kingdom entirely vs. finding career outlets within the hierarchy despite exclusion from the throne?',
    'Historical demographic analysis of emigration patterns, military promotion structures in hereditary monarchies, merchant access to trade monopolies, and institutional career paths for non-dynastic figures',
    'If escape is common (>20% of ambitious non-dynastic figures leave): exit is more mobile than trapped, and the snare classification is overestimated. If escape is rare (<5%): the trapped classification is confirmed, and suppression should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_dynastic_talent_alternative_outlets, empirical, 'Availability of exit paths for non-dynastic ambitious actors').

omega_variable(
    comparison_to_sibling_readings,
    'What structural mechanisms distinguish hereditary monarchy from military junta, party autocracy, and personalist dictatorship?',
    'This is a kernel reading. The sibling readings are distinct constraints with different beneficiary sets, suppression mechanisms, and failure modes. Hereditary monarchy suppresses succession via birth rule; military junta suppresses it via officer consensus; party autocracy via organizational discipline; personalist dictatorship via leader cult and institution-destruction. Each reading is internally consistent and generates a different constraint story.',
    'This omega documents that the kernel is a contested political reality, not an error in classification. Different readings coexist in political discourse because different autocratic regimes instantiate different solutions to the succession problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comparison_to_sibling_readings, conceptual, 'Structural relationship between hereditary monarchy and sibling readings of the autocratic rule kernel').

omega_variable(
    legitimacy_narrative_collapse,
    'When the hereditary line fails (extinction, weak heir, disputed succession), does the regime collapse, or does it transition to an alternative form (military junta, strongman, new dynasty)?',
    'Historical case studies of line failure: Ottoman succession crises, succession wars in European monarchies, Chinese dynastic cycles, African post-colonial states. Track whether failure is catastrophic or transitional.',
    'If collapse is common: hereditary monarchy is fragile and extractiveness is higher than measured (suppression works only as long as the line holds). If transition to alternative forms is common: the kernel reading structure is confirmed — different autocratic forms address the same succession problem in different ways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_narrative_collapse, empirical, 'Regime stability under hereditary succession rule failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autocratic_rule__hereditary_monarchy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hm_tr_t0, autocratic_rule__hereditary_monarchy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hm_tr_t20, autocratic_rule__hereditary_monarchy, theater_ratio, 20, 0.62).
narrative_ontology:measurement(hm_tr_t50, autocratic_rule__hereditary_monarchy, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(hm_be_t0, autocratic_rule__hereditary_monarchy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hm_be_t20, autocratic_rule__hereditary_monarchy, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(hm_be_t50, autocratic_rule__hereditary_monarchy, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hm_su_t0, autocratic_rule__hereditary_monarchy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hm_su_t20, autocratic_rule__hereditary_monarchy, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hm_su_t50, autocratic_rule__hereditary_monarchy, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autocratic_rule__hereditary_monarchy, enforcement_mechanism).
narrative_ontology:affects_constraint(autocratic_rule__hereditary_monarchy, autocratic_rule__military_junta).
narrative_ontology:affects_constraint(autocratic_rule__hereditary_monarchy, autocratic_rule__party_autocracy).
narrative_ontology:affects_constraint(autocratic_rule__hereditary_monarchy, autocratic_rule__personalist_dictatorship).
narrative_ontology:affects_constraint(autocratic_rule__hereditary_monarchy, succession_war_frequency).
narrative_ontology:affects_constraint(autocratic_rule__hereditary_monarchy, elite_extraction_hierarchy).

% DUAL FORMULATION NOTE:
% Hereditary monarchy is one reading of the autocratic_rule kernel. The sibling readings (military junta, party autocracy, personalist dictatorship) are separate constraint stories with different ε values and structural mechanics. All four readings share the same upstream kernel (succession problem) but diverge on the mechanism. Network links show constraint family structure: they affect each other through regime transitions (a failing dynasty may be replaced by a military takeover; a junta may establish a party structure). The dual formulation is not reading-symmetry but rather constraint-family hierarchy: the kernel (succession problem) upstream of all four readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
