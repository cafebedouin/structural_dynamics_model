% ============================================================================
% CONSTRAINT STORY: ideological_diversity_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ideological_diversity_convergence, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: ideological_diversity_convergence
 *   human_readable: Ideological Diversity Convergence in Zionist Territorial Policy
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The ideological diversity convergence constraint describes how multiple
 *   Zionist ideological streams — Labor Zionism's socialist collectivism,
 *   Revisionist Zionism's militant nationalism, Cultural Zionism's emphasis
 *   on Hebrew revival, and Religious Zionism's theological territorialism —
 *   produced unified territorial maximalism despite offering radically
 *   different justifications. Labor Zionists justified territorial expansion
 *   through security needs and socialist land redemption; Revisionists
 *   through historical rights and national honor; Cultural Zionists through
 *   the creation of a 'New Hebrew' identity rooted in the land; Religious
 *   Zionists (especially post-1967) through divine mandate and messianic
 *   process. The convergence mechanism operated throughout the movement's
 *   history but became most visible during partition debates (1937 Peel
 *   Commission, 1947 UN partition, post-1967 territorial disposition, Oslo
 *   process). In each case, ideological diversity provided a rich menu of
 *   justifications for territorial maximalism while systematically
 *   delegitimizing territorial compromise. The constraint exhibits both
 *   genuine coordination (unifying diverse factions behind state-building)
 *   and substantial extraction (overriding Palestinian territorial claims and
 *   marginalizing internal partition advocates). The theater ratio reflects
 *   that ideological debates were partly genuine (different factions held
 *   real ideological commitments) and partly performative (the debates masked
 *   a prior convergence on territorial outcomes). The constraint's
 *   suppression requirement increased sharply at statehood (1948) and again
 *   after the 1967 war, as the state apparatus gained capacity to enforce
 *   territorial maximalism through military, legal, and settlement
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - State-Building Apparatus: Primary beneficiary (institutional/arbitrage) — the convergence mechanism solves the coordination problem of unifying diverse ideological factions while providing diplomatic flexibility through selective deployment of justifications
 *   - Palestinian Territorial Claims: Primary victim (powerless/trapped) — bears extraction regardless of which Zionist ideology dominates discourse; no exit from the convergence mechanism's territorial outcomes
 *   - Partition Advocates Within Zionism: Secondary victim (moderate/constrained) — coordinated through unified state-building but systematically overridden on territorial compromise; includes Labor Zionist doves, Brit Shalom binationalists, and post-Oslo peace camp
 *   - Settlement Movement: Mixed beneficiary-victim (organized/constrained) — Religious Zionist settlers gain state support through the convergence but their theological autonomy is instrumentalized by secular state logic
 *   - International Partition Framework: Degraded institutional actor (institutional/mobile) — UN Resolution 181 persists as formal reference but has no operative force; maintained theatrically in diplomatic discourse
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both the coordination efficiency (solving factional collective action problem) and the extraction structure (systematic override of Palestinian claims and internal dissenters)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ideological_diversity_convergence, 0.68).
domain_priors:suppression_score(ideological_diversity_convergence, 0.75).
domain_priors:theater_ratio(ideological_diversity_convergence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ideological_diversity_convergence, extractiveness, 0.68).
narrative_ontology:constraint_metric(ideological_diversity_convergence, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ideological_diversity_convergence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ideological_diversity_convergence, tangled_rope).
narrative_ontology:human_readable(ideological_diversity_convergence, "Ideological Diversity Convergence in Zionist Territorial Policy").
narrative_ontology:topic_domain(ideological_diversity_convergence, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(ideological_diversity_convergence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ideological_diversity_convergence, '3c011a48-e611-44ff-86ea-179da6ec77a3').
narrative_ontology:cs_kernel_codification('3c011a48-e611-44ff-86ea-179da6ec77a3', distributed).
narrative_ontology:cs_authority_grounding('3c011a48-e611-44ff-86ea-179da6ec77a3', lineage).
narrative_ontology:cs_interpretation_layer_present('3c011a48-e611-44ff-86ea-179da6ec77a3').
narrative_ontology:cs_reading_relation('3c011a48-e611-44ff-86ea-179da6ec77a3', ideological_diversity_convergence__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('3c011a48-e611-44ff-86ea-179da6ec77a3', ideological_diversity_convergence__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('3c011a48-e611-44ff-86ea-179da6ec77a3', foundational, persecution_driven_necessity).
narrative_ontology:cs_axiom_status(persecution_driven_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3c011a48-e611-44ff-86ea-179da6ec77a3', persecution_driven_necessity, empirically_contingent).
narrative_ontology:cs_axiom('3c011a48-e611-44ff-86ea-179da6ec77a3', foundational, indigenous_return_legitimacy).
narrative_ontology:cs_axiom_status(indigenous_return_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3c011a48-e611-44ff-86ea-179da6ec77a3', indigenous_return_legitimacy, deontological).
narrative_ontology:cs_axiom('3c011a48-e611-44ff-86ea-179da6ec77a3', secondary, self_determination_primacy).
narrative_ontology:cs_axiom_status(self_determination_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3c011a48-e611-44ff-86ea-179da6ec77a3', self_determination_primacy, deontological).
narrative_ontology:cs_reference_frame('3c011a48-e611-44ff-86ea-179da6ec77a3', national_liberation_framework).
narrative_ontology:cs_drift_state('3c011a48-e611-44ff-86ea-179da6ec77a3', post_1967_territorial_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c011a48-e611-44ff-86ea-179da6ec77a3', '2026-06-06T03:31:32.471075+00:00').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, state_building_apparatus).
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, settlement_movement).
narrative_ontology:constraint_beneficiary(ideological_diversity_convergence, territorial_maximalist_coalition).
narrative_ontology:constraint_victim(ideological_diversity_convergence, palestinian_territorial_claims).
narrative_ontology:constraint_victim(ideological_diversity_convergence, partition_advocates).
narrative_ontology:constraint_victim(ideological_diversity_convergence, binational_framework_proponents).
narrative_ontology:constraint_vindicates(ideological_diversity_convergence, territorial_indivisibility_doctrine).
narrative_ontology:constraint_vindicates(ideological_diversity_convergence, security_requires_maximal_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN TERRITORIAL CLAIMS (SNARE) — Trapped by the convergence mechanism: regardless of which Zionist ideology dominates discourse, territorial outcomes remain maximalist. No exit from the extraction — Labor's 'security needs,' Revisionist 'historical rights,' Religious Zionist 'divine mandate,' and Cultural Zionist 'redemption of the land' all produce the same territorial dispossession. The ideological diversity is theater; the extraction is structural.
constraint_indexing:constraint_classification(ideological_diversity_convergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PARTITION ADVOCATES (TANGLED ROPE) — Constrained by the convergence dynamic but also coordinated through it: partition rhetoric provides diplomatic cover and internal legitimacy while the convergence mechanism ensures maximal territorial outcomes. Experience both coordination (unified state-building) and extraction (their territorial compromise position is systematically overridden). Can advocate for partition but cannot exit the maximalist policy outcome.
constraint_indexing:constraint_classification(ideological_diversity_convergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE-BUILDING APPARATUS (ROPE) — Primary beneficiary with arbitrage capacity: the convergence mechanism solves a genuine coordination problem (unifying diverse ideological factions behind territorial expansion) while the state apparatus can selectively deploy whichever ideological justification suits the diplomatic or domestic context. Experiences the constraint as pure coordination — ideological diversity is a resource, not a cost.
constraint_indexing:constraint_classification(ideological_diversity_convergence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLEMENT MOVEMENT (TANGLED ROPE) — Organized agents who both benefit from and are constrained by the convergence: Religious Zionist settlers gain state support through the mechanism's territorial maximalism, but their theological justifications are instrumentalized by secular state actors. Experience coordination (state resources, legal protection) and extraction (ideological autonomy subordinated to state strategic logic).
constraint_indexing:constraint_classification(ideological_diversity_convergence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL PARTITION FRAMEWORK (PITON) — The 1947 UN partition resolution persists as a formal reference point in international law and diplomacy, but its functional authority has atrophied completely. The convergence mechanism rendered partition unimplementable from the outset, yet the framework is maintained theatrically in diplomatic discourse. What remains is performance — states reference partition while recognizing it has no operative force.
constraint_indexing:constraint_classification(ideological_diversity_convergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, the convergence mechanism exhibits both genuine coordination (solving the collective action problem of unifying diverse Zionist factions behind state-building) and substantial extraction (systematically overriding Palestinian territorial claims and internal partition advocates). The mechanism's efficiency at producing territorial maximalism from ideological diversity is structurally impressive and morally contested.
constraint_indexing:constraint_classification(ideological_diversity_convergence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ideological_diversity_convergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ideological_diversity_convergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ideological_diversity_convergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ideological_diversity_convergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ideological_diversity_convergence, TR),
    TR >= 0.70.

:- end_tests(ideological_diversity_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The convergence mechanism produces substantial extraction from Palestinian territorial claims (complete override) and from internal partition advocates (systematic marginalization). The extraction is not total (0.68 rather than 0.85+) because the coordination function is genuine — the mechanism does solve a real collective action problem for the Zionist movement, and some internal debate space persists. The extractiveness increased sharply at statehood (1948: 0.68) and peaked post-1967 (0.72) when Religious Zionist theology was integrated into the state's territorial logic, then stabilized as the convergence became institutionalized. Suppression (0.75): High. Significant barriers to challenging the convergence include: state military and legal enforcement of territorial outcomes, delegitimization of territorial compromise as betrayal of Zionist principles across ideological streams, international diplomatic isolation of partition advocates, and internal movement discipline mechanisms. Suppression increased dramatically at statehood (0.65) and again post-1967 (0.78) as state capacity matured. Theater ratio (0.58): Moderate-high. Ideological debates were partly genuine (factions held real commitments and competed for movement leadership) and partly performative (debates masked prior convergence on territorial maximalism). The theater increased through the mandate period (0.35 → 0.55) as the convergence mechanism matured, peaked during the Oslo process (0.62) when partition rhetoric was most divorced from territorial policy, then declined slightly (0.58) as the convergence became more openly acknowledged in Israeli political discourse.
 *
 * PERSPECTIVAL GAP:
 *   The state-building apparatus sees pure coordination (Rope) — ideological diversity is a resource that enables diplomatic flexibility and factional unity. Palestinian territorial claims see pure extraction (Snare) — the ideological diversity is theater masking structural dispossession. Partition advocates see mixed coordination and extraction (Tangled Rope) — they are unified through state-building but systematically overridden on territorial compromise. The settlement movement also sees Tangled Rope — they gain state support but lose ideological autonomy. The international partition framework sees degraded performance (Piton) — the 1947 resolution persists formally but has no operative force. The analytical observer sees Tangled Rope at the civilizational scale — genuine coordination efficiency combined with substantial extraction. The perspectival gap is diagnostic: the same mechanism that solves a coordination problem for one agent (state apparatus) produces systematic extraction for another (Palestinian claims), and the ideological diversity that appears as genuine debate from one angle (partition advocates) appears as theater from another (trapped victims).
 *
 * DIRECTIONALITY LOGIC:
 *   The state-building apparatus is the primary beneficiary with arbitrage-level exit options — it can selectively deploy whichever ideological justification suits the context and can exit any particular ideological frame without losing territorial gains. Derived directionality is low (near 0.2), producing low or negative effective extraction (the constraint subsidizes this agent). Palestinian territorial claims are the primary victim with trapped exit options — no escape from the convergence mechanism's outcomes regardless of which ideology dominates. Derived directionality is maximal (near 0.95), producing maximum effective extraction. Partition advocates within Zionism are moderate-power victims with constrained exit — they can advocate for compromise but cannot exit the maximalist policy outcome; derived directionality is moderate-high (near 0.65). The settlement movement is organized with constrained exit — they benefit from state support but are instrumentalized by state logic; derived directionality is moderate (near 0.50), reflecting mixed coordination and extraction. The analytical observer has analytical exit and sees the full structure; derived directionality depends on the observer's normative frame but the structural extraction is visible regardless.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the convergence mechanism is simultaneously a coordination device (unifying diverse Zionist factions behind state-building) and an extraction mechanism (systematically overriding Palestinian territorial claims and internal partition advocates). The tangled_rope classification captures this duality: the coordination function is genuine (the mechanism does solve a real collective action problem for the Zionist movement), but the extraction is substantial and structurally embedded (the convergence produces territorial maximalism regardless of which ideology dominates discourse). The theater ratio (0.58) reflects that ideological debates were partly genuine and partly performative — factions held real commitments but the debates masked a prior convergence on territorial outcomes. The constraint is not a false summit (it is not a mountain naturalized as coordination) nor pure extraction (the coordination function is real). It is a hybrid where coordination and extraction are structurally inseparable: the same mechanism that coordinates diverse factions necessarily extracts from those outside the coalition and from dissenters within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_ideological_homogeneity,
    'Would ideological homogeneity within Zionism have produced different territorial outcomes, or does the convergence mechanism reveal that diverse justifications were always instrumentalizations of a prior territorial commitment?',
    'Comparative analysis of settler-colonial movements with ideologically homogeneous vs heterogeneous founding coalitions; examination of internal Zionist debates where territorial restraint was proposed and the mechanisms by which it was rejected',
    'If homogeneity would produce same outcome: the ideological diversity is pure theater, and the constraint is closer to pure snare. If diversity genuinely shaped outcomes: the coordination function is real, and tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_ideological_homogeneity, conceptual, 'Whether ideological diversity affected territorial outcomes or merely provided varied justifications for predetermined expansion').

omega_variable(
    partition_sincerity_threshold,
    'At what point did Labor Zionist partition advocacy shift from genuine territorial compromise position to diplomatic theater masking maximalist intent?',
    'Archival analysis of internal Labor party documents, private correspondence of leadership (Ben-Gurion, Sharett), and comparison of public partition rhetoric with contemporaneous settlement planning and military strategic documents',
    'If partition advocacy was sincere pre-1948 but became theater post-state-establishment: the convergence mechanism emerged historically rather than being inherent. If partition was always theater: the coordination function was present from the movement''s inception, and the extractiveness is higher than base metrics suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_sincerity_threshold, empirical, 'Historical timing of shift from genuine partition advocacy to theatrical deployment of partition rhetoric').

omega_variable(
    religious_zionist_autonomy,
    'Do Religious Zionist territorial claims retain ideological autonomy, or have they been fully instrumentalized by the secular state apparatus as a legitimation resource?',
    'Analysis of state resource allocation to settlements, legal frameworks protecting settlement expansion, and instances where Religious Zionist territorial demands were overridden by state security logic; examination of whether theological justifications constrain or merely rationalize state territorial policy',
    'If Religious Zionism retains autonomy: the convergence is a coalition of independent actors (lower extraction on Religious Zionist perspective). If fully instrumentalized: Religious Zionism is a captured ideology serving state territorial logic (higher extraction, Religious Zionist settlers are also victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_zionist_autonomy, empirical, 'Degree of Religious Zionist ideological autonomy vs state instrumentalization').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel ''historical right to Palestine'' or the broader legitimacy structure ''Zionist leadership''s authority to interpret Jewish collective needs''? The former is a territorial claim; the latter is an authority claim that produces territorial outcomes.',
    'Examination of which framing better predicts internal Zionist debates: do disputes center on the validity of the historical claim itself, or on who has authority to interpret what the claim requires? Analysis of how challenges to territorial maximalism are delegitimized — as denial of the historical claim or as illegitimate interpretation by unauthorized voices.',
    'If kernel is the territorial claim: drift is about the claim''s empirical or moral validity. If kernel is the interpretive authority: drift is about who can legitimately speak for Jewish collective interests, and territorial outcomes are downstream of that authority structure. The latter framing makes the convergence mechanism more visible as an authority-maintenance device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether the kernel is the territorial claim itself or the authority structure that interprets collective needs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ideological_diversity_convergence, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idc_theater_1920s, ideological_diversity_convergence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(idc_theater_1930s, ideological_diversity_convergence, theater_ratio, 15, 0.42).
narrative_ontology:measurement(idc_theater_1948, ideological_diversity_convergence, theater_ratio, 28, 0.55).
narrative_ontology:measurement(idc_theater_1967, ideological_diversity_convergence, theater_ratio, 47, 0.58).
narrative_ontology:measurement(idc_theater_1993, ideological_diversity_convergence, theater_ratio, 73, 0.62).
narrative_ontology:measurement(idc_theater_2020, ideological_diversity_convergence, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(idc_extract_1920s, ideological_diversity_convergence, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(idc_extract_1930s, ideological_diversity_convergence, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(idc_extract_1948, ideological_diversity_convergence, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(idc_extract_1967, ideological_diversity_convergence, base_extractiveness, 47, 0.72).
narrative_ontology:measurement(idc_extract_1993, ideological_diversity_convergence, base_extractiveness, 73, 0.7).
narrative_ontology:measurement(idc_extract_2020, ideological_diversity_convergence, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(idc_suppress_1920s, ideological_diversity_convergence, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(idc_suppress_1948, ideological_diversity_convergence, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(idc_suppress_1967, ideological_diversity_convergence, suppression_requirement, 47, 0.78).
narrative_ontology:measurement(idc_suppress_1993, ideological_diversity_convergence, suppression_requirement, 73, 0.75).
narrative_ontology:measurement(idc_suppress_2020, ideological_diversity_convergence, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ideological_diversity_convergence, identity_coordination).
narrative_ontology:affects_constraint(ideological_diversity_convergence, partition_plan_rejection_1947).
narrative_ontology:affects_constraint(ideological_diversity_convergence, settlement_expansion_post_1967).
narrative_ontology:affects_constraint(ideological_diversity_convergence, oslo_process_territorial_outcome).

% DUAL FORMULATION NOTE:
% The ideological diversity convergence is a meta-constraint that operates across multiple specific territorial decisions (1937 Peel partition, 1947 UN partition, post-1967 territorial disposition, Oslo process). Each specific decision could be modeled as its own constraint story with its own extractiveness reflecting the particular historical context, but they share the convergence mechanism as a common structural feature. The convergence constraint is the general pattern; the specific partition debates are instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
