% ============================================================================
% CONSTRAINT STORY: political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_swap_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: political_swap_reading
 *   human_readable: The Reformation as Political Realignment and Asset Seizure
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   The political_swap_reading interprets the Reformation as primarily an
 *   institutional realignment driven by secular rulers' material interests in
 *   breaking papal authority and seizing church assets. In this reading,
 *   theological disputes over indulgences, scriptural authority, and clerical
 *   corruption provided the vocabulary and justification for a power transfer
 *   that would have occurred on fiscal and jurisdictional grounds regardless
 *   of doctrine. The constraint models this as a Tangled Rope: genuine
 *   coordination benefits existed (dispersal of monopolistic ecclesiastical
 *   authority, local jurisdictional autonomy), but these benefits accrued
 *   asymmetrically to secular rulers while the Catholic Church bore the costs
 *   of institutional subordination and asset loss. The theater ratio
 *   increases over the 131-year interval (0.25 in 1517 when asset seizure was
 *   most active, rising to 0.65 by 1648 Westphalia when religious
 *   justification became largely ceremonial). This reading explicitly rejects
 *   the theological_climb reading (which treats theological claims as
 *   primary) and coexists uneasily with the composite_overdetermination
 *   reading (which allocates causal weight to both theology and politics).
 *   The political_swap_reading asserts materialism as foundational:
 *   theological innovation follows institutional pressure, not precedes it.
 *   Periodization extends to 1648 Westphalia because the political settlement
 *   — cuius regio eius religio codifying confessional fragmentation — marks
 *   when the constraint's original extraction mechanism (unchallenged papal
 *   authority → asset transfer → princely power consolidation) becomes
 *   structurally impossible and persists only through institutional theater.
 *
 * KEY AGENTS:
 *   - Secular Rulers (Princes, Kings): Primary beneficiary (institutional/arbitrage) — capture ecclesiastical assets, consolidate territorial jurisdiction, eliminate papal jurisdictional competition. Material gain flows directly to state treasuries and institutional autonomy.
 *   - Catholic Church Institutional Authority: Primary victim (institutional/trapped) — loses territorial holdings, asset bases, jurisdictional claims, and spiritual monopoly. Cannot exit the competitive confessional field created by schism. Structured subordination to secular powers.
 *   - Reform-Minded Clergy: Secondary actor (moderate/constrained) — articulate genuine theological grievances that become instrumentalized for political realignment. Gain institutional autonomy from Rome but subordinated to secular rulers. Constrained by inability to separate theological voice from institutional apparatus.
 *   - Merchant Urban Class: Tertiary beneficiary (organized/constrained) — benefit from regulatory fragmentation and reduced monopolistic extraction, but constrained by confessional settlement requirements and religious civil wars. Mixed extraction/coordination experience.
 *   - Papal Authority Structure: Institutional victim (institutional/trapped) — loses universal claim to spiritual governance, becomes one confessional option among competitors rather than uncontested authority. Theater persists through papal institutional continuity but real power is transferred.
 *   - Westphalian Settlement System: Institutional degradation (institutional/arbitrage) — by 1648, the religious legitimacy for institutional arrangements becomes performative. Cuius regio eius religio enforcement persists through inertia despite widespread hidden heterodoxy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_swap_reading, 0.58).
domain_priors:suppression_score(political_swap_reading, 0.65).
domain_priors:theater_ratio(political_swap_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_swap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_swap_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(political_swap_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_swap_reading, tangled_rope).
narrative_ontology:human_readable(political_swap_reading, "The Reformation as Political Realignment and Asset Seizure").
narrative_ontology:topic_domain(political_swap_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(political_swap_reading, formalized).
narrative_ontology:cs_authority_grounding(political_swap_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(political_swap_reading).
narrative_ontology:cs_kernel_id(political_swap_reading, reformation_event_boundary).
narrative_ontology:cs_reading_relation(political_swap_reading, theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation(political_swap_reading, composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom(political_swap_reading, foundational, political_interests_primary_causal_driver).
narrative_ontology:cs_axiom_status(political_interests_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding(political_swap_reading, political_interests_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_axiom(political_swap_reading, secondary, theology_ratifies_material_interests).
narrative_ontology:cs_axiom_status(theology_ratifies_material_interests, holdable).
narrative_ontology:cs_axiom_grounding(political_swap_reading, theology_ratifies_material_interests, instrumental).
narrative_ontology:cs_reference_frame(political_swap_reading, unified_papal_authority_medieval_order).
narrative_ontology:cs_drift_state(political_swap_reading, westphalian_fragmentation_settlement, gap(authority_erosion, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(political_swap_reading, territorial_princes).
narrative_ontology:constraint_victim(political_swap_reading, papal_authority).
narrative_ontology:constraint_victim(political_swap_reading, church_institutional_continuity).
narrative_ontology:constraint_victim(political_swap_reading, theological_claims_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECCLESIASTICAL INSTITUTION (SNARE) — The Catholic Church loses institutional control, asset bases, and claims to universal spiritual authority. Trapped within the structural collapse of its political-economic framework. No exit option from the confessional competition created by schism. Maximum extraction: asset seizure, territorial loss, institutional subordination to secular rulers.
constraint_indexing:constraint_classification(political_swap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECULAR RULERS (ROPE) — Princes experience the constraint as coordination problem solved: break papal authority, seize assets, consolidate territorial power. The theological disputes are instrumental — doctrinal realignment serves institutional reorientation. Benefits from asset transfer and jurisdictional consolidation. Low suppression from beneficiary's view; this is voluntary coordination in service of their own interests.
constraint_indexing:constraint_classification(political_swap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORM-MINDED CLERGY (TANGLED ROPE) — Constrained actors who articulated theological grievances (indulgences, corruption, scriptural authority) that became instrumentalized for political realignment. They gain institutional autonomy from Rome but lose independence to secular rulers. Mixed: coordination benefit (theological voice) + extraction (subordination to princes). Cannot exit without abandoning both religious order and livelihood.
constraint_indexing:constraint_classification(political_swap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MERCHANT CLASS & URBAN CENTERS (TANGLED ROPE) — Benefits from jurisdictional fragmentation (regulatory arbitrage, reduced papal tax), but constrained by confessional settlement terms and religious civil wars. Genuine coordination function (dispersed authority reduces monopolistic extraction) alongside asymmetric enforcement (religious conformity mandates). Theater ratio rises during 1550-1648 as cuius regio eius religio becomes performative (hidden heterodoxy persists).
constraint_indexing:constraint_classification(political_swap_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WESTPHALIAN SETTLEMENT (PITON) — By 1648, the religious legitimacy for the entire political-asset transfer becomes ceremonial. The treaty codifies confessional pluralism not from theological conviction but from military stalemate. The institutional order persists through inertia: rulers continue cuius regio eius religio enforcement despite widespread religious heterodoxy. Theater ratio high (0.65+) — performative confessional identity masks actual pragmatic coexistence. The constraint's original extraction mechanism (asset seizure, authority transfer) has atrophied; the settlement persists through institutional theater.
constraint_indexing:constraint_classification(political_swap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATERIALIST VIEW (MOUNTAIN) — From a civilizational-universal perspective, institutional realignment follows necessarily from fiscal and jurisdictional pressures on feudal-ecclesiastical order. The Reformation 'had to happen' — theology is window-dressing on material necessity. Theological disputes are epiphenomena of structural economic change. However, this perspective risks naturalizing what is actually a contested historical reading. The engine will flag this as a false-summit candidate: the 'inevitability' frame naturalizes one interpretation (political swap) as a natural law.
constraint_indexing:constraint_classification(political_swap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_swap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_swap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_swap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_swap_reading, TR),
    TR >= 0.70.

:- end_tests(political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, declining to 0.42 by 1648): Moderate-high, trending downward. The constraint exhibits strongest extraction during the asset-seizure phase (1517-1534) when princely material gain is maximum (extractiveness 0.72). By 1548 (Peace of Augsburg formalizing confessional fragmentation) extractiveness declines to 0.58 as political settlement stabilizes and the initial transfer is complete. By 1648 (Westphalia) extractiveness falls to 0.42 because the constraint's primary extraction mechanism (unchallenged papal authority enabling asset seizure) is no longer operative — the church is already institutionally subordinated and asset bases already transferred. What remains is maintenance of the settlement through theater (rising theater ratio 0.65). Suppression (0.65): High. The constraint operates through suppression of alternatives: theological orthodoxy mandates eliminate public religious heterodoxy; religious civil wars foreclose peaceful coexistence without settlement; cuius regio eius religio enforcement suppresses individual confession choice. However, suppression is not total — the theater ratio's rise indicates hidden heterodoxy persists (private Catholics in Protestant territories, private Protestants in Catholic territories, Jewish communities navigating confessional boundaries). Theater ratio (0.25 → 0.65): Indicates increasing performativity. Initial period (1517) exhibits low theater because the constraint is fundamentally about asset transfer and institutional subordination — the extraction mechanism is direct and material. By Westphalia (1648) theater rises to 0.65 because religious conformity becomes ceremonial while material incentives for actual doctrinal uniformity disappear. The constraint persists through institutional inertia and narrative legitimacy rather than through the original extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces the fullest perspectival diversity across the six constraint types. The papal authority and reform clergy see Snare (trapped institutional collapse). Secular rulers see Rope (coordination on shared jurisdictional interests). The merchant class and urban centers see Tangled Rope (mixed coordination benefits and enforcement asymmetry). The Westphalian settlement observers see Piton (performative persistence through theater). The materialist analytical observer risks seeing Mountain (inevitable institutional realignment following from fiscal necessity). This gap reveals the reading's core claim: the same structural phenomenon — religious fragmentation and authority transfer — is experienced as either institutional catastrophe (for Rome), power consolidation (for princes), mixed coordination (for urban merchants), or natural inevitability (for analysts who assume materialism). The reading's claim to political-primary causation is strongest from the beneficiary's perspective (secular rulers) and weakest from the analytical perspective (which risks naturalizing politics as necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options for each perspective. Secular rulers as beneficiaries with arbitrage-level exit options derive low d (roughly 0.15), producing negative effective extraction f(d) ≈ -0.01 — they experience the constraint as beneficial coordination. Papal authority as victim with trapped exit derives high d (roughly 0.95), producing maximum effective extraction f(d) ≈ 1.42 — they experience maximum extraction. Reform clergy as moderate-power constrained actors with mixed beneficiary/victim status derive medium d (roughly 0.50-0.60), producing moderate effective extraction f(d) ≈ 0.65-0.75, consistent with their Tangled Rope perception. The analytical observer's canonical d of 0.73 produces f(d) ≈ 1.15, but this perspective risks being identity-locked into materialist framing (theological contingency becomes invisible from within the materialist frame). The directionality chain here operates without overrides because the structural relationships are clear: defined beneficiaries (secular rulers), defined victims (papal authority and institutional church), and differentiated exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The political_swap_reading resolves mandatrophy by asserting that the Reformation IS a constrained Tangled Rope — there is genuine coordination (dispersal of monopolistic ecclesiastical authority benefits multiple parties) alongside asymmetric extraction (disproportionate benefit to secular rulers, costs borne by the institutional church). This is not coordination misidentified as extraction, nor extraction disguised as coordination. The constraint has both functions, and the beneficiary/victim asymmetry proves the tangled status. The analytical observer's Mountain perspective (institutional realignment as inevitable material necessity) is flagged as a false summit: it naturalizes the political-swap reading's particular causal claim as a law of history when in fact the Reformation's political outcome was contingent on theological disputes, military outcomes, and institutional choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_instrumentalization_versus_motivation,
    'Were theological disputes genuine primary motivators for the Reformation, or post-hoc rationalizations for power-consolidation goals that would have proceeded on any pretext?',
    'Chronological analysis of causal ordering: did theological grievances precede or follow political rupture? Counterfactual reasoning: would rulers have seized assets without theological justification? Comparative analysis: do asset seizures and authority transfers require theological cover, or would princes consolidate power on fiscal grounds alone?',
    'If theology was primary motivation: constraint reclassifies to Rope from multiple perspectives (coordination on shared values). If purely instrumental: constraint remains Snare/Tangled Rope (asset extraction rationalized by theology). If mixed causal structure: constraint remains Tangled Rope with omega uncertainty about proportional weighting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_instrumentalization_versus_motivation, empirical, 'Whether theology was primary motivation or post-hoc rationalization for political realignment').

omega_variable(
    asset_seizure_quantification,
    'What percentage of Reformation political support derived from material incentives (asset seizure, tax collection, territorial consolidation) versus ideological alignment (theological reform, clerical independence)?',
    'Fiscal analysis of princely revenue streams before and after Reformation; correlation between asset seizure magnitude and reformation adoption speed; analysis of principalities that resisted Reformation despite fiscal incentives.',
    'If > 60% material incentive: supports political-swap reading, extractiveness remains high (0.58+). If < 30% material incentive: theology becomes primary, tangled-rope classification shifts toward rope. If 30-60% mixed: current tangled-rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asset_seizure_quantification, empirical, 'Proportion of political support driven by material vs. ideological incentives').

omega_variable(
    theological_claim_legitimacy_status,
    'From within this reading''s framework, are the theological claims animating the Reformation (sola scriptura, priesthood of believers, salvation by faith) treated as empirically true, pragmatically useful, or purely instrumental covers for institutional realignment?',
    'Textual analysis of reformer justifications; whether theological claims are defended on their own merits or defended primarily for institutional stability; post-Reformation examination of whether theology becomes less contested when institutional settlement stabilizes.',
    'If theological claims are treated as true: this reading''s axioms shift to include theology as a legitimate knowledge domain, reducing the reading''s foreclosure of theological-climb reading. If purely instrumental: reinforces this reading''s axiomatic materialism. Classification remains tangled_rope regardless, but the meaning of ''tangled'' shifts from ''mixed coordination and extraction'' to ''extraction wrapped in sincere but secondary theology.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_claim_legitimacy_status, conceptual, 'Whether theology is genuine or instrumental within this reading''s framework').

omega_variable(
    alternative_institutional_arrangements,
    'Were secular-authority institutional arrangements achievable without Reformation rupture, through reform-within-Catholicism or negotiated concordats?',
    'Historical analysis of late-medieval church-state negotiations; examination of why earlier reform movements (Waldensians, Hussites, Conciliarists) did not achieve comparable institutional realignment; comparison with post-Reformation Catholic consolidation (Council of Trent) showing how church adapted without total asset loss.',
    'If alternative arrangements were viable: the Reformation appears contingent, not necessary, supporting the political-swap reading''s contention that institutional realignment was achieved through schism rather than reform. If alternatives were foreclosed: reduces the reading''s characterization of theology as ''post-hoc rationalization'' — theology becomes the vehicle through which deeper institutional incompatibilities surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institutional_arrangements, empirical, 'Whether secular-authority institutional arrangements were achievable without Reformation rupture').

omega_variable(
    long_duration_committer_choice,
    'Is the Reformation boundary set at 1517 (Luther''s 95 Theses), 1534 (Act of Supremacy), 1648 (Westphalia), or some other date? The choice of boundary reflects a reading''s commitment: theological origin, institutional consolidation, or political settlement?',
    'This is a conceptual omega — no empirical data resolves it. Different readings implicitly choose different boundaries. Political-swap reading should explicitly declare whether the constraint spans 1517-1534 (asset seizure period), 1517-1648 (full political-institutional settlement), or focuses on a narrower window where material incentives are most salient.',
    'Boundary choice affects which actors occupy which structural positions. If 1517-1534: princes are primary beneficiaries, church is victim, extraction is clearest. If 1517-1648: extraction attenuates as political settlement stabilizes, theater ratio rises, classification drifts toward Piton. Current analysis assumes 1517-1648 window.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_duration_committer_choice, conceptual, 'Periodization choice: Reformation boundary (1517, 1534, 1648) reflects reading''s commitment').

omega_variable(
    reading_kernel_distinction,
    'This story instantiates the political_swap_reading of the reformation_event_boundary kernel. What is the relationship between this reading and the theological_climb_reading and composite_overdetermination_reading?',
    'The committer frame (Rules 1-5) routes this to cs_structure.reading_relations and cs_structure.axioms. Reading_relations specify logical connections (forecloses, coexists_with, influences); axioms declare foundational normative claims unique to this reading.',
    'This omega is documentary: it marks that the political_swap_reading is ONE reading of a contested kernel, not the only possible historical interpretation. The engine uses reading_relations and axioms to compute how alternative readings would constrain or enable this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Kernel reading identification and sibling relationship mapping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_swap_reading, 0, 131).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(political_swap_theater_1517, political_swap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(political_swap_theater_1534, political_swap_reading, theater_ratio, 17, 0.35).
narrative_ontology:measurement(political_swap_theater_1648, political_swap_reading, theater_ratio, 131, 0.65).

% Extraction over time
narrative_ontology:measurement(political_swap_extractiveness_1517, political_swap_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(political_swap_extractiveness_1534, political_swap_reading, base_extractiveness, 17, 0.58).
narrative_ontology:measurement(political_swap_extractiveness_1648, political_swap_reading, base_extractiveness, 131, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(political_swap_reading, 0.18).
narrative_ontology:affects_constraint(political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(political_swap_reading, composite_overdetermination_reading).
narrative_ontology:affects_constraint(political_swap_reading, westphalian_treaty_system).
narrative_ontology:affects_constraint(political_swap_reading, ecclesiastical_subordination_mechanism).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures: political_swap_reading (ε=0.58, secular rulers as primary beneficiaries), theological_climb_reading (ε=0.35, reform clergy as primary beneficiaries), composite_overdetermination_reading (ε=0.48, causal weight distributed). Each story is a coherent reading of the same historical event boundary. The three are linked through network.affects_constraints to enable comparative analysis of how reading choice affects classification. Political_swap_reading influences westphalian_treaty_system (the 1648 settlement legitimacy depends on prior political realignment) and ecclesiastical_subordination_mechanism (the mechanism by which papal authority was structurally subordinated to secular power).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
