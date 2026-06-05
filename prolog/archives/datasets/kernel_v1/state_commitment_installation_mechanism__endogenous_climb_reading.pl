% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: State Commitment Installation via Endogenous Climb (Fringe-to-Apex Legitimacy Gradient)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the mechanism by which new cultural,
 *   institutional, or political commitments gain legitimacy by climbing from
 *   fringe innovation networks to apex institutional adoption. The
 *   endogenous_climb reading specifies this as a process driven by
 *   demonstrated superiority: fringe actors (scholars, reformers, advocates)
 *   develop and test new commitments on the periphery; their success in
 *   showing superiority over incumbent alternatives attracts institutional
 *   adopters; eventually apex authorities ratify the new commitment as
 *   legitimate, displaced the old framework. This reading presumes that
 *   superior commitments have inherent advantages (operational efficiency,
 *   coherence, empirical fit, moral consistency) that make them attractive to
 *   rational institutional actors. The climb is endogenous: driven by
 *   qualities of the commitment itself and the effectiveness of fringe
 *   advocacy networks, not by external imposition or structural coercion.
 *   However, the constraint exhibits tangled rope structure: genuine
 *   coordination function (facilitating institutional adoption of superior
 *   commitments) coexists with asymmetric extraction (early adopters gain
 *   prestige and power relative to laggards; incumbent authorities lose
 *   legitimacy). Suppression increases during the climb as incumbent actors
 *   resist displacement and institutional adoption becomes costly to refuse.
 *   Theater ratio rises as apex authorities perform ratification ceremonies
 *   and implement formal adoption while potentially delaying genuine
 *   behavioral change. The measurement profile shows a gradual climb:
 *   extractiveness and suppression both rise over the interval as the new
 *   commitment moves from fringe to institutional centrality, while theater
 *   ratio increases as the adoption becomes less about demonstrated
 *   superiority and more about institutional ritual.
 *
 * KEY AGENTS:
 *   - Fringe Innovator Networks: Primary beneficiaries (organized/mobile) — develop the new commitment on the periphery, demonstrate its superiority, gain prestige and influence as the climb proceeds
 *   - Incumbent Authority Holders: Primary victims (powerless/trapped) — lose legitimacy and institutional standing as the new commitment displaces the old framework; cannot exit without abandoning their authority base
 *   - Middle-Tier Institutional Actors: Secondary victims and conditional beneficiaries (moderate/constrained) — face adoption costs but also early-adopter advantages; constrained by institutional pressures and reputation risks
 *   - Institutional Adopters: Strategic actors (institutional/constrained) — at the inflection point where adopting the new commitment stabilizes legitimacy; active enforcement required; generate extraction asymmetry between early and late adopters
 *   - Apex Authority Structure: Gatekeeping actor (powerful/arbitrage) — controls ratification timing and can suppress or accelerate the climb; experiences low suppression because of high optionality
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the climb mechanism as inherent to legitimacy systems rather than as a contingent institutional process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.38).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "State Commitment Installation via Endogenous Climb (Fringe-to-Apex Legitimacy Gradient)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '7cbbcd72-6471-4102-9ec7-7d21c940c9a5').
narrative_ontology:cs_kernel_codification('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', formalized).
narrative_ontology:cs_authority_grounding('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', lineage).
narrative_ontology:cs_interpretation_layer_present('7cbbcd72-6471-4102-9ec7-7d21c940c9a5').
narrative_ontology:cs_reading_relation('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', foundational, superior_commitments_attract_adoption).
narrative_ontology:cs_axiom_status(superior_commitments_attract_adoption, holdable).
narrative_ontology:cs_axiom_grounding('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', superior_commitments_attract_adoption, empirically_contingent).
narrative_ontology:cs_axiom('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', foundational, fringe_innovation_enables_climb).
narrative_ontology:cs_axiom_status(fringe_innovation_enables_climb, holdable).
narrative_ontology:cs_axiom_grounding('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', fringe_innovation_enables_climb, empirically_contingent).
narrative_ontology:cs_reference_frame('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', institutional_selection_from_superior_alternatives).
narrative_ontology:cs_drift_state('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', contemporary_organizational_sociology, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7cbbcd72-6471-4102-9ec7-7d21c940c9a5', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_innovators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_networks).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_authority_coalitions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, displaced_legitimacy_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCUMBENT APEX ACTORS (SNARE) — Face entrenchment loss as the new commitment climbs. Cannot exit without abandoning their entire legitimacy structure. Suppression is high: social position, institutional power, and authority claims all depend on maintaining the old commitment. No alternatives appear viable during the transition period. This is pure extraction from the incumbent perspective: they lose standing as the fringe commitment gains legitimacy.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE-TIER ACTORS (TANGLED ROPE) — Constrained but not trapped. Face costs of adopting the new commitment (reputation damage, retraining, operational disruption) but also stand to gain from early adoption (operational efficiency, alignment with rising authority). Suppression is moderate: they have some flexibility in timing and framing their adoption, and their institutional position allows negotiated transitions. Genuine coordination function exists (facilitating the adoption mechanism itself), alongside asymmetric extraction (early adopters gain relative advantage over laggards).
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FRINGE INNOVATORS (ROPE) — Primary beneficiaries. Organized networks (scholarly circles, reformist movements, underground advocacy groups) develop the new commitment on the periphery. They experience the constraint as coordination: demonstrating superiority of the new commitment requires collective action, shared standards, and mutual reinforcement. Suppression is relatively low for this group because they operate outside incumbent institutional authority and can adopt rules freely. Exit options are mobile: they can shift frames or return to fringe status if the climb falters. Net coordination benefit, low extraction.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL ADOPTERS (TANGLED ROPE) — Institutional actors (religious bodies, state bureaucracies, scholarly academies) at the inflection point. Face genuine coordination pressures: adopting the superior commitment stabilizes institutional legitimacy and operational effectiveness. But also face extraction dynamics: late adopters lose prestige and institutional standing relative to early adopters. Suppression is high for laggards (reputation loss, institutional isolation) but lower for early-moving institutions. Active enforcement required: institutions must visibly adopt and promulgate the new commitment to avoid obsolescence. Genuine coordination function (improving institutional legitimacy/efficacy) plus asymmetric extraction (early vs late adopters).
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: APEX AUTHORITY STRUCTURE (SCAFFOLD) — The state/church/empire apparatus holds the ratification gate. Low effective extraction because apex actors experience high optionality: they can adopt the new commitment selectively, maintain hybrid legitimacy frames, or suppress the climb entirely if it threatens institutional stability. Their arbitrage option allows them to choose the adoption rate and timing. Theater ratio is moderate: ratification requires performative endorsement (proclamations, formal adoption, institutional reorganization) but actual implementation can be delayed. From apex perspective, this is a temporary coordination problem with a sunset: once the climb succeeds (commitment becomes legitimized), the old framework is deprecated and the constraint dissolves. Suppression is low because the apex controls the adoption timeline.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, this constraint may appear as a natural law: legitimacy structures are always subject to displacement by superior alternatives, and the climb mechanism is inherent to any system where performance and authority are partially decoupled. Authority must eventually align with capability or lose coherence. However, the structural data undermines the mountain classification: the climb mechanism requires active institutional enforcement, involves identifiable beneficiaries and victims, and has a finite timeline. This is a false summit — the naturalizing framing ('superior ideas always rise') obscures the contingent institutional machinery that enables or blocks the climb.
constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_commitment_installation_mechanism__endogenous_climb_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint shows mixed coordination and extraction dynamics. Early in the climb (t0-t25), extractiveness is low because fringe innovation is collaborative and benefits flow to all participants equally. Mid-climb (t25-t50), extractiveness rises sharply as institutional adoption begins: early adopters gain relative advantage over laggards, creating asymmetric extraction. Late climb (t50-t100), extractiveness plateaus as the adoption becomes normalized and extraction differentials stabilize. The moderate final value reflects that the constraint genuinely coordinates institutional adoption (function present) while also enabling power consolidation by early adopters (extraction present). Suppression (0.42): Moderate. Low initial suppression (fringe networks face minimal institutional pressure) rises as the climb proceeds. Middle-tier actors face rising costs of non-adoption; incumbent actors face rising costs of resisting the climb. Suppression reaches plateau around t75 as the new commitment becomes institutionally dominant and remaining resistance becomes futile. Theater ratio (0.55): Moderate-high. Initial theater is low (fringe work is largely functional development of the new commitment) but rises as institutional adoption begins. Apex authorities perform ratification ceremonies, formal adoption procedures, and public endorsements that have significant performative content. By t50-t75, substantial gap opens between rhetoric of adoption and behavioral implementation. Some institutional actors claim adoption while maintaining old practices. Theater ratio plateaus around t75 as the performative adoption phase stabilizes into permanent institutional degradation (piton dynamics in later periods).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same structural process (climb from fringe to institutional centrality) appears as coordination (fringe view), mixed extraction (middle-tier view), pure extraction (incumbent view), and potentially as natural law (analytical view). The gap is widest between incumbent authorities (snare) and fringe innovators (rope) because they experience opposite extraction flows: innovation value flows toward fringe actors as their commitment gains legitimacy; incumbent actors lose legitimacy as their framework is displaced. This is not a disagreement about the same phenomenon but genuine divergence in structural position — the climb does extract value from incumbents and transfers it to innovators. The middle-tier actors occupy the transition zone where both mechanisms operate simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural relationship to the constraint. Fringe innovators as beneficiaries with mobile exit options (can return to non-advocacy status if climb falters) have low d (full beneficiary end). Incumbent authorities as victims with trapped exit (cannot exit without losing all authority claim) have high d (full target end). Middle-tier actors face mixed costs and benefits with constrained exit — d ≈ 0.55-0.65 (moderate extraction target). Institutional adopters face high costs of non-adoption (d ≈ 0.60) but some benefits of adoption (early adopter prestige), producing effective d ≈ 0.50-0.55. Apex authorities face minimal suppression and high optionality, producing low d ≈ 0.30-0.40 (partial beneficiary). The sigmoid f(d) function converts these d values to experienced extractiveness chi: high d (incumbent victims) → high f(d) → high χ; low d (fringe beneficiaries) → low f(d) → low or negative χ; moderate d (middle actors) → moderate f(d) → moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by decomposing the kernel state_commitment_installation_mechanism into three separate readings with incommensurable measurement basis. The endogenous_climb reading specifies that extractiveness and suppression measurements reflect the cost/benefit asymmetry between early adopters and laggards, and between fringe innovators and incumbent authorities. A different reading (exogenous_imposition) would use the same structural elements but interpret extractiveness as coercion by apex authorities, not as natural selection of superior commitments. The two readings produce different measurement profiles and different classifications from the same agents' perspectives — the measurement basis is not observable-dependent within a single reading, but rather reflects the reading's own causal model of how the mechanism operates. No single constraint can span both readings; each reading is a complete, ε-invariant constraint with its own measurements and perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superiority_measurement_underdetermination,
    'What constitutes ''demonstrated superiority'' of a new commitment? Who adjudicates superiority claims?',
    'Historical case analysis: track which metrics were used to justify adoption of successful new commitments (productivity, legitimacy, coherence, empirical fit, moral consistency) and compare against metrics used to reject competing commitments. Identify the authority structure that performed the adjudication.',
    'If superiority is objective/measurable: climb is a natural selection process (approaches mountain). If superiority is socially constructed/adjudicated: climb is a political process (remains tangled_rope or snare). If different parties use incommensurable metrics: climb is contested (hybrid_cascade dynamics emerge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superiority_measurement_underdetermination, conceptual, 'What constitutes demonstrated superiority in the climb mechanism').

omega_variable(
    fringe_to_apex_timeline_variance,
    'Why do some new commitments climb rapidly to institutional adoption (decades) while others plateau on the fringe indefinitely?',
    'Comparative institutional analysis: map the network positions and resource availability of fringe networks for successful vs failed climb cases. Track institutional permeability (degree to which apex actors have contact with fringe innovation sites). Identify blocking mechanisms (institutional gatekeepers, competing commitments, resource constraints).',
    'If variance is primarily internal to the new commitment (superior design climbs faster): endogenous_climb reading is robust. If variance is primarily external (institutional openness, crisis windows, political opportunity structures): the climb mechanism is better modeled as hybrid_cascade (exogenous triggers enable endogenous climb). If blocking mechanisms are systematic: snare reading (apex actors suppress climbs that threaten their legitimacy) may be more accurate than scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_to_apex_timeline_variance, empirical, 'Timeline variance in fringe-to-apex institutional climb').

omega_variable(
    commitment_authenticity_vs_performative_adoption,
    'When institutional actors adopt new commitments, how much represents genuine belief/adoption vs performative compliance?',
    'Behavioral analysis: track alignment between stated commitment adoption and actual institutional practice changes. Measure rate of genuine behavioral change vs rhetorical reframing (actors claim adoption but maintain old practices). Track institutional stability after adoption: genuine adopters consolidate changes; performative adopters revert when incentive pressure decreases.',
    'If adoption is primarily authentic: tangled_rope classification holds (genuine coordination function plus extraction asymmetry). If adoption is primarily performative: piton classification (degraded ritual) emerges, and theater_ratio should increase over the climb timeline. If adoption is mixed: multi-constraint decomposition needed (separate stories for authentic vs performative adoption mechanisms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commitment_authenticity_vs_performative_adoption, empirical, 'Authenticity of institutional commitment adoption during climb').

omega_variable(
    reading_underdetermination_climb_vs_imposition,
    'In any specific historical case (Protestant Reformation, scientific methodology adoption, bureaucratic rationalization), how do we distinguish between endogenous climb (fringe superiority demonstrated → apex adoption) vs exogenous imposition (apex actors impose a commitment → retroactively frame it as superior)?',
    'Historical textual analysis: examine contemporaneous justifications offered by institutional actors at adoption time. Track whether superiority claims were primary (adoption justified by framing new commitment as superior to alternatives) or post-hoc (adoption justified by obedience, tradition, crisis response; superiority retroactively asserted). Identify who raised superiority arguments first: fringe networks or apex authorities? Compare adoption timing against external shocks (crises, wars, economic shifts) that might have enabled exogenous imposition.',
    'This omega resolves the kernel underdetermination between endogenous_climb_reading and exogenous_imposition_reading. If superiority claims are primary and originate from fringe: endogenous_climb is the correct reading. If superiority claims are post-hoc and originate from apex authorities seeking legitimacy: exogenous_imposition is correct. If both mechanisms are present (fringe develops new commitment; apex later adopts it for strategic reasons and frames it as superior): hybrid_cascade_reading is needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_underdetermination_climb_vs_imposition, conceptual, 'Kernel underdetermination: climb vs imposition in commitment installation').

omega_variable(
    apex_actor_heterogeneity_in_climb_dynamics,
    'Are apex actors monolithic, or do they disagree internally about adopting the new commitment?',
    'Institutional genealogy: map factions within apex authority structures that favored vs opposed the new commitment. Identify cost-benefit asymmetries: which faction benefited from the old commitment (and thus opposed adoption)? Which faction benefited from the new commitment (and thus advocated adoption)? Track whether adoption was contested or consensual within apex structures.',
    'If apex is monolithic: the scaffold perspective captures apex experience accurately (low suppression, arbitrage options, ratification gate function). If apex is factionally divided: separate perspectives needed for pro-adoption and anti-adoption factions at the institutional level. Anti-adoption factions experience snare dynamics (losing power as the commitment climbs); pro-adoption factions experience rope dynamics (beneficial coordination). The extracted value flow becomes clearer: from losers to winners within apex structures, not just between fringe and apex.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apex_actor_heterogeneity_in_climb_dynamics, empirical, 'Internal heterogeneity of apex actors in commitment climb dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climb_theater_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(climb_theater_t25, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(climb_theater_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(climb_theater_t75, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 75, 0.58).

% Extraction over time
narrative_ontology:measurement(climb_extractiveness_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(climb_extractiveness_t25, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(climb_extractiveness_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(climb_extractiveness_t75, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 75, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(climb_suppression_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(climb_suppression_t25, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(climb_suppression_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(climb_suppression_t75, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 75, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The kernel state_commitment_installation_mechanism decomposes into three distinct readings modeling alternative mechanisms for how new cultural/institutional commitments become legitimized in state systems. The endogenous_climb reading specifies this as driven by demonstrated superiority of fringe innovations. The exogenous_imposition reading specifies apex-driven coercive adoption. The hybrid_cascade reading specifies structural crises that enable endogenous climb. Each reading has different base_extractiveness, suppression, and theater_ratio values because they model different causal mechanisms. The three readings coexist as live positions in historical sociology; they affect one another through academic discourse (advocates of one reading influence adoption rates of the others) and through historical evidence (cases that instantiate one reading become paradigms that anchor arguments about that reading's validity). This file models the endogenous_climb reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
