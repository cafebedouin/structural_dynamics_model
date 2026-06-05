% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Revolutionary Seizure of State Power via Organized Party (Vanguard Rupture Reading)
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   The vanguard rupture reading claims that revolutionary transformation
 *   from capitalism to communism requires the organized party seizure of
 *   state power and the establishment of a dictatorship of the proletariat as
 *   a transitional state form. The party apparatus provides centralized
 *   direction of labor, suppresses counter-revolutionary forces, and
 *   allegedly guides the working class toward communism. This constraint
 *   operates through the total subordination of autonomous worker
 *   organizations to party control, the liquidation of political pluralists
 *   and their organizational capacity, and the expropriation of the
 *   bourgeoisie. The reading asserts that without this organized rupture and
 *   party guidance, capitalist interests would reassert themselves and abort
 *   the revolution. This is ONE READING of the contested kernel of
 *   revolutionary method — the other readings (democratic gradualism, council
 *   communism) dispute both the necessity of the vanguard and the structure
 *   of the transition. The vanguard reading instantiates high extractiveness
 *   (0.62) because the suppression of alternatives is structural and the
 *   beneficiaries (party cadre, state apparatus) consolidate power
 *   indefinitely rather than transcending it. The constraint exhibits Tangled
 *   Rope properties: it claims a genuine coordination function (organizing
 *   production, eliminating unemployment, directing labor to collective ends)
 *   while suppressing the organizational autonomy that could verify this
 *   claim.
 *
 * KEY AGENTS:
 *   - Revolutionary Vanguard Party: Primary beneficiary (institutional/arbitrage) — monopolizes political power, controls state apparatus, directs labor allocation
 *   - Party Cadre: Secondary beneficiary (organized/arbitrage) — advances through party ranks, gains administrative positions, participates in planning decisions
 *   - State Planning Apparatus: Secondary beneficiary (institutional/arbitrage) — receives monopoly on production coordination, eliminates market competition, concentrates resource allocation authority
 *   - Industrial Working Class: Constituency (organized/constrained) — claimed beneficiary but with suppressed voice; benefits from coordination (employment, services) while bearing extraction (wage control, work direction)
 *   - Political Pluralists: Primary victim (powerless/trapped) — liquidated or suppressed; lose all political voice and organizational capacity
 *   - Autonomous Worker Organizations: Primary victim (organized/trapped) — subordinated to party control; lose independence and decision-making authority
 *   - Pre-Revolutionary Bourgeoisie: Secondary victim (powerful/trapped) — expropriated; lose property and productive control; some integrated as technical experts under state direction
 *   - International Socialist Movement: Secondary beneficiary/victim (institutional/constrained) — benefits from proof-of-concept of socialist state, but forced into subordination to Soviet foreign policy interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.62).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Revolutionary Seizure of State Power via Organized Party (Vanguard Rupture Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'e1fcd50d-1473-45b9-9e18-0f7fda1953a2').
narrative_ontology:cs_kernel_codification('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', fixed_text).
narrative_ontology:cs_authority_grounding('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', extraction).
narrative_ontology:cs_interpretation_layer_present('e1fcd50d-1473-45b9-9e18-0f7fda1953a2').
narrative_ontology:cs_reading_relation('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', foundational, bourgeois_state_cannot_be_captured).
narrative_ontology:cs_axiom_status(bourgeois_state_cannot_be_captured, holdable).
narrative_ontology:cs_axiom_grounding('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', bourgeois_state_cannot_be_captured, empirically_contingent).
narrative_ontology:cs_axiom('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', foundational, vanguard_party_necessary_coordination_agent).
narrative_ontology:cs_axiom_status(vanguard_party_necessary_coordination_agent, holdable).
narrative_ontology:cs_axiom_grounding('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', vanguard_party_necessary_coordination_agent, empirically_contingent).
narrative_ontology:cs_axiom('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', secondary, dictatorship_proletariat_transitional).
narrative_ontology:cs_axiom_status(dictatorship_proletariat_transitional, overridden).
narrative_ontology:cs_axiom_grounding('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', dictatorship_proletariat_transitional, deontological).
narrative_ontology:cs_reference_frame('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', working_class_liberation_via_party_rupture).
narrative_ontology:cs_drift_state('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', post_soviet_collapse_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e1fcd50d-1473-45b9-9e18-0f7fda1953a2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadre).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_vanguard).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, market_coordinating_agents).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, pre_revolutionary_bourgeoisie).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED POLITICAL PLURALIST (SNARE) — Cannot exit the revolutionary dictatorship; suppression is total and structural. Opposition parties are liquidated, press is censored, exit (emigration) is prohibited. The pluralist experiences maximum extraction — loss of political voice, property confiscation, danger of persecution. No alternative pathway; constraint operates as pure coercion without coordination benefit.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AUTONOMOUS WORKER ORGANIZATION (SNARE) — Independent unions, factory councils, and horizontal worker coordination are subordinated to party control. Exit is prohibited; suppression is enforced through strikes bans, leadership arrest, and absorption into state-controlled unions. The autonomous organization loses its independence and decision-making authority. Experiences extraction as loss of autonomy; coordination function is replaced by party hierarchy.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL WORKING CLASS (TANGLED ROPE) — The constituency the vanguard claims to represent experiences a genuine coordination function: the party mobilizes labor, organizes production, eliminates unemployment, and provides education/healthcare. But the extraction is asymmetric: the party controls distribution, dictates work conditions, and suppresses wage demands. Workers benefit from coordination (resource allocation, job security, social services) but bear the cost of totalizing control. Exit is constrained by the abolition of labor markets and dependent on state employment.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REVOLUTIONARY PARTY APPARATUS (ROPE) — The vanguard party experiences the constraint as coordination: mobilizing the working class, organizing production, suppressing counter-revolutionary opposition, and building the planned economy. The apparatus benefits from the constraint's structure (power, resources, cadre advancement). Exit is not an option the party considers; it is beneficiary and sole authority. However, from the party's perspective, extraction is justified as necessary to prevent capitalist restoration and to allocate labor to productive ends. The constraint functions as coordination from this position.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEPOSED BOURGEOISIE (TANGLED ROPE) — The expropriated capitalist class experiences severe extraction: property confiscation, loss of control over production, prohibition on capital accumulation. Some may be recruited as technical experts for state industry (constrained exit as class); others face execution or exile. The coordination function they experience is coerced participation in state production (if recruited) or total exclusion (if not). This perspective sees the constraint as pure extraction masquerading as necessary suppression of a counter-revolutionary threat.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL SOCIALIST MOVEMENT (TANGLED ROPE) — The broader socialist and communist internationals experience the vanguard rupture as both coordination (building socialism in one country as proof of feasibility) and extraction (party subordination of international worker movements to Soviet foreign policy interests). Exit is constrained by ideological commitment and material dependence on Soviet support. The coordination function is real (knowledge transfer, military aid, industrial planning templates) but asymmetrically distributed toward the vanguard center.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective grounded in historical materialism, the vanguard seizure and dictatorship of the proletariat appear as necessary structural phases in the transition from capitalism to communism. The constraint (revolutionary rupture via organized party) is framed as an inevitable consequence of capitalist contradictions and the bourgeoisie's intransigence. Suppression and extraction appear as immutable features of historical necessity — the only way to break the bourgeois state apparatus is through concentrated force. This reading naturalizes the vanguard rupture as a law of revolutionary development. However, this perspective is vulnerable to false-summit detection: identifiable beneficiaries (party cadre, state apparatus) benefit from the framing of the constraint as inevitable.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__vanguard_rupture_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate, reflecting the tension between claimed coordination function and structural suppression of alternatives. The measurement trajectory (0.42 → 0.68 at peak → 0.62) shows extraction rising as the party consolidates power (violent suppression phase, liquidation of alternatives) then stabilizing as the new equilibrium is established. The 0.62 reflects that the constraint does not reach maximal extraction (0.85+) because some genuine coordination occurs — labor is organized, production is directed, unemployment is eliminated — but it sustains high extraction because the party monopolizes all benefits of coordination and suppresses challenges. Suppression (0.68): High, reflecting the liquidation or forced subordination of all organizational forms except the party. Measurement trajectory (0.55 → 0.75 → 0.68) shows suppression requirement peaking during the violent revolutionary period and the subsequent consolidation, then plateauing as the apparatus becomes normalized. The plateau at 0.68 rather than declining indicates that suppression does not meaningfully decrease because the dictatorship does not transcend itself — the vanguard stabilizes at a high-suppression equilibrium. Theater ratio (0.55): Moderate, reflecting that while the party claims transparency and scientific planning, actual decision-making is opaque, succession is by purge rather than democratic process, and the dictatorship's 'temporary' character becomes increasingly performative as permanent party rule is established. Theater rises from 0.35 to 0.55 as the revolutionary euphoria of the seizure period gives way to bureaucratic normalization.
 *
 * PERSPECTIVAL GAP:
 *   The vanguard rupture reading produces maximal perspectival divergence. The party apparatus sees coordination and necessity (Rope). The working class sees mixed coordination with trapped exit (Tangled Rope). The political pluralists see pure extraction with no alternative (Snare). The autonomous worker organizations see suppression of their autonomy under the guise of coordination (Snare). The analytical observer risks seeing a Mountain — the inevitable structural consequence of capitalism's contradictions — but this is a false summit revealed by the presence of identifiable beneficiaries and the measurement pattern showing stabilization at high extraction rather than decline toward the transition's promised end state. The gap between the reading's claim (temporary dictatorship guiding transition to communism) and the measurement reality (extraction stabilizing at 0.62 with suppression plateauing at 0.68) is diagnostic of whether the dictatorship is truly transitional (Scaffold with sunset) or permanent (Piton with theater of transitionality).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position within the constraint. The powerless political pluralists are full targets (d ≈ 0.95) — they bear maximum suppression with no alternative and no benefit. The autonomous worker organizations are also high-target (d ≈ 0.88) — their independence is the directly extracted good, and they are trapped by state control of employment. The working class as constituency experiences asymmetric positioning: they are nominally the beneficiary (the dictatorship claims to serve them) but structurally trapped in state employment with suppressed voice, yielding d ≈ 0.65 (mixed target-beneficiary). The party apparatus is full beneficiary (d ≈ 0.10) — power and resources flow toward them with no extraction cost. The deposed bourgeoisie experience total extraction if liquidated (d ≈ 0.98) or constrained extraction if recruited as technical experts (d ≈ 0.80). The international movement experiences constrained extraction through ideological dependence (d ≈ 0.55). The analytical observer computing historical inevitability experiences the constraint as immutable law (d ≈ 0.72) but the false-summit detector will identify the beneficiaries and question the naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by making explicit what the reading presupposes: that organized party seizure is both necessary (for coordination against capital) and temporary (declining to irrelevance as communism is achieved). The omega variables directly address the mandatrophy: (1) Is vanguard necessity empirical or naturalization? If empirical, suppression_requirement should decline as the transition succeeds; if naturalization, suppression stabilizes indefinitely. (2) Is the dictatorship truly transitional? If yes, measurements should show suppression declining toward zero as the proletariat is educated and capital is eliminated; our data shows plateau at 0.68, questioning the reading's own claims. (3) Does the party actually coordinate on behalf of the working class, or does 'working-class interest' become whatever the party decides? The reading claims the former; measurement and historical evidence suggests the latter. The mandatrophy is not 'which type is correct' but 'does the vanguard reading's own internal logic — that the dictatorship is transitional and serves worker liberation — match the empirical trajectory of extraction and suppression?' If suppression plateaus instead of declining, the reading has failed on its own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_necessity_contingency,
    'Is organized party seizure of state power a structural necessity for worker liberation, or a contingent political choice that naturalizes party dictatorship?',
    'Historical comparison of revolutionary outcomes: worker movements that succeeded through horizontal/council structures vs. those requiring vanguard centralization; counterfactual analysis of alternative pathways available at revolutionary moments',
    'If necessary: the constraint is an immutable Mountain — suppression and extraction are structural costs of revolutionary transformation. If contingent: the constraint is a Snare — extraction masquerading as necessity. This is the core reading-differentiator between vanguard_rupture_reading and council_communist_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vanguard_necessity_contingency, empirical, 'Whether vanguard party organization is structurally necessary or politically contingent').

omega_variable(
    dictatorship_proletariat_transitionality,
    'Is the dictatorship of the proletariat a genuinely temporary state form with mechanisms for its own transcendence, or does it establish a permanent party monopoly that reproduces itself indefinitely?',
    'Empirical analysis of the dictatorship in practice: timeline to loosening of party control, mechanisms for depoliticization and transcendence of the state form, actual movement toward statelessness vs. bureaucratic entrenchment',
    'If transitional (functioning as described): the constraint is Scaffold — temporary coordination structure with sunset clause toward communism. If permanent: the constraint is Piton — the transition language becomes theater concealing permanent party rule. This determines whether suppression_requirement should decline over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dictatorship_proletariat_transitionality, empirical, 'Whether the dictatorship of the proletariat is truly transitional or permanently self-reproducing').

omega_variable(
    working_class_coordination_authenticity,
    'Does the party''s claimed coordination function on behalf of the working class represent genuine worker interest, or is it extraction disguised by revolutionary rhetoric and subordination to party control?',
    'Evidence of worker agency: worker input into production planning decisions, ability to challenge party direction, genuine (vs. ritualistic) participation in governance; analysis of whether workers'' material condition improves relative to alternative pathways available',
    'If authentic coordination: Tangled Rope classification stands — real benefits alongside extraction. If rhetorical cover: the constraint reclassifies to pure Snare — extraction masked by revolutionary legitimacy claims. This omega is distinct from omega_vanguard_necessity because it addresses not whether the vanguard is necessary but whether it actually serves the working class once in power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(working_class_coordination_authenticity, empirical, 'Whether party coordination of labor serves working-class interests or masks extraction').

omega_variable(
    reading_framework_incompatibility,
    'Can the vanguard_rupture_reading and council_communist_reading coexist within a single Marxist framework, or does acceptance of one logically foreclose the other?',
    'Analysis of party history: parties attempting to integrate council-communist critiques; Marxist texts claiming compatibility between vanguard and council approaches; empirical outcomes when vanguard and council structures compete in the same revolutionary moment',
    'If forecloses: the two readings are logically incompatible within historical materialism; only one can be correct. If coexists_with: both readings remain live options for different socialist factions. If influences: vanguard success creates structural pressure against council alternatives without logically ruling them out. This omega routes the committer relation classification into the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framework_incompatibility, conceptual, 'Logical compatibility between vanguard and council-communist readings within Marxist theory').

omega_variable(
    beneficiary_identity_stability,
    'Are the beneficiaries (party cadre, state-planning apparatus) stable or do they fragment and compete in ways that destabilize the constraint''s coordination function?',
    'Historical analysis: intra-party conflicts, succession struggles, fragmentation of the planning apparatus, purges and internal competitions among cadre; correlation between internal beneficiary conflict and constraint effectiveness',
    'If stable: the Tangled Rope classification holds with suppression enforced as stable coordination. If unstable: the constraint degrades into Piton as theater becomes necessary to maintain the coordination illusion when beneficiaries fragment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_stability, empirical, 'Whether beneficiary coalition remains stable or fragments into competing factions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_vanguard_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mani_vanguard_tr_t6, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(mani_vanguard_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mani_vanguard_be_t3, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(mani_vanguard_be_t6, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(mani_vanguard_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mani_vanguard_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_vanguard_su_t3, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(mani_vanguard_su_t6, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(mani_vanguard_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, proletarian_state_apparatus_institutional_inertia).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadre_class_formation).

% DUAL FORMULATION NOTE:
% The vanguard rupture reading is one constraint within the manifesto_revolutionary_method kernel. The democratic gradualism and council communist readings are separate constraints with different ε values and beneficiary/victim structures. Network relationships trace the logical and structural dependencies: the vanguard reading claims to prevent bourgeois restoration (affects other readings by blocking their pathways); the gradualist reading disputes the necessity of rupture (influences the vanguard by suggesting alternatives); the council reading disputes the party monopoly (forecloses the vanguard's claim to necessity within a horizontal-coordination framework). Each reading should be a separate JSON story, linked via this network array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
