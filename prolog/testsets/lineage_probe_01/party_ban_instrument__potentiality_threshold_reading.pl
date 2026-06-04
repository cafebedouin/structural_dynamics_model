% ============================================================================
% CONSTRAINT STORY: party_ban_instrument__potentiality_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_ban_potentiality_threshold, []).

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
 *   constraint_id: party_ban_instrument__potentiality_threshold_reading
 *   human_readable: Party Ban Instrument — Potentiality Threshold Reading (NPD Doctrine)
 *   domain: constitutional_law/political_party_regulation
 *
 * SUMMARY:
 *   The party-ban instrument in German constitutional law represents a
 *   refined balance between protecting constitutional democracy and
 *   preserving political pluralism. The NPD judgment (2003) established a
 *   potentiality threshold: a party's hostility to the constitutional order
 *   is insufficient grounds for dissolution; the state must also demonstrate
 *   that the party possesses the capability or realistic potential to achieve
 *   those unconstitutional aims. This reading refines but does not eliminate
 *   the ban power — it conditions suppression on both intent
 *   (unconstitutional aims) and capacity (potentiality). The constraint
 *   operates primarily as a tangled_rope: it has genuine coordination content
 *   (legitimating party regulation under rule-of-law procedure) while
 *   simultaneously concentrating power in the judiciary to define what counts
 *   as potentiality and dangerous capability. The suppression mechanism
 *   operates not through direct prohibition but through the conditional
 *   threat of dissolution — movements must continually negotiate the boundary
 *   between protected political activity and actionable capability-building.
 *   This reading instantiates a specific doctrinal choice among competing
 *   interpretations of the constitutional party-ban instrument, reflected in
 *   the kernel contest between the founding_precedents_reading (the SRP and
 *   KPD bans establish the instrument's foundational shape), the
 *   chilling_critique_reading (the ban power's suppressive effect operates
 *   through the standing threat of dissolution regardless of actual
 *   enforcement), and this reading (the potentiality threshold refines
 *   suppression to be conditioned on demonstrated capability).
 *
 * KEY AGENTS:
 *   - Marginalized Extremist Movements: Primary beneficiary paradox (powerless/trapped) — constrained by dissolution threat but protected by potentiality requirement; cannot exit politics but can strategically remain below capability threshold
 *   - Constitutional Court (Enforcer): Primary institutional actor (institutional/constrained) — benefits from legitimated authority to regulate parties while bearing evidentiary burden of demonstrating capability
 *   - Mainstream Political Parties: Secondary actors (moderate/constrained) — benefit from suppression of genuine threats but face chilling effects and monitoring through the doctrine
 *   - Ban-on-Ideology Doctrinal Tradition: Victim set (institutional/arbitrage) — the doctrine that justified broad party dissolution on ideological grounds alone; refined and narrowed by potentiality threshold
 *   - Constitutional Establishment: Implicit beneficiary (institutional/arbitrage) — state authority to regulate parties legitimated through rule-of-law procedure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the potentiality threshold as constitutional necessity rather than judicial innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_ban_instrument__potentiality_threshold_reading, 0.38).
domain_priors:suppression_score(party_ban_instrument__potentiality_threshold_reading, 0.52).
domain_priors:theater_ratio(party_ban_instrument__potentiality_threshold_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_ban_instrument__potentiality_threshold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(party_ban_instrument__potentiality_threshold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(party_ban_instrument__potentiality_threshold_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_ban_instrument__potentiality_threshold_reading, tangled_rope).
narrative_ontology:human_readable(party_ban_instrument__potentiality_threshold_reading, "Party Ban Instrument — Potentiality Threshold Reading (NPD Doctrine)").
narrative_ontology:topic_domain(party_ban_instrument__potentiality_threshold_reading, "constitutional_law/political_party_regulation").

domain_priors:requires_active_enforcement(party_ban_instrument__potentiality_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_ban_instrument__potentiality_threshold_reading, 'ed63dcc9-4686-4b5c-83a4-f703e23be417').
narrative_ontology:cs_kernel_codification('ed63dcc9-4686-4b5c-83a4-f703e23be417', formalized).
narrative_ontology:cs_authority_grounding('ed63dcc9-4686-4b5c-83a4-f703e23be417', lineage).
narrative_ontology:cs_interpretation_layer_present('ed63dcc9-4686-4b5c-83a4-f703e23be417').
narrative_ontology:cs_reading_relation('ed63dcc9-4686-4b5c-83a4-f703e23be417', party_ban_instrument__founding_precedents_reading, influences).
narrative_ontology:cs_reading_relation('ed63dcc9-4686-4b5c-83a4-f703e23be417', party_ban_instrument__chilling_critique_reading, coexists_with).
narrative_ontology:cs_axiom('ed63dcc9-4686-4b5c-83a4-f703e23be417', foundational, potentiality_requirement_conditions_dissolution).
narrative_ontology:cs_axiom_status(potentiality_requirement_conditions_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('ed63dcc9-4686-4b5c-83a4-f703e23be417', potentiality_requirement_conditions_dissolution, deontological).
narrative_ontology:cs_axiom('ed63dcc9-4686-4b5c-83a4-f703e23be417', secondary, rule_of_law_evidentiary_rigor).
narrative_ontology:cs_axiom_status(rule_of_law_evidentiary_rigor, holdable).
narrative_ontology:cs_axiom_grounding('ed63dcc9-4686-4b5c-83a4-f703e23be417', rule_of_law_evidentiary_rigor, deontological).
narrative_ontology:cs_reference_frame('ed63dcc9-4686-4b5c-83a4-f703e23be417', capability_conditioned_party_dissolution).
narrative_ontology:cs_drift_state('ed63dcc9-4686-4b5c-83a4-f703e23be417', contemporary_post_npd, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed63dcc9-4686-4b5c-83a4-f703e23be417', '').
narrative_ontology:cs_kernel_id(party_ban_instrument__potentiality_threshold_reading, party_ban_instrument).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_ban_instrument__potentiality_threshold_reading, marginal_extremist_political_movements).
narrative_ontology:constraint_victim(party_ban_instrument__potentiality_threshold_reading, ban_on_ideology_doctrinal_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED EXTREMIST MOVEMENT (ROPE) — Under the potentiality threshold, hostility to constitutional order alone is insufficient grounds for dissolution. The movement is trapped (cannot exit the political system) but experiences the constraint as primarily coordination rather than pure extraction: the doctrine limits state authority while simultaneously denying the movement any immunity. The movement cannot organize legally, but neither can the state act without demonstrating capability for violent overthrow. Paradoxically, this creates a stabilizing coordination between the state and the movement — each respects a procedural boundary. The movement sees rope rather than snare because the mechanism has genuine coordination content: it prevents bans on ideology alone.
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT / ENFORCER INSTITUTION (TANGLED ROPE) — The court is constrained by its own doctrine: it has both the coordination function (legitimating party regulation via rule-of-law procedure) and the extraction mechanism (defining what counts as unconstitutional aims and demonstrating capability thresholds, thereby controlling which political movements are sanctionable). The court benefits from the doctrine because it legitimates judicial authority over party dissolution while appearing neutral. The court bears costs because it must continually assess capability claims with evidentiary rigor, limiting its discretionary reach. Active enforcement required: the court must produce evidence of capability, not merely infer it from rhetoric. This asymmetry between coordination (rule-based procedure) and extraction (gatekeeping power) is the defining tangled_rope signature.
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAINSTREAM POLITICAL PARTIES (SNARE) — Mainstream parties are constrained by the doctrine but benefit from it asymmetrically. The potentiality threshold protects mainstream parties from dissolution while preserving the state's power to ban genuinely threatening movements. However, the doctrine also chills mainstream party behavior: any serious capability-building for extraconstitutional aims risks triggering dissolution scrutiny. The suppression mechanism is soft — no explicit ban on speech or organization, only the threat of dissolution if capability is demonstrated. Moderate experienced extraction because mainstream parties can exit the constraint (by choosing not to build coercive capacity) but face continuous monitoring and legitimacy pressure. Classification as snare reflects that suppression exceeds genuine coordination benefit: the chilling effect extends beyond the narrow class of genuinely dangerous movements.
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BAN-ON-IDEOLOGY DOCTRINAL TRADITION (VICTIM-BENEFICIARY PARADOX) — The ban-on-ideology tradition is refined but not eliminated by the potentiality threshold. The doctrine continues to vest state authority in party regulation but narrows the grounds. The tradition has a genuine coordination function: it permits political pluralism while reserving state power against existential threats. But the extraction mechanism is embedded in the definition of 'potentiality' and 'unconstitutional aims' — the court retains discretionary power to interpret what counts as genuine capability. The doctrine benefits the constitutional establishment by legitimating party regulation under rule-of-law cover. The tradition bears costs because the evidentiary burden (demonstrating potentiality) limits regulatory reach and requires continuous judicialization of political questions. The tangled_rope classification reflects that the tradition simultaneously benefits from the constraint (legitimacy, procedural authority) and bears costs from it (evidentiary burden, narrowed reach).
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL THRESHOLD VIEW (MOUNTAIN) — From a civilizational perspective, the potentiality threshold appears as a natural legal principle: any stable constitutional order must distinguish between hostility and capability to overthrow. The line between protected speech and actionable threat is inherent to law itself. However, this mountain classification instantiates the false-summit signature — the 'natural' threshold is actually a contingent doctrinal refinement produced by the NPD judgment, not a timeless principle. The appearance of naturalness depends on treating the court's interpretation as judicial discovery rather than judicial choice.
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PERFORMATIVE POTENTIALITY ASSESSMENT (PITON) — The doctrine's application has increasingly become performative ritual. Courts are asked to assess whether a party has 'demonstrated the capability to achieve unconstitutional aims' through weapons stockpiling, paramilitary training, or financial resources. In practice, the assessment is highly theatrical: courts examine rhetoric, conduct, and institutional capacity through the lens of doctrinal categories that pre-determine the outcome. The court's forensic procedure (Is there capability? Is there clear and present danger?) creates the appearance of rigorous evidentiary scrutiny while actually implementing discretionary political judgment. Theater ratio reflects that the procedural rigor masks political choice in technical language.
constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_ban_instrument__potentiality_threshold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_ban_instrument__potentiality_threshold_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(party_ban_instrument__potentiality_threshold_reading, TR),
    TR >= 0.70.

:- end_tests(party_ban_instrument__potentiality_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The potentiality threshold reserves party-dissolution authority to cases where genuine capability for constitutional overthrow is demonstrated. This narrows but does not eliminate the extraction mechanism. Beneficiaries are paradoxical: marginalized movements benefit from the potentiality requirement (protection against bans on ideology alone) but are victimized by the dissolution threat itself. The state benefits from legitimate authority to ban genuinely dangerous movements. Extractiveness is moderate rather than high because the doctrine creates procedural limitations on suppression — the state cannot act on hostility alone. Measurement trajectory shows declining extractiveness over time as the doctrine stabilizes and courts develop consistent evidentiary standards. Suppression (0.52): Moderate-high. The doctrine suppresses through multiple mechanisms: direct threat of party dissolution, monitoring and surveillance of organizational capacity, legal scrutiny of paramilitary organization and weapons acquisition, chilling effects on mainstream party extraconstitutional preparation. However, suppression is conditioned on capability, not ideology alone. The measurement trajectory shows declining suppression as the doctrine matures and movements internalize the potentiality threshold as an operating constraint. Theater ratio (0.58): Moderate-high. The potentiality assessment involves significant performative elements. Courts conduct forensic analysis of organizational capacity, financial resources, paramilitary training, and weapons acquisition — producing the appearance of rigorous evidentiary scrutiny. But the underlying question (Does this movement genuinely threaten constitutional overthrow?) admits no objective measurement standard; courts apply doctrine post-hoc to reach predetermined political judgments. The theater increases over time as courts develop more elaborate evidentiary procedures to justify dissolution decisions.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates stark perspectival divergence. The marginalized movement sees primarily rope (protection through procedure) while simultaneously experiencing snare (dissolution threat). The court sees tangled_rope (coordination through rule-of-law procedure, extraction through judicial gatekeeping). The mainstream party sees snare (suppression through doctrine even though movement is not formally banned). The analytical observer risks seeing mountain (naturalizes potentiality threshold as constitutional necessity). The ban-on-ideology tradition sees itself victimized (narrowed grounds for action). The perspectival gaps reflect that potentiality_threshold_reading is a judicial choice about how to balance suppression and protection, not a natural principle — different observers experience the same doctrine differently depending on their structural position relative to the suppression mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The potentiality_threshold_reading establishes directionality through the condition of capability: beneficiaries are those who benefit from state power to suppress genuine threats (the constitutional establishment, mainstream parties) while bearing the cost of having that power conditioned on evidentiary demonstration (the court must prove capability). Victims are those who benefit from the potentiality requirement (marginalized movements gain protection against pure ideological bans) while bearing the threat of dissolution if capability is demonstrated (movements remain perpetually exposed to dissolution scrutiny). The ban-on-ideology tradition is the victim set because the doctrine narrows grounds for dissolution, diminishing the tradition's reach. Each agent's experienced extraction depends on their structural position: a marginalized movement bearing trapped exit experiences higher chi (d ≈ 0.85); the institutional beneficiary with arbitrage exit experiences lower chi (d ≈ 0.15); the court as enforcer with constrained exit experiences moderate chi (d ≈ 0.55).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potentiality_evidentiary_burden,
    'What evidence suffices to demonstrate ''potentiality to achieve unconstitutional aims''? Is the threshold judicial assessment or doctrinal creation?',
    'Comparative analysis of dissolution cases: what evidentiary bases did courts cite for capability findings? Historical counterfactual: would the NPD have been dissolved under pre-NPD doctrine (chilling_critique_reading) despite identical conduct?',
    'If courts have genuine evidentiary standards: potentiality_threshold_reading stands as principled refinement. If courts apply doctrine post-hoc to reach predetermined political outcomes: the constraint reverts to chilling_critique_reading (suppression operates through threat, not actual threshold enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potentiality_evidentiary_burden, empirical, 'Whether potentiality threshold is enforced or performative').

omega_variable(
    capability_asymmetry_across_ideologies,
    'Is the potentiality threshold applied symmetrically across left-extremist and right-extremist movements, or does assessment reflect political asymmetry?',
    'Systematic review of dissolution proceedings: comparison of evidentiary standards applied to Marxist-Leninist vs. neo-Nazi parties; tracking of timing and thresholds across the ideological spectrum; analysis of German Constitutional Court decisions on die Linke, MLPD, and other left-extremist movements relative to NPD standard',
    'If symmetric: potentiality_threshold_reading instantiates genuine doctrinal principle. If asymmetric: doctrine masks differential suppression; constraint operates as ideological snare rather than potentiality-conditioned rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_asymmetry_across_ideologies, empirical, 'Whether potentiality threshold is applied symmetrically across ideologies').

omega_variable(
    alternative_mechanisms_to_potentiality,
    'Could constitutional order protect itself against genuine threats without reserving party-dissolution authority? Do democracies without explicit party-ban instruments (US, UK) achieve constitutional stability through other mechanisms?',
    'Comparative constitutional law: analysis of how non-German democracies address extremist parties; examination of whether alternative regulatory mechanisms (deplatforming, campaign finance restrictions, paramilitary prosecution) achieve equivalent protective function; assessment of whether party-ban instrument uniquely necessary or contingent institutional choice',
    'If alternatives exist: party-ban instrument is contingent institutional choice (refined by NPD into potentiality_threshold_reading) rather than natural constitutional requirement. If no alternatives: potentiality threshold appears as closest approach to constitutional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanisms_to_potentiality, conceptual, 'Whether party-dissolution authority is constitutionally necessary or contingent').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Does the potentiality_threshold_reading foreclose the chilling_critique_reading (does the doctrine actually block the chilling effect), or do both mechanisms operate simultaneously?',
    'Empirical analysis of doctrinal effect: do political movements report reduced chilling effects post-NPD? Do mainstream parties reduce extraconstitutional preparation post-doctrine? Compare pre-NPD and post-NPD literature on chill. Structural analysis: can a doctrine that conditions suppression on capability simultaneously suppress through capability-demonstration threat?',
    'If chilling effect persists: both readings coexist; the doctrine refines suppression mechanisms rather than replacing them. If chilling effect genuinely reduced: potentiality_threshold_reading forecloses chilling_critique_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, empirical, 'Whether potentiality threshold actually forecloses chilling effect').

omega_variable(
    natural_law_vs_contingent_doctrine,
    'Is the potentiality threshold a natural principle (law inherent to constitutional order) or a contingent doctrinal refinement (judicial choice that could have been otherwise)?',
    'Genealogical analysis: trace the doctrine from founding precedents (KPD, SRP) through interim doctrine to NPD. Identify points where alternative doctrinal paths were available. Assess whether potentiality requirement was inherent to earlier cases or introduced as judicial innovation by NPD judgment.',
    'If natural principle: mountain classification appropriate. If contingent refinement: mountain is false summit (misrecognizes doctrinal choice as legal necessity). The engine''s FSM detector will flag this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_doctrine, conceptual, 'Whether potentiality threshold is natural constitutional law or judicial innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_ban_instrument__potentiality_threshold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(party_ban_theater_t0, party_ban_instrument__potentiality_threshold_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(party_ban_theater_t5, party_ban_instrument__potentiality_threshold_reading, theater_ratio, 5, 0.53).
narrative_ontology:measurement(party_ban_theater_t10, party_ban_instrument__potentiality_threshold_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(party_ban_extractiveness_t0, party_ban_instrument__potentiality_threshold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(party_ban_extractiveness_t5, party_ban_instrument__potentiality_threshold_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(party_ban_extractiveness_t10, party_ban_instrument__potentiality_threshold_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(party_ban_suppression_t0, party_ban_instrument__potentiality_threshold_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(party_ban_suppression_t5, party_ban_instrument__potentiality_threshold_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(party_ban_suppression_t10, party_ban_instrument__potentiality_threshold_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_ban_instrument__potentiality_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_ban_instrument__potentiality_threshold_reading, party_ban_instrument__founding_precedents_reading).
narrative_ontology:affects_constraint(party_ban_instrument__potentiality_threshold_reading, party_ban_instrument__chilling_critique_reading).

% DUAL FORMULATION NOTE:
% The potentiality_threshold_reading is one of three structurally distinct readings of the party_ban_instrument kernel. The founding_precedents_reading emphasizes historical precedent (SRP, KPD dissolutions); the chilling_critique_reading emphasizes suppressive effect through threat rather than enforcement; this reading emphasizes doctrinal refinement (capability as condition). Each reading has distinct extractiveness, suppression, and type profiles. Network links represent the doctrinal contest — each reading influences how courts apply the party-ban doctrine. All three readings inhabit the same authoritative tradition (German Constitutional Court jurisprudence) and coexist in contemporary practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
