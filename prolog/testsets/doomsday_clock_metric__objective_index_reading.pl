% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock is a symbolic and institutional device maintained by
 *   the Bulletin of the Atomic Scientists to communicate the proximity of
 *   global catastrophe. Each year, the board convenes to assess existential
 *   risks and adjust the clock's setting — 'minutes to midnight' where
 *   midnight represents human extinction or civilizational collapse. The
 *   constraint modeled here is the OBJECTIVE_INDEX_READING: the reading that
 *   treats clock-setting as objective expert synthesis of empirical
 *   indicators of existential risk. This reading claims the clock's position
 *   reflects measurable, aggregable facts about risk levels rather than
 *   normative judgments about which risks matter most. This is ONE of three
 *   competing readings of the same kernel (the clock mechanism itself). The
 *   other readings — the performative_tool_reading (clock is primarily a
 *   narrative device) and the hybrid_legitimacy_reading (clock mixes
 *   empirical and normative in acknowledged way) — are structurally distinct
 *   constraints that coexist in institutional space. This story instantiates
 *   ONLY the objective reading, treating it as a clean ε-invariant constraint
 *   with its own beneficiaries (scientific authority), victims (democratic
 *   deliberation), and classification (tangled_rope).
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists: Institution maintaining the clock; primary beneficiary (institutional/arbitrage) — derives authority and relevance from clock's perceived objectivity
 *   - Scientific Authority Community: Experts on nuclear weapons, biosecurity, AI, climate who convene to set the clock; beneficiary (institutional/arbitrage) — clock legitimizes expert judgment without requiring public ratification of value premises
 *   - Democratic Publics and Non-Expert Stakeholders: Victim (powerless/trapped) — subject to expert synthesis but cannot participate in risk-weighting deliberation; trapped by framing that clock reflects objective measurement
 *   - Policy Makers and Governance Systems: Mixed beneficiary/victim (institutional/constrained) — use clock to justify resource allocation but constrained by expert consensus that defines what counts as legitimate action
 *   - Civil Society Advocacy Organizations: Organized victims (organized/constrained) — can produce counter-expertise but asymmetrically positioned relative to expert board authority
 *   - Media Ecosystem: Institutional actor (institutional/arbitrage) — reproduces clock narrative through inertia; could exit but benefits from narrative convenience
 *   - Analytical Observer: Sees both coordination and extraction simultaneously (analytical/analytical) — observes that the constraint genuinely synthesizes expert judgment while simultaneously suppressing normative deliberation about risk prioritization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.68).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '913eb83a-11b3-423e-bb1c-378985baf583').
narrative_ontology:cs_kernel_codification('913eb83a-11b3-423e-bb1c-378985baf583', formalized).
narrative_ontology:cs_authority_grounding('913eb83a-11b3-423e-bb1c-378985baf583', extraction).
narrative_ontology:cs_interpretation_layer_present('913eb83a-11b3-423e-bb1c-378985baf583').
narrative_ontology:cs_reading_relation('913eb83a-11b3-423e-bb1c-378985baf583', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('913eb83a-11b3-423e-bb1c-378985baf583', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('913eb83a-11b3-423e-bb1c-378985baf583', foundational, existential_risk_measurable_through_expert_synthesis).
narrative_ontology:cs_axiom_status(existential_risk_measurable_through_expert_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('913eb83a-11b3-423e-bb1c-378985baf583', existential_risk_measurable_through_expert_synthesis, empirically_contingent).
narrative_ontology:cs_axiom('913eb83a-11b3-423e-bb1c-378985baf583', foundational, risk_weighting_is_technical_not_normative).
narrative_ontology:cs_axiom_status(risk_weighting_is_technical_not_normative, holdable).
narrative_ontology:cs_axiom_grounding('913eb83a-11b3-423e-bb1c-378985baf583', risk_weighting_is_technical_not_normative, empirically_contingent).
narrative_ontology:cs_reference_frame('913eb83a-11b3-423e-bb1c-378985baf583', technical_risk_synthesis_framework).
narrative_ontology:cs_drift_state('913eb83a-11b3-423e-bb1c-378985baf583', contemporary_post_2000s_multi_risk_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('913eb83a-11b3-423e-bb1c-378985baf583', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority_community).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_consensus_institutions).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, public_interpretive_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEMOCRATIC PUBLICS (SNARE) — Trapped by expert monopoly on clock interpretation. No exit from the framing that clock-setting is an objective technical procedure rather than a normative judgment. Cannot participate in the actual deliberation that produces the index. Bears full cost of deference to authority without capacity to contest the underlying value premises.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL SOCIETY ADVOCATES (TANGLED ROPE) — Can organize and produce counter-expertise, but constrained by asymmetric media access and institutional credibility. The clock reading provides a coordination device for diverse civil society actors (nuclear weapons, biosecurity, AI risk, climate) to align threat assessments. Also subject to extraction: their concerns are legible only insofar as they align with the board's categories. Moderate agency with significant extraction.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SCIENTIFIC AUTHORITY (ROPE) — Primary beneficiary. The clock reading legitimizes expert authority to speak on existential risk without requiring democratic ratification or transparent normative debate. Experiences the constraint as pure coordination: synthesizing diverse expert views into a single metric that communicates urgency. Arbitrage option: can exit by simply ceasing to publish clock readings; does not do so because the institutional benefit is high.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY MAKERS (TANGLED ROPE) — Use the clock as a coordination device to justify resource allocation to existential risk reduction (biosafety, nuclear nonproliferation, AI safety). But constrained by the clock's legitimacy: cannot easily repudiate it without losing the expert consensus that justifies spending. The constraint facilitates coordination on risk governance while limiting policy options to those the expert consensus endorses.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDIA ECOSYSTEM (PITON) — The clock provides a narrative peg and visual metaphor that media outlets habitually reproduce. Theater_ratio is moderate (0.55) because the clock performs both a coordination function AND a theatrical one — it's genuinely useful for communicating urgency but also functions as a symbolic ritual that substitutes for deeper policy discussion. Media could exit by cease to cover the clock; continue covering it through institutional inertia and narrative convenience.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint clearly: the clock is a hybrid mechanism that genuinely synthesizes expert judgment (coordination function) while simultaneously suppressing the normative commitments embedded in risk weighting and parameter selection (extraction via naturalization of values). This reading instantiates the objective_index_reading: clock setting tracks measurable existential risk through expert synthesis. The analytical frame sees both the real coordination and the real suppression simultaneously.
constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doomsday_clock_metric__objective_index_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, TR),
    TR >= 0.70.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The objective_index_reading interprets the clock as embedding technical synthesis of empirical indicators. But the empirical/normative boundary is permeable — risk weighting (how much nuclear weapons vs biosecurity vs AI vs climate?) is a normative choice disguised as technical synthesis. The extraction mechanism is the suppression of democratic deliberation about these weightings. The constraint extracts institutional authority (beneficiaries control the definition of existential risk) and suppresses public interpretive access (victims cannot contest the normative premises). Extractiveness increases over the 75-year interval (0.32 → 0.58) as the clock's authority has consolidated and as the number of risk categories has grown, making the weighting question ever more normatively laden. Suppression (0.68): High. Multiple suppression mechanisms: (1) technical framing suppresses recognition of normative commitments; (2) institutional monopoly prevents alternative risk syntheses from competing; (3) public deference to expert authority prevents contestation; (4) media reproduction of clock as objective fact reinforces the frame. Suppression has increased as the clock's cultural salience has grown. Theater ratio (0.55): Moderate. The clock performs both genuine coordination (expert synthesis of heterogeneous risk assessments) and performative ritual (annual announcement, visual metaphor of urgency). The objective_index_reading emphasizes the coordination function; the sibling performative_tool_reading would emphasize the theater. The moderate value reflects that BOTH are real — the constraint is genuinely hybrid.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the beneficiary's rope and the victim's snare is maximal. Scientific authority experiences the constraint as pure coordination: genuine challenge of synthesizing diverse expert views into a single metric that communicates urgency. The public experiences the constraint as a snare: trapped by expert authority with no exit from the framing that clock-setting is objective technical work. The analytical observer sees tangled_rope: both the real coordination function AND the real suppression of normative deliberation are structurally present. The policy-maker perspective shows the constraint as coordination tool (rope) for resource allocation but with constrained exit if the expert consensus changes (tangled_rope). Civil society advocates see mixed coordination and extraction (tangled_rope): the clock provides a valuable shared metric but also constrains what counts as legitimate risk. The media perspective shows piton dynamics: the clock persists through institutional inertia and narrative convenience rather than strong functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by whether the agent is structural beneficiary or victim of the clock-setting authority. Scientific authority and the bulletin are beneficiaries with arbitrage options (could stop publishing the clock but choose not to) — they derive d from low beneficiary position + arbitrage exit, yielding low d values and negative or low f(d). Democratic publics are victims with trapped exit options (cannot exit from living under expert-defined risk frameworks) — yielding high d values (0.90+) and correspondingly high f(d). Policy makers are institutionally positioned as both beneficiaries (use clock to justify spending) and victims of constraint (cannot pursue policies the clock does not endorse) — yielding mid-range d values. Organized civil society has better exit (can produce counter-expertise) than trapped publics but remains constrained by asymmetric institutional positioning — yielding d in the 0.55–0.70 range. The analytical observer is positioned at d ≈ 0.72 (observer position), enabling clear sight of both the coordination mechanism and the suppression mechanism operating simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the doomsday_clock_metric kernel is whether the clock is a genuine risk measurement or a performative legitimacy claim. The objective_index_reading resolves this by saying: BOTH, with tangled_rope classification. The clock genuinely synthesizes expert judgment on existential risk (coordination function ε ≈ 0.30) AND genuinely suppresses democratic deliberation about risk-weighting priorities (extraction function ε ≈ 0.28, adding to 0.58 total). The sibling performative_tool_reading emphasizes only the theater component (ε higher, closer to 0.72+, classification snare or piton). The hybrid_legitimacy_reading would acknowledge both but claim the authority structure is transparent about its normative commitments (ε lower, classification rope or hybrid-rope). This story instantiates ONLY the objective reading and routes the mandatrophy dispute to omega variables: omega risk_weighting_normativity addresses whether the clock embeds normative or empirical commitments; omega democratic_deliberation_replacement addresses whether the clock substitutes for or supplements public deliberation; omega sibling_reading_empirical_closure asks whether evidence can resolve whether the clock is objective (supports this reading) or performative (supports sibling reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_weighting_normativity,
    'Are the clock-setting board''s risk weightings (e.g., nuclear weapons vs biosecurity vs AI risk vs climate) technical derivations from empirical data or normative policy commitments disguised as technical judgment?',
    'Decompose clock-setting deliberations into separable empirical vs normative components. Identify which parameter changes would reverse classification (e.g., doubling the weight of biosecurity risk). Model counterfactual boards with different normative priors and compare outputs.',
    'If primarily empirical: clock is closer to rope (pure coordination). If significantly normative: extraction mechanism is confirmed (suppression of democratic deliberation about risk priorities). This is the core mandatrophy ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_weighting_normativity, conceptual, 'Extent to which clock-setting embeds normative rather than empirical commitments').

omega_variable(
    democratic_deliberation_replacement,
    'Does the clock substitute for democratic deliberation about existential risk governance, or supplement it?',
    'Empirical comparison: jurisdictions with high clock salience vs low salience; tracking of public understanding of clock methodology; presence/absence of broader deliberation about risk prioritization in clock-exposed vs clock-unexposed populations.',
    'If substitutes: suppression mechanism confirmed; clock reading forecloses democratic alternatives. If supplements: extraction is real but not total; democratic deliberation occurs alongside expert framing. This changes the magnitude of suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deliberation_replacement, empirical, 'Whether clock reading substitutes for or supplements democratic deliberation').

omega_variable(
    sibling_reading_empirical_closure,
    'Can the objective_index_reading (this reading) and the performative_tool_reading coexist as live positions, or does empirical evidence on democratic understanding resolve the dispute?',
    'Test whether publics treat the clock as an objective measurement (objective reading) or a symbolic tool (performative reading). Measure: literal interpretation of ''minutes to midnight'' among surveyed populations; ability to articulate the board''s risk weighting logic; resistance to alternative clock framings.',
    'If evidence supports literal interpretation: objective reading is empirically grounded. If evidence supports symbolic treatment: performative reading is empirically grounded. If mixed: hybrid_legitimacy_reading is the stronger claim, suggesting coexistence is unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_closure, empirical, 'Whether empirical evidence resolves the objective vs performative reading dispute').

omega_variable(
    kernel_codification_stability,
    'Is the clock''s kernel (the procedure and values defining ''minutes to midnight'') genuinely fixed/formalized, or does it have latent ambiguity that interpretation absorbs?',
    'Historical analysis of clock-setting rationales across decades. Map consistency of risk weighting and board composition changes. Identify which judgment calls have shifted without formal rule change.',
    'If formalized and stable: this reading''s authority_grounding is expertise (technical derivation). If latently ambiguous: authority is distributed or driven by interpretation (interpretation_layer_present = true). This affects whether foreclosure is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_stability, empirical, 'Whether the Doomsday Clock kernel is formally stable or interpretively fluid').

omega_variable(
    scientific_authority_monopoly_necessity,
    'Is the suppression of democratic deliberation about risk weighting a necessary cost of expert coordination on existential risk, or a contingent institutional arrangement that could be reformed?',
    'Design space analysis: model alternative governance structures that maintain expert synthesis but include explicit democratic deliberation about risk prioritization (e.g., participatory budgeting for existential risk, deliberative polling on risk weighting). Assess feasibility and information loss relative to current expert monopoly.',
    'If necessary: suppression is justified as coordination cost; classification remains tangled_rope. If contingent: suppression is extracted overhead; reclassification to snare or pure extraction is possible. This is a preference omega — the resolution depends on value commitments about expertise vs democracy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scientific_authority_monopoly_necessity, preference, 'Whether expert monopoly on clock-setting is necessary or reformable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomclock_obj_theater_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(doomclock_obj_theater_t35, doomsday_clock_metric__objective_index_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement(doomclock_obj_theater_t75, doomsday_clock_metric__objective_index_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(doomclock_obj_extractiveness_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(doomclock_obj_extractiveness_t35, doomsday_clock_metric__objective_index_reading, base_extractiveness, 35, 0.5).
narrative_ontology:measurement(doomclock_obj_extractiveness_t75, doomsday_clock_metric__objective_index_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doomclock_obj_suppression_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(doomclock_obj_suppression_t35, doomsday_clock_metric__objective_index_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(doomclock_obj_suppression_t75, doomsday_clock_metric__objective_index_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.12).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, existential_risk_epistemic_authority).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, expert_consensus_governance_legitimacy).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three structurally distinct constraint stories: (1) objective_index_reading (ε=0.58, tangled_rope) — clock synthesizes empirical risk while suppressing normative deliberation; (2) performative_tool_reading (ε≈0.72, snare/piton) — clock is primarily narrative device with minimal empirical grounding; (3) hybrid_legitimacy_reading (ε≈0.40, tangled_rope with more transparent normative component) — clock genuinely mixes empirical and normative, but authority would acknowledge this explicitly. Each reading has a different beneficiary/victim structure and different suppression mechanism. They are linked via network.affects_constraints because the institutional dominance of one reading (currently objective) marginalizes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
