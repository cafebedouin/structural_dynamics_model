% ============================================================================
% CONSTRAINT STORY: universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_universality_paradox_reading, []).

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
 *   constraint_id: universality_paradox_reading
 *   human_readable: Unconditional Income Support as Universality Paradox (Political Ambiguity Reading)
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   Unconditional Income Support (UIS) policies — including Universal Basic
 *   Income (UBI) proposals and various cash transfer schemes — exhibit a
 *   structural ambiguity that allows ideologically incompatible coalitions to
 *   unite behind the same policy label. Left-libertarians support UIS as a
 *   replacement for paternalistic categorical programs (respecting recipient
 *   autonomy). Right-wing efficiency advocates support it as a simplification
 *   that reduces bureaucratic overhead. Redistributive leftists support it as
 *   a universal cash floor. Conservative fiscal hawks support it as a more
 *   efficient delivery mechanism than means-tested welfare. The universality
 *   paradox reading emphasizes that this ambiguity is not incidental — it is
 *   the constraint's central coordinating mechanism. The same policy design
 *   can be implemented with high taxing-back rates (functionally targeted,
 *   redistributive) or low taxing-back rates (functionally universal, less
 *   redistributive), yet all versions are rhetorically claimed to embody
 *   'universality.' This reading treats the constraint as a tangled rope: it
 *   genuinely coordinates disparate actors into a single policy vehicle
 *   (coordination function), but the ambiguity about what universality means
 *   enables extraction by political entrepreneurs and policy designers who
 *   benefit from being able to claim success to multiple constituencies while
 *   parameter choices determine actual winners and losers.
 *
 * KEY AGENTS:
 *   - Political Entrepreneurs: Primary beneficiary (organized/arbitrage) — capture framing and coalition-building power, can claim credit to multiple ideologies simultaneously
 *   - Policy Designers/Bureaucrats: Secondary beneficiary (institutional/constrained) — gain discretion through ambiguous mandate, insulated from ideological accountability
 *   - Targeted Program Recipients: Primary victim (powerless/trapped) — face program cuts justified by universality rhetoric they cannot influence
 *   - Ideological Clarity (Public Discourse): Victim (powerless/trapped) — abstract good that cannot organize; ambiguity prevents coherent evaluation of actual redistributive consequences
 *   - Left-Libertarian Coalition Partner: Mixed (moderate/constrained) — benefits from universality frame but endures extraction through design choices
 *   - International Development Institutions: Secondary actor (institutional/arbitrage) — reify universality as neutral technical solution, obscuring political parameter choices
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political ambiguity as fiscal inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(universality_paradox_reading, 0.38).
domain_priors:suppression_score(universality_paradox_reading, 0.52).
domain_priors:theater_ratio(universality_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(universality_paradox_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(universality_paradox_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(universality_paradox_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(universality_paradox_reading, "Unconditional Income Support as Universality Paradox (Political Ambiguity Reading)").
narrative_ontology:topic_domain(universality_paradox_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(universality_paradox_reading, distributed).
narrative_ontology:cs_authority_grounding(universality_paradox_reading, distributed).
narrative_ontology:cs_kernel_id(universality_paradox_reading, unconditional_income_support).
narrative_ontology:cs_reading_relation(universality_paradox_reading, freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation(universality_paradox_reading, dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom(universality_paradox_reading, foundational, universality_as_political_ambiguity).
narrative_ontology:cs_axiom_status(universality_as_political_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding(universality_paradox_reading, universality_as_political_ambiguity, conventional).
narrative_ontology:cs_axiom(universality_paradox_reading, foundational, fiscal_parameter_indeterminacy).
narrative_ontology:cs_axiom_status(fiscal_parameter_indeterminacy, holdable).
narrative_ontology:cs_axiom_grounding(universality_paradox_reading, fiscal_parameter_indeterminacy, empirically_contingent).
narrative_ontology:cs_reference_frame(universality_paradox_reading, ambiguous_policy_vehicle).
narrative_ontology:cs_drift_state(universality_paradox_reading, contemporary_uis_proposals, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(universality_paradox_reading, policy_designers).
narrative_ontology:constraint_beneficiary(universality_paradox_reading, dominant_coalition_actors).
narrative_ontology:constraint_victim(universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(universality_paradox_reading, fiscal_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED PROGRAM RECIPIENTS (SNARE) — Trapped in a system where universality rhetoric justifies eliminating categorical programs they depend on. The universality framing ('everyone gets the same basic income') becomes cover for defunding disability supplements, housing assistance, and childcare support. No exit: they depend on transfers and cannot influence the political coalition-building that determines which programs persist. Extraction is maximized — they bear costs (program cuts) while universality discourse prevents recognition that the redistributive design choices favor higher-income universalists.
constraint_indexing:constraint_classification(universality_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEFT-LIBERTARIAN COALITION PARTNER (TANGLED ROPE) — Constrained by need to maintain coalition to pass any income support reform. Benefits from universality framing (it aligns with libertarian anti-paternalism) but endures extraction in the form of taxing-back mechanisms that are more regressive than targeted programs would be. Coordination function: the universality language allows alliance with right-wing efficiency advocates. But extraction hidden: the coalition's design choices (funding through broad consumption taxes, minimal labor-market conditionality) embed asymmetries that benefit higher earners.
constraint_indexing:constraint_classification(universality_paradox_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL ENTREPRENEURS AND COALITION BUILDERS (ROPE) — Arbitrage position. They benefit from universality's ambiguity: it allows them to claim credit with left-libertarians (anti-paternalism), conservatives (efficiency/simplicity), pragmatists (universal programs easier to administer), and redistributive left (cash support). The constraint is functionally a coordination mechanism from their perspective — it coordinates disparate ideological constituencies into a single policy vehicle. Extraction runs toward these actors; they capture the agenda-setting and framing power.
constraint_indexing:constraint_classification(universality_paradox_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY DESIGN BUREAUCRACY (TANGLED ROPE) — Institutional actors who design the transfer mechanisms. They benefit from universality's ambiguity because it allows flexibility in implementation: the same policy can be calibrated with high taxing-back (functionally targeted), low taxing-back (functionally universal), conditional work requirements, or unconditional transfers. The bureaucracy gains discretion and insulation from ideological accountability — they can claim they are 'just implementing the universal program' while parameter choices determine winners/losers. Extraction: the ambiguity prevents external evaluation of whether design choices serve the stated universalist goal or serve narrower interests.
constraint_indexing:constraint_classification(universality_paradox_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT INSTITUTION (PITON) — Organizations like the World Bank, IMF, and OECD present UBI/UIS as a neutral technical solution to welfare-state inefficiencies. This perspective reifies the universality framing as depoliticized: the institutional actor claims to operate above ideology, implementing what 'evidence shows works.' The performative content is high (theater_ratio approaching 0.70) because the institutional neutrality framing obscures that parameter choices (tax rates, phase-out schedules, labor-market conditions) are intensely political and determine whose interests are served. The institution maintains its authority by treating design as technical rather than political.
constraint_indexing:constraint_classification(universality_paradox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FISCAL EQUIVALENCE MOUNTAIN VIEW (MOUNTAIN) — From a civilizational economic analysis perspective, the universality paradox reflects an immutable feature of fiscal design: any income transfer system funded by taxation on the same population exhibits equivalent distributional outcomes if the tax schedules and transfer parameters are sufficiently flexible. This perspective naturalizes the ambiguity as a mathematical law — the political coalition-building is epiphenomenal to the fiscal arithmetic. However, this perspective risks false-summit classification by treating designed political ambiguity as natural inevitability.
constraint_indexing:constraint_classification(universality_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(universality_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(universality_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(universality_paradox_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(universality_paradox_reading, TR),
    TR >= 0.70.

:- end_tests(universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The universality paradox is not a high-extraction constraint in the sense that the policy does not impose direct coercive costs on most beneficiaries. However, the extraction operates through ambiguity rather than force: political entrepreneurs extract agenda-setting power and coalition-building control; policy designers extract discretion and insulation from accountability; targeted program recipients lose protections as categorical programs are cut in favor of universal programs with high phase-out rates. The extractiveness score reflects that the political mechanism (ambiguity enabling coalition-building) is the primary extraction vector, not fiscal redistribution per se. Suppression (0.52): Moderate-high. The suppression operates through cognitive and institutional mechanisms: (a) universality language obscures the fact that different implementations have different distributional consequences, (b) technical/economic expertise frames parameter choices as neutral rather than political, (c) the coalition's diversity prevents any single member from clearly articulating the actual design, (d) program recipients lack channels to articulate whether they are benefited or harmed because universality rhetoric makes alternatives seem incoherent. Theater ratio (0.68): High. The performative content is substantial: universality is invoked as a principle while actual implementation choices (tax rates, phase-outs, labor conditions) are presented as technical details rather than political commitments. The institutional development community particularly exhibits high theater — presenting what is a fundamentally contested political design as a neutral efficiency improvement backed by 'evidence.'
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across five of six classifications. Targeted program recipients see a snare: they are trapped in a system where universality rhetoric covers program cuts they cannot resist. Political entrepreneurs see a rope: the constraint coordinates disparate ideologies into a single policy vehicle, solving a collective action problem (how to build a coalition for cash transfers when agreement on underlying values is impossible). Policy designers see a tangled rope with net benefits: genuine coordination function plus discretion gained through ambiguity. Left-libertarian partners see a tangled rope with costs: benefit from the anti-paternalism framing but endure regressive taxing-back mechanisms. The international development institution sees a piton: a neutral technical solution sustained through institutional authority despite its political content. The analytical observer risks a false mountain: naturalizing the ambiguity as inevitable fiscal equivalence. The perspectival gap reveals that 'universality' means fundamentally different things to each coalition member — it is the label they share, not the commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position and benefit/cost relationship to the ambiguity. Political entrepreneurs benefit from ambiguity (low d: 0.12, derived as beneficiary + arbitrage exit) — they capture coalition-building power and can present themselves as victors to multiple constituencies. Policy designers benefit from discretion but are constrained by their institutional role (d: 0.35, derived as beneficiary + constrained exit). Targeted program recipients are trapped as costs are externalized through universality framing (d: 0.93, derived as victim + trapped exit) — maximum experienced extractiveness. The ideological clarity victim is abstract and cannot organize (d: 0.95, canonical powerless + trapped). The left-libertarian coalition partner has moderate costs (d: 0.58, derived as victim + constrained exit) — they benefit from the coordination but bear costs in design parameters. The international institution frames itself as neutral observer (d: 0.70, canonical institutional + analytical boundary) but is actually a beneficiary through authority maintenance. The analytical observer would derive d=0.72 (canonical analytical) but risks false-summit misclassification through naturalizing contingent political designs as natural laws.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading RESOLVES the mandatrophy by identifying the universality paradox as the constraint's central structural feature. The question 'Is UIS coordination or extraction?' is false — it is both, entangled through ambiguity. The constraint is a tangled rope precisely because the same policy label ('universality') enables both genuine coordination (bridging ideological divides to pass legislation) and extraction (obscuring whose interests the parameter choices serve). The mandatrophy resolution is that no single classification is 'correct' across all perspectives because the constraint's mechanism IS the perspectival divergence. The ambiguity is not a bug to fix; it is the feature that enables the coalition. Resolving the ambiguity (e.g., by committing to high vs. low taxing-back) would dissolve the coalition and hence the constraint's coordination function. The reading thus explains why UIS policies remain ambiguous even when proposed: clarity is existentially threatening to the political coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_as_design_vs_accident,
    'Is the universality paradox — the ambiguity that allows incompatible ideologies to support the same policy — a deliberate design choice or an unintended consequence of coalition politics?',
    'Historical process tracing of policy design stages: Did early drafts make design choices explicit and then muddy them? Did actors consciously craft language to accommodate multiple readings? Or did ambiguity emerge emergently from compromise?',
    'If deliberate design: extraction is intentional and political entrepreneurs are culpable (snare classification confirmed for recipients). If accidental: the constraint may be more unstable (vulnerable to rhetorical collapse if ambiguity is exposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_design_vs_accident, empirical, 'Whether ambiguity is designed or emergent').

omega_variable(
    taxing_back_empirical_equivalence,
    'Do the distributional outcomes of high-taxing-back UIS designs actually converge on the same fiscal redistribution as the low-taxing-back designs from the same implementation, or do parameter differences create materially different outcomes?',
    'Microsimulation studies comparing multiple design implementations with identical base transfer but different tax-back schedules, phase-out rates, and conditionality. Direct empirical comparison of post-transfer income distributions across design variants.',
    'If outcomes genuinely converge: the universality paradox is confirmed as fiscal equivalence (mountain perspective justified). If outcomes diverge: the political design choices mask distinct redistributive consequences (snare and extraction are more severe than ε=0.38 suggests).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taxing_back_empirical_equivalence, empirical, 'Empirical equivalence of distributional outcomes across taxing-back designs').

omega_variable(
    ideological_foreclosure_or_coexistence,
    'Does the universality paradox foreclose the coherence of any of the sibling readings (freedom floor, dependency trap), or do all three readings remain live positions that different actors can maintain simultaneously?',
    'Logical analysis: Can an actor hold the freedom floor reading AND the universality paradox reading in the same framework? Can an actor hold the dependency trap reading AND recognize that the universality framing masks incompatible designs? Or does accepting one reading force rejection of the others?',
    'If universality paradox forecloses the freedom floor: the readings are mutually exclusive (forecloses relation). If all three coexist across different coalition members: the paradox is structural (coexists_with relation). If the paradox influences but doesn''t eliminate the others: influences relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_foreclosure_or_coexistence, conceptual, 'Logical relationships between sibling readings').

omega_variable(
    recipient_preference_revelation,
    'Given the ambiguity of UIS designs, can targeted program recipients express preferences that would reveal whether they benefit from universality (as politically ambiguous coordination) or are harmed (as cover for program cuts)?',
    'Survey and deliberative research asking recipients: (a) Do you prefer a universal transfer with high phase-out rates or a narrower categorical program? (b) When told the universality is ambiguous, does your preference change? (c) Do you believe universality language will protect or erode the specific benefits you depend on?',
    'If recipients prefer universality even when informed: ambiguity may be genuinely coordinating their interests (constraint may be rope rather than snare). If preferences reverse when ambiguity is revealed: the constraint is primarily extractive (snare confirmed for recipient perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recipient_preference_revelation, empirical, 'Whether recipients benefit or are harmed by universality ambiguity').

omega_variable(
    reading_identity_kernel_ambiguity,
    'This constraint instantiates one reading of the ''unconditional income support'' kernel. The kernel itself is ambiguous: does it refer to the fiscal mechanism (unconditional in tax schedule) or the normative goal (unconditioned human dignity)? Does this reading depend on a specific resolution of the kernel''s ambiguity?',
    'Analyze which sibling readings rely on which interpretation of the kernel. Trace whether this reading''s coherence depends on treating the kernel as fiscal (mechanism-level) or normative (goal-level) or both.',
    'If this reading requires a specific kernel interpretation that sibling readings reject: the readings are incompatible (forecloses). If the kernel ambiguity is the source of the paradox: the paradox is structural to the kernel itself and cannot be resolved within a single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_ambiguity, conceptual, 'Dependence of this reading on specific kernel interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(universality_paradox_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(univ_paradox_tr_t0, universality_paradox_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(univ_paradox_tr_t3, universality_paradox_reading, theater_ratio, 3, 0.6).
narrative_ontology:measurement(univ_paradox_tr_t6, universality_paradox_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(univ_paradox_be_t0, universality_paradox_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(univ_paradox_be_t3, universality_paradox_reading, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(univ_paradox_be_t6, universality_paradox_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(universality_paradox_reading, resource_allocation).
narrative_ontology:affects_constraint(universality_paradox_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(universality_paradox_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(universality_paradox_reading, welfare_state_categoricity).

% DUAL FORMULATION NOTE:
% The universality_paradox_reading is one component of the unconditional_income_support kernel family. The freedom_floor_reading treats UIS as instantiating a normative commitment; the dependency_trap_reading treats it as an empirical hazard. This reading treats UIS as a political coalition mechanism. The three readings are linked through the kernel they all interpret, not through causal dependency. Each is a complete constraint story with its own epsilon value and perspectival structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(universality_paradox_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
