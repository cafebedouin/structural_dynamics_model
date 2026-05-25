% ============================================================================
% CONSTRAINT STORY: co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-04-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_co_constitution_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: co_constitution_reading
 *   human_readable: Press-Reformer Co-Constitution in the Reformation Era
 *   domain: history_of_technology/religious_reformation/media_studies
 *
 * SUMMARY:
 *   The Reformation-era press represents a moment of co-constitutive
 *   causality: reformers did not determine the press, but they shaped its
 *   deployment and content; the press did not determine the Reformation, but
 *   it enabled the reformers' scale and reach. This constraint is ONE READING
 *   of the contested kernel 'technology-reformation causality.' The
 *   co-constitution reading instantiates this as a Tangled Rope: genuine
 *   coordination function (vernacular access, text standardization) paired
 *   with asymmetric extraction (reformer authority consolidation, printer
 *   market concentration, ecclesiastical monopoly disruption that
 *   concentrates knowledge among the literate). The constraint's
 *   extractiveness (0.38) reflects the interaction term between technological
 *   affordance and human agency — neither alone produces the outcome, and the
 *   extraction arises from their coupled dynamics, not from one determining
 *   the other. This reading is structurally distinct from the
 *   technological_determinism_reading (which treats the press as
 *   independently determining, reducing reformers to passive beneficiaries,
 *   ε=0.25) and the beneficiary_agency_reading (which treats reformers as
 *   independently determining, reducing the press to an incidental tool,
 *   ε=0.48). All three readings share the kernel but produce different
 *   classifications, beneficiary/victim structures, and historical
 *   implications.
 *
 * KEY AGENTS:
 *   - Reformation Movement Leadership (Luther, Calvin, etc.): Primary institutional beneficiary (institutional/arbitrage) — shapes press content and strategy, coordinates with printers, consolidates doctrinal authority. Benefits from coordination and captures authority rents.
 *   - Printing Press Operators and Merchants: Primary institutional beneficiary (institutional/arbitrage) — expands market, coordinates with reformers, captures economic rents. Both roles are coordinated, both benefit.
 *   - Urban Literate Merchant Class: Secondary beneficiary and moderate victim (moderate/constrained) — benefits from access to vernacular texts (coordination function) but face suppression from ecclesiastical enforcement. Constrained exit.
 *   - Oral Tradition Knowledge Keepers (Rural clergy, catechists): Primary victim (powerless/trapped) — epistemic displacement from literacy shift, no adaptive capacity, structural elimination. Trapped in obsolescence.
 *   - Ecclesiastical Authority (Catholic Church hierarchy): Institutional victim (institutional/constrained) — loses monopoly on scriptural interpretation, maintains performative authority through theater, atrophies functionally. Constrained by loss of enforcement capacity.
 *   - Non-Literate Rural Populations: Structural victim (powerless/trapped) — access to texts requires literacy and capital, excluded from coordination benefits, suppressed by both technological and religious barriers. Trapped in exclusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(co_constitution_reading, 0.38).
domain_priors:suppression_score(co_constitution_reading, 0.42).
domain_priors:theater_ratio(co_constitution_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(co_constitution_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(co_constitution_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(co_constitution_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(co_constitution_reading, "Press-Reformer Co-Constitution in the Reformation Era").
narrative_ontology:topic_domain(co_constitution_reading, "history_of_technology/religious_reformation/media_studies").

domain_priors:requires_active_enforcement(co_constitution_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(co_constitution_reading, reformation_movement_leadership).
narrative_ontology:constraint_beneficiary(co_constitution_reading, printing_press_operators).
narrative_ontology:constraint_beneficiary(co_constitution_reading, literate_urban_populations).
narrative_ontology:constraint_victim(co_constitution_reading, ecclesiastical_authority_monopoly).
narrative_ontology:constraint_victim(co_constitution_reading, oral_tradition_knowledge_keepers).
narrative_ontology:constraint_victim(co_constitution_reading, rural_non_literate_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORAL TRADITION KEEPERS (SNARE) — Rural clergy and lay catechists whose authority derived from memorized liturgy and oral transmission face structural elimination. The printed text displaces their epistemic role without offering exit — they lack literacy, capital, and mobility to adapt. The constraint traps them in obsolescence.
constraint_indexing:constraint_classification(co_constitution_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN LITERATE MERCHANTS (TANGLED ROPE) — Benefit from access to printed religious texts (coordination function: enables individual scripture reading) but face ecclesiastical enforcement against vernacular Bible ownership. Suppression is real but costly rather than absolute — literacy provides some exit capacity. The constraint both enables and restricts their religious autonomy.
constraint_indexing:constraint_classification(co_constitution_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMATION LEADERSHIP (ROPE) — Luther, Calvin, and their networks experience the printing press as pure coordination mechanism: amplifies their messaging, solves the collective action problem of reaching dispersed congregations, enables standardization of doctrine. They coordinate with printers; both benefit from the arrangement. No significant extraction burden — the constraint serves their agency.
constraint_indexing:constraint_classification(co_constitution_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRESS OPERATORS (ROPE) — Printers benefit from Reformation demand (expanded market), coordinate with reformers to produce vernacular texts, and capture economic rents. The constraint is pure coordination from their perspective — they solve the problem of text distribution for a willing customer. No coercion; arbitrage exit always available.
constraint_indexing:constraint_classification(co_constitution_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ECCLESIASTICAL AUTHORITY (PITON) — The late medieval Church maintained its knowledge monopoly through scribal networks and Latin gatekeeping. The printing press does not create extraction; it degrades enforcement of an existing constraint. Church authority over scriptural interpretation becomes performative (theater ratio rising as enforcement capacity falls). The institutional actor persists through inertia while its functional control atrophies.
constraint_indexing:constraint_classification(co_constitution_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CO-CONSTITUTION VIEW (TANGLED ROPE) — From civilizational distance, the constraint emerges from reciprocal reinforcement: reformers shape the press's content and dissemination (institutional ideology drives technological deployment), while the press's material affordances (reproducibility, speed, reach) enable the reformers' strategy (technology constrains and enables their message choices). Neither is determining; both co-constitute the outcome. This is the reading's core: bidirectional causality producing moderate extractiveness through asymmetric distribution of gains.
constraint_indexing:constraint_classification(co_constitution_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(co_constitution_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(co_constitution_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(co_constitution_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(co_constitution_reading, TR),
    TR >= 0.70.

:- end_tests(co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, reflecting the coupled interaction. The co-constitution reading rejects technological determinism (which would set ε ≈ 0.20) and pure beneficiary agency (which would set ε ≈ 0.50). The moderate value captures that neither technology nor agency alone produces the extractive outcome — it emerges from their interaction. The press enables reformers' reach, but reformers shape how the press is deployed. The extractiveness accumulates over the measurement interval (0.15 → 0.38) as the institutional consolidation intensifies. Suppression (0.42): Moderate. Both religious enforcement (ecclesiastical prohibition of vernacular texts) and technological barriers (literacy requirements, capital costs) suppress alternatives. Suppression does not fully prevent coordination benefits (hence not Snare, which requires suppression ≥ 0.60), but significantly reduces access and agency. Theater ratio (0.55): Moderate. The ecclesiastical authority's response to press-enabled Reformation becomes increasingly performative — maintaining doctrinal authority through counter-reformation theater while losing actual enforcement capacity. Theater rises as functional control atrophies, but the overall theater ratio remains moderate because the reformer and printer sides exhibit genuine coordination function (not pure theater).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is maximally revealing. The Reformation leadership sees rope (pure coordination with printers; both benefit from solving the distribution problem). The printers see rope (economic and technical coordination; expanded market). The urban literates see tangled rope (coordination benefits from vernacular access, suppression costs from enforcement). The ecclesiastical authority sees piton (atrophied authority, performative ritual, inertial maintenance). The oral tradition keepers see snare (displacement, no exit, no adaptation path). The analytical observer sees tangled rope (genuine coordination function, asymmetric extraction, co-constitutive causality producing the outcome). The gap between rope (leadership/printer view) and snare (oral tradition keeper view) reflects the full range of extraction distribution — some actors benefit coordinatively, others lose everything. The civilization-scale analytical view of tangled rope (not mountain) indicates that this is a contingent institutional arrangement, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The co-constitution reading distributes directionality asymmetrically across the beneficiary/victim map. Reformation leadership and press operators occupy institutional/arbitrage positions — low d (0.15–0.20), experiencing negative effective extraction (they benefit). Urban literate populations occupy moderate/constrained positions — moderate d (0.55–0.65), experiencing moderate extraction (coordination benefits offset by suppression costs). Ecclesiastical authority occupies institutional/constrained position — higher d (0.45–0.55) due to loss of monopoly, but not as high as trapped agents because institutional actors retain some adaptive capacity. Oral tradition keepers and non-literate populations occupy powerless/trapped positions — high d (0.90–0.95), experiencing maximum effective extraction because they lose epistemic role without compensation or exit. The perspective gap is acute: the beneficiaries (leadership and printers) see rope; the constrained moderate classes see tangled rope; the trapped agents see snare. The analytical observer at civilizational scale sees the tangled rope classification because the coordination function is genuine (vernacular access is real coordination) and the extraction is real (asymmetric distribution of authority and capital). Neither determining alone — both required to explain the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through the co-constitution framing. The question 'Is this technology determining outcomes or is human agency determining outcomes?' produces contradictory answers if treated as binary. The co-constitution reading dissolves the contradiction by making clear that BOTH are necessary — the constraint cannot be classified by choosing one. The moderate extractiveness (0.38) reflects that neither alone produces the outcome; the interaction does. If forced to a binary, you would misclassify: technological determinism alone (ε ≈ 0.20, Rope) ignores reformer agency in shaping press deployment; pure agency (ε ≈ 0.50, Snare) ignores the enabling role of press affordances. The tangled rope classification captures that institutional actors (reformers, printers) genuinely coordinate while distributing benefits asymmetrically to non-institutional actors (literate and non-literate populations). The mandatrophy is resolved by accepting the interaction term rather than projecting it onto one causal agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_ambiguity,
    'What is the direction of primary causality: Did reformer ideology drive technological deployment, or did printing press affordances enable reformer success, or did both co-constitute the outcome?',
    'Counterfactual analysis: Would Reformation have achieved similar scale without printing? Would printing have been deployed similarly without Reformation demand? Comparative cases: printing in Islamic and Orthodox territories with different religious movements.',
    'If reformer-driven (technological_determinism_reading sibling): ε=0.25, Rope. If press-driven (beneficiary_agency_reading sibling): ε=0.48, Snare. If co-constituted (this reading): ε=0.38, Tangled Rope. Classification hinges on causal direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, conceptual, 'Primary direction of causality in press-reformation relationship').

omega_variable(
    extraction_mechanism_specificity,
    'Is the extractiveness in this constraint a property of the press-reformer relationship itself, or is it inherited from the prior ecclesiastical monopoly that the constraint disrupts?',
    'Distinguish new extraction (created by press-reformer co-constitution) from redistribution of existing extraction (former Church monopoly now held by reformers and printers). Measure income/authority flows before and after adoption.',
    'If new extraction: constraint is Tangled Rope (both coordination and asymmetric gain). If redistribution of existing extraction: constraint is Rope (both parties benefit from the coordination relative to prior regime). Impacts classification and victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_specificity, empirical, 'Whether extractiveness is newly created or redistributed from prior monopoly').

omega_variable(
    reading_versus_determinism,
    'This is the co-constitution reading: both reformers and technology shaped outcomes. How does this differ structurally from the sibling readings (technological_determinism_reading and beneficiary_agency_reading)?',
    'The three readings share the kernel (technology-reformation causality) but instantiate different constraints with different ε values and beneficiary/victim structures. This reading: bidirectional causality, moderate extractiveness (ε=0.38), both reformers and press as beneficiaries, literate and ecclesiastical authority as victims. Technological_determinism_reading: press drives outcome independently (ε=0.25, Rope, reformers as victims of press constraints). Beneficiary_agency_reading: reformers drive outcome, press is incidental tool (ε=0.48, Snare, press operators as victims of reformer demand capture).',
    'Each reading produces different classification, different victim identification, different policy implications. The kernel is the contested causality; the readings are three structurally distinct constraints derived from different framing of causality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_determinism, conceptual, 'This reading (co-constitution) versus sibling readings (determinism, agency) as instantiations of shared kernel').

omega_variable(
    suppression_mechanism_religious_versus_technological,
    'Is the suppression (0.42) driven by religious/ecclesiastical enforcement against vernacular texts, or by technological barriers to literacy and text access, or by both?',
    'Historical analysis of enforcement mechanisms: Were non-literate populations suppressed by lack of access to technology, or by active religious prohibition, or both? Did suppression decrease as literacy increased even without technological change? Did suppression decrease as technology improved even without religious liberalization?',
    'If primarily religious: ecclesiastical actor bears responsibility for suppression (victim declaration is justified). If primarily technological: the constraint''s suppression is a material feature not attributable to agency. Affects moral and analytical framing of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_religious_versus_technological, empirical, 'Whether suppression is primarily religious prohibition or technological/literacy barrier').

omega_variable(
    coordination_function_genuine_or_cover,
    'Is the coordination function (enabling vernacular scripture access, standardizing doctrine) genuine coordination that beneficiaries actively value, or a cover story for reformer expansion of control over textual interpretation?',
    'Comparison of demand: Did non-clergy populations independently demand vernacular access, or was demand created by reformer advocacy? Did coordination benefits accrue to beneficiaries measurably (education, literacy rates, autonomy), or only to reformers and printers (authority, market share)?',
    'If genuine coordination: Tangled Rope classification sustained (both coordination and extraction present). If cover story: reclassify toward Snare (primarily extraction, coordination claim is theater). Affects assessment of whether the constraint is sustainable or inherently contradictory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuine_or_cover, empirical, 'Whether coordination benefits are genuine or instrumental to expansion of reformer control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(co_constitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(co_c_tr_t0, co_constitution_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(co_c_tr_t30, co_constitution_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(co_c_tr_t60, co_constitution_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(co_c_tr_t100, co_constitution_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(co_c_be_t0, co_constitution_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(co_c_be_t30, co_constitution_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(co_c_be_t60, co_constitution_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(co_c_be_t100, co_constitution_reading, base_extractiveness, 100, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(co_constitution_reading, information_standard).
narrative_ontology:affects_constraint(co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(co_constitution_reading, beneficiary_agency_reading).
narrative_ontology:affects_constraint(co_constitution_reading, ecclesiastical_authority_information_control).
narrative_ontology:affects_constraint(co_constitution_reading, literacy_access_barrier).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (technology-reformation causality). Two sibling readings exist: technological_determinism_reading (ε=0.25, Rope, press-determined) and beneficiary_agency_reading (ε=0.48, Snare, reformer-determined). The three readings are structurally distinct constraints with different epsilon values reflecting different causal framings. This reading's ε=0.38 reflects the interaction term: neither technology nor agency alone produces the outcome; both co-constitute through reciprocal reinforcement. All three should be included in the corpus linked via network.affects_constraints for full representation of the kernel's causal ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(co_constitution_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
