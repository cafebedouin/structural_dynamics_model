% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Coordination (Renegotiable)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the localized-practice reading of jati
 *   boundaries — the claim that jati categories function primarily as
 *   coordination norms for occupational identity, marriage networks, and
 *   ritual participation, subject to continuous renegotiation at the village
 *   and regional level. This reading emphasizes the empirical proliferation
 *   of jati categories (3000+ documented types across India) and the
 *   mechanisms of local boundary fluidity: jati fusion and fission,
 *   occupational transitions, marriage alliance shifts, and ritual practice
 *   variation. Under this reading, jati boundaries are low-extraction
 *   coordination mechanisms analogous to guild systems or professional
 *   standards — they solve genuine collective action problems (labor market
 *   organization, marriage network stability, ritual ordering) without
 *   relying primarily on coercive enforcement. The constraint has low base
 *   extractiveness (0.22) because the primary mechanism is social reputation
 *   and ritual participation benefit rather than coercive suppression.
 *   Suppression (0.35) reflects barriers to exit: occupational dependency,
 *   identity fusion, and ritual participation requirements create constrained
 *   exit options, but these are not the foundational mechanism of the
 *   constraint — they are consequences of its coordination function.
 *
 * KEY AGENTS:
 *   - Local community members (moderate/constrained): experience jati boundaries as flexible coordination norms negotiated within their village; benefit from marriage network stability and occupational identity clarity
 *   - Ritual specialist guilds (organized/mobile): use jati boundaries to regulate apprenticeship and occupational standards; maintain regional networks; exit is possible through occupational change
 *   - Landowning elite (powerful/arbitrage): coordinate labor recruitment and ritual status through jati classification; have exit options (capital mobility, marriage arbitrage); primary beneficiaries of coordination benefits
 *   - Ritual outcaste members (powerless/identity_locked): structurally mobile but identity-fused with exclusionary ritual status; experience tangled_rope rather than pure rope due to asymmetric access to sacred commons
 *   - Analytical observer (analytical/analytical): sees jati proliferation as evidence of continuous local renegotiation rather than rigid hierarchy; recognizes coordination function dominates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.22).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.35).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Coordination (Renegotiable)").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '934323dd-f81c-46c2-9416-76cfc8c7557f').
narrative_ontology:cs_kernel_codification('934323dd-f81c-46c2-9416-76cfc8c7557f', distributed).
narrative_ontology:cs_authority_grounding('934323dd-f81c-46c2-9416-76cfc8c7557f', practice).
narrative_ontology:cs_interpretation_layer_present('934323dd-f81c-46c2-9416-76cfc8c7557f').
narrative_ontology:cs_reading_relation('934323dd-f81c-46c2-9416-76cfc8c7557f', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('934323dd-f81c-46c2-9416-76cfc8c7557f', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('934323dd-f81c-46c2-9416-76cfc8c7557f', foundational, jati_practice_fundamentally_flexible).
narrative_ontology:cs_axiom_status(jati_practice_fundamentally_flexible, holdable).
narrative_ontology:cs_axiom_grounding('934323dd-f81c-46c2-9416-76cfc8c7557f', jati_practice_fundamentally_flexible, empirically_contingent).
narrative_ontology:cs_axiom('934323dd-f81c-46c2-9416-76cfc8c7557f', foundational, coordination_function_dominates_extraction).
narrative_ontology:cs_axiom_status(coordination_function_dominates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('934323dd-f81c-46c2-9416-76cfc8c7557f', coordination_function_dominates_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('934323dd-f81c-46c2-9416-76cfc8c7557f', village_level_practice_autonomy).
narrative_ontology:cs_drift_state('934323dd-f81c-46c2-9416-76cfc8c7557f', contemporary_postcolonial, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('934323dd-f81c-46c2-9416-76cfc8c7557f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_community_groups).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_guilds).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, ritual_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% LOCAL COMMUNITY PERSPECTIVE (ROPE) — Members experience jati boundaries as coordination norms negotiated continuously within their village or caste council. Boundaries shift with resource access, occupation, marriage alliances, and ritual practice. Exit requires relocation or occupation change (constrained, not trapped). The constraint solves coordination problems: marriage networks, occupational identity, ritual status, property inheritance. Extraction is minimal — the coordination function dominates.
constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% RITUAL SPECIALIST PERSPECTIVE (ROPE) — Occupational guilds (brahmin purohits, washermen, potters) use jati boundaries to regulate apprenticeship, maintain occupational standards, and coordinate ritual services across regions. Boundaries are enforceable through guild mechanisms (reputation, exclusion from ceremonies, occupational licensing) but fundamentally voluntary — members can apprentice elsewhere or change occupations. Coordination benefits (market protection, quality standards, knowledge transmission) are genuine. Low extraction.
constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% DOMINANT CASTE PERSPECTIVE (ROPE) — Landowning elites use jati classification to coordinate labor recruitment, maintain ritual status in the village hierarchy, and enforce agrarian contracts. But these benefits derive from coordination (labor supply, ritual ordering) rather than pure coercion. Jati boundaries are continuously renegotiated through patronage relationships, not maintained by external enforcement. Elite exit options are arbitrage (can move capital, shift occupations, marry outside caste for political advantage). Net beneficiary, but through coordination rather than extraction.
constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% RITUAL OUTCASTE PERSPECTIVE (TANGLED ROPE) — Some jati categories (untouchables, excluded occupations) enforce asymmetric access to wells, temples, and ritual participation. The victim is structurally mobile (could relocate) but identity-fused with their ritual status — their personhood is constituted through exclusion, making exit unthinkable within their identity frame. The constraint has both coordination (ritual ordering) and asymmetric extraction (exclusion from sacred commons). This perspective reveals the tension within the localized-practice reading: coordination and extraction are entangled, not separable.
constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% ANALYTICAL OBSERVER PERSPECTIVE (ROPE) — From a civilizational analytical stance, jati practice coordinates multiple functions: occupational identity, marriage alliance, ritual participation, property inheritance, and labor supply. The proliferation to 3000+ documented categories indicates continuous local renegotiation, not rigid caste hierarchy. Enforcement is through social reputation and ritual participation, not coercive machinery. Extraction exists at the ritual outcaste boundary but is not the primary function. The constraint is fundamentally a coordination mechanism with embedded asymmetries, classified as rope.
constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jati_practice_norm__localized_practice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The localized-practice reading locates jati's primary function in coordination — occupational identity, marriage networks, ritual participation, labor supply organization. These are genuine collective action problems. Extraction exists (ritual outcaste exclusion, labor asymmetries) but is neither the foundational mechanism nor the dominant experience for most jati members. The measurement trajectory (0.18 → 0.24) shows slight upward drift as brahminical textual authority increased during the colonial and postcolonial periods, but remains well below the tangled_rope threshold (0.30). Suppression (0.35): Moderate. Barriers to exit exist — occupational dependency, geographic isolation, identity fusion, ritual participation requirements — but are not the mechanism of enforcement. The constraint is maintained through social reputation, ritual benefits, and coordination gains, not through coercive denial of alternatives. Theater ratio (0.38): Low-moderate. Performance exists in ritual contexts (hierarchy assertion, status display) but is not the dominant mechanism. Occupational guilds operate on functional standards; marriage networks operate on practical kinship logic; ritual specialists maintain genuine expertise. The theater rises slightly over time (0.35 → 0.40) as brahminical brahmin-centered ritual performance became more salient, but remains below the piton threshold.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap in this reading lies between the local community experience (rope — coordination dominates) and the ritual outcaste experience (tangled_rope — coordination entangled with extraction). Both perspectives operate within the localized-practice reading, but they reveal internal tension: jati coordination functions for occupational guilds, marriage networks, and ritual ordering, but simultaneously enforces asymmetric access to sacred space and ritual participation. A robust analytical reading would decompose the constraint family: one story for occupational coordination (pure rope), another for ritual inclusion/exclusion (tangled_rope), linked by network.affects_constraints. This story chooses to keep both within the localized reading to show how the reading itself must account for the tangled_rope minority case.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from power level, exit options, and beneficiary/victim status. Local community members (moderate power + constrained exit + mixed benefits/costs) derive d ≈ 0.50, placing them near symmetry — they benefit from coordination but bear some suppression cost. Ritual specialists (organized power + mobile exit + beneficiary status) derive d ≈ 0.35, placing them slightly toward beneficiary side. Landowning elite (powerful + arbitrage + beneficiary) derive d ≈ 0.15, placing them as net beneficiaries with low experienced extraction. The ritual outcaste (powerless + identity_locked + victim) derives d ≈ 0.88, placing them as full extraction targets — but they are a minority perspective within the localized-practice reading, and the reading classifies them as tangled_rope (mixed coordination/extraction) rather than pure snare. The analytical observer (analytical context) derives d ≈ 0.72 (canonical fallback for analytical power), but the measured extractiveness is low because the constraint has low base extraction and no dramatic scope amplification at the analytical level. No directionality overrides are needed — the derivation chain produces values consistent with the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the jati_practice_norm kernel reflects genuine ambiguity about which reading corresponds to reality. The localized-practice reading claims that actual ground-level practice exhibits far more flexibility than brahminical textual hierarchy or colonial racial classification would suggest. But the presence of the tangled_rope perspective (ritual outcaste) shows that the localized reading cannot fully escape the charge of romanticizing flexibility while downplaying extraction. The resolution lies in careful measurement: How much jati stability derives from identity fusion versus structural barriers? How much boundary renegotiation actually occurs versus how much is constrained by brahminical hierarchy? How does extractiveness vary across regions and time periods? The measurement trajectory (0.18 → 0.24) shows upward drift as brahminical authority increased, suggesting that the localized reading captures pre-colonial or early-colonial dynamics better than contemporary practice. Mandatrophy is resolved not by choosing one reading as 'correct' but by recognizing that jati_practice_norm admits multiple readings with different empirical domains: localized practice in flexible pre-colonial contexts, brahminical textual system in orthodox textual interpretation, rigid racial system in colonial enumeration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_mobility,
    'How much of the observed jati stability derives from internal identity fusion (people see jati membership as constitutive of self) versus external structural barriers (geographic isolation, occupational dependency, ritual participation requirements)?',
    'Historical analysis of voluntary jati switching during periods of occupational mobility (urbanization, migration to new regions, colonial employment opportunities); comparison of switching rates when external barriers removed vs. identity-internal reasons for staying',
    'If predominantly identity-locked: the constraint''s stability is cognitive, not structural — external mobility alone does not enable exit. If predominantly structural: reclassify some perspectives from identity_locked to constrained or mobile. Classification shifts from rope with identity-lock to rope with pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_mobility, empirical, 'Relative weight of identity fusion versus structural barriers in jati stability').

omega_variable(
    local_renegotiation_vs_rigid_hierarchy,
    'Are jati boundaries subject to genuine continuous renegotiation at the local level, or does hierarchical ordering (brahmin > shudra, pure > impure) constrain what counts as a valid negotiation?',
    'Ethnographic documentation of successful boundary renegotiations; frequency of jati fission and fusion events; comparison of stated local practices against pan-Indian brahminical ranking systems',
    'If boundaries genuinely renegotiable: localized-practice reading is correct — constraint is rope. If hierarchy constrains negotiation: constraint may be tangled_rope or snare at civilizational scope, with local rope only at immediate scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_renegotiation_vs_rigid_hierarchy, empirical, 'Degree of genuine local renegotiation versus brahminical hierarchy constraints').

omega_variable(
    coordination_vs_extraction_boundary_definition,
    'How do we distinguish jati boundaries that coordinate occupational standards and marriage networks (legitimate coordination function) from those that extract ritual labor or enforce untouchability (asymmetric extraction)?',
    'Ethnographic case comparison: occupational guilds with positive membership benefits versus ritual outcaste exclusions; measurement of benefit distribution across jati categories; analysis of which boundaries are enforced through reward versus punishment',
    'If distinction is clear: some jati boundaries classify as rope, others as tangled_rope or snare. If extraction is universally embedded: reclassify entire constraint to tangled_rope. If distinction is context-dependent: requires constraint family decomposition (separate stories per region/period).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary_definition, conceptual, 'Distinguishing coordination function from extractive enforcement in jati practice').

omega_variable(
    reading_committer_ambiguity,
    'This constraint is one reading of the jati_practice_norm kernel. Is the localized-practice reading (continuous renegotiation, coordination dominance) an accurate characterization of how jati boundaries actually function, or does it romanticize flexibility while underestimating brahminical hierarchy enforcement?',
    'Comparative ethnographic analysis across regions and time periods; examination of which agent groups report jati boundaries as flexible versus rigid; measurement of actual switching versus structural opportunity for switching',
    'If localized reading is accurate: jati is fundamentally rope. If hierarchy dominates: reclassify to tangled_rope or snare, and the localized reading becomes an aspirational counter-reading held by reformers rather than an accurate description. If both coexist: constraint family decomposition required (separate stories for hierarchical and localized readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, empirical, 'Accuracy of localized-practice reading versus brahminical-hierarchy reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_local_theater_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jati_local_theater_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(jati_local_theater_t200, jati_practice_norm__localized_practice_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_local_extract_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jati_local_extract_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(jati_local_extract_t200, jati_practice_norm__localized_practice_reading, base_extractiveness, 200, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel admits three structurally distinct constraint stories with different epsilon values. The orthodox_textual_reading (brahminical varna system, ε ≈ 0.45-0.60, tangled_rope) locates stability in religious texts and maintenance through ritual hierarchy. The colonial_census_reading (rigid enumerated racial categories, ε ≈ 0.55-0.72, tangled_rope to snare) locates stability in administrative enumeration and enforcement. The localized_practice_reading (this constraint, ε ≈ 0.22, rope) locates stability in coordination functions and continuous local renegotiation. All three are live interpretations held by different epistemic communities; none forecloses the others within a single framework, but they make different empirical claims about actual jati practice. Decomposition is required because epsilon differs substantially (0.22 vs 0.55-0.72) and reflects different structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
