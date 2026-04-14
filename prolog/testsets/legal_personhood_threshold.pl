% ============================================================================
% CONSTRAINT STORY: legal_personhood_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_threshold, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_personhood_threshold
 *   human_readable: Legal Personhood Threshold
 *   domain: law/philosophy/political_economy
 *
 * SUMMARY:
 *   The legal personhood threshold defines which entities possess legal
 *   standing, rights-bearing capacity, and the ability to hold property or
 *   enter contracts. This threshold is simultaneously a coordination
 *   mechanism (enabling predictable legal relationships) and an extraction
 *   apparatus (concentrating rights and power in those deemed legal persons
 *   while denying them to those classified as non-persons). The constraint
 *   exhibits the full spectrum of DR classification depending on the
 *   observer's structural position relative to the boundary. Historically,
 *   the threshold has shifted dramatically — excluding enslaved persons,
 *   women, non-property-owning males, and animals while latterly expanding to
 *   include corporations, some animals, and environmental entities. These
 *   shifts reveal that the threshold is not a natural law but a contestable
 *   institutional boundary whose placement systematically advantages some
 *   groups while suppressing others. The theater ratio has increased over the
 *   measurement interval (0.48 to 0.65) as legal discourse becomes more
 *   theoretically sophisticated about personhood while the actual boundary
 *   enforcement remains substantially unchanged, creating a gap between
 *   performative inclusion (academic recognition of animal sentience, rights
 *   of nature concepts) and structural exclusion (enforcement of property law
 *   that treats non-human beings as objects). The extractiveness has
 *   decreased over the interval (0.72 to 0.58) due to successful advocacy
 *   movements that have expanded the personhood boundary, yet suppression
 *   remains high (0.72) because the fundamental mechanism — designating some
 *   beings as non-persons — persists despite boundary adjustments.
 *
 * KEY AGENTS:
 *   - The Excluded Being (powerless/trapped): Entities denied personhood status — historically enslaved persons, currently animals, AI systems, nature entities, future beings. Bear maximum extraction.
 *   - The Advocacy Coalition (moderate/constrained): Animal rights organizations, indigenous sovereignty movements, disability advocates, environmental protection groups. Seek boundary expansion; constrained by legal and political barriers.
 *   - The Property-Owning Class (institutional/arbitrage): Beneficiaries of the personhood framework who use it to secure property rights and contractual power. Experience framework as coordination mechanism; have exit options through jurisdiction selection.
 *   - The Legal Establishment (institutional/constrained): Courts, legislatures, legal academia. Coordinate dispute resolution through personhood doctrine while extracting interpretive authority. Constrained by precedent and legitimacy requirements.
 *   - Alternative Legal Frameworks (organized/mobile): Indigenous systems recognizing personhood in nature, relational ontologies treating humans as embedded in webs of mutual obligation. Theoretically viable but institutionally marginalized; performing alternative personhood while lacking enforcement power.
 *   - The Analytical Observer (analytical/analytical): Risks naturalizing a contingent institutional boundary as a metaphysical necessity, obscuring the real extraction mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_threshold, 0.58).
domain_priors:suppression_score(legal_personhood_threshold, 0.72).
domain_priors:theater_ratio(legal_personhood_threshold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(legal_personhood_threshold, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_threshold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_threshold, tangled_rope).
narrative_ontology:human_readable(legal_personhood_threshold, "Legal Personhood Threshold").
narrative_ontology:topic_domain(legal_personhood_threshold, "law/philosophy/political_economy").

domain_priors:requires_active_enforcement(legal_personhood_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_threshold, property_owners).
narrative_ontology:constraint_beneficiary(legal_personhood_threshold, legal_establishment).
narrative_ontology:constraint_beneficiary(legal_personhood_threshold, institutional_power_holders).
narrative_ontology:constraint_victim(legal_personhood_threshold, marginalized_groups).
narrative_ontology:constraint_victim(legal_personhood_threshold, non_human_beings).
narrative_ontology:constraint_victim(legal_personhood_threshold, future_entities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXCLUDED BEING (SNARE) — Those denied personhood status (enslaved persons historically, non-human animals, potential future artificial minds, environmental entities) experience the threshold as inescapable structural exclusion. No exit option exists from within the personhood boundary itself; recognition requires external advocacy. The constraint extracts full dignity and legal protection from those it classifies as non-persons.
constraint_indexing:constraint_classification(legal_personhood_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ADVOCACY COALITION (TANGLED ROPE) — Groups seeking to expand personhood (animal rights organizations, indigenous sovereignty movements, disability rights advocates, future-oriented policy groups) experience both coordination benefits and extraction costs. The personhood framework coordinates boundary disputes and provides a shared language for rights claims, but the threshold itself constrains which claims gain traction. Expansion requires mobilization of resources and political capital; the outcome is uncertain and distributed among coalition members.
constraint_indexing:constraint_classification(legal_personhood_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PROPERTY-OWNING CLASS (ROPE) — Institutional beneficiaries experience the personhood threshold as coordination mechanism: it reliably defines who can hold property, sign contracts, and exercise legal will. The framework enables commerce and predictability. These actors have arbitrage options (jurisdiction selection, asset structuring, legal form selection) and benefit from the threshold's stability. Extraction runs toward them.
constraint_indexing:constraint_classification(legal_personhood_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE LEGAL ESTABLISHMENT (TANGLED ROPE) — Courts, legislatures, and legal academies coordinate dispute resolution through personhood doctrine (real coordination function) while simultaneously extracting interpretive authority and gate-keeping power. Judges and legal scholars gain professional status and institutional power through control of personhood boundary interpretation. The establishment is constrained by prior precedent and public legitimacy requirements, limiting true arbitrage. This is both coordination (providing predictable legal categories) and extraction (monopolizing interpretive authority).
constraint_indexing:constraint_classification(legal_personhood_threshold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE LEGAL FRAMEWORKS (PITON) — Indigenous legal systems, relational ontologies, and non-Western personhood concepts (which recognize personhood in rivers, mountains, animals) exist as theoretically viable alternatives but remain marginalized. Their institutional presence is substantial but largely theatrical — recognized in academic discussion and some jurisdictional islands (Te Urewera in New Zealand, rights of nature in Ecuador) but not operationally dominant. These alternatives maintain potential but are functionally inertial, sustained by indigenous power-holders and philosophical commitment rather than by systemic integration. Theater ratio reflects that alternative frameworks perform existence without reshaping the dominant threshold's operation.
constraint_indexing:constraint_classification(legal_personhood_threshold, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER — METAPHYSICAL NECESSITY VIEW (MOUNTAIN) — From a universal civilizational perspective, some threshold distinguishing entities with legal standing from those without is logically necessary to any coherent legal system. Without a boundary, law cannot function. This perspective treats the personhood threshold as a metaphysical and logical necessity — invariant across all observable measurements. However, this classification functions as a false summit detector: while SOME threshold is necessary, the specific threshold at any moment (e.g., adult human males with property, or all humans, or animals with sentience) is contingent, chosen, and contestable. The mountain obscures the real extraction: not the existence of a threshold, but its specific placement and who controls its movement.
constraint_indexing:constraint_classification(legal_personhood_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_personhood_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_personhood_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_personhood_threshold, TR),
    TR >= 0.70.

:- end_tests(legal_personhood_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The personhood threshold systematically denies legal capacity, property rights, and protection from harm to those deemed non-persons. The extraction is not absolute (some non-persons receive limited protections; some persons lose rights) but structural. The value reflects that the boundary is actively enforced and strongly correlates with power asymmetry, yet is partially contestable through advocacy. Over the 75-year measurement window, extractiveness has declined from 0.72 to 0.58 due to successful threshold expansions (abolition of slavery, recognition of animal sentience, corporate personhood, rights of nature), but the mechanism remains substantially intact. Suppression (0.72): High. Non-persons have no legal recourse against harm, no property rights, no contractual capacity. Barriers to threshold crossing are extremely high: require coordination of power coalitions, legislative change, or court doctrine shifts. Escape routes are limited to legal argument and advocacy. Theater ratio (0.65): Moderate-high. Legal discourse performs sophisticated philosophical and scientific argument about personhood (consciousness, sentience, rationality, moral status) while actual threshold placement remains driven by power politics and economic interest. The gap between performative personhood theory and enforcement practice has widened over the measurement interval — more sophisticated theory (0.48 theater at t=0) but less actual boundary movement, creating appearance of complexity masking relative institutional stability. Claimed type (Tangled Rope): The constraint provides genuine coordination of legal rights and property relationships (coordination function present) while simultaneously extracting power and dignity from those classified as non-persons (asymmetric extraction present). Active enforcement is required to maintain the boundary against expansion attempts. The classification fits the tangled rope gate: beneficiaries + victims + active enforcement + 0.40 ≤ χ ≤ 0.90.
 *
 * PERSPECTIVAL GAP:
 *   The excluded being (snare) and property owner (rope) perspectives have maximum gap: the same boundary is experienced as pure extraction from one side and pure coordination from the other. This gap width is itself diagnostic. It reveals that the constraint's apparent type depends entirely on which side of the personhood boundary you occupy. The framework works smoothly for those inside it (property owners see coordination, predictability, enablement) and brutally for those outside (non-persons see denial, powerlessness, objectification). The gap indicates that the real constraint is not the logical necessity of some threshold but the specific institutional placement of THIS threshold and the enforcement mechanisms that maintain it against boundary-expansion attempts. The tangled rope classifications (advocacy coalition, legal establishment) occupy the middle ground: they recognize both coordination function and extraction mechanism, experiencing the constraint as mixed. The alternative legal frameworks (piton) represent an organizational response to the gap: they maintain theoretical viability of alternative thresholds while remaining institutionally marginalized. Their persistence despite marginalization suggests the dominant threshold is maintained partly by suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from the agent's relationship to the personhood boundary and their exit options. Property owners who benefit from personhood status and have arbitrage options (jurisdiction selection, legal form selection) experience low or negative directionality (d ≈ 0.15-0.20), producing low effective extraction despite high base extractiveness. The legal establishment, though institutional, is constrained by precedent and legitimacy requirements — they cannot exit the role without delegitimizing the entire system. Their directionality is elevated (d ≈ 0.40-0.50) relative to pure beneficiaries. Advocacy coalitions are organized but constrained — they can mobilize but face institutional resistance. Directionality reflects this constraint (d ≈ 0.45-0.55). Non-persons/excluded beings have zero exit options and bear maximum extraction cost. Their directionality is highest (d ≈ 0.95), producing maximum f(d) and experienced extractiveness chi. The analytical observer observes from outside the boundary-placement system (analytical/analytical), experiencing neither extraction nor benefit directly but risking epistemic capture by the false mountain (naturalizing contingency). Directionality for analytical context is derived as moderate (d ≈ 0.72-0.73) reflecting the observer's structural distance from the constraint's core mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION OPPORTUNITY: The legal personhood threshold actually decomposes into at least two structurally distinct constraints with different ε values: (1) The logical necessity of SOME threshold (ε ≈ 0.05-0.08, approaching mountain if the threshold exists only as an abstract logical boundary), and (2) The contingent institutional placement of THIS specific threshold at the human/non-human boundary with enforcement mechanisms that prevent boundary movement (ε = 0.58, tangled rope). The current story addresses (2) — the institutional placement and enforcement. The false summit mountain classification from the analytical perspective attempts to conflate (1) and (2), arguing that any threshold is as extractive as the current one. But these are different constraints. If one wrote separate stories, the logical necessity would be nearly mountain (low extraction, high accessibility collapse — any viable legal system must have some boundary), while the placement would remain tangled rope (contested, enforceable, beneficiary/victim structure). The mandatrophy is resolved by recognizing that 'the personhood threshold' is ambiguous between these two meanings, and the analytical observer's mountain classification is a false summit that conflates them. The real structure is tangled rope for the institutional placement constraint (which this story correctly addresses) and near-mountain for the logical necessity constraint (which should be a separate story if pursued). The current story shows mandatrophy resolution by clarifying that the legal establishment's extraction of interpretive authority (perspective 4) is the mechanism by which the threshold location is contested and constrained — mandatrophy would claim 'the threshold is necessary and inevitable,' but the institutional analysis shows the establishment's active enforcement of a specific threshold placement suggests alternatives are possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_placement_versus_threshold_necessity,
    'Is the constraint the logical necessity of SOME personhood threshold, or the contingent institutional placement of THIS specific threshold?',
    'Historical analysis of threshold shifts (slavery abolition, women''s suffrage, corporate personhood, animal sentience recognition); identification of patterns in threshold movement and resistance. If thresholds move in response to power dynamics rather than logical necessity, the constraint is placement not necessity.',
    'If necessity: mountain classification for all perspectives — any threshold will extract similarly. If placement: tangled_rope or snare depending on who controls the boundary — extraction is artifact of specific positioning, not logical inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_placement_versus_threshold_necessity, conceptual, 'Whether the constraint is logical necessity or contingent institutional placement').

omega_variable(
    extraction_rate_from_boundary_vs_enforcement,
    'Does the measured extractiveness (0.58) come from the boundary''s existence or from active enforcement practices that maintain the boundary against internal challenges?',
    'Measurement of enforcement cost (legal resources devoted to boundary maintenance, litigation against boundary-expansion attempts, suppression of alternative personhood frameworks). If enforcement costs exceed coordination benefits, the constraint is largely extractive theater. If enforcement is minimal, the boundary is relatively stable and extraction is low.',
    'If extraction comes from enforcement: reducing enforcement (removing suppression measures) would lower measured extractiveness. If extraction is inherent to boundary existence: even minimal enforcement leaves extractiveness high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_rate_from_boundary_vs_enforcement, empirical, 'Whether extractiveness derives from boundary existence or active enforcement').

omega_variable(
    sentience_versus_legal_category_identity_lock,
    'Is the suppression experienced by excluded beings (0.72) structural/material (they lack legal capacity regardless of cognition) or identity-locked (they are socialized to accept their exclusion as natural)?',
    'Post-inclusion behavior: when groups gain personhood status (formerly enslaved persons, women gaining voting rights, corporate personhood, animal welfare recognition), does suppression immediately drop or does internalized subordination persist? Persistence indicates identity lock component.',
    'If structural: expanding legal personhood status alone reduces suppression. If identity-locked: legal recognition without identity reframing leaves suppression partially intact. Suggests need for simultaneous epistemic reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_versus_legal_category_identity_lock, empirical, 'Whether suppression is structural or identity-locked').

omega_variable(
    coordination_function_versus_coordination_theater,
    'Does the personhood framework genuinely coordinate legal relationships, or does it primarily perform the appearance of rational legal categorization while actual power allocation proceeds through other means?',
    'Comparative analysis: in jurisdictions with multiple personhood frameworks (common law, civil law, indigenous systems, religious law), does personhood status predict actual legal outcomes? If outcomes depend more on wealth, social status, or political power than on personhood classification, the coordination is largely theatrical.',
    'If genuine coordination: the framework provides real predictability and enables legitimate commerce. If theatrical: the framework is a rationalization layer for distribution of power by other means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_versus_coordination_theater, empirical, 'Whether personhood framework genuinely coordinates or performs coordination').

omega_variable(
    threshold_convergence_across_jurisdictions,
    'Are global personhood thresholds converging toward a single standard (suggesting naturalness/inevitability) or diverging (suggesting contingency)?',
    'Longitudinal analysis of personhood rules across 50+ jurisdictions over 100+ years. Measure: (1) rate of threshold changes, (2) direction of changes (expansion vs contraction), (3) correlation between threshold changes and power dynamics, (4) persistence of non-convergence in alternative legal systems.',
    'Convergence suggests movement toward a natural law threshold. Divergence suggests thresholds are chosen and contestable. Correlation with power dynamics suggests extraction mechanism rather than logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_convergence_across_jurisdictions, empirical, 'Whether personhood thresholds are converging or diverging globally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_threshold, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legpers_tr_t0, legal_personhood_threshold, theater_ratio, 0, 0.48).
narrative_ontology:measurement(legpers_tr_t25, legal_personhood_threshold, theater_ratio, 25, 0.55).
narrative_ontology:measurement(legpers_tr_t50, legal_personhood_threshold, theater_ratio, 50, 0.65).
narrative_ontology:measurement(legpers_tr_t75, legal_personhood_threshold, theater_ratio, 75, 0.72).

% Extraction over time
narrative_ontology:measurement(legpers_be_t0, legal_personhood_threshold, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(legpers_be_t25, legal_personhood_threshold, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(legpers_be_t50, legal_personhood_threshold, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(legpers_be_t75, legal_personhood_threshold, base_extractiveness, 75, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_threshold, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_threshold, 0.12).
narrative_ontology:affects_constraint(legal_personhood_threshold, animal_sentience_recognition).
narrative_ontology:affects_constraint(legal_personhood_threshold, corporate_legal_rights_expansion).
narrative_ontology:affects_constraint(legal_personhood_threshold, environmental_entity_standing).
narrative_ontology:affects_constraint(legal_personhood_threshold, artificial_personhood_status).

% DUAL FORMULATION NOTE:
% Legal personhood threshold decomposes into two structurally distinct constraints: (1) logical necessity of some threshold (near-mountain, ε ≈ 0.07), and (2) institutional placement and enforcement of this specific threshold (tangled rope, ε = 0.58). This story addresses (2). The false summit in the analytical observer perspective arises from conflating these two claims. Write separate stories for empirical decomposition of threshold-necessity versus threshold-placement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_threshold, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
