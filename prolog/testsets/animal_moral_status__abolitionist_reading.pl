% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Moral Status: Abolitionist Reading (Property Status as Structural Violation)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the ABOLITIONIST READING of the contested
 *   animal_moral_status kernel. The abolitionist reading rejects both the
 *   property framework (which denies animals independent moral standing) and
 *   the welfare framework (which permits regulated use). Its core claim:
 *   animals are rights-bearing individuals; property status itself is the
 *   violation; all use, however 'humane,' perpetuates victimization. This
 *   reading produces a snare classification because the constraint operates
 *   through legal immunity for exploiters (property law), cultural
 *   suppression of alternatives (meat-normativity, material-tradition
 *   attachment), and selective breeding that locks animal bodies into
 *   suffering trajectories across generations. The abolitionist reading
 *   differs fundamentally from its siblings in its treatment of harm: whereas
 *   the welfare reading locates harm in unnecessary cruelty (remediable
 *   through regulation), and the property reading denies harm entirely (by
 *   definitional exclusion of animals from moral status), the abolitionist
 *   reading locates harm in the property status itself. Using an animal,
 *   however painlessly, violates its standing as a rights-bearing individual.
 *   This produces a victim set of unprecedented size: all domesticated and
 *   captive animals, plus wild animals subject to human extraction (habitat
 *   destruction, culling, control). The constraint's extractiveness (0.78)
 *   and suppression (0.82) have risen across the measurement interval
 *   (1800–2026) because selective breeding intensified extraction (modern
 *   broiler chickens cannot walk; dairy cows produce unsustainable milk
 *   volumes) while the welfare framework paradoxically increased suppression
 *   of abolitionist alternatives by creating the illusion of resolved
 *   tension.
 *
 * KEY AGENTS:
 *   - Domesticated and Captive Animals: Primary victim (powerless/trapped) — locked into property status by law and breeding; no exit options; experience maximum extraction and suppression
 *   - Animal Generations (across time): Secondary victim (powerless/trapped/generational) — selective breeding practices lock future animal bodies into suffering trajectories; no agency across temporal dimension
 *   - Abolitionist Coalition: Organized victims (organized/constrained) — advocacy groups, sanctuary networks, vegan movements; face suppression through law, culture, and economic pressure; have some agency and mobility to exit dominant institutions
 *   - Regulatory Authorities (USDA, EFSA, national ag ministries): Institutional actors (institutional/constrained) — manage welfare regulation; experience constraint as tangled coordination (genuine minimum-standards function) and extraction (legitimizing property status); benefit from illusion of resolution
 *   - Animal Agriculture Industry: Primary beneficiary (institutional/arbitrage) — captures all surplus from property framework; experiences constraint as pure coordination; has maximum arbitrage options (species substitution, geographic arbitrage, product substitution); sees itself as solving subsistence problem
 *   - Analytical Ethics Establishment: Attempted status-neutral observer (analytical/analytical/piton-risk) — academic discourse grants equal legitimacy to radically unequal readings; risks becoming complicit through performative openness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.78).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.82).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Moral Status: Abolitionist Reading (Property Status as Structural Violation)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0').
narrative_ontology:cs_kernel_codification('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', distributed).
narrative_ontology:cs_authority_grounding('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', distributed).
narrative_ontology:cs_reading_relation('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', foundational, sentient_beings_as_moral_patients).
narrative_ontology:cs_axiom_status(sentient_beings_as_moral_patients, holdable).
narrative_ontology:cs_axiom_grounding('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', sentient_beings_as_moral_patients, empirically_contingent).
narrative_ontology:cs_axiom('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', foundational, property_status_incompatible_with_moral_patienthood).
narrative_ontology:cs_axiom_status(property_status_incompatible_with_moral_patienthood, holdable).
narrative_ontology:cs_axiom_grounding('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', property_status_incompatible_with_moral_patienthood, deontological).
narrative_ontology:cs_reference_frame('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', universal_animal_abolition).
narrative_ontology:cs_drift_state('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', contemporary_2026, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7af5a70c-4b7b-4b81-bc58-71d0ecfb77a0', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, domesticated_and_captive_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, wild_animals_subject_to_human_extraction).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, future_animal_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTICATED ANIMAL SUBJECT (SNARE) — Trapped by selective breeding, habitat destruction, and legal property status. No exit options. All use-modalities (food, fiber, labor, research, entertainment) extract maximum value from this agent with zero veto rights. The 'humane' framing obscures the underlying constraint: property status itself is the violation. Suppression is maximal — alternatives (plant-based foods, synthetic materials, computational modeling) exist but are actively suppressed through subsidy, cultural norms, and legal immunity for exploiters.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL GENERATIONS ACROSS TIME (SNARE) — The constraint's temporal dimension. Selective breeding locks future animal bodies into existence-trajectories of suffering (dairy cows bred for impossible milk yields, broiler chickens for leg failure, egg-layers for osteoporosis). No exit for descendants. Extraction runs across generations. Suppression includes erasure of these costs from public consciousness and legal structures that treat breeding decisions as property-owner prerogatives.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ABOLITIONIST COALITION (SNARE) — Organized agents (animal rights groups, sanctuary networks, vegan advocacy) face the constraint from the opposite direction: their advocacy for animal personhood status and cessation of all use encounters massive suppression. Career cost for scientists who study animal cognition/emotion, legal barriers to animal standing, cultural hostility, and economic pressure from agribusiness. Yet they have some agency and mobility — they can exit dominant institutions and build alternatives. Their snare experience is real but less severe than the trapped animal's experience.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY / WELFARE FRAMEWORK (TANGLED ROPE) — Government agencies tasked with animal welfare regulation (USDA, European Food Safety Authority) experience the constraint as tangled coordination and extraction. Genuine coordination function: defining minimum standards prevents a race-to-the-bottom in animal treatment. But embedded extraction: the welfare framework legitimizes the property relationship itself by establishing that 'humane exploitation' is permissible. The regulatory authority's enforcement of welfare standards simultaneously enables and masks the underlying property violation. They benefit from the illusion of resolved tension (we have rules now, the problem is solved) while the abolitionist reading says the rules are complicit.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANIMAL AGRICULTURE INDUSTRY (ROPE) — From the beneficiary's view, this is pure coordination: the property framework enables efficient allocation of animal bodies to profitable uses. The industry sees itself as solving a coordination problem (how to feed billions, provide livelihoods, maintain cultural practices) and experiences the constraint as a cooperative equilibrium, not as extraction. This perspective entirely denies that victimization is occurring — animals are resources optimally distributed. The industry has maximal arbitrage options: it can substitute species (fish for mammals if regulatory pressure mounts), substitute geographies (move production to jurisdictions with weaker animal welfare laws), or substitute products (cultured meat only when forced by market competition or regulation). From the abolitionist reading, this perspective is evidence of successful suppression: the industry's rationalization naturalizes what the reading identifies as systematic violation.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — ATTEMPTING STATUS-NEUTRAL FRAMING (PITON) — The academic ethics establishment attempts to describe animal status as an open question with multiple live readings (abolitionist, welfarist, property-based). Theater ratio 0.55: significant performative content in the 'neutral academic discourse' framing that grants equal epistemic legitimacy to readings with radically unequal empirical and normative content. The analytical position risks becoming piton-like when it sustains the illusion of genuine philosophical openness while structural power concentrates all actual policy-making in the property and welfare frameworks. True analytical status requires acknowledging that the constraint's structural reality — animal bodies are trapped, property status is enforced globally, alternatives are suppressed — is not a matter of perspective but of measurable fact.
constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_moral_status__abolitionist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, TR),
    TR >= 0.70.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and rising. The abolitionist reading defines extraction as any use of animal bodies for human benefit, regardless of method. Modern industrial animal agriculture extracts unprecedented value: meat, dairy, fiber, labor, organs for xenotransplantation, bodies for research, genetic material for pharmaceutical development. The rise from 0.62 (1800) to 0.78 (2026) reflects intensification of extraction through selective breeding (animals bred for faster growth, higher productivity, or docility) and technological increase in extraction efficiency (factory farming enabling 70+ billion animals/year into industrial systems). The reading accepts no offset for 'humane' practices — welfare improvements are cosmetic relative to the fundamental violation of property status. Suppression (0.82): Extremely high. The suppression trajectory (0.68 → 0.82) reflects that alternatives to animal use exist but are actively suppressed through multiple mechanisms: subsidies favoring animal agriculture over plant-based alternatives, cultural norms enforcing meat-eating and animal-material consumption, legal structures granting property immunity to exploiters, erasure of animal cognition/emotion from scientific discourse, and deliberate marginalization of abolitionist frameworks in academic and policy institutions. The welfare framework paradoxically increased suppression because it legitimated property status as such by establishing 'ethical use.' Theater ratio (0.55): Moderate and rising. The 'humane' framing of animal agriculture (free-range labels, animal-welfare certifications, pastoral marketing imagery) obscures industrial reality. The rise from 0.35 to 0.55 reflects increasing theatrical sophistication of legitimation: welfare reforms produce performative improvements (larger cages, slower growth rates) that appear to resolve the ethical problem while leaving property status and systematic extraction unchanged. The theater_ratio of 0.55 (not higher) reflects that abolitionist activists successfully maintain awareness of the performative gap — large segments of the population now recognize the distinction between marketing imagery and industrial reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The domesticated animal experiences a pure snare with no alternative framing — property status is enforced by selective breeding (body design locks them in) and law (no legal standing). The abolitionist coalition sees the snare clearly and experiences suppression for advocating alternatives. The regulatory authority experiences genuine coordination (welfare standards do prevent races-to-bottom) layered over embedded extraction (welfare legitimizes property). The animal agriculture industry sees rope (pure coordination for subsistence and livelihood) and denies victimization entirely by definitional exclusion. The analytical observer risks piton status by granting equal legitimacy to all three readings when they are not empirically or normatively equivalent — the property reading contradicts empirical evidence of animal sentience, and the welfare reading internally contains logical inconsistency (granting that suffering is wrong while maintaining property status that enables systematic production of suffering). The abolitionist reading is the only one consistent with both animal cognition science and non-contradiction: if animals are sentient, if sentience grounds moral status, and if property status enables systematic extraction, then abolition follows necessarily.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis for this constraint: The domesticated animal (powerless/trapped) derives d from victim status + insurmountable barriers to exit (breeding-locked body, legal immunity for exploiters, no alternative habitat) → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. The abolitionist coalition (organized/constrained) derives d from victim-adjacent status + constrained mobility (can exit dominant institutions but face cultural/legal/economic costs) → d ≈ 0.62 → f(d) ≈ 0.85 → high but not maximal experienced extraction. The regulatory authority (institutional/constrained) derives d from mixed position (enforces standards, benefits from legitimation) → d ≈ 0.55 → f(d) ≈ 0.75 → moderate-high extraction. The animal agriculture industry (institutional/arbitrage) derives d from full beneficiary status + exit arbitrage (can relocate, substitute species, change products) → d ≈ 0.15 → f(d) ≈ -0.01 → negative/near-zero experienced extraction. The analytical observer (analytical/analytical) derives d from observer position with internal constraint (identity-locked in academic neutrality norms) → d ≈ 0.72 → f(d) ≈ 1.15 → high experienced extraction paradoxically imposed by the analytical framework itself. No directionality overrides are necessary — the derivation chain captures the actual structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's extractiveness (0.78) exceeds the mandatrophy threshold (0.70). The abolitionist reading resolves mandatrophy by rejecting the false choice between 'exploitation' and 'coordination.' The reading states: the property framework itself is the violation, not a necessary evil that can be ethically managed. This reframes the tension. If the property reading is correct (animals have no independent moral status), then animal use is coordination, not extraction — a different constraint (the property_reading) with different victims. If the welfare reading is correct (animals are sentient beings whose suffering should be minimized), then the reading faces mandatrophy: the welfare framework permits regulated use while sentience grounds moral status. The abolitionist reading eliminates mandatrophy by denying that regulated use resolves the tension — property status is the unresolved violation. This produces a snare (high extraction with minimal coordination function) rather than tangled_rope, because the abolitionist reading rejects the premise that any coordination benefit could justify property status for moral agents. The resolution is STRUCTURAL NOT EMPIRICAL: it does not depend on facts about animal cognition or subsistence feasibility (though those facts support it). It depends on accepting that moral status and property status are incompatible. Once that premise is accepted, mandatrophy vanishes — the constraint is pure snare, extraction runs one direction only, and abolition is not a compromise but the only coherent position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_threshold_for_rights,
    'Does the moral status boundary run at sentience/capacity-for-suffering (which the abolitionist reading assumes) or at some other criterion (rationality, autonomy, species membership, relational interdependence)?',
    'Comparative neurobiology across species; empirical testing of pain/affect/preference hierarchies; cross-cultural ethical frameworks mapping moral boundaries',
    'If sentience suffices: abolitionist reading stands. If higher threshold required: abolitionist reading''s victim set shrinks (some animals fall below the boundary). If relational interdependence is the criterion: wild animals outside human dominion may not be victims, narrowing scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sentience_threshold_for_rights, empirical, 'Criterion for animal moral status and victim identification').

omega_variable(
    property_status_contingency,
    'Is property status (legal assignment of animals as objects of ownership) a contingent institutional choice or a structural necessity grounded in biological/economic reality?',
    'Historical analysis of alternative institutional arrangements (hunting commons, sacred animal protections, legal personhood regimes like some indigenous frameworks); feasibility studies of large-scale non-property animal care; economic modeling of animal agriculture without property assignment',
    'If contingent: abolitionist reading''s core claim (property status is the violation, can be abolished) is structurally viable. If structural: snare status is permanent and abolition is unachievable fantasy — constraint reclassifies to eternal entrapment. ε remains high but interpretability shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, empirical, 'Whether property status is contingent institutional choice or structural necessity').

omega_variable(
    alternative_subsistence_feasibility,
    'Can human populations at current scale be sustained without systematic animal use (food, fiber, labor, research)?',
    'Large-scale transition modeling (nutritional adequacy of plant-based diets scaled globally); technological feasibility of cultured meat, synthetic materials, computational alternatives to animal research; cost and timeline analysis for infrastructure transition',
    'If fully feasible: suppression metric must increase — alternatives exist but are suppressed by policy/culture/capital. If infeasible at current scale: constraint''s suppression reflects hard structural constraint rather than political choice. If feasible but only at lower population/consumption levels: constraint becomes coupled to Malthusian limits and overpopulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_subsistence_feasibility, empirical, 'Feasibility of large-scale human subsistence without animal use').

omega_variable(
    kernel_reading_contest,
    'Which reading of the animal_moral_status kernel does THIS constraint instantiate, and what are the structural implications of the sibling readings?',
    'Explicit declaration in cs_structure.reading_relations and cs_structure.axioms. The abolitionist reading forecloses the property reading (incompatible core premises) and coexists with the welfare reading (different parties hold both simultaneously, no logical resolution).',
    'The contest itself is part of the constraint''s structure. The suppression metric (0.82) reflects that abolitionist framing is actively marginalized in policy and practice — the welfare reading has captured institutional authority despite logical inconsistency with abolitionist premises. Engine will detect false summit if any reading naturalizes property status as inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest structure and reading relationship topology').

omega_variable(
    temporal_scope_of_victimization,
    'Does victimization extend only to animals currently alive and trapped, or backward to animals who suffered under domestication in the past, or forward to animal descendants locked into selective breeding pathways?',
    'Metaphysical analysis of causation and responsibility (can we harm the dead? can we wrong potential future beings?); empirical data on heritability of suffering-inducing traits; cultural analysis of practices like memorial advocacy for ''ancestor animals''',
    'If temporal scope extends backward/forward: victim count increases dramatically, extractiveness and suppression metrics rise. If present-only: victim count shrinks but snare status remains. Scope affects whether abolition is seen as justice for past wrongs or merely prevention of future wrongs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_scope_of_victimization, conceptual, 'Temporal boundary of victimization').

omega_variable(
    suppression_mechanism_embedding,
    'Is suppression of abolitionist reading primarily institutional (law, policy, economic incentives) or cognitive (cultural norms, identity investment in animal-use practices, epistemic closure in animal science)?',
    'Historical policy analysis (tracking where animal-rights legislation is blocked and by whom); cognitive science research on moral disengagement mechanisms; ethnographic studies of belief-formation about animal use in different communities',
    'If primarily institutional: suppression can be reduced by legal/policy change (clear activism target). If primarily cognitive: suppression is internalized in identity and worldview (much harder to shift). Mixed mechanisms require different intervention strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_embedding, empirical, 'Institutional vs. cognitive suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ams_abolitionist_theater_1800, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ams_abolitionist_theater_1920, animal_moral_status__abolitionist_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ams_abolitionist_theater_2026, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ams_abolitionist_extractiveness_1800, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(ams_abolitionist_extractiveness_1920, animal_moral_status__abolitionist_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(ams_abolitionist_extractiveness_2026, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ams_abolitionist_suppression_1800, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(ams_abolitionist_suppression_1920, animal_moral_status__abolitionist_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(ams_abolitionist_suppression_2026, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel generates three distinct constraint stories: property_reading (ε≈0.15, Mountain — naturalizes animal status as resource), welfare_reading (ε≈0.48, Tangled_Rope — permits regulated use while granting sentience), and abolitionist_reading (ε≈0.78, Snare — prohibits all use because property status violates rights). Each reading has its own beneficiary/victim structure, its own perspectives, and its own measurement trajectory. The network links them as siblings in kernel contest, not as observational variants of a single constraint. The abolitionist reading's high extractiveness reflects not empirical measurement change but the reading's own structural claim: extraction is systematically underestimated when property status is naturalized (property reading) or partial abolition is called progress (welfare reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
