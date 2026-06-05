% ============================================================================
% CONSTRAINT STORY: civic_eugenic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civic_eugenic_reading, []).

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
 *   constraint_id: civic_eugenic_reading
 *   human_readable: Personhood Contingent on Civic Viability (Eugenic Reading)
 *   domain: moral_philosophy/commitment_systems/personhood_boundary
 *
 * SUMMARY:
 *   This constraint models one reading of the personhood boundary kernel:
 *   that personhood is contingent on state assessment of civic viability, and
 *   that the state retains legitimate authority to exclude infants deemed
 *   unviable (due to disability, disease burden, or lack of military/economic
 *   utility) from the moral community. This reading instantiates the civic
 *   eugenic tradition, wherein the state's survival interest justifies active
 *   infanticide or exposure of populations deemed unsuitable. The constraint
 *   exhibits high extraction (0.68) with moderate theater (0.55), and
 *   classifies as Snare from the powerless agent perspectives and natural-law
 *   False Summit from the analytical perspective. The structural data reveals
 *   this as a naturalized extraction mechanism: the beneficiary class
 *   (warrior/administrative) experiences it as legitimate coordination, while
 *   the victim class (rejected infants and their families) experiences it as
 *   pure coercion with no appeal. The theater ratio reflects that much of the
 *   legitimation narrative is divorced from demonstrated efficacy of
 *   viability assessment — the constraint persists through appeal to state
 *   necessity rather than through validated outcomes.
 *
 * KEY AGENTS:
 *   - Rejected Infants: Primary victims (powerless/trapped) — excluded from moral community via state viability determination; face exposure or institutionalized death
 *   - Parents of Rejected Infants: Secondary victims (moderate/trapped) — stripped of parental authority and family integrity; legally compelled to relinquish children deemed unviable
 *   - Warrior Class: Primary beneficiary (institutional/arbitrage) — benefits from population filtering that concentrates military-suitable cohorts; experiences constraint as legitimate population optimization
 *   - State Administrative Apparatus: Enforcer and secondary beneficiary (institutional/constrained) — benefits from centralized control over personhood definition; bears enforcement and infrastructure costs
 *   - Medical/Philosophical Authority: Legitimation provider (institutional/arbitrage) — authorizes viability criteria and assessment mechanisms; maintains interpretive authority over what constitutes 'viable'
 *   - Sibling Reading Communities: Competing moral frameworks (organized/mobile) — inherent dignity reading and birth threshold reading offer alternative personhood boundaries; coexist or foreclose depending on logical analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civic_eugenic_reading, 0.68).
domain_priors:suppression_score(civic_eugenic_reading, 0.78).
domain_priors:theater_ratio(civic_eugenic_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civic_eugenic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(civic_eugenic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(civic_eugenic_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civic_eugenic_reading, snare).
narrative_ontology:human_readable(civic_eugenic_reading, "Personhood Contingent on Civic Viability (Eugenic Reading)").
narrative_ontology:topic_domain(civic_eugenic_reading, "moral_philosophy/commitment_systems/personhood_boundary").

domain_priors:requires_active_enforcement(civic_eugenic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(civic_eugenic_reading, '977979c3-f37e-40b5-bbf2-78d6140d2816').
narrative_ontology:cs_created_at('977979c3-f37e-40b5-bbf2-78d6140d2816', '').
narrative_ontology:cs_kernel_codification('977979c3-f37e-40b5-bbf2-78d6140d2816', fixed_text).
narrative_ontology:cs_authority_grounding('977979c3-f37e-40b5-bbf2-78d6140d2816', lineage).
narrative_ontology:cs_interpretation_layer_present('977979c3-f37e-40b5-bbf2-78d6140d2816').
narrative_ontology:cs_kernel_id(civic_eugenic_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('977979c3-f37e-40b5-bbf2-78d6140d2816', birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('977979c3-f37e-40b5-bbf2-78d6140d2816', inherent_dignity_reading, forecloses).
narrative_ontology:cs_axiom('977979c3-f37e-40b5-bbf2-78d6140d2816', foundational, personhood_contingent_on_civic_viability).
narrative_ontology:cs_axiom_status(personhood_contingent_on_civic_viability, holdable).
narrative_ontology:cs_axiom_grounding('977979c3-f37e-40b5-bbf2-78d6140d2816', personhood_contingent_on_civic_viability, instrumental).
narrative_ontology:cs_axiom('977979c3-f37e-40b5-bbf2-78d6140d2816', foundational, state_authority_determines_moral_community).
narrative_ontology:cs_axiom_status(state_authority_determines_moral_community, holdable).
narrative_ontology:cs_axiom_grounding('977979c3-f37e-40b5-bbf2-78d6140d2816', state_authority_determines_moral_community, deontological).
narrative_ontology:cs_reference_frame('977979c3-f37e-40b5-bbf2-78d6140d2816', classical_state_viability_framework).
narrative_ontology:cs_drift_state('977979c3-f37e-40b5-bbf2-78d6140d2816', contemporary_human_rights_era, gap(authority_erosion, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civic_eugenic_reading, warrior_class).
narrative_ontology:constraint_beneficiary(civic_eugenic_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(civic_eugenic_reading, disabled_infants).
narrative_ontology:constraint_victim(civic_eugenic_reading, infants_deemed_economically_unviable).
narrative_ontology:constraint_victim(civic_eugenic_reading, families_of_rejected_infants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REJECTED INFANT (SNARE) — Zero agency. State viability assessment determines exclusion from the moral community with no appeal. Death by exposure is the institutionalized outcome for those deemed unviable. Maximum suppression: no alternative exists; no self-advocacy is possible. Pure extraction with zero coordination function — the constraint solely serves to concentrate viable personhood in the state's selected population.
constraint_indexing:constraint_classification(civic_eugenic_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PARENTS OF REJECTED INFANT (SNARE) — Trapped within the state's assessment framework. Parents lack both legal standing to contest the viability determination and social protection to keep a rejected child. The constraint extracts their child from the moral community; family bonds are rendered legally meaningless. High suppression: law enforcement may separate and expose the infant against parental will. No coordination function — pure extraction of reproductive choice and family integrity.
constraint_indexing:constraint_classification(civic_eugenic_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: WARRIOR CLASS (ROPE) — Primary institutional beneficiary. The viability assessment concentrates personhood within the population deemed militarily and economically productive. This perspective experiences the constraint as coordinating population composition for state strength. They see a legitimate coordination mechanism: filtering for the viable produces a robust, standardized population capable of military service and civic participation. Low experienced extraction because they benefit and have arbitrage options (can migrate to other warrior-class populations or leverage their status).
constraint_indexing:constraint_classification(civic_eugenic_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (TANGLED ROPE) — Dual role as enforcer and beneficiary. The state apparatus benefits from centralized control over personhood definition (coordination of status and exclusion), but also bears enforcement costs: the viability assessment requires institutional infrastructure, medical evaluation capacity, and separation/exposure logistics. The constraint both coordinates state power and extracts costs. High suppression due to enforcement requirements; moderate extractiveness because state actors have constrained but non-zero exit options (can decline to participate in assessments, though at career cost).
constraint_indexing:constraint_classification(civic_eugenic_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PHILOSOPHICAL AUTHORITY / THEATER VIEW (PITON) — The civic viability framework is maintained through sustained performative appeals to state necessity and population optimization, decoupled from actual functional verification of the viability criteria. The constraint persists via institutional inertia and legitimation narrative ('the state knows viability') rather than demonstrated efficacy of the assessment mechanism. Theater ratio is moderate-high (0.55) because much of the justification is rhetorical health/strength optimization divorced from measurable outcome validation. The framework was once a live coordination mechanism (classical Athens); it degrades into inertial performance.
constraint_indexing:constraint_classification(civic_eugenic_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — This reading risks naturalizing a constructed institutional arrangement as an immutable feature of political life: 'states must control population composition to survive; viability assessment is inherent to governance.' From a civilizational perspective, this reading treats the state's authority to exclude as logically necessary given survival constraints. However, the structural data contradicts the mountain classification — beneficiary concentration and enforcement requirements reveal this as a false summit (a naturalized snare, not a natural law).
constraint_indexing:constraint_classification(civic_eugenic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civic_eugenic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civic_eugenic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civic_eugenic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civic_eugenic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civic_eugenic_reading, TR),
    TR >= 0.70.

:- end_tests(civic_eugenic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The civic eugenic reading concentrates personhood definition in state authority, denying moral status to infants deemed unviable. The extraction is severe because it is total — rejected infants have zero moral standing. However, extractiveness does not reach 0.85 because some coordination function exists (the reading does optimize for state military capacity, which is a genuine coordination problem); the extraction is not entirely parasitic. The measurement trajectory shows extractiveness rising from 0.55 to 0.68 over the interval, reflecting how viability standards expand and enforcement mechanisms become more institutionalized. Suppression (0.78): Very high. Barriers to resistance include: legal prohibition on keeping rejected infants, loss of parental authority, cultural normalization of exposure, concentration of viability assessment power in state medical authority, and lack of alternative moral frameworks accessible to rejected infants' families. The suppression is near-total because escape routes are structurally closed. Theater ratio (0.55): Moderate-high. The legitimation narrative emphasizes state survival necessity and population health, but enforcement patterns reveal that viability assessment correlates more strongly with military utility and state power preferences than with objective health outcomes. The theater has increased over the interval as the philosophical justification (appeals to natural selection, population optimization, Aristotelian natural law) has become more elaborate while the correlation with actual viability outcomes weakens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence between beneficiary and victim framings. The warrior class and administrative apparatus see Rope (legitimate coordination) or Tangled Rope (mixed coordination and extraction cost). The rejected infants and their families see pure Snare (total extraction with zero agency). The analytical observer risks seeing Mountain (treating state authority as natural law), but structural data reveals this as a false summit — the constraint requires active enforcement, benefits identifiable groups, and its 'necessity' claims are contestable. The perspectival gap is not merely observational disagreement but reflects genuine structural asymmetry: the beneficiary classes have arbitrage options and political voice, while the victim classes have no exit options and zero standing in the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) maps to their structural position: Rejected infants are complete victims with zero exit options, yielding d ≈ 1.0 and maximum f(d). Parents are victims with trapped exit but some residual agency (resistance is possible though costly), yielding d ≈ 0.95. Warrior class are beneficiaries with arbitrage options, yielding d ≈ 0.10 (low effective extraction experienced). State apparatus are mixed beneficiaries and enforcers with constrained exit, yielding d ≈ 0.35. The beneficiary/victim declarations feed the directionality chain: victims populate high-d contexts; beneficiaries populate low-d contexts. The chi formula (χ = ε × f(d) × σ(S)) scales the base extractiveness by these directionality values: for victims, chi approaches or exceeds the base extractiveness; for beneficiaries, chi can be negative (they experience subsidy). The local scope (σ = 0.8) slightly dampens chi, reflecting that this constraint's verification happens within specific city-states or polities rather than globally.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by exposing how a naturalized moral claim (state authority over personhood) conceals a pure extraction mechanism. The analytical observer's mountain classification is a false summit: the constraint requires active state enforcement, concentrates benefits in the warrior class, and suppresses alternatives. The mandatrophy is not resolved by discovering 'the true type' but by recognizing that the reading achieves its false-summit status precisely because it conflates a coordination problem (state military viability) with a moral question (who counts as a person). The coordination problem is real; the extraction is real; the naturalization is the mechanism that allows the state to enact both simultaneously while claiming only the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_criterion_contingency,
    'Is viability assessment grounded in objective, stable criteria (disease burden, disability severity, life expectancy) or in contingent state preferences (strength for warfare, economic productivity, population genetics)?',
    'Historical analysis of stated viability criteria vs. actual enforcement patterns; correlation between assessed ''unviable'' populations and those politically disfavored or militarily unproductive',
    'If objective and stable: constraint is legitimate population health policy (weaker snare classification). If contingent and state-preference-driven: constraint is pure selection mechanism for state power (stronger snare, potential reclassification to enforced extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(viability_criterion_contingency, empirical, 'Whether viability criteria are objective or state-preference driven').

omega_variable(
    axiom_reversal_foreclosure,
    'Does adoption of the ''inherent dignity'' reading logically foreclose this reading''s core axiom (that personhood is contingent on state viability assessment), or can both coexist in different institutional frameworks?',
    'Conceptual analysis: can a state simultaneously hold that infants have inherent dignity AND reserve the right to exclude infants from moral community based on viability assessment? Or are these mutually exclusive commitments?',
    'If mutually exclusive (forecloses): the inherent dignity reading logically rules out civic eugenic reading within any single framework. If coexistable (coexists_with): both readings can be held by different parties without logical contradiction, and the constraint represents factional disagreement. This determines the reading_relations configuration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_reversal_foreclosure, conceptual, 'Whether inherent dignity axiom forecloses civic contingency axiom').

omega_variable(
    naturalization_mechanism_detection,
    'What authority structure sustains the false summit mechanism (presenting constructed viability assessment as natural law)? Is naturalization maintained through interpretive reframing, suppression of counterclaims, or epistemic closure?',
    'Examination of how the reading''s authority grounds justifies naturalization: lineage-based appeals to ancestral practice, expertise-based appeals to medical authority, theological/metaphysical appeals to natural order, or distributed practice-based normalization through custom',
    'If naturalization is maintained by single interpretive authority (lineage/expertise): constraint is vulnerable to authority erosion. If maintained by distributed practice (custom, cultural normalization): constraint has higher structural resilience. This informs drift_state direction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_mechanism_detection, empirical, 'Authority mechanism sustaining false summit naturalization').

omega_variable(
    moral_community_boundary_definition,
    'Is the ''moral community'' in this reading defined as: (a) the set of beings with inherent rights preceding state action, or (b) the set of beings the state designates as possessing rights through viability assessment?',
    'Logical analysis of the reading''s commitment: does it treat moral community as discovered by the state (a — natural law framing) or constructed by the state (b — positive law framing)? Each entails different foreclosure relationships with sibling readings.',
    'If (a) natural law: reading makes stronger forecloses claim on inherent dignity reading (the state cannot ''designate'' what naturally exists). If (b) positive law: reading coexists_with inherent dignity reading (both are positions on what grounds rights, not whether rights exist). This affects the axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_community_boundary_definition, conceptual, 'Whether moral community is discovered or state-constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civic_eugenic_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civic_eugenic_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(civi_tr_t2, civic_eugenic_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(civi_tr_t4, civic_eugenic_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civic_eugenic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(civi_be_t2, civic_eugenic_reading, base_extractiveness, 2, 0.61).
narrative_ontology:measurement(civi_be_t4, civic_eugenic_reading, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civic_eugenic_reading, identity_coordination).
narrative_ontology:affects_constraint(civic_eugenic_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(civic_eugenic_reading, inherent_dignity_reading).

% DUAL FORMULATION NOTE:
% The civic eugenic reading is one vertex of a constraint family decomposing the personhood_boundary kernel. Each sibling reading (birth threshold, inherent dignity) instantiates a different answer to 'what grounds personhood membership.' The ε values differ: civic eugenic reading ε=0.68 (snare, state-contingent), birth threshold reading ε≈0.12 (rope, biological coordination), inherent dignity reading ε≈0.05 (mountain-candidate or rope, intrinsic status). Each reading has distinct beneficiaries, victims, and naturaliz mechanisms. The network links establish that all three are readings of ONE kernel (personhood boundary) not separate domains. Choosing among readings is not an observational question but a normative/framework question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
