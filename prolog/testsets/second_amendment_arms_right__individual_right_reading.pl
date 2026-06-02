% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment as Individual Right to Bear Arms (Pre-Governmental Liberty)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   The Second Amendment's individual-right reading instantiates one
 *   interpretation of a contested kernel: 'A well regulated Militia, being
 *   necessary to the security of a free State, the right of the people to
 *   keep and bear Arms, shall not be infringed.' This story generates the
 *   constraint as understood by those who read this text as grounding a
 *   pre-governmental individual right to possess and carry arms, prior to and
 *   independent of government authorization. The constraint's structure
 *   exhibits an irreducible tension: it grants an individual liberty
 *   (coordinate action by dispersed gun owners and militia organizations)
 *   while simultaneously extracting from regulatory authority and
 *   public-health coordination capacity. From different structural positions,
 *   the same textual commitment appears as rope (individual beneficiary
 *   perspective), snare (diffuse public-health perspective), mountain
 *   (natural-law interpretation that risks false summitry), and tangled rope
 *   (for organized advocates, regulatory bodies, and public-health actors who
 *   experience both coordination benefits and asymmetric constraints).
 *
 * KEY AGENTS:
 *   - Individual Gun Owners: Primary beneficiary (institutional/arbitrage) — the reading grants them a pre-governmental right to arms possession and carry; experience minimal suppression and high exit options
 *   - Gun Rights Advocacy Organizations: Organized beneficiary-adjacent (organized/constrained) — benefit from constitutional frame but face suppression through litigation and periodic regulatory threats
 *   - Federal and State Regulatory Authorities: Primary constrained party (institutional/constrained) — the reading limits their capacity to regulate arms; face high suppression (foreclosed regulatory pathways)
 *   - Public-Health and Gun Violence Prevention Advocates: Secondary constrained party (moderate/constrained) — experience extraction of policy options; suppression through constitutional barriers to comprehensive measures
 *   - Non-Gun-Owning Public: Diffuse victim (powerless/trapped) — face externalized public-health and safety costs; no organized voice; cannot exit the nation-state
 *   - The Judiciary: Institutional custodian (institutional/arbitrage) — performs adherence to the reading while managing doctrinal complexity through scrutiny tiers and categorical carve-outs; piton classification reflects performative aspect
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the reading as immutable when it is historically contingent and interpretively constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment as Individual Right to Bear Arms (Pre-Governmental Liberty)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '0f0688a7-bc90-4bd9-858c-20a31af4850b').
narrative_ontology:cs_kernel_codification('0f0688a7-bc90-4bd9-858c-20a31af4850b', fixed_text).
narrative_ontology:cs_authority_grounding('0f0688a7-bc90-4bd9-858c-20a31af4850b', lineage).
narrative_ontology:cs_interpretation_layer_present('0f0688a7-bc90-4bd9-858c-20a31af4850b').
narrative_ontology:cs_reading_relation('0f0688a7-bc90-4bd9-858c-20a31af4850b', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('0f0688a7-bc90-4bd9-858c-20a31af4850b', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('0f0688a7-bc90-4bd9-858c-20a31af4850b', foundational, right_to_arms_pre_governmental).
narrative_ontology:cs_axiom_status(right_to_arms_pre_governmental, holdable).
narrative_ontology:cs_axiom_grounding('0f0688a7-bc90-4bd9-858c-20a31af4850b', right_to_arms_pre_governmental, deontological).
narrative_ontology:cs_axiom('0f0688a7-bc90-4bd9-858c-20a31af4850b', foundational, individual_liberty_prior_to_state_authorization).
narrative_ontology:cs_axiom_status(individual_liberty_prior_to_state_authorization, holdable).
narrative_ontology:cs_axiom_grounding('0f0688a7-bc90-4bd9-858c-20a31af4850b', individual_liberty_prior_to_state_authorization, deontological).
narrative_ontology:cs_reference_frame('0f0688a7-bc90-4bd9-858c-20a31af4850b', natural_rights_individual_liberty).
narrative_ontology:cs_drift_state('0f0688a7-bc90-4bd9-858c-20a31af4850b', contemporary_post_heller, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f0688a7-bc90-4bd9-858c-20a31af4850b', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, private_militia_advocates).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, regulatory_authority_capacity).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, public_safety_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGULATED POPULATION (SNARE) — Citizens subject to diffuse firearm risk have no exit option and no organized voice. Cannot exit the nation-state. The constraint extracts public-health coordination capacity in exchange for privileging an individual pre-governmental right. No beneficiary from this perspective; pure extraction of the collective good (coordinated safety norms) in service of individual liberty.
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL GUN OWNER (ROPE) — Experiences the constraint as a coordination mechanism for property ownership and self-defense capability. This reading grants an individual pre-governmental right, which from the beneficiary's perspective appears as natural coordination: affirming what was already true before government existed. Minimal suppression experienced by beneficiaries; arbitrage options available (purchase, training, advocacy).
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: GUN RIGHTS ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents (NRA, gun clubs, state militia advocates) experience both coordination and extraction. The reading provides them a constitutional frame for organizing political power (coordination). But they also face suppression in the form of regulations, litigation costs, and periodic legislative threats (constrained exit). The extraction is asymmetric: regulatory burden falls primarily on the advocacy organizations and their members, while the public-health cost is externalized.
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — The federal and state governments face a mixed constraint. The reading limits their capacity to regulate arms in the name of public safety (extraction of regulatory authority, constrained exit from the constitutional commitment). But they also benefit from the coordination frame: the individual-right reading provides legal clarity and reduces litigation uncertainty by settling a core question. Suppression is high because the reading forecloses entire regulatory pathways (assault weapon bans, background-check universality, licensing schemes).
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUDICIARY (PITON) — From the view of the judiciary as guardian of constitutional meaning, this reading has become increasingly performative over the 20th–21st centuries. For much of the 20th century, the collective militia reading dominated jurisprudence (DC v. Heller in 2008 marked the inversion). The judiciary now performs adherence to the individual-right reading while simultaneously engaging in substantial doctrinal complexity (strict scrutiny, intermediate scrutiny, categorical carve-outs) that obscures whether the reading actually governs outcomes. Theater ratio is moderate (0.48) because the doctrine is not purely theatrical — real constraints operate — but the gap between the reading's plain language and the judicial doctrine is substantial.
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the perspective that the right to arms is a self-evident pre-governmental liberty (grounded in natural law or universal human rights), the constraint appears immutable. The right pre-exists government; government cannot legitimately remove what it did not grant. This perspective naturalizes the individual-right reading as an unchangeable fact about human nature or rights logic. However, the structural data contradicts the mountain classification — the constraint's history shows it has been read differently (collective militia interpretation dominated for 200+ years), benefits identifiable parties (gun owners, advocacy organizations), and is enforced through active legal doctrine development. The engine's false summit detector will identify this as naturalization of a contingent institutional reading.
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PUBLIC HEALTH / GUN VIOLENCE PREVENTION ADVOCATES (TANGLED ROPE) — Face a constrained exit from the constitutional commitment to individual arms rights. They benefit from some coordination (clarity that the right exists provides a legal baseline for negotiation). But they experience asymmetric extraction: the constraint severely limits their policy options, and regulatory measures are perpetually vulnerable to constitutional challenge. Suppression is high; exit options are constrained to litigating case-by-case boundaries rather than enacting comprehensive public-health measures.
constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_arms_right__individual_right_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The individual-right reading grants an individual liberty that extracts regulatory capacity from the state and public-health coordination potential from the diffuse public. The constraint is not extraction of wealth or direct coercion (like a snare) but extraction of institutional authority and collective-action capacity in exchange for clarifying an individual pre-governmental right. The trajectory (0.35 → 0.48 → 0.58 over time intervals representing rough 33-year generational periods) shows accumulating extractiveness: as the reading has become institutionally entrenched post-Heller (2008), its effect on regulatory capacity has increased. Suppression (0.62): High. The reading forecloses entire regulatory pathways (universal background checks, assault weapon bans, licensing schemes, red-flag laws) and imposes ongoing litigation costs on regulatory bodies. Suppression is structural, not primarily from diffuse non-compliance but from constitutional doctrine preventing regulation. Theater ratio (0.48): Moderate. The doctrine has moderate theatrical content: post-Heller jurisprudence employs multiple scrutiny tiers (strict, intermediate, categorical), carve-outs (felon prohibition, school zones), and fact-specific balancing that obscures whether the plain reading actually governs outcomes. The theater is lower than in many constitutional contexts because the reading has genuine bite (regulations are struck down) and the plain language is relatively clear. Theater has increased slightly over time (0.32 → 0.48) as courts have developed doctrinal complexity to manage tensions the reading creates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The individual beneficiary sees coordination (Rope): the reading affirms what was already true — their pre-governmental right to self-defense and property ownership. The gun rights organization sees mixed coordination and constraint (Tangled Rope): the reading provides a legal frame for political organization but faces ongoing suppression. The regulatory authority sees extraction of authority (Tangled Rope): constrained exit from a constitutional commitment that limits policy options. The public-health advocate sees extraction of policy capacity (Tangled Rope): forced to work within narrow constitutional corridors. The diffuse non-gun-owning public sees pure extraction (Snare): no exit, no benefit, externalized costs. The judiciary sees a performative ritual (Piton): ostensible adherence to a clear reading while managing doctrinal complexity that reduces transparency. The natural-law observer risks seeing an immutable law (Mountain) — the pre-governmental right as grounded in nature itself — but this classification fails the false-summit test: the reading has a 200-year history of being read differently (collective militia interpretation), benefits identifiable parties, and is actively enforced through doctrine development. The perspectival gap reveals that the constraint is not about discovering a pre-existing right but about *enforcing one interpretation* of an ambiguous text against competing readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The individual-right reading produces asymmetric directionality across agents. Individual gun owners (beneficiary + arbitrage) experience very low or negative effective extraction — the reading affirms their property rights and freedom of action. Advocacy organizations (beneficiary + constrained) experience moderate extraction — the reading provides coordination capacity (advocacy frame) but faces regulatory suppression. Regulatory authorities (victim + constrained) experience high extraction — the reading removes policy tools they would otherwise possess. Public-health advocates (victim + constrained) experience high extraction — their preferred policy options are foreclosed. The non-gun-owning diffuse public (victim + trapped) experiences maximum extraction — they have no exit option and no organized advocacy capacity; the public-health cost is externalized. The judiciary (institutional/arbitrage) manages directionality through doctrinal complexity — the appearance of regulated discretion masks the fact that the reading substantially constrains regulatory outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the individual-right reading is genuinely a mixed coordination-extraction hybrid (Tangled Rope). It coordinates individual liberty and gun-owner preference aggregation (genuine coordination function). It simultaneously extracts regulatory capacity and public-health coordination potential (genuine asymmetric extraction). The constraint cannot be classified as pure coordination (Rope) because the regulatory suppression and asymmetric burden distribution are real and structural, not incidental. It cannot be classified as pure extraction (Snare) because gun owners genuinely benefit from clarified rights and reduced uncertainty — this is coordination, not coercion. The Tangled Rope classification holds across multiple perspectives (advocacy organizations, regulatory authorities, public-health advocates all see Tangled Rope). The false-summit risk (Mountain classification from the natural-law observer) is real and diagnostically significant: it reveals how the reading naturalizes what is actually a contingent institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_governmental_right_grounding,
    'What grounds the claim that the right to bear arms pre-exists government — natural law, historical practice, or interpretation of English common law tradition?',
    'Historical-philosophical analysis: examination of founding-era documents, natural-rights theory, English common-law sources, and indigenous weapon practices to determine whether the right is grounded in nature or constructed through legal interpretation.',
    'If grounded in nature/natural law: mountain classification strengthened (immutable). If grounded in interpretation/tradition: tangled_rope classification confirmed (contingent, enforced, subject to reinterpretation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_governmental_right_grounding, conceptual, 'Grounding of pre-governmental right claim').

omega_variable(
    individual_vs_collective_logical_relationship,
    'Does the individual-right reading logically foreclose the collective militia reading, or can both coexist within a single constitutional framework?',
    'Close textual analysis of the Second Amendment and jurisprudential development post-Heller. Examine whether courts must choose one reading or can maintain both as alternative hermeneutic paths.',
    'If forecloses: the readings have a foreclosure relation in cs_structure. If coexists: the readings are live alternatives held by different interpretive communities (coexists_with relation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_vs_collective_logical_relationship, conceptual, 'Logical relationship between individual and collective interpretations').

omega_variable(
    extractiveness_of_regulatory_constraint,
    'How much of the measured extractiveness (0.58) derives from the individual-right reading itself versus from the constitutional supremacy doctrine that makes the reading enforceable against state/federal authority?',
    'Decomposition analysis: isolate the extractiveness contributed by (a) the reading''s core claim (pre-governmental right) versus (b) the enforcement mechanism (individual can sue to block regulations). If they are separable, consider whether two constraint stories are needed.',
    'If they are separable and contribute differently to ε: the constraint may decompose into two stories (the reading itself with lower ε; the enforcement mechanism with higher ε). If inseparable: current single-story approach confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_of_regulatory_constraint, empirical, 'Decomposition of extractiveness sources').

omega_variable(
    heller_watershed_magnitude,
    'Did the 2008 Heller decision represent a genuine inversion of the reading''s dominance, or had the individual-right interpretation been increasingly influential in academic and political discourse for decades prior?',
    'Historical mapping of jurisprudential and scholarly consensus from 1939 (US v. Miller) to 2008. Examine law review articles, judicial opinions, and political rhetoric to determine when the individual-right reading crossed from minority to majority position.',
    'If sharp inversion (Heller was watershed): the reading is a recent construction with contingent institutional backing (supports piton and false-summit perspectives). If gradual drift: the reading has deeper institutional roots, and the mountain perspective gains plausibility (though still false summit due to structural data).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heller_watershed_magnitude, empirical, 'Historical trajectory of individual-right reading dominance').

omega_variable(
    suppression_mechanism_intentionality,
    'Is the measured suppression (0.62) intentional — regulations explicitly designed to restrict arms ownership — or derivative of regulations with other primary purposes (licensing, background checks designed for conviction detection)?',
    'Legislative history analysis: examine statutory language, legislative records, and regulatory intent to determine which restrictions target arms ownership directly versus treating it as a secondary effect.',
    'If primarily intentional: the suppression is active enforcement against the constraint (high extractiveness confirmed). If primarily derivative: the apparent suppression may reflect the constraint''s secondary effects rather than primary design, and extractiveness should be recalibrated downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intentionality, empirical, 'Intentionality of regulatory suppression').

omega_variable(
    beneficial_side_effects_of_constraint,
    'Does the individual-right reading provide benefits to non-beneficiary groups (e.g., clarity for manufacturers, investment certainty, reduced litigation complexity)? If so, should those groups be added as secondary beneficiaries?',
    'Structural analysis of who benefits from the clarity and predictability of a settled individual-right reading versus a contested reading. Map secondary benefits to specific actor groups.',
    'If significant secondary benefits exist: directionality for some institutional actors may be misclassified (should shift from victim to mixed status). If minimal: current beneficiary set is adequate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficial_side_effects_of_constraint, empirical, 'Secondary benefits of individual-right reading clarity').

omega_variable(
    natural_law_false_summit_candidate,
    'Is this a genuine natural law (pre-governmental right immutable across human history) or a contingent institutional reading that has been naturalized by legal doctrine and advocacy?',
    'Comparative historical and cross-cultural analysis: examine whether pre-governmental rights to bear arms appear across non-Western, non-Anglo-Saxon legal traditions, or whether the concept is specific to European natural-rights philosophy and its American reception.',
    'If pre-governmental right is universal: mountain classification holds (natural law). If culture-specific: false summit confirmed — the reading is a contingent institutional arrangement grounded in one philosophical tradition, not an immutable law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_candidate, conceptual, 'Whether pre-governmental right is universal natural law or contingent tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_indiv_theater_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sa_indiv_theater_t50, second_amendment_arms_right__individual_right_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(sa_indiv_theater_t100, second_amendment_arms_right__individual_right_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(sa_indiv_extract_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_indiv_extract_t50, second_amendment_arms_right__individual_right_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(sa_indiv_extract_t100, second_amendment_arms_right__individual_right_reading, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel generates three constraint stories with different ε values and structural properties. The individual-right reading (this story, ε=0.58) represents the reading currently dominant in federal jurisprudence. The collective-right reading (sibling, ε≈0.25, Mountain) represents the interpretation that dominated 20th-century doctrine pre-Heller. The civic-republican reading (sibling, ε≈0.42, Tangled Rope) represents a reading grounded in communal defense rather than individual pre-governmental right. All three are readings of the same kernel but constitute structurally distinct constraints. The network links represent the historical and logical relationships between readings: the individual-right reading has been influencing (and gradually foreclosing) the collective-right reading since the 1990s scholarly revival and Heller inversion; the civic-republican reading influences both but coexists as a live alternative in academic discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__individual_right_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
