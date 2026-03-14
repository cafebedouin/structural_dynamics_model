% ============================================================================
% CONSTRAINT STORY: imperial_legitimacy_apparatus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_legitimacy_apparatus, []).

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
 *   constraint_id: imperial_legitimacy_apparatus
 *   human_readable: Imperial Legitimacy Apparatus
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The imperial legitimacy apparatus is the institutional-ideological
 *   mechanism through which empire justifies and naturalizes extraction.
 *   Rather than naked coercion alone, the apparatus constructs a framing
 *   where extraction appears inevitable, beneficial, or morally necessary:
 *   the 'civilizing mission' of the metropole, the 'natural hierarchy' of
 *   peoples, the 'development' of colonial territories, the 'order' imposed
 *   on 'chaos.' This framing operates simultaneously as coordination
 *   mechanism (enables metropolitan support for extraction) and extraction
 *   mechanism (neutralizes resistance by making it unthinkable within the
 *   dominant frame). The constraint exhibits snare characteristics from the
 *   colonized perspective: high extractiveness, high suppression (both
 *   structural and internalized through identity-locking), and existence that
 *   depends on suppressing the alternative of indigenous autonomy. The
 *   theater ratio rises over time (0.65 → 0.85) as the legitimacy apparatus
 *   becomes increasingly performative — the gap between the stated purpose
 *   (civilization, development, order) and actual function (extraction,
 *   subordination, resource monopoly) widens, requiring more elaborate
 *   institutional theater to maintain the frame. The extractiveness rises
 *   more slowly (0.55 → 0.68) because the apparatus gradually faces
 *   resistance, forcing the extraction to become more explicit and thus
 *   harder to conceal under legitimacy claims.
 *
 * KEY AGENTS:
 *   - Colonized populations: Primary victims (powerless/identity_locked, powerless/trapped) — bear extraction through resource monopoly, labor capture, and cultural subordination; identity-locked into belief that metropolitan civilization is natural and superior
 *   - Indigenous institutions: Secondary victims (institutional/constrained) — formal authority structures degraded to theatrical shells while actual power transfers to metropolitan appointees
 *   - Imperial metropolitan center: Primary beneficiary (institutional/arbitrage) — captures economic surplus, labor value, geopolitical leverage; experiences apparatus as legitimate coordination mechanism
 *   - Indigenous elite collaborators: Powerful agents (powerful/constrained) — trade autonomy for relative status and commerce access; bear extraction of delegitimation but gain power over collaborator populations
 *   - Colonial administrators: Institutional beneficiaries (institutional/arbitrage) — implement apparatus through law, education, religious conversion; capture administrative rents
 *   - Post-colonial state apparatus: Institutional inheritor (institutional/constrained) — absorbs imperial legitimacy framing into post-colonial institutions, reproducing extraction patterns absent original empire
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_legitimacy_apparatus, 0.68).
domain_priors:suppression_score(imperial_legitimacy_apparatus, 0.72).
domain_priors:theater_ratio(imperial_legitimacy_apparatus, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_legitimacy_apparatus, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_legitimacy_apparatus, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_legitimacy_apparatus, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_legitimacy_apparatus, snare).
narrative_ontology:human_readable(imperial_legitimacy_apparatus, "Imperial Legitimacy Apparatus").
narrative_ontology:topic_domain(imperial_legitimacy_apparatus, "political/institutional").

domain_priors:requires_active_enforcement(imperial_legitimacy_apparatus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_legitimacy_apparatus, imperial_center).
narrative_ontology:constraint_beneficiary(imperial_legitimacy_apparatus, metropolitan_elites).
narrative_ontology:constraint_beneficiary(imperial_legitimacy_apparatus, colonial_administrators).
narrative_ontology:constraint_victim(imperial_legitimacy_apparatus, colonized_populations).
narrative_ontology:constraint_victim(imperial_legitimacy_apparatus, indigenous_institutions).
narrative_ontology:constraint_victim(imperial_legitimacy_apparatus, subaltern_political_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIZED SUBJECT (SNARE) — Structurally mobile (could physically organize resistance) but identity-locked into colonial framing where empire is natural order, indigenous authority is 'primitive,' and metropolitan culture is civilization. Exit is cognitively impossible from within the identity frame despite material possibilities. Maximum extraction with internalized suppression.
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(continental))).

% PERSPECTIVE 2: ECONOMIC EXTRACTION VICTIM (SNARE) — Structurally trapped through resource monopoly (colonial state controls trade), legal prohibition (colonial law prohibits indigenous commerce), and geographic isolation (infrastructure routes through imperial centers). Material barriers preclude exit. Experiences maximum extractiveness without cognitive filter.
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: IMPERIAL METROPOLITAN CENTER (ROPE) — Benefits from resource extraction, labor capture, and market monopoly. Experiences the legitimacy apparatus as coordination mechanism: publicizing civilization mission creates political support domestically and justifies extraction internationally. Net beneficiary with high exit options (could abandon colony but chooses extraction leverage).
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIGENOUS ELITE COLLABORATOR (TANGLED ROPE) — Powerful agents (tribal chiefs, merchants, priests) collaborate with empire for relative advantage. Extracted from: lose autonomy, delegitimized before subjects. Coordinated with: gain commerce access, security guarantees, status legitimation. High suppression of alternative (resistance to empire) offset by real benefits. Active enforcement required to maintain coalition.
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: DEGRADED INDIGENOUS INSTITUTION (PITON) — Former autonomous institutions (indigenous courts, councils, trade systems) persist as theatrical shells: they appear to function within colonial framework but actual authority has transferred to metropolitan agents. Theater ratio 0.85 reflects that institutional form survives but substantive function has atrophied. Persists through institutional inertia and legitimacy theater.
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-COLONIAL ANALYTICAL OBSERVER (SNARE) — From outside the colonial moment, the apparatus appears as pure extraction disguised as civilization mission. The legitimacy theater (legal codes, educational systems, religious conversion) is revealed as mechanism for neutralizing resistance and naturalizing extraction. The analytical view cuts through identity-locking to see structural extraction beneath ideological framing.
constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_legitimacy_apparatus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_legitimacy_apparatus, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_legitimacy_apparatus, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imperial_legitimacy_apparatus, TR),
    TR >= 0.70.

:- end_tests(imperial_legitimacy_apparatus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The apparatus extracts resource wealth, labor value, political autonomy, and cultural authority. The extractiveness is not at maximum (0.90+) because the constraint requires some genuine coordination between metropolitan and indigenous elites to function — pure extraction alone would trigger resistance faster than the apparatus can suppress it. The collaboration component (indigenous elites gaining status access) creates a true Tangled Rope layer within the broader Snare. Suppression (0.72): High. Multi-layered suppression operates structurally (resource monopoly, legal prohibition, military control) and psychologically (identity-locking through education, religious conversion, cultural prestige hierarchies). The suppression is not total because organized resistance emerges at certain thresholds. Theater ratio (0.85): Very high. The apparatus is predominantly performative: legal codes present colonial law as universal justice; educational systems present metropolitan culture as civilization; religious conversion presents Christianity as enlightenment. The functional extraction (resource monopoly) requires far less institutional elaboration than the legitimacy theater maintaining it. Over time, as resistance grows and the gap between stated purpose and actual function widens, theater ratio increases (more elaborate institutions needed to maintain a frame increasingly contradicted by reality). The rising theater ratio signals constraint degradation — movement toward Piton classification as institutional inheritance persists beyond functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The metropole sees Rope (coordination mechanism enabling legitimate expansion), while the colonized see Snare (pure extraction). The indigenous elites see Tangled Rope (mixed coordination and extraction), while the general colonized populations see Snare (no benefit, only extraction). The degraded indigenous institutions appear as Piton from inside (we persist, but our function is theatrical) and as Snare from outside (you are instruments of extraction). The analytical observer outside the colonial moment sees through the legitimacy frame to the pure extraction beneath, classifying as Snare. The post-colonial institutional inheritor may misclassify inherited colonial apparatus as Rope (coordination mechanism) rather than recognizing it as Piton (degraded extraction apparatus maintained by institutional inertia). This perspectival gap is diagnostic: if a post-colonial state experiences its own legitimacy apparatus as Rope when the analytical observer sees Snare, institutional capture through inheritance has succeeded.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from structural position: colonized populations occupy the victim-trapped position (d approaching 1.0), experiencing maximum effective extraction through the sigmoid f(d). Indigenous elites occupy beneficiary-constrained position (d around 0.40), experiencing moderate negative extraction (they benefit from collaboration) offset by subordination costs. Metropolitan center occupies beneficiary-arbitrage position (d near 0.0), experiencing maximum negative extraction (constraint extracts value toward them). The powerful collaborator perspective derives d from their hybrid position: they are partly beneficiaries (relative power gain) and partly victims (delegitimization, autonomy loss), yielding d around 0.55. The identity-locked exit option for the colonized population increases experienced suppression because the agent cannot exercise structural exit options (which exist) due to cognitive capture — the frame makes exit unthinkable. The apparatus's ability to maintain extraction depends on this cognitive lock; if the frame breaks (decolonization moment), the physical barriers alone prove insufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the apparatus is genuinely Snare from the colonized perspective (maximum extraction + maximum suppression, existence depends on suppressing indigenous autonomy) while also functioning as Tangled Rope from the collaborator perspective (mixed coordination and extraction) and Rope from the metropole perspective (pure coordination enabling expansion). The mandatrophy dissolves when the perspectival structure is made explicit: there is no universal 'true' type, only perspectival readings. However, from the external analytical position, the apparatus is structurally Snare — it exists to extract without genuine coordination benefit to the extracted-from populations. The climbing metaphor: the rope the metropole sees (coordination) is the snare the colonized see (extraction with suppressed alternatives). The theater ratio rising over time (0.65 → 0.85) indicates constraint degradation — as resistance grows and the gap between legitimacy claims and reality widens, more institutional theater is required, signaling movement toward Piton (degraded apparatus maintained by inertia rather than function). Post-colonial institutional inheritance extends the constraint beyond formal colonialism: the apparatus degrades to Piton status (legitimacy claims hollow, institutional form persists) within the post-colonial state, creating a self-reproducing extraction mechanism through institutional inheritance rather than direct metropolitan control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_depth_mechanism,
    'What proportion of colonial suppression is structural (material barriers) versus internalized (identity fusion with civilizing mission)?',
    'Post-colonial data: suppression intensity post-independence (if barriers remain but identity frame breaks, suppression should drop); intergenerational psychological studies; elite collaboration persistence (if purely structural extraction, collaboration ends with barrier removal; if identity-locked, it persists through institutional inheritance)',
    'If identity-locked dominates: constraint''s effective suppression is higher than structural metric suggests — internalized frames persist after formal colonialism ends, enabling institutional extraction through post-colonial state inheritance. If structural dominates: constraints are material and dissipate with political independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_mechanism, empirical, 'Internalization depth of colonial suppression mechanisms').

omega_variable(
    legitimacy_apparatus_functional_necessity,
    'Is the legitimacy apparatus a necessary coordination cost for empire or pure extractive theater?',
    'Comparative colonialism: empires with elaborate legitimacy apparatus (British ''civilization mission,'' Spanish ''Christianization'') versus minimal legitimacy rhetoric (extraction-focused regimes like plantation slavery). Do legitimacy-rich empires extract more efficiently (lower enforcement costs) or extract more total value (theater enables coordination for exploitation)?',
    'If necessary coordination: apparatus has rope component and should be classified Tangled Rope. If pure theater: apparatus is Snare mechanism and suppression is maximized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_apparatus_functional_necessity, empirical, 'Whether legitimacy apparatus serves genuine coordination or pure extraction function').

omega_variable(
    institutional_inheritance_entrapment,
    'Do post-colonial states inherit the legitimacy apparatus as an inertial institutional frame that reproduces extraction patterns even absent the original imperial power?',
    'Longitudinal institutional analysis: do post-colonial bureaucracies, legal systems, and educational curricula maintain imperial legitimacy framing decades after independence? If yes: constraint extends beyond formal colonialism into post-colonial state structure.',
    'If institutional inheritance strong: snare persists through Piton mechanisms — degraded imperial institutions embedded in post-colonial state create perpetual extraction. The constraint is not historically bounded to colonial period but extends into present institutional structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_inheritance_entrapment, empirical, 'Post-colonial institutional inheritance of imperial legitimacy structures').

omega_variable(
    resistance_coalition_critical_mass,
    'At what scale of organized resistance does the legitimacy apparatus lose suppressive force and transform the classification?',
    'Historical analysis of anti-colonial movements: independence movements that cracked elite collaborator coalitions, delegitimized colonial apparatus, or fractured identity-lock among colonized populations. What organizational thresholds triggered apparatus failure?',
    'If low threshold: snare can transition to organized resistance (shifting from powerless to organized agent perspective); classification becomes Tangled Rope or even shifts to organized agent seeing Rope (coordination against empire). If high threshold: suppression is robust and snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_coalition_critical_mass, empirical, 'Critical mass threshold for organized resistance to crack legitimacy apparatus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_legitimacy_apparatus, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imp_leg_tr_t0, imperial_legitimacy_apparatus, theater_ratio, 0, 0.65).
narrative_ontology:measurement(imp_leg_tr_t20, imperial_legitimacy_apparatus, theater_ratio, 20, 0.78).
narrative_ontology:measurement(imp_leg_tr_t40, imperial_legitimacy_apparatus, theater_ratio, 40, 0.85).

% Extraction over time
narrative_ontology:measurement(imp_leg_be_t0, imperial_legitimacy_apparatus, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(imp_leg_be_t20, imperial_legitimacy_apparatus, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(imp_leg_be_t40, imperial_legitimacy_apparatus, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_legitimacy_apparatus, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_legitimacy_apparatus, colonial_resource_monopoly).
narrative_ontology:affects_constraint(imperial_legitimacy_apparatus, indigenous_elite_collaboration).
narrative_ontology:affects_constraint(imperial_legitimacy_apparatus, post_colonial_institutional_inheritance).
narrative_ontology:affects_constraint(imperial_legitimacy_apparatus, identity_lock_through_education).
narrative_ontology:affects_constraint(imperial_legitimacy_apparatus, religious_conversion_apparatus).

% DUAL FORMULATION NOTE:
% Imperial legitimacy apparatus decomposes into multiple structurally distinct constraints: the resource monopoly has different ε than the ideological apparatus; the education system has different extraction mechanisms than the legal code. The overall apparatus (this story) represents the integration of these mechanisms into a unified extraction system. Downstream constraints (colonial resource monopoly, institutional inheritance) depend on the legitimacy apparatus; upstream constraints (identity-locking mechanisms, elite collaboration structures) feed into it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_legitimacy_apparatus, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
