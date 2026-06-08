% ============================================================================
% CONSTRAINT STORY: commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commemorative_husk_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: commemorative_husk_reading
 *   human_readable: Aneyoshi Stone as Commemorative Husk (Directive Lost)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, erected in 1933 after the Shōwa Sanriku
 *   tsunami, carries the directive: 'High dwellings are the peace and harmony
 *   of our descendants. Remember the calamity of the great tsunamis. Do not
 *   build any homes below this point.' This constraint story models ONE
 *   READING of the stone's authority structure during the inter-catastrophe
 *   period (1933-2011): the commemorative husk reading, in which the stone's
 *   directive force decayed into memorial significance while its physical
 *   form was preserved. Under this reading, the stone became a cultural
 *   heritage artifact whose commemorative interpretation removed its
 *   behavioral regulatory force, enabling coastal development that the
 *   directive explicitly prohibited. The constraint exhibits high
 *   extractiveness (0.68) because development interests captured land value
 *   by treating the stone as memorial rather than directive, while coastal
 *   residents bore mortality risk. Theater ratio is high (0.81) because
 *   heritage preservation rituals (signage, maintenance, tourism) maintained
 *   the stone's visibility while its directive function atrophied. This
 *   reading is structurally distinct from its sibling
 *   (behavioral_competence_reading), which models the stone as retaining
 *   directive force that economic incentives actively suppress rather than as
 *   having lost directive force entirely.
 *
 * KEY AGENTS:
 *   - Coastal Residents: Primary victim (powerless/trapped) — bear mortality risk from tsunami exposure; trapped by housing affordability and employment concentration in coastal zones
 *   - Coastal Development Interests: Primary beneficiary (institutional/arbitrage) — capture land value by developing below stone directive line; commemorative interpretation provides social license
 *   - Municipal Revenue Authorities: Secondary beneficiary (institutional/arbitrage) — benefit from property tax base expansion in coastal zones
 *   - Municipal Planning Authority: Mixed position (moderate/constrained) — coordinate land-use planning while dependent on development revenue; bear reputational risk if disaster occurs
 *   - Cultural Heritage Preservation System: Institutional actor (institutional/constrained) — maintain stone as artifact through performative conservation; directive function has atrophied
 *   - Future Tsunami-Exposed Populations: Victim (powerless/trapped) — abstract collective bearing long-term mortality risk from directive violation
 *   - Disaster Risk Reduction Coalition: Organized agents (organized/mobile) — building alternative risk governance pathways post-2011; see commemorative reading as temporary failure mode with sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commemorative_husk_reading, 0.68).
domain_priors:suppression_score(commemorative_husk_reading, 0.72).
domain_priors:theater_ratio(commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commemorative_husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commemorative_husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commemorative_husk_reading, snare).
narrative_ontology:human_readable(commemorative_husk_reading, "Aneyoshi Stone as Commemorative Husk (Directive Lost)").
narrative_ontology:topic_domain(commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commemorative_husk_reading, '960de9a6-cc96-46af-8e44-c949f2f258d0').
narrative_ontology:cs_kernel_codification('960de9a6-cc96-46af-8e44-c949f2f258d0', fixed_text).
narrative_ontology:cs_authority_grounding('960de9a6-cc96-46af-8e44-c949f2f258d0', lineage).
narrative_ontology:cs_reading_relation('960de9a6-cc96-46af-8e44-c949f2f258d0', commemorative_husk_reading__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('960de9a6-cc96-46af-8e44-c949f2f258d0', foundational, memorial_interpretation_disables_directive).
narrative_ontology:cs_axiom_status(memorial_interpretation_disables_directive, holdable).
narrative_ontology:cs_axiom_grounding('960de9a6-cc96-46af-8e44-c949f2f258d0', memorial_interpretation_disables_directive, empirically_contingent).
narrative_ontology:cs_axiom('960de9a6-cc96-46af-8e44-c949f2f258d0', foundational, directive_force_decays_across_generations).
narrative_ontology:cs_axiom_status(directive_force_decays_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('960de9a6-cc96-46af-8e44-c949f2f258d0', directive_force_decays_across_generations, empirically_contingent).
narrative_ontology:cs_reference_frame('960de9a6-cc96-46af-8e44-c949f2f258d0', directive_behavioral_competence_1933).
narrative_ontology:cs_drift_state('960de9a6-cc96-46af-8e44-c949f2f258d0', pre_2011_inter_catastrophe_period, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('960de9a6-cc96-46af-8e44-c949f2f258d0', '').
narrative_ontology:cs_kernel_id(commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, municipal_revenue_authorities).
narrative_ontology:constraint_victim(commemorative_husk_reading, coastal_residents).
narrative_ontology:constraint_victim(commemorative_husk_reading, future_tsunami_exposed_populations).
narrative_ontology:constraint_vindicates(commemorative_husk_reading, economic_rationality_primacy).
narrative_ontology:constraint_vindicates(commemorative_husk_reading, memorial_sufficiency_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL RESIDENTS (SNARE) — Trapped by economic necessity in tsunami-exposed zones. The stone's commemorative interpretation removes behavioral force from the directive, allowing development that would otherwise be prohibited. Maximum extraction: residents bear full mortality risk while development interests capture land value. No exit: housing affordability and employment concentrate in coastal zones.
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL PLANNING AUTHORITY (TANGLED ROPE) — Constrained by revenue dependence on coastal development but also coordinating legitimate land-use planning. Benefits from property tax base expansion; bears reputational and legal risk if disaster occurs. Mixed extraction: the commemorative reading enables revenue generation while maintaining plausible deniability about directive violation.
constraint_indexing:constraint_classification(commemorative_husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPMENT INTERESTS (ROPE) — Primary beneficiary. The commemorative reading removes the stone's regulatory force, converting prohibited land into developable assets. Experiences the constraint as pure coordination: the memorial interpretation provides social license for economically rational development. Net beneficiary with full exit capacity.
constraint_indexing:constraint_classification(commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HERITAGE PRESERVATION SYSTEM (PITON) — Maintains the stone as cultural artifact through performative conservation rituals (signage, tours, maintenance) while its directive function has atrophied. The preservation apparatus persists through institutional inertia, not because it transmits behavioral competence. Theater dominates: the stone is preserved as memorial precisely because that interpretation is functionally inert.
constraint_indexing:constraint_classification(commemorative_husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DRR COALITION (SCAFFOLD) — Organized agents (tsunami early warning systems, building codes, evacuation infrastructure) see the commemorative reading as a temporary failure mode with a sunset: the 2011 Tōhoku tsunami created conditions for reinterpreting the stone's directive force. Post-disaster institutional learning is building alternative risk governance pathways that bypass memorial interpretation. Sunset logic: as engineered systems mature and disaster memory refreshes, the stone's behavioral directive may be recoverable.
constraint_indexing:constraint_classification(commemorative_husk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the commemorative reading exhibits both coordination (memorial function preserves disaster knowledge across generations) and extraction (the interpretation that preserves the stone is precisely the one that disables its directive force). The constraint coordinates cultural memory while enabling development interests to capture land value. Analytical classification: tangled rope, not snare, because genuine coordination function exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(commemorative_husk_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commemorative_husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commemorative_husk_reading, TR),
    TR >= 0.70.

:- end_tests(commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Development interests capture substantial land value by treating the stone as memorial rather than directive, while coastal residents bear mortality risk. The extraction is not maximal (not 0.85+) because some genuine coordination function exists — the memorial does preserve disaster knowledge, even if not in behaviorally competent form. The value reflects that the commemorative interpretation enables extraction but is not purely extractive cover. Suppression (0.72): High. Coastal residents face significant barriers to exit: housing affordability concentrates in coastal zones, employment is coastal-dependent (fishing, tourism, port industries), and relocation costs are prohibitive for low-income households. Suppression is not total (not 0.85+) because some mobility exists for higher-income residents and because the risk is probabilistic rather than certain. Theater ratio (0.81): Very high. Heritage preservation activities (stone maintenance, interpretive signage, memorial ceremonies, disaster museum exhibits) are substantially performative — they maintain the stone's visibility and cultural significance while its directive function is inert. The theater has increased over the interval as the stone's commemorative interpretation became institutionalized through tourism and heritage designation. The high theater ratio is the primary signal for piton classification from the heritage preservation perspective.
 *
 * PERSPECTIVAL GAP:
 *   The commemorative husk reading produces a wide perspectival gap. Development interests see pure coordination (Rope) — the memorial interpretation provides social license for economically rational land use. Coastal residents see pure extraction (Snare) — they bear mortality risk while others capture land value, with no exit. Municipal planners see mixed coordination and extraction (Tangled Rope) — legitimate planning function exists alongside revenue dependence on risky development. The heritage system sees degraded ritual (Piton) — preservation activities persist through inertia while directive function has atrophied. The DRR coalition sees temporary failure with sunset (Scaffold) — post-2011 institutional learning is building alternative pathways. The analytical observer sees genuine coordination (memorial preserves knowledge) alongside asymmetric extraction (the interpretation that preserves the stone disables its directive force), producing Tangled Rope at the analytical level. The gap reveals that 'commemorative vs directive' is not a property of the stone itself but of the observer's structural position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests and municipal revenue authorities are primary beneficiaries — they capture land value and tax revenue from coastal development enabled by the commemorative interpretation. Their directionality values are low (near 0.0), producing negative or near-zero effective extraction (they experience the constraint as subsidy). Coastal residents and future exposed populations are primary victims — they bear mortality risk with no compensation and limited exit options. Their directionality values are high (near 1.0), producing maximum effective extraction amplified by trapped exit status. Municipal planning authorities occupy a mixed position — they benefit from revenue but bear reputational and legal risk, producing moderate directionality (near 0.5). The heritage preservation system is neither clear beneficiary nor victim — it maintains the stone but does not collect rents from the commemorative interpretation; directionality is near 0.5, producing moderate effective extraction. The DRR coalition has organized power and mobile exit, producing low effective extraction despite being structurally opposed to the commemorative reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the stone's 'function' is observer-dependent. From the development perspective, the stone functions as intended — it memorializes disaster while permitting rational land use. From the resident perspective, the stone has failed — its directive is violated and they bear the cost. From the heritage perspective, the stone functions as cultural artifact — preservation is success regardless of behavioral compliance. From the analytical perspective, the constraint exhibits both coordination (memorial function) and extraction (directive violation enables rent capture). The mandatrophy is not 'has the stone succeeded or failed?' but 'which function are you measuring?' The commemorative husk reading models one structural possibility: directive force decayed during the inter-catastrophe period. The sibling reading (behavioral_competence) models the alternative: directive force persists but is suppressed by economic incentives. Both are coherent structural descriptions; the empirical question is which better describes the 1933-2011 period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Aneyoshi stone''s commemorative interpretation a genuine loss of directive force (this reading), or is the directive still behaviorally competent but suppressed by economic incentives (behavioral_competence_reading sibling)?',
    'Ethnographic study of local decision-making: do residents and planners reference the stone''s directive when evaluating coastal development, or only its memorial significance? Counterfactual analysis: would removing economic incentives for coastal development restore behavioral compliance with the directive?',
    'If directive is lost: this reading (commemorative_husk) is structurally accurate — the stone is a memorial artifact with no regulatory force. If directive is suppressed but competent: the sibling reading (behavioral_competence) is accurate — the stone retains directive force that economic interests actively override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether stone directive is lost (commemorative husk) or suppressed (behavioral competence)').

omega_variable(
    inter_catastrophe_periodization,
    'What duration of inter-catastrophe period is sufficient for directive force to decay from behavioral competence to commemorative husk?',
    'Comparative analysis of tsunami stone sites with different time-since-last-event: correlation between inter-catastrophe duration and behavioral compliance rates. Historical analysis of directive transmission across generational boundaries.',
    'If threshold < 50 years: directive decay is rapid and this reading applies to most inter-catastrophe periods. If threshold > 150 years: directive decay requires multiple generations and this reading applies only to extended quiescent periods.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inter_catastrophe_periodization, empirical, 'Time threshold for directive force decay').

omega_variable(
    post_2011_reinterpretation_trajectory,
    'Did the 2011 Tōhoku tsunami restore the stone''s directive force, or merely refresh its commemorative significance?',
    'Longitudinal study of post-2011 coastal development patterns near tsunami stones: are development decisions constrained by stone directives, or does development continue with enhanced memorial tourism? Comparison of pre-2011 and post-2011 land-use planning documents.',
    'If directive restored: the commemorative husk reading is time-bounded (applies only to pre-2011 period) and the constraint has sunset. If only memorial refreshed: the commemorative husk reading persists and the stone remains functionally inert despite renewed attention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_2011_reinterpretation_trajectory, empirical, 'Whether 2011 tsunami restored directive force or only memorial significance').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the stone-as-physical-artifact (this framing: fixed_text + lineage authority), or the transmitted-directive-practice (alternative framing: practice + practice authority)?',
    'Ethnographic analysis: do communities treat the stone itself as authoritative (text-based legitimacy), or the practice of heeding tsunami warnings as authoritative (practice-based legitimacy)? The stone-as-text framing produces commemorative_husk when the text is preserved but not obeyed; the practice-as-kernel framing produces behavioral_competence when the practice persists despite memorial interpretation of the artifact.',
    'Stone-as-kernel framing (this choice): commemorative_husk reading is coherent — the text is preserved as memorial while its directive force decays. Practice-as-kernel framing (alternative): behavioral_competence reading is coherent — the practice persists independently of how the artifact is interpreted. The readings'' structural difference maps to kernel framing choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether kernel is stone-artifact or transmitted-practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1933, commemorative_husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1953, commemorative_husk_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(theater_1973, commemorative_husk_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement(theater_1993, commemorative_husk_reading, theater_ratio, 60, 0.77).
narrative_ontology:measurement(theater_2011, commemorative_husk_reading, theater_ratio, 78, 0.81).

% Extraction over time
narrative_ontology:measurement(extract_1933, commemorative_husk_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extract_1953, commemorative_husk_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(extract_1973, commemorative_husk_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(extract_1993, commemorative_husk_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(extract_2011, commemorative_husk_reading, base_extractiveness, 78, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1933, commemorative_husk_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(suppress_1953, commemorative_husk_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(suppress_1973, commemorative_husk_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(suppress_1993, commemorative_husk_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(suppress_2011, commemorative_husk_reading, suppression_requirement, 78, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The commemorative_husk_reading and behavioral_competence_reading are sibling readings of the same kernel (aneyoshi_stone_directive). They model structurally distinct mechanisms producing the same observable outcome (coastal development below directive line). This reading (commemorative husk) models directive force decay; the sibling models directive suppression. The readings are linked because they are alternative interpretations of the same authority structure, but they have different epsilon values and different beneficiary/victim structures. The commemorative husk reading has higher extractiveness because the loss of directive force removes any behavioral constraint, while the behavioral competence reading models active suppression costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
