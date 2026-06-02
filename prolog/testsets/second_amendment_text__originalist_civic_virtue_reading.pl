% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment as Civic Militia Right (Originalist Civic Virtue Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The originalist civic virtue reading of the Second Amendment interprets
 *   the constitutional right to bear arms as protection for armed citizenry
 *   functioning in a militia capacity—a political community check against
 *   centralized military power and state tyranny. This reading grounds the
 *   right in civic republican political theory rather than individual
 *   self-defense or state regulation, and it instantiates one specific
 *   reading of the contested kernel: the text of the Second Amendment. The
 *   reading creates a constraint that both coordinates legitimate civic
 *   participation (armed citizens as a structural safeguard in republican
 *   government) and extracts from those seeking disarmament regulation (whose
 *   preferred policies are constitutionally blocked). The constraint operates
 *   across seven distinct structural perspectives, revealing how the same
 *   constitutional text can appear as pure coordination (Rope), mixed
 *   coordination-extraction (Tangled Rope), pure extraction (Snare), degraded
 *   ritual (Piton), and natural law (Mountain)—depending on the observer's
 *   structural relationship to the protection it offers.
 *
 * KEY AGENTS:
 *   - Civic Republican Constitutional Tradition: Beneficiary (organized/constrained) — sustains and benefits from the reading's protection of civic militia capacity; sees genuine coordination
 *   - Individual Firearm Owners: Mixed beneficiary-victim (powerful/mobile) — benefit from constitutional protection but are constrained by requirement to justify access through civic virtue language rather than personal preference
 *   - Urban Disarmament-Seeking Populations: Primary victim (powerless/trapped) — structurally foreclosed from preferred regulatory policies; cannot exit the constraint
 *   - Legislative Firearms Regulation: Institutional actor (institutional/arbitrage) — constrained by constitutional blockade but persists in attempting functional control; operates as Piton (performative)
 *   - Standing Military and State Security Apparatus: Mixed actor (institutional/constrained) — benefits from potential emergency militia capacity but constrained by institutional check on military monopoly
 *   - Militia Movement Organizations: Organized participants (organized/constrained) — enact and sustain the civic militia reading through training and mobilization; operate under Scaffold logic with sunset conditions
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing a contingent 18th-century institutional compromise as immutable political philosophy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.38).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.52).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment as Civic Militia Right (Originalist Civic Virtue Reading)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__originalist_civic_virtue_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'b8e1a051-d0f6-4b99-bc39-244f4cca3451').
narrative_ontology:cs_kernel_codification('b8e1a051-d0f6-4b99-bc39-244f4cca3451', fixed_text).
narrative_ontology:cs_authority_grounding('b8e1a051-d0f6-4b99-bc39-244f4cca3451', lineage).
narrative_ontology:cs_interpretation_layer_present('b8e1a051-d0f6-4b99-bc39-244f4cca3451').
narrative_ontology:cs_reading_relation('b8e1a051-d0f6-4b99-bc39-244f4cca3451', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8e1a051-d0f6-4b99-bc39-244f4cca3451', second_amendment_text__individual_right_reading, influences).
narrative_ontology:cs_axiom('b8e1a051-d0f6-4b99-bc39-244f4cca3451', foundational, civic_militia_check_on_tyranny).
narrative_ontology:cs_axiom_status(civic_militia_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('b8e1a051-d0f6-4b99-bc39-244f4cca3451', civic_militia_check_on_tyranny, deontological).
narrative_ontology:cs_axiom('b8e1a051-d0f6-4b99-bc39-244f4cca3451', foundational, universal_armed_citizenship_structural_requirement).
narrative_ontology:cs_axiom_status(universal_armed_citizenship_structural_requirement, holdable).
narrative_ontology:cs_axiom_grounding('b8e1a051-d0f6-4b99-bc39-244f4cca3451', universal_armed_citizenship_structural_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('b8e1a051-d0f6-4b99-bc39-244f4cca3451', founders_republican_theory_militia_function).
narrative_ontology:cs_drift_state('b8e1a051-d0f6-4b99-bc39-244f4cca3451', contemporary_standing_military_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8e1a051-d0f6-4b99-bc39-244f4cca3451', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, civic_republic_institutional_capacity).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizen_soldiery_political_participation).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, standing_military_monopoly_constraint).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, mass_disarmament_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIC REPUBLICAN TRADITION (ROPE) — This reading emerges from and sustains a constitutional tradition that coordinates political legitimacy around armed citizenry as a check on centralized military power. The tradition benefits from the right's protection and sees the constraint as genuine coordination: preserving the structural capacity for collective armed self-governance. The constraint is enforceable through constitutional interpretation and does not extract disproportionately from the tradition itself.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL FIREARM OWNERS (TANGLED ROPE) — Owners classified under this reading benefit from constitutional protection tied to civic militia function, but the reading's grounding in collective civic virtue (not personal self-defense) constrains their exit options and usage justifications. The reading coordinates their weapon access with a civic duty narrative, requiring rhetorical alignment with militia/political participation framing. Mixed: genuine coordination (access is protected) and extraction (must justify through civic rather than individual-liberty language).
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN DISARMAMENT-SEEKING POPULATIONS (SNARE) — Communities seeking to reduce firearm prevalence through regulation are trapped by the civic militia framing: the reading provides constitutional immunity to disarmament measures by grounding the right in a civic function rather than individual preference. These agents cannot exit the constraint; their preferred regulatory pathways are structurally foreclosed by the reading's constitutional architecture. Maximum extraction: the reading prevents their primary policy objective.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE FIREARMS REGULATION (PITON) — Modern legislative frameworks that attempt firearm classification and access control face persistent constitutional blockade from the civic militia reading, despite the reading's original function (maintaining citizen armed capacity against military tyranny) being structurally obsolete in a state with professional standing military and internal security infrastructure. The regulatory institutions persist in attempting functional control (background checks, licensing) while the constitutional reading operates theatrically — it performs civic virtue without solving the stated functional problem (reducing firearm violence). Theater ratio indicates the reading's performative function: asserting a civic obligation that is no longer operationally required.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STANDING MILITARY AND STATE SECURITY APPARATUS (TANGLED ROPE) — The professional military and security establishment benefit from the civic militia reading's potential as a coordination mechanism for emergency armed capacity during state failure or invasion (extraction avoided: standing military monopoly is not absolute, legitimizing reserve armed capacity). But they are also constrained by it: the reading prevents complete disarmament or monopolistic military control, maintaining an institutional check. Moderate extraction and coordination: the constraint both protects and limits this actor's power.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MILITIA MOVEMENT ORGANIZATIONS (SCAFFOLD) — Organized militia groups interpret and enact the civic militia reading through training, organization, and public performance. For these actors, the reading is scaffolding for political participation and counter-power organization. The constraint has a sunset logic: it functions as long as the civic militia groups can organize and mobilize; if state monitoring and legal barriers to organization increase, the functional basis for the reading's civic virtue claim erodes. Theater is present but functional: the groups coordinate around the reading, creating real armed capacity even if the broader 'check on tyranny' function is disputed.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational and universal analytical standpoint, the right to armed citizenry appears as an immutable structural feature of republican government itself: the separation of military power from civilian authority logically requires that civilians retain armed capacity to resist monopolistic military seizure. This perspective treats the constraint as a law of political philosophy, not a contingent constitutional text. However, the structural data contradicts this classification — modern standing militaries, intelligence services, and police capacity make citizen-scale arms inadequate for the stated function, suggesting the 'natural law' reading may be naturalizing a contingent 18th-century institutional design.
constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__originalist_civic_virtue_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading's extractiveness reflects the asymmetry between those who benefit from constitutional protection of armed capacity (civic republicans, firearm owners) and those whose preferred regulatory policies are blocked (urban disarmament movements). The value is moderate rather than high because the constraint also provides genuine coordination benefit: it maintains structural capacity for armed civic participation and guards against military monopoly, which has real political value. The trajectory from 0.22 to 0.38 reflects increasing asymmetry as modern standing military capacity grows, making the 'check on tyranny' function more theatrical. Suppression (0.52): Moderate-high. Suppression derives from multiple mechanisms: legal barriers and prosecution risk for militia organizing, state monitoring of militia activity, background check and licensing requirements for firearm access, and constitutional lock-in that prevents disarmament-focused polities from pursuing preferred policies. The trajectory from 0.38 to 0.52 reflects increasing enforcement intensity as state security interests prioritize domestic terrorism prevention. Theater ratio (0.68): Moderately high. The civic virtue claim operates theatrically: in a modern state with professional military and surveillance infrastructure, citizen-scale firearm ownership does not functionally preserve the capacity to resist tyranny (asymmetry of force is overwhelming), yet the reading persists by performing civic virtue without solving the functional problem it originally addressed. The trajectory from 0.48 to 0.68 reflects increasing theater as civic militia claims become more performative relative to actual counter-power capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximal perspectival gap across the seven classified positions. The civic republican tradition sees pure coordination (Rope)—the reading sustains the political structure it depends on. Individual firearm owners see mixed benefit and constraint (Tangled Rope)—constitutional protection but required rhetorical alignment with civic duty. Urban disarmament-seeking communities see pure extraction (Snare)—their preferred policies are constitutionally blocked with no exit. Legislative regulation institutions see degraded ritual (Piton)—they persist in attempting functional control despite constitutional blockade, maintaining performative regulation rather than substantive policy. The standing military sees mixed benefit and constraint (Tangled Rope)—emergency militia capacity is available but institutional check limits monopoly. Militia movements see temporary scaffolding (Scaffold)—the reading supports their organization as long as legal barriers don't escalate. The analytical observer risks seeing natural law (Mountain)—republican political philosophy as immutable—but the structural data suggests naturalization rather than genuine inevitability. The perspectival gaps reveal that no single type captures the constraint's structure; instead, the constraint's multi-perspectival profile IS the insight.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim status and exit options for each perspective. Civic republicans see low d (they benefit from the reading's protection, are organized, and have mobile exit within constitutional interpretation—d ≈ 0.25, f(d) ≈ 0.02). Urban disarmament-seekers see high d (they are victims of foreclosed policy options, powerless, and trapped by constitutional text—d ≈ 0.95, f(d) ≈ 1.42). Individual firearm owners see moderate d (they benefit from protection but constrained by civic duty framing—d ≈ 0.45, f(d) ≈ 0.50). Militia organizations see moderate d (they benefit organizationally but constrained by legal barriers—d ≈ 0.40, f(d) ≈ 0.40). Standing military sees moderate d (mixed benefit and constraint—d ≈ 0.50, f(d) ≈ 0.65). Regulatory institutions see moderate d (trying to function within constitutional blockade—d ≈ 0.55, f(d) ≈ 0.75). The analytical observer derives d from the position at civilizational scope (d ≈ 0.72, f(d) ≈ 1.15). No directionality overrides are required; the structural derivation captures the constraint's architecture accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the tension between 'is this pure coordination or asymmetric extraction' is not a classification error but a structural feature of the constraint's operation. From the civic republican perspective, it IS pure coordination (Rope)—the reading sustains the political tradition that grounds legitimacy in armed citizens. From the disarmament-seeking perspective, it IS pure extraction (Snare)—their preferred policies are permanently blocked. From the modern regulatory perspective, it IS degraded ritual (Piton)—the constraint persists through constitutional lock-in rather than functional necessity. The mandatrophy resolves by rejecting the demand for a single classification in favor of accepting that the constraint instantiates different logical structures from different structural positions. The constraint's unity is not in a shared type but in the perspectival presheaf that maps each observer position to its correct classification. The engine's computed constraint_claim (derived from the analytical observer at civilizational scope) may classify the reading as Tangled Rope or Snare, but this represents one perspectival position, not the constraint's true nature. The true nature IS the distribution across perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_militia_functionality_modern_world,
    'In a modern state with professional standing military, surveillance infrastructure, and rapid response capability, does citizen-scale firearm ownership functionally preserve the civic militia check against tyranny, or does the reading''s civic virtue claim operate theatrically?',
    'Historical-counterfactual analysis: comparison of actual armed resistance outcomes in modern states with and without civilian firearm prevalence; assessment of military capacity asymmetry and coordination barriers for mass civilian uprising',
    'If functionally preserved: civic militia reading remains a genuine coordination mechanism (Rope classification sustained). If theatrical: the reading is maintained through institutional inertia despite functional obsolescence (Piton reclassification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_militia_functionality_modern_world, empirical, 'Whether citizen-scale firearms provide functional check on modern military/state power').

omega_variable(
    kernel_reading_distinction_collective_vs_individual,
    'Does the originalist civic virtue reading (weaponized citizens organized in militia capacity) foreclose or coexist with the individual-right reading (protected personal self-defense ownership)?',
    'Textual analysis of founders'' militia language vs. personal-liberty language; historical precedent for dual-reading interpretation; analysis of whether collective civic function can be grounded without individual ownership rights',
    'If forecloses: the readings cannot coexist in a single constitutional framework (rare, strong claim). If coexists: both readings are live in different constitutional traditions (the standard position). If influences: civic reading creates upstream pressure on individual-right reading''s justification logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction_collective_vs_individual, conceptual, 'Logical relationship between civic militia reading and individual rights reading').

omega_variable(
    suppression_mechanism_regulatory_capture,
    'Is the suppression score (0.52) primarily structural (regulatory barriers to organizing militia capacity, legal risks to civic militia speech and assembly) or institutional (state actively capturing constitutional interpretation to prevent disarmament threats)?',
    'Analysis of prosecution patterns for militia activity; comparison of state enforcement intensity for civic militia organizing vs other protected speech; documentation of interpretive shifts in Supreme Court doctrine correlated with state security interests',
    'If structural: suppression reflects legitimate regulatory barriers to dangerous coordination (supports Tangled Rope classification). If institutional: suppression reflects state capture of the constitutional reading to preserve military monopoly (shifts toward Snare classification from regulatory perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_regulatory_capture, empirical, 'Whether suppression is structural regulation or state capture of constitutional interpretation').

omega_variable(
    false_summit_natural_law_naturalization,
    'Is the analytical observer''s mountain classification a genuine insight into republican political philosophy, or naturalization of a contingent 18th-century institutional compromise?',
    'Comparative constitutional analysis: examination of republican governments without individualized firearm rights; assessment of whether civic militia function can be preserved through alternative institutional designs (militia drafts, public armories, periodic training); historical analysis of whether founders intended universal civilian armament or militia-indexed access',
    'If genuine natural law: mountain classification is correct; the reading reflects irreducible republican logic. If naturalization: the engine''s false-summit detector reclassifies to Tangled Rope or Snare depending on whose interests are advanced by the naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_naturalization, conceptual, 'Whether civic militia right is natural law or naturalized contingent compromise').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who specifically benefits from the reading''s protection: abstract civic republican theory (institution/non-agent), individual firearm owners, militia organizations, or those opposed to standing military monopoly?',
    'Policy analysis of who gains material advantage from the reading''s protection (reduced legal barriers, constitutional immunity); documentary evidence of intentional beneficiary design vs. downstream benefit; comparison of advocacy intensity among potential beneficiary groups',
    'If beneficiary is individual owners: classification may shift toward Rope (pure coordination) or Snare (extraction from disarmament-seekers). If beneficiary is civic republic abstraction: classification sustains Tangled Rope (mixed coordination and institutional extraction). If no clear beneficiary: reclassify as pure coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Identity and intentionality of beneficiary groups under this reading').

omega_variable(
    originalist_methodology_constraint,
    'Does the originalist interpretive method (grounding the reading in founders'' intent and historical meaning) itself constrain which contemporary readings are permissible, thereby extracting from modern constitutional politics?',
    'Meta-analysis of originalist jurisprudence: documentation of how originalist method constrains contemporary policy options; comparison of regulatory flexibility under originalist vs. living-constitution interpretive frameworks; assessment of whether originalism serves institutional extraction (preserving certain readings as ''locked in'')',
    'If originalism constrains: the reading''s enforcement mechanism includes methodological lock-in (increases suppression, shifts toward Snare classification). If flexible: originalism is a genuine coordination mechanism among constitutional interpreters (supports Rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_methodology_constraint, conceptual, 'Whether originalist methodology itself constrains and extracts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ocv_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sa_ocv_tr_t2, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2, 0.58).
narrative_ontology:measurement(sa_ocv_tr_t5, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(sa_ocv_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sa_ocv_be_t2, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(sa_ocv_be_t5, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 5, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sa_ocv_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sa_ocv_su_t2, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(sa_ocv_su_t5, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, standing_military_monopoly_institutional_logic).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, firearm_regulation_constitutional_blockade).

% DUAL FORMULATION NOTE:
% The Second Amendment text (kernel) admits three structurally distinct readings, each instantiating a different constraint with different epsilon values and beneficiary/victim structures. The originalist civic virtue reading (this story) has epsilon ≈ 0.38 (moderate extraction). The collective security reading (separate file) emphasizes state militia systems and has different beneficiary structure. The individual right reading (separate file) grounds the right in personal liberty and self-defense, with yet different extractiveness. All three readings affect downstream constraints: standing military monopoly logic, firearm regulation as constitutional blockade, and urban disarmament movements. Each reading should be authored as a clean, ε-invariant constraint with its own perspectives and beneficiary/victim declarations, not as a single constraint with measurement parameters. The kernel-level analysis (which reading is correct?) is handled by comparing the three separate constraints, not by internal conditioning within a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
