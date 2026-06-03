% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate Under Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story represents ONE READING of a contested kernel: the
 *   vaccine mandate balance. The reading instantiated here is bodily autonomy
 *   primary — the position that individual consent to medical intervention is
 *   inviolable and that the state cannot compel vaccination regardless of
 *   collective benefit. This is a kernel reading, not an independent
 *   constraint. Sibling readings (public_health_primary and
 *   proportionality_reading) are OTHER constraint stories in the same family;
 *   they are not part of this story. This story models ONLY the bodily
 *   autonomy primary reading as a structurally consistent normative
 *   commitment. The core empirical claim within this reading is that mandates
 *   constitute direct coercion of bodily autonomy (suppression ≥ 0.70)
 *   without sufficient coordination benefit to justify the coercion
 *   (extractiveness ≥ 0.65). The measurement trajectory shows extraction and
 *   suppression rising as mandates intensify (employment-level enforcement,
 *   school exclusion, health-pass requirements), while theater ratio rises
 *   modestly as constitutional objections accumulate without blocking
 *   enforcement. The constraint family decomposes a single contested
 *   institutional question into three structurally distinct constraint
 *   stories: each reading has its own ε, its own beneficiary/victim
 *   structure, its own measurement trajectory. Together they model the
 *   persistent institutional conflict over vaccine mandate legitimacy.
 *
 * KEY AGENTS:
 *   - Unvaccinated coerced individuals (powerless/trapped): Primary victims under this reading — bear full extraction cost through employment loss, school exclusion, medical access denial. Suppressions operate at maximum force.
 *   - Risk-averse unvaccinated (moderate/constrained): Secondary victims — face constrained exit (exemptions, relocation, employment delay) rather than complete trap. Experience mixed extraction and some agency.
 *   - Public health authority (institutional/arbitrage): Beneficiary in institutional sense (mandate is state's tool to solve collective action problem). Experiences no extraction — sees mandate as coordination function. Can arbitrage between mandate and alternative policies.
 *   - Immunocompromised exposed (moderate/trapped): Under this reading, treated as bearing their own risk. Cannot force vaccination of others; must isolate or accept exposure. Victims of the autonomy principle's harsh consequences.
 *   - Constitutionalist authority (institutional/arbitrage): Institutional actor maintaining constitutional principle while permitting administrative circumvention. Performs constitutional theater (courts reject mandates) while mandates persist (employers, schools implement them). Piton classification captures the degradation.
 *   - Analytical observer (analytical/analytical): Risk of naturalizing a contested normative commitment (bodily autonomy as inviolable principle) as a civilizational constant.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate Under Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2').
narrative_ontology:cs_kernel_codification('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', formalized).
narrative_ontology:cs_authority_grounding('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', extraction).
narrative_ontology:cs_interpretation_layer_present('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2').
narrative_ontology:cs_reading_relation('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', foundational, bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', bodily_autonomy_inviolable, deontological).
narrative_ontology:cs_axiom('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', foundational, collective_benefit_cannot_override_autonomy).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_override_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', collective_benefit_cannot_override_autonomy, deontological).
narrative_ontology:cs_reference_frame('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', liberal_individual_rights_primacy).
narrative_ontology:cs_drift_state('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', pandemic_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ed3daf2-3c9e-4c79-9e04-8a028a8b07a2', '2026-02-26T14:23:45Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED UNVACCINATED (SNARE) — Individual facing employment loss, school exclusion, or health care denial has no meaningful exit. The mandate's suppressive force is total: employment, education, and medical access are non-negotiable. The extraction is straightforward — bodily autonomy is compelled in exchange for participation in civil society. This agent experiences maximum chi: powerless + trapped + national scope amplifies extractiveness.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RISK-AVERSE UNVACCINATED (TANGLED ROPE) — Individual with medical concerns or prior adverse events faces constrained exit: can seek exemptions, relocate, or delay employment. Some agency exists but costs are high. Experiences both extraction (mandate coercion) and coordination benefit (community disease reduction, even if unwilling participant). The constraint has mixed character from this position — genuine resistance to the mandate but also structural entanglement in the collective action problem.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — State agency views mandate as pure coordination: achieving vaccination thresholds solves the collective action problem (free-rider problem in vaccination). The authority has arbitrage options (mandate vs incentive vs information campaigns) and implements the mandate strategically. From this perspective, the constraint solves a genuine coordination problem. No extraction is experienced — the authority sees itself as managing risk, not coercing. Effective extraction chi is low: institutional power + arbitrage exit means beneficiary directionality.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED EXPOSED (SNARE) — Under bodily autonomy primary reading, the immunocompromised bear their own risk. They cannot exit (cannot avoid unvaccinated contacts without isolating), and the state does not compel others' vaccination to protect them. This reading treats their exposure as a consequence of living in a society where bodily autonomy is inviolable. High suppression (cannot control others' vaccination choices); high extraction (forced to accept risk without recourse). This perspective reveals the reading's harsh structural consequence: vulnerable populations are sacrificed to the autonomy principle.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONALIST AUTHORITY (PITON) — Legal institutions charged with enforcing bodily autonomy principles increasingly perform a ritual defense of the principle while mandates persist through administrative pressure, employer-level enforcement, and de facto coercion. Constitutional courts in some jurisdictions have declared mandates impermissible while simultaneously allowing them to operate through parallel mechanisms (health pass requirements, workplace policies). The authority sees its own constitutional framework as degraded — the principle is maintained performatively while circumvented structurally. Theater ratio (0.48) reflects that constitutional theater persists alongside functional mandate enforcement.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational standpoint, bodily autonomy is treated as a foundational right, inviolable regardless of collective benefit — an immutable principle like fundamental human dignity. This perspective naturalizes bodily autonomy as a civilizational constant. However, this classification triggers false summit detection: the principle is a contested normative commitment, not a natural law. Empirical observation shows that bodily autonomy is regularly overridden (quarantine, organ donation law, mandatory vaccination), and the principle's strength varies across historical periods and jurisdictions. The mountain classification naturalizes what is actually a contingent normative choice.
constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_balance__bodily_autonomy_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): HIGH. Within the bodily autonomy primary reading, vaccine mandates constitute direct extraction of bodily autonomy without compensating coordination benefit. The reading rejects the public health framing (collective benefit justifies coercion) and the proportionality framing (thresholds of severity/safety can permit intervention). From this reading's internal logic, the mandate extracts bodily autonomy directly and without remainder. The trajectory shows extraction rising as enforcement mechanisms intensify: voluntary guidance (ε≈0.15) → employer policies (ε≈0.45) → school exclusion (ε≈0.60) → employment-level enforcement (ε≈0.68). Suppression (0.72): VERY HIGH. The suppressive force is overwhelming: employment, education, and medical access are non-negotiable civil participation routes. An unvaccinated person cannot realistically exit these requirements without abandoning economic participation, education, or health care. The trajectory shows suppression rising as jurisdictions close exemption pathways and as enforcement extends to private employment (not just government). Theater ratio (0.48): MODERATE-LOW. This reading presents relatively straightforward coercion with minimal performative content. The mandate is enforced directly through concrete mechanisms (termination, school exclusion, health pass requirements). Constitutional objections accumulate in some jurisdictions, creating a piton layer (courts reject mandates, employers implement them anyway), which raises theater ratio slightly. But the underlying mechanism is functional extraction, not theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the public health authority and the coerced individual is maximum. The authority experiences the mandate as coordination (rope/low extraction); the individual experiences it as extraction (snare/high extraction). This gap is irreducible within the bodily autonomy primary reading: if autonomy is truly inviolable, then ANY coercion is extractive (not coordinate), and the authority's coordination framing is self-deception masking extraction. The gap between the coerced individual (snare) and the constitutionalist authority (piton) shows institutional degradation: the constitutional principle (bodily autonomy is protected) is maintained in court decisions while being circumvented through employment and administrative mechanisms. The gap between the coerced individual and the immunocompromised exposed (both snare but for different reasons) reveals the reading's harsh distributional consequence: under bodily autonomy primary, vulnerable populations cannot demand that others be vaccinated to protect them. They must isolate or accept exposure — their vulnerability is not sufficient grounds to override the unvaccinated's autonomy. This is a real structural consequence of the reading, not a contingent policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The bodily autonomy primary reading centers extraction through coercion of protected interests. The unvaccinated-coerced individual is structurally positioned as the primary target (victim): powerless individuals facing trapped exit options experience maximum d ≈ 0.95, producing f(d) ≈ 1.42, amplifying chi to 0.68 × 1.42 × 1.0 = 0.96 (full snare classification). The public health authority is positioned as beneficiary (institutional power, arbitrage exit) with d ≈ 0.05, producing f(d) ≈ -0.12, yielding negative chi (rope/coordination classification). The immunocompromised exposed occupy a secondary victim position: moderate power, trapped exit (cannot compel others' vaccination), d ≈ 0.85, producing f(d) ≈ 1.15. The constitutional authority performs the principle while permitting violation — institutional power, arbitrage exit, but captured by both readings (enforcement pressure + autonomy principle) — d ≈ 0.50, producing piton's performative asymmetry. These directionality values are derived from the reading's own structural logic, not from empirical outcomes: the reading claims autonomy is inviolable, so anyone subjected to coercion is a victim, regardless of health consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint resolves the mandatrophy (high extraction and high enforcement) by making the extraction explicit and primary. The bodily autonomy primary reading does NOT claim that mandates are coordination solutions with side effects (which would be mandatrophy: calling extraction 'coordination'). Instead, it claims mandates ARE extraction — direct compulsion of protected bodily autonomy — and that this extraction is categorically impermissible regardless of collective benefit. The reading is logically coherent: if autonomy is inviolable, extraction is the problem, not a necessary cost of coordination. The mandatrophy is resolved by accepting the snare classification and rejecting the tangled_rope compromise position (the proportionality reading's frame). The sibling public_health_primary reading resolves mandatrophy differently: it claims high execution masks successful coordination (collective protection outweighs individual extraction), making the constraint rope or tangled_rope rather than snare. These are genuinely different metaphysical commitments about whether coercion can be justified by collective benefit. They cannot both be true within a single legal framework — one reading forecloses the other's core premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the vaccine mandate kernel is structurally correct: bodily autonomy primary, proportionality, or public health primary?',
    'This is a conceptual/preference omega — no empirical data resolves which normative axiom should dominate. Resolution depends on constitutional tradition (does this jurisdiction ground legitimacy in individual rights or collective welfare?), on axiom_overriding evidence (has empirical science invalidated any axiom?), and on political choice (which reading does the polity commit to?).',
    'If bodily autonomy primary is adopted: ε=0.68 (high extraction from coercion). If public health primary is adopted: ε=0.35 (high cooperation benefit, moderate coordination cost). If proportionality reading is adopted: ε=0.42 (context-dependent, varying by disease severity and vaccine safety profile).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'Which foundational axiom (autonomy, collective welfare, or proportionality) should govern mandate legitimacy').

omega_variable(
    coercion_mechanism_classification,
    'Are employment-based and school-based vaccine mandates truly coercive (forced choice between bodily autonomy and civil participation), or are they conditions on access to state-provided or state-regulated services (legitimate quid pro quo)?',
    'Conceptual analysis of the coercion threshold: Does exclusion from public employment constitute coercion, or merely consequences of refusing a condition of service? Does school exclusion fall differently than employment exclusion? This turns on how ''coercion'' is defined — instrumental coercion (threat to vital interests) vs normative coercion (violation of inviolable rights). Different legal traditions resolve this differently.',
    'If employment/school mandates count as coercive: suppression ≥ 0.70, snare classification. If they count as legitimate conditions: suppression drops to 0.40, tangled_rope classification. The ε value hinges on this classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_classification, conceptual, 'Whether service exclusion conditions constitute coercive mandates').

omega_variable(
    autonomy_inviolability_limits,
    'Does ''inviolable bodily autonomy'' have any exceptions, or is it truly categorical? If exceptions exist (quarantine, quarantine during plague, organ conscription for life-saving transplants), where does the exception boundary lie relative to vaccine mandates?',
    'Historical and comparative analysis: Which medical interventions are recognized as permissible despite bodily autonomy concerns? Does the principle hold differently for infectious disease vs non-infectious? For voluntary participation in institutions vs mandatory inclusion? This is an empirical question about how the principle has been actually applied across jurisdictions and periods.',
    'If truly categorical (no exceptions): mandates are categorically impermissible, ε=0.72, snare. If categorical only for vaccines but not quarantine: inconsistent principle application, suggests extractive motivation. If exceptions exist and include pandemic prevention: mandate may be permissible within exception bounds, ε ≤ 0.42, tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_inviolability_limits, empirical, 'Historical application boundaries of bodily autonomy principle').

omega_variable(
    sibling_reading_foreclosure,
    'Does the bodily autonomy primary reading logically foreclose the public health primary reading, or do they coexist as live competing commitments?',
    'Logical analysis: If the core axiom of bodily autonomy is truly inviolable, then the public health reading''s core axiom (collective welfare can override autonomy) is logically incompatible with any single legal framework. If both readings can coexist within different parties'' interpretive traditions (constitutional courts recognizing autonomy while policy makers implement mandates), the relation is coexists_with rather than forecloses.',
    'If forecloses: only one reading can be institutionalized consistently. If coexists_with: both readings remain live, creating persistent institutional conflict and the piton observation (constitutional theater masking administrative reality). The engine uses reading_relations to compute which framings are stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between autonomy and public health readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmbap_theater_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vmbap_theater_t2, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 2, 0.42).
narrative_ontology:measurement(vmbap_theater_t4, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(vmbap_extractiveness_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(vmbap_extractiveness_t2, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(vmbap_extractiveness_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vmbap_suppression_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(vmbap_suppression_t2, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(vmbap_suppression_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine mandate balance is a contested kernel decomposed into three constraint stories with different ε values and different victim/beneficiary structures. The bodily_autonomy_primary reading (this story, ε=0.68, snare) treats mandates as extraction of protected autonomy. The public_health_primary reading (sibling story, ε≈0.35, rope/tangled_rope) treats mandates as coordination solution for collective action problem. The proportionality_reading (sibling story, ε≈0.42-0.55, context-dependent) treats mandates as permissible only within strict thresholds. All three are linked via network.affects_constraints; together they model the persistent institutional conflict over mandate legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
