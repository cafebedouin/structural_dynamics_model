% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_universal_rights, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Protections Universal Scope Reading: All Persons Affected by Armed Conflict
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   The Geneva Conventions' protective scope is contested across three
 *   structurally distinct readings of a single kernel: the contested
 *   interpretation of who qualifies for protection and what threshold of
 *   conflict triggers which protections. The universal-rights reading
 *   instantiated in this constraint expands protective scope to all persons
 *   affected by armed conflict, grounding protection in Common Article 3
 *   (minimum humanitarian standards applicable to all conflicts) plus
 *   integration with international human rights law. This reading dissolves
 *   the combatant-status gating that the state-centric reading uses as the
 *   primary axis of protection eligibility. Instead, the universal-rights
 *   reading says: all persons have baseline rights (life, protection from
 *   torture, due process) in all conflicts; these rights apply regardless of
 *   combatant status, nationality, or insurgent affiliation. The
 *   hybrid-proportionality reading occupies middle ground: protections scale
 *   by conflict classification (international vs. non-international), with
 *   Additional Protocol I's higher standards applying only to inter-state
 *   wars and Additional Protocol II/Common Article 3 applying to internal
 *   conflicts. The universal-rights reading subsumes both by extending the
 *   human rights floor universally. This constraint models the structural
 *   consequence of that reading: it raises extraction on state military
 *   operations (restricts targeting, detention, interrogation discretion)
 *   while expanding beneficiary set (includes non-state combatants and all
 *   civilians under unified protection logic). The extractiveness value
 *   (0.62) reflects moderate constraint on state operations — significant
 *   reduction in military discretion but not absolute prohibition;
 *   enforcement remains selective and slow. The theater ratio (0.35) is low
 *   because the universal-rights reading relies on doctrinal integration and
 *   legal principle rather than institutional ritual; enforcement bodies
 *   (ICC, UN mechanisms) are theatrical (piton from institutional
 *   perspective), but the reading itself is substantive.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary beneficiary (powerless/trapped) — gain protection floor from universal-rights reading but remain vulnerable to targeting discretion and displacement
 *   - Non-State Combatants: Primary beneficiary (moderate/constrained) — gain legal status and Common Article 3 protections but remain asymmetrically positioned versus state military
 *   - State Military Authority: Primary victim (institutional/arbitrage) — face extraction through reduced targeting discretion, detention/interrogation rules, and accountability exposure
 *   - Detained Unprivileged Belligerents: Secondary beneficiary (powerless/trapped) — gain torture prohibitions and minimum due process, but remain trapped in asymmetric detention regime
 *   - International Humanitarian Law Community: Organized beneficiary (organized/mobile) — advocacy groups, ICRC, legal scholars benefit from legitimacy of expanded reading without bearing operational costs
 *   - State Compliance Infrastructure: Institutional actor (institutional/arbitrage) — ICC, UN bodies, treaty monitors maintain enforcement machinery but with limited actual deterrent effect (piton component)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.48).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Protections Universal Scope Reading: All Persons Affected by Armed Conflict").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'e8fc8147-5e68-4575-ba9a-07bd0fd1adab').
narrative_ontology:cs_kernel_codification('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', formalized).
narrative_ontology:cs_authority_grounding('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', lineage).
narrative_ontology:cs_interpretation_layer_present('e8fc8147-5e68-4575-ba9a-07bd0fd1adab').
narrative_ontology:cs_reading_relation('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', foundational, universal_human_rights_floor_primacy).
narrative_ontology:cs_axiom_status(universal_human_rights_floor_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', universal_human_rights_floor_primacy, deontological).
narrative_ontology:cs_axiom('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', foundational, common_article_3_as_universal_minimum).
narrative_ontology:cs_axiom_status(common_article_3_as_universal_minimum, holdable).
narrative_ontology:cs_axiom_grounding('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', common_article_3_as_universal_minimum, conventional).
narrative_ontology:cs_reference_frame('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', human_rights_integrated_ihl_framework).
narrative_ontology:cs_drift_state('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', contemporary_ihl_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8fc8147-5e68-4575-ba9a-07bd0fd1adab', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, wounded_and_sick).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocacy_community).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_flexibility).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, military_targeting_discretion).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, detention_interrogation_latitude).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINED UNPRIVILEGED BELLIGERENT (SNARE) — Under state-centric readings, falls outside combatant protections entirely. Under universal-rights reading, gains Common Article 3 + human rights floor but state military power remains asymmetric. Cannot exit detention; faces interrogation, torture risk, execution. No arbitrage. Maximum extraction.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN IN CONFLICT ZONE (SNARE) — Universal-rights reading expands protections to all civilians and non-combatants. However, enforcement is weak (state military interprets proportionality); targeting based on contested dual-use status. Trapped by geography; bears asymmetric extraction through targeting and displacement.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-STATE ARMED GROUP (TANGLED ROPE) — Universal reading includes non-state actors in protective scope, but enforcement depends on group compliance with IHL standards. Constrained: cannot easily exit conflict, faces state military asymmetry. But coordination benefit exists — Common Article 3 floor enables humanitarian access, medical care, negotiated protections. Mixed extraction and coordination.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE MILITARY AUTHORITY (TANGLED ROPE) — Universal-rights reading constrains state targeting, detention, and interrogation practices through expanded Common Article 3 interpretation plus human rights law floor. Benefits from coordination (humanitarian access to own wounded, protection from reprisal attacks, operational legitimacy). Costs: restricted targeting discretion, detention/interrogation rules, international accountability exposure. Can arbitrage (selectively comply or reinterpret), but extraction runs TOWARD this agent — they are beneficiary of the coordination mechanism's efficiency gains.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IHL ADVOCACY COMMUNITY (ROPE) — Human rights organizations, ICRC, legal scholars advocating universal-rights reading experience this as pure coordination. Mobilize around shared norm (universal protections). Can exit (work on other issues). No extraction — the constraint benefits the community through legitimacy, funding, and movement cohesion. Pure coordination benefit.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE COMPLIANCE INFRASTRUCTURE (PITON) — International Criminal Court, UN Human Rights Council, treaty monitoring bodies. Institutional enforcement of universal-rights reading remains largely theatrical: investigations slow, prosecutions few, state sovereignty shields powerful actors. Theater ratio high because institutional machinery persists through legitimacy maintenance rather than actual prevention of violations. States arbitrage compliance (rhetorical endorsement + selective enforcement).
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, universal human rights and inherent dignity form an immutable foundation: all persons possess inalienable rights regardless of status. Cannot be exempted by conflict context. This perspective risks naturalizing the universal-rights reading as a discovered natural law rather than a contested interpretive reading. Engine false-summit detector will flag this: the 'universal dignity' framing is institutionally constructed, not naturally emergent.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__universal_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, TR),
    TR >= 0.70.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The universal-rights reading constrains state military operations through three mechanisms: (1) expanded beneficiary scope eliminates targeting exemptions for non-state combatants; (2) Common Article 3 + human rights law integration creates detention/interrogation floor that supersedes military necessity arguments; (3) post-conflict accountability exposure through ICC and universal jurisdiction. However, extraction is not maximal (0.75+) because state military retains interpretation latitude (proportionality analysis, dual-use targeting logic, classification disputes) and enforcement is selective — powerful states face lower prosecution risk. The rising trajectory (0.48→0.62) reflects increasing institutional maturity of the universal-rights reading: ICC prosecutions accumulate, national courts develop doctrine, military training integrates human rights standards. Suppression (0.48): Moderate. State compliance mechanisms include rhetorical endorsement, selective enforcement, reinterpretation of scope boundaries, and operational discretion. Powerless agents (detainees, civilians) face high suppression; organized agents (IHL community) face low suppression. Theater ratio (0.35, declining): Low, declining over interval. The universal-rights reading emphasizes substantive legal principle and doctrinal integration rather than institutional ritual. Enforcement bodies (ICC, UN) are theatrical, but the reading itself is doctrinal — it justifies principle rather than performing legitimacy. The declining trajectory reflects growing normalized integration into military doctrine and international law (less theater as the reading becomes settled).
 *
 * PERSPECTIVAL GAP:
 *   The universal-rights reading produces different classifications across structural positions. Detainees see maximum extraction (Snare) — protection floor exists but state military power remains asymmetric. Non-state armed groups see mixed coordination and extraction (Tangled Rope) — they gain legal status and humanitarian benefits but remain constrained by state military superiority. State military sees the constraint as mixed (Tangled Rope) — their operational discretion is reduced but they benefit from rule clarity, humanitarian access reciprocity, and operational legitimacy. The IHL advocacy community sees pure coordination (Rope) — they mobilize around the shared norm with no extraction cost. The institutional enforcement machinery (ICC, UN) sees its own degraded ritual (Piton) — the machinery persists through legitimacy maintenance rather than actual prevention or deterrence. The civilizational analytical observer risks seeing this as natural law (Mountain) — universal human dignity as immutable foundation — but the structural data reveals this as a contested institutional reading, not discovered natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The universal-rights reading's directionality values derive from structural position relative to the expanded protective scope. Detainees and civilians are powerless victims with high d → maximum experienced extraction. Non-state combatants are moderate victims with constrained exit (cannot easily leave conflict) but some benefit from legal status → moderate d → tangled_rope experience. State military institutions are institutional beneficiaries (benefit from operational legitimacy, humanitarian reciprocity) but also victims of constrained discretion (d ambiguous, derived as 0.48 reflecting institutional power offset by extraction of operational latitude) → tangled_rope classification. The IHL advocacy community are beneficiaries with mobile exit (can work on other issues) → low d → rope. The enforcement machinery are institutional actors with arbitrage options (selective enforcement, jurisdictional shopping) but also dependent on the reading's legitimacy → moderate d → piton (degraded institutional function). The engine derives d for each perspective from beneficiary/victim declarations and spatial scope modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The universal-rights reading resolves mandatrophy by grounding protective scope in rights claims rather than status claims. The mandatrophy question is: Does IHL protect combatants (status-based) or persons (rights-based)? The state-centric reading answers: combatants under lawful criteria. The universal-rights reading answers: persons affected by armed conflict. These are not compatible within a single framework — they cannot coexist in the same institutional practice without contradiction. However, the universal-rights reading does not claim to represent the current law as practiced; it claims to represent the law as it should be interpreted (human rights integration). The mandatrophy is resolved at the architectural level: human rights law and IHL are now integrated into a unified protective framework. The extraction on state military operations is the structural consequence of this integration — it constrains targeting, detention, interrogation in ways the pure IHL approach (combatant-status gating) would not. The engine's mandatrophy detector will compute whether this constraint at ε=0.62 and suppression=0.48 genuinely instantiates a unified protective regime or merely names a contested aspiration without institutional teeth. The measurements showing rising extractiveness (0.48→0.62) over time suggest the reading is gaining institutional force; if the trajectory reverses, the constraint would degrade to piton (theatrical commitment without functional enforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_efficacy,
    'Does international enforcement (ICC, UN bodies, domestic prosecutions) actually deter state violations under universal-rights reading, or is enforcement too slow and selective to constitute real constraint on military operations?',
    'Comparative analysis: state compliance rates under universal-rights interpretation vs. state-centric interpretation; prosecution timeline vs. conflict duration; deterrence signal strength measured against recidivism rates in successive conflicts',
    'If enforcement effective: constraint genuinely reduces extraction (snare → tangled_rope from powerless perspective). If ineffective: universal-rights reading is theater (piton classification strengthened), and extraction remains high despite formal legal expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_efficacy, empirical, 'Whether international enforcement deters state violations or remains performative').

omega_variable(
    non_state_actor_compliance_heterogeneity,
    'Does including non-state actors in universal-rights protective scope change their actual behavior toward civilians/detained persons, or does it merely provide legal grounds for prosecution without changing practices?',
    'Comparative field analysis: treatment of detainees by groups accepting IHL standards vs. groups rejecting them; displacement/targeting patterns before/after groups formally adopt Common Article 3 commitment',
    'If compliance driven by legal status: universal reading strengthens coordination function (tangled_rope from non-state group perspective). If compliance-independent: legal reading is performative overlay (piton component rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_compliance_heterogeneity, empirical, 'Whether non-state actor legal status drives behavioral compliance with IHL standards').

omega_variable(
    human_rights_law_integration_boundary,
    'Where exactly does Common Article 3 + human rights law integration produce NEW constraints on state military action that state-centric IHL alone would not impose?',
    'Doctrinal analysis: identify specific detention, interrogation, targeting, and post-conflict accountability rules that emerge only under universal-rights + human rights law synthesis; trace causal pathway to state behavior change or resistance',
    'If human rights integration materially changes state constraint: extractiveness value (0.62) is accurate. If human rights layer is redundant to Common Article 3: ε should be lower (universal-rights reading collapses toward hybrid_proportionality reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_law_integration_boundary, conceptual, 'Structural novelty of human rights law integration into IHL protective scope').

omega_variable(
    kernel_reading_framework_contest,
    'Is this constraint a reading of a single contested kernel (Geneva Conventions'' protective scope), or are universal-rights and state-centric readings incommensurable frameworks with no shared kernel?',
    'Hermeneutic analysis: do both readings claim authority from the same treaty texts (Common Articles, AP I, AP II)? Do they disagree on interpretation or on what counts as valid authority? Do they coexist in practice or actively foreclose each other?',
    'If single kernel: reading_relations and axioms accurately capture structural differences. If incommensurable frameworks: the ''kernel'' is a constructed artifact, and both readings rest on foundational axioms that cannot coexist. This routes to mandate-level redesign of the constraint family structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framework_contest, conceptual, 'Whether universal-rights and state-centric readings contest a single kernel or represent incommensurable interpretive frameworks').

omega_variable(
    civilian_combatant_boundary_contestation,
    'Does universal-rights reading actually resolve the civilian/combatant boundary problem, or does it simply shift the contestation from ''who is a lawful combatant'' to ''who is a protected person''?',
    'Doctrinal and empirical analysis: cases where universal-rights reading clarifies status of contested actors (dual-use personnel, irregular combatants, organized armed groups) vs. cases where the reading creates new boundary disputes over ''affected'' persons',
    'If boundary resolved: universal reading reduces ambiguity and constrains military discretion. If boundary shifted: extractiveness remains high because state military retains interpretation power over ''affected by armed conflict'' scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_combatant_boundary_contestation, empirical, 'Whether universal-rights reading resolves or relocates civilian-combatant boundary contestation').

omega_variable(
    institutional_identity_lock_of_military_authority,
    'Do state military institutions experience universal-rights reading constraints as legitimate legal limits, or as external delegitimization of military necessity and operational judgment?',
    'Institutional analysis: military doctrine evolution; internal resistance/reframing strategies; alignment of actual practices with stated IHL commitment; officer training curriculum on universal-rights interpretation vs. state-centric interpretation',
    'If integrated as legitimate constraint: military institutions model practices on universal-rights reading (true constraint). If experienced as delegitimization: institutions reframe/resist (constraint degrades to piton). Determines whether tangled_rope classification holds from state military perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_of_military_authority, conceptual, 'Whether state military institutions experience universal-rights reading as legitimate constraint or delegitimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_univ_theater_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(geneva_univ_theater_t10, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(geneva_univ_theater_t20, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(geneva_univ_extractiveness_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(geneva_univ_extractiveness_t10, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(geneva_univ_extractiveness_t20, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(geneva_univ_suppression_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(geneva_univ_suppression_t10, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(geneva_univ_suppression_t20, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'Geneva Conventions' protective scope.' The universal-rights reading expands protective scope to all persons and integrates human rights law. The state-centric reading restricts scope to uniformed combatants meeting Article 4 criteria. The hybrid-proportionality reading scales protections by conflict classification. All three are readings of the same contested kernel; each has its own extractiveness value and perspectival classification. Network links show that the universal-rights reading's institutional adoption would constrain the state-centric reading's operational space while absorbing the hybrid reading's conflict-scaling into a unified human-rights framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
