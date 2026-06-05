% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment as Conditioned Individual Right (Civic Militia Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The civic militia reading of the Second Amendment frames the right to
 *   bear arms as a conditioned individual right: persons retain a
 *   constitutional right to firearm ownership only insofar as they meet
 *   militia eligibility criteria and participate in or remain eligible for
 *   civic service. This reading attempts to resolve the interpretive contest
 *   by distinguishing between the right (individual) and its source of
 *   legitimacy (civic participation). Unlike the collective reading (which
 *   grounds the right in state militia authority) or the pure individual
 *   reading (which severs the right from militia service entirely), the civic
 *   reading preserves both: individuals have a right, but that right is
 *   conceptually tied to and gated by civic militia participation. The
 *   constraint exhibits the characteristic structure of Tangled Rope: it
 *   provides genuine coordination function (vetting firearms ownership
 *   through civic commitment) while simultaneously enabling extraction (state
 *   authority to define militia eligibility and exclude populations). The
 *   theater ratio has increased over the interval (0.38→0.52) as the actual
 *   militia institution has become vestigial—the militia framing persists as
 *   legitimating narrative even as the institutional mechanism (actual state
 *   service) has decoupled from firearm ownership practice.
 *
 * KEY AGENTS:
 *   - Militia-Eligible Individuals: Primary beneficiaries (institutional/arbitrage) — receive constitutional protection framed as rooted in civic participation; moderate extraction through eligibility conditioning
 *   - Non-Militia Persons: Primary victims (powerless/trapped) — categorically excluded from protection through fixed, external gating criteria with no exit mechanism
 *   - State Regulatory Authority: Secondary beneficiary (institutional/arbitrage) — gains legitimacy and control mechanism for defining militia eligibility and restricting access to protected class
 *   - Public Safety Commons: Victim (moderate/constrained) — subject to extraction risks through state discretion in eligibility definition; enjoys theoretical safety benefit of civic vetting
 *   - Historical Militia Institution: Vestigial institutional actor (institutional/arbitrage) — maintains performative authority over eligibility while actual militia participation has decoupled from rights exercise
 *   - Constitutional Amendment Coalition: Organized constraint-boundary actor (organized/constrained) — sees the conditionality as revisable through amendment process; has potential sunset exit
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — risks naturalizing the civic conditionality as immutable principle rather than contingent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.48).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment as Conditioned Individual Right (Civic Militia Reading)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c').
narrative_ontology:cs_kernel_codification('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', fixed_text).
narrative_ontology:cs_authority_grounding('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', lineage).
narrative_ontology:cs_interpretation_layer_present('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c').
narrative_ontology:cs_reading_relation('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', foundational, individual_right_grounded_in_civic_participation).
narrative_ontology:cs_axiom_status(individual_right_grounded_in_civic_participation, holdable).
narrative_ontology:cs_axiom_grounding('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', individual_right_grounded_in_civic_participation, deontological).
narrative_ontology:cs_axiom('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', foundational, militia_eligibility_as_constitutional_gate).
narrative_ontology:cs_axiom_status(militia_eligibility_as_constitutional_gate, holdable).
narrative_ontology:cs_axiom_grounding('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', militia_eligibility_as_constitutional_gate, conventional).
narrative_ontology:cs_reference_frame('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', founding_era_militia_conditionality_framework).
narrative_ontology:cs_drift_state('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', contemporary_vestigial_militia, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6d1e66f0-a137-4f8f-8d0b-65644d6e1d5c', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_individuals).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_persons).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, public_safety_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MILITIA PERSONS (SNARE) — Those ineligible for militia participation (felons, immigrants, those failing civic requirements) face categorical exclusion from constitutional protection with no exit mechanism. The gating criterion is fixed and external. Maximum experienced extraction: the constraint defines them structurally outside the protected class with no remedy.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITIA-ELIGIBLE INDIVIDUALS (TANGLED ROPE) — Benefit from constitutional protection and the legitimacy framing that grounds it in civic participation, but face conditioning: genuine militia eligibility/service requirement acts as both coordination mechanism (vetting civic commitment) and extraction mechanism (restricting access to those meeting state criteria). Mixed: coordination function exists (vetting ensures responsible participation) alongside asymmetric extraction (state controls eligibility gates). Constrained exit — could theoretically relocate or forgo firearm ownership but at significant life disruption.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY AUTHORITY (ROPE) — Institutional beneficiary with arbitrage exit (can adjust militia definitions and eligibility criteria, implement alternative regulatory schemes). Experiences the constraint as pure coordination: the civic framing legitimizes state authority to regulate participation, define militia membership, vet participants, and tie rights to service. State maintains full agency — can raise or lower militia eligibility thresholds, define service expectations, adjust regulatory burden. Benefits from the constitutional legitimacy that militia conditioning provides.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC SAFETY COMMONS (TANGLED ROPE) — Generational view. The civic conditioning provides genuine coordination benefit: restricting firearms to those vetted through militia participation is theoretically a safety mechanism. But the extraction component is real: state authority to define 'militia' and 'civic participation' has historically been used to exclude populations (disenfranchised groups, women, minorities), treating public safety as subordinate to maintaining the conditionality gate. Mixed mechanism: coordination (vetting) + asymmetric extraction (whose eligibility counts). Constrained exit — the commons cannot opt out of the regulatory regime.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL MILITIA INSTITUTION (PITON) — The actual militia system (National Guard, state forces) has become largely vestigial relative to the constitutional protection it supposedly grounds. The civic militia framing persists (theater ≈ 0.52) as a legitimating narrative, but the lived militia participation is not the actual mechanism through which Second Amendment rights function. Citizens exercise individual ownership rights independently of state service; the militia conditionality is a theoretical gate that doesn't function as active enforcement. Piton: the militia institution maintains performative authority to define eligibility, but the real mechanism (individual ownership protected by the amendment) has decoupled from civic participation.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AMENDMENT COALITION (SCAFFOLD) — Organized actors (state legislatures, civil society, amendment advocates) see the civic conditioning as a temporary framework with a sunset implicit in constitutional amendment mechanics: if the conditionality ceases to reflect reality (militia participation is no longer the operative mechanism for rights), amendment clarification becomes viable. Sunset logic: the Constitution can be revised to clarify individual rights independent of militia, or to restrict rights to militia-participants, depending on which coalition mobilizes amendment support. Low effective extraction because the organized perspective has exit via amendment process.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the civic militia framing appears as an immutable foundational principle: the right to bear arms is inherently tied to civic participation in collective defense; no individual can claim a right disconnected from communal obligation. This perspective treats the militia condition as a natural law of political philosophy — the essential binding between right and civic duty. However, the structural data contradicts the mountain classification: beneficiaries are identifiable (militia-eligible individuals, state authority), extraction mechanisms are present (eligibility gating, selective exclusion), and the constraint requires active enforcement. The engine will detect this as a false summit — naturalization of a contestable reading.
constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_scope__civic_right_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The civic reading creates extraction through state gatekeeping (who counts as militia-eligible, what service requirements apply), but not severe extraction because the coordination function is genuine—vetting firearms ownership through civic participation is a real coordination mechanism, not purely extractive overhead. The measurement trajectory (0.28→0.38) reflects increasing decoupling between militia service and actual firearm ownership, causing the stated conditionality to become increasingly performative (theater ratio rises). Suppression (0.48): Moderate. Significant barriers to entry (militia eligibility requirements, civic service obligations, potential exclusion criteria) exist and are enforced, but not absolute—some persons can and do satisfy militia eligibility and exercise the right. Suppression is built into the model itself (conditionality gates access). Theater ratio (0.52): Moderate-high. The civic militia framing has become increasingly theatrical as the actual militia institution (state national guards, organized reserves) has become vestigial. Most firearm ownership operates independent of any actual militia participation or service, yet the constitutional framing maintains that participation is the ground of legitimacy. The theater reflects the gap between stated conditionality (militia-based) and actual practice (individual ownership with nominal or nonexistent militia connection).
 *
 * PERSPECTIVAL GAP:
 *   The civic_right_reading exhibits sharp perspectival divergence. Militia-eligible individuals (moderate/constrained) experience Tangled Rope: genuine protection plus real conditioning costs. Non-militia persons (powerless/trapped) experience Snare: categorical exclusion with no remedy. The state (institutional/arbitrage) experiences Rope: pure coordination framing that legitimizes regulatory authority. The public safety commons experiences mixed Tangled Rope: coordination benefit (vetting) alongside extraction risk (state can manipulate eligibility). The historical militia institution (institutional/arbitrage) experiences Piton: maintains performative authority while actual function has atrophied. The organized amendment coalition (organized/constrained) sees Scaffold: a revisable framework with sunset implicit in constitutional amendment mechanics. The analytical observer risks Mountain (false summit): treating civic participation as a natural law of political philosophy rather than a contestable constitutional reading. This perspectival spread—from Snare to Rope to Piton to Mountain—demonstrates why the Second Amendment scope contest remains unresolved: each reading generates empirically coherent but incompatible classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the constraint. Non-militia persons (powerless/trapped) have maximum d (0.95): they bear extraction costs with no exit or arbitrage option. Militia-eligible individuals (moderate/constrained) have moderate-high d (0.60): they benefit from protection but face conditioning costs and state gatekeeping. The state regulatory authority (institutional/arbitrage) has low d (0.15): it is a net beneficiary with multiple exit options (adjust definitions, implement alternative frameworks). The public safety commons (moderate/constrained) has moderate-high d (0.65): it faces extraction risk through state discretion while potentially benefiting from vetting coordination. The analytical observer (analytical/analytical) has derived d (0.73): observing the constraint as a system, the observer perceives the structure's asymmetries without suffering direct extraction. Militia-eligible persons experience the lowest chi despite moderate d because their beneficiary status and the coordination function reduce effective extraction experienced. Non-militia persons experience highest chi: high d combined with powerlessness produces maximum experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The civic_right_reading resolves mandatrophy by accepting Tangled Rope as the core classification while acknowledging that different perspectives legitimately perceive different types. The mandatrophy question—'Is this a coordination mechanism (Rope) or an extraction mechanism (Snare)?'—is answered perspectivally: the state sees Rope (coordination framing), the excluded see Snare (categorical exclusion), and the conditioned-but-eligible see Tangled Rope (mixed). The reading resists collapse to a single type because the civic conditionality genuinely provides coordination function (vetting) while simultaneously enabling extraction (state gatekeeping). This is not instability in the reading; it is the reading's structural signature. The false summit risk (analyzing the conditionality as a natural law rather than a contentious reading) is the actual mandatrophy hazard here: the analytical observer's mountain classification naturalizes what is actually a contingent constitutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_eligibility_definition_ambiguity,
    'What constitutes ''militia participation'' for purposes of Second Amendment protection — formal state service, informal readiness, historical militia membership, all able-bodied persons, or merely eligibility?',
    'Historical analysis of militia service requirements; comparison across state definitions; analysis of how courts have defined ''militia'' in Second Amendment jurisprudence; empirical measurement of actual militia participation rates vs. claimed firearm ownership',
    'Narrow definition (formal service only): ε increases (extraction becomes severe, protection limited to small group). Broad definition (all eligible persons): ε decreases toward rope (protection approaches universal, conditioning becomes nominal). Definition ambiguity allows both camps to claim civic ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_eligibility_definition_ambiguity, conceptual, 'Ambiguous definition of militia participation enables both reading and sibling readings').

omega_variable(
    civic_conditionality_vs_individual_right_coexistence,
    'Can a right be simultaneously individual (belonging to persons) and conditioned (limited to those meeting civic criteria)? Are these conceptually compatible or do they foreclose each other?',
    'Philosophical analysis of right-conditionality logic; comparison to other conditional rights (voting, jury service); examination of whether conditioning on civic participation is distinction-preserving or distinction-destroying',
    'If coexistent: civic_right_reading, individual_right_reading, and collective_right_reading are three legitimate live positions in constitutional discourse. If foreclosing: the readings have logical incompatibility requiring adjudication. If influencing: one reading creates structural pressure on others'' viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_conditionality_vs_individual_right_coexistence, conceptual, 'Logical coherence of individual right + civic conditionality').

omega_variable(
    state_regulatory_capture_risk,
    'Does grounding the right in militia participation create structural opportunity for states to restrict rights by manipulating militia eligibility definitions?',
    'Historical review of state militia definitions and how they have been used to exclude groups (women, minorities, immigrants); analysis of how militia definition changes correlate with right restrictions; comparison to constitutional regimes without militia conditioning',
    'If high capture risk: conditionality is an extraction mechanism masked as coordination (ε increases, snare features emerge). If low risk: conditioning is genuine coordination gating (ε decreases, rope features strengthen). This determines whether the reading itself tends toward snare-like exploitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_regulatory_capture_risk, empirical, 'Risk of state using militia conditionality to restrict rights').

omega_variable(
    contested_kernel_reading_status,
    'Is the civic_right_reading a coherent constitutional position or a hybrid that borrows language from both individual_right_reading and collective_right_reading without establishing its own independent ground?',
    'Textual analysis of Second Amendment language; review of Founding-era militia concept; analysis of how civic_right_reading appears in actual constitutional jurisprudence (is it actively held, aspirational, or a strawman?))',
    'If independent ground: the reading forecloses neither sibling and coexists stably. If hybrid/unstable: the reading may be less viable than the siblings, or may influence both without standing independently. This determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_status, conceptual, 'Coherence and independence of civic_right_reading as constitutional position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(seco_tr_t100, second_amendment_scope__civic_right_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(seco_be_t100, second_amendment_scope__civic_right_reading, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment scope constraint decomposes into three constraint stories corresponding to the three live readings of the kernel. Each reading has its own ε, its own beneficiary/victim structure, and its own classification landscape. civic_right_reading (this file) has ε=0.38 (moderate Tangled Rope). The sibling readings will have different ε values reflecting their different structural claims: individual_right_reading will have lower ε (fewer gatekeeping mechanisms, less extraction), collective_right_reading will have different beneficiary/victim structure (state authority as primary beneficiary, individuals as victims). The three stories are linked via network.affects_constraints to preserve the kernel-reading relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
