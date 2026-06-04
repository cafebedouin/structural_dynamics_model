% ============================================================================
% CONSTRAINT STORY: remedies_article_32__writ_arsenal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remedies_article_32__writ_arsenal_reading, []).

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
 *   constraint_id: remedies_article_32__writ_arsenal_reading
 *   human_readable: Article 32: The Writ Arsenal Reading (Prerogative Writs as Fundamental Rights)
 *   domain: legal/doctrinal/constitutional_remedies
 *
 * SUMMARY:
 *   Article 32 of the Indian Constitution grants a fundamental right to move
 *   the Supreme Court by appropriate proceeding for enforcement of any of the
 *   rights conferred by Part III (Fundamental Rights). The dominant judicial
 *   reading treats Article 32 as carrying five prerogative writs — habeas
 *   corpus, mandamus, prohibition, quo warranto, and certiorari —
 *   nationalized as fundamental-rights enforcement machinery. This reading
 *   emphasizes the completeness and coordination of the writ arsenal: no
 *   single writ forecloses another, petitioners can choose the fitting remedy
 *   for the specific violation, and the machinery exists solely to restore
 *   violated rights. This is a 'pure coordination' reading. The constraint it
 *   models is not the writs themselves (which are tools) but the coordination
 *   function Article 32 provides: a complete toolkit that suppresses
 *   remedy-shopping obstacles and enables petitioners to access the right
 *   remedy without prior knowledge of strict doctrinal categories. This
 *   reading coexists with two sibling readings: the PIL epistolary reading,
 *   which emphasizes how Article 32 was remade through epistolary petitions
 *   and standing relaxation into a movement for voiceless constituencies; and
 *   the PIL overreach critique, which counts the costs of judicial
 *   administration of forests, budgets, and social policy. The
 *   writ_arsenal_reading is the canonical doctrinal reading — it claims
 *   Article 32 works as designed, that the writ arsenal is complete and
 *   appropriately coordinated, and that enforcement of fundamental rights
 *   through the five writs is the constraint's primary function.
 *
 * KEY AGENTS:
 *   - Petitioners / Remedy-Seekers (powerless/mobile): Primary beneficiary — can choose among multiple writs without being trapped by remedy-shopping obstacles; experience pure coordination.
 *   - Victims of Specific Violations (moderate/constrained): Secondary actor — experience the writs as coordination among remedies, though some barriers (literacy, legal knowledge, cost) persist. Suppression is modest (0.35) because the constitutional commitment is explicit and enforced.
 *   - Constitutional Court System (institutional/constrained): Tertiary actor — enforces the writ arsenal, genuinely coordinating access while also benefiting from expanded jurisdictional power. This produces tangled_rope classification (mixed coordination and institutional extraction).
 *   - Formal Writ Doctrine Tradition (institutional/arbitrage): Quaternary actor — maintains the ceremonial and doctrinal form of writs; experiences piton classification (performative persistence amid PIL's creative displacement).
 *   - Analytical Observer (analytical/analytical): Sees the writ arsenal as a pure coordination mechanism with minimal structural extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remedies_article_32__writ_arsenal_reading, 0.28).
domain_priors:suppression_score(remedies_article_32__writ_arsenal_reading, 0.35).
domain_priors:theater_ratio(remedies_article_32__writ_arsenal_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remedies_article_32__writ_arsenal_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(remedies_article_32__writ_arsenal_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(remedies_article_32__writ_arsenal_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remedies_article_32__writ_arsenal_reading, rope).
narrative_ontology:human_readable(remedies_article_32__writ_arsenal_reading, "Article 32: The Writ Arsenal Reading (Prerogative Writs as Fundamental Rights)").
narrative_ontology:topic_domain(remedies_article_32__writ_arsenal_reading, "legal/doctrinal/constitutional_remedies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remedies_article_32__writ_arsenal_reading, 'f43586bb-26c0-440e-b0cb-4f7c6d9579a2').
narrative_ontology:cs_kernel_codification('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', formalized).
narrative_ontology:cs_authority_grounding('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', lineage).
narrative_ontology:cs_interpretation_layer_present('f43586bb-26c0-440e-b0cb-4f7c6d9579a2').
narrative_ontology:cs_reading_relation('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', remedies_article_32__pil_epistolary_reading, coexists_with).
narrative_ontology:cs_reading_relation('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', remedies_article_32__pil_overreach_critique_reading, coexists_with).
narrative_ontology:cs_axiom('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', foundational, five_writs_constitute_complete_remedy_set).
narrative_ontology:cs_axiom_status(five_writs_constitute_complete_remedy_set, holdable).
narrative_ontology:cs_axiom_grounding('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', five_writs_constitute_complete_remedy_set, deontological).
narrative_ontology:cs_axiom('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', foundational, writ_arsenal_pure_coordination).
narrative_ontology:cs_axiom_status(writ_arsenal_pure_coordination, holdable).
narrative_ontology:cs_axiom_grounding('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', writ_arsenal_pure_coordination, conventional).
narrative_ontology:cs_reference_frame('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', prerogative_writs_nationalized_as_fundamental_rights).
narrative_ontology:cs_drift_state('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', contemporary_pil_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f43586bb-26c0-440e-b0cb-4f7c6d9579a2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(remedies_article_32__writ_arsenal_reading, remedies_article_32).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remedies_article_32__writ_arsenal_reading, petitioners_remedy_choice).
narrative_ontology:constraint_beneficiary(remedies_article_32__writ_arsenal_reading, fundamental_rights_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PETITIONER / REMEDY-SEEKER (ROPE) — Experiences Article 32's writ arsenal as pure coordination: the availability of habeas corpus, mandamus, prohibition, quo warranto, and certiorari provides multiple pathways to enforce fundamental rights. No single writ forecloses another; the petitioner can choose the fitting remedy. This is coordination without extraction — the machinery solves the collective action problem of how violated rights get vindicated. The petitioner is not trapped by remedy-shopping obstacles; the arsenal suppresses such obstacles.
constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: VIOLATION VICTIM / REMEDY COORDINATION (ROPE) — An agent whose specific right is violated (unlawful detention, denial of license, arbitrary executive action, defective statutory construction, excess jurisdictional power) experiences Article 32's writ arsenal as coordination among remedies. The victim does not experience extraction — the writs exist to vindicate the victim's claim. The suppression metric reflects that despite the arsenal, some victims face barriers to access (literacy, legal knowledge, resource costs), but the suppression is modest because the constitutional commitment to the writ arsenal is explicit and enforced by courts with no countervailing institutional benefit from non-enforcement.
constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT SYSTEM (TANGLED ROPE) — The judiciary, as institutional enforcer of Article 32, experiences a genuine coordination function (making remedies available to all citizens) alongside institutional extraction (the writ petitions constitute the court's docket, workload, and jurisdictional power; courts have expanded their role in PIL, potentially displacing legislative or administrative processes). The courts genuinely coordinate remedy-seeking while also benefiting from the expanded institutional reach that writ jurisdiction provides. This is not pure coordination (Rope) because the court system has structural incentives to maintain and expand writ availability — extraction and coordination are fused. The suppression (0.35) reflects that the court system faces no serious alternative to enforcing the remedies, yet also faces institutional pressures (docket management, federalism concerns) that moderate the remedies' reach.
constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE FORMAL WRIT RITUAL (PITON) — At the civilizational timescale, Article 32 writs as formally codified remedies appear largely performative. The substantive enforcement of fundamental rights increasingly happens through PIL and ad hoc judicial creativity rather than through strict application of writ doctrines (habeas corpus scope, mandamus discretionary vs. mandatory duties, etc.). The theatrical performance of 'filing a writ petition' persists (the ritual of the writ category, the formal pleadings, the doctrine-based oral arguments), but the actual remedy-granting turns on broader equitable and policy reasoning. The theater_ratio is elevated (0.52) reflecting that the writ form maintains ceremonial authority while enforcement mechanisms have drifted toward PIL principles. This is a piton — the formal writ category remains institutional inertia, maintained because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal perspective, the writ arsenal represents a pure coordination solution to the problem of how fundamental rights get enforced. The five prerogative writs (habeas corpus suppressing unlawful detention, mandamus compelling official duty, prohibition forbidding excess jurisdiction, quo warranto challenging authority, certiorari reviewing high court decisions) form a complete set covering all structural violations of legal authority. No agent extracts benefit from the writ machinery itself — the machinery exists solely to restore violated rights to their baseline. This perspective sees the constraint as a genuine coordination mechanism with minimal structural extraction and no systematic suppression.
constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remedies_article_32__writ_arsenal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remedies_article_32__writ_arsenal_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(remedies_article_32__writ_arsenal_reading, TR),
    TR >= 0.70.

:- end_tests(remedies_article_32__writ_arsenal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate, trending upward over the 50-year interval. The writ arsenal, as a pure coordination mechanism, has minimal inherent extraction — the machinery exists to restore violated rights, not to benefit any agent. However, extractiveness drifts upward from 0.15 (early period, formal writs rigidly applied, narrow standing) to 0.28 (contemporary, PIL expansion, broader standing, judicial discretion in remedy selection). The drift reflects institutional creep: as the court system gains power through expanded writ jurisdiction and PIL creativity, the 'pure coordination' character is compromised by the court's expanded institutional role. Suppression (0.35): Moderate. Despite the constitutional commitment to Article 32 remedies, significant barriers persist: unequal legal literacy, cost of petitioning (filing fees, lawyer costs in High Courts), geographic distance from Delhi for Supreme Court petitions, and knowledge barriers (many potential petitioners don't know which writ applies to their situation). However, suppression is not severe because the constitutional guarantee is explicit, courts actively enforce access, and pro bono legal services exist. Theater ratio (0.52): Moderate. The formal writ categories (habeas corpus, mandamus, prohibition, quo warranto, certiorari) maintain doctrinal and ceremonial significance, but actual remedy-granting increasingly turns on PIL principles (public interest, equitable discretion, policy reasoning) rather than strict writ doctrine. The formal writ ritual persists (the petitions are labeled by writ type, doctrinal arguments are made), but the functional enforcement drifts toward PIL creativity, producing moderate theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival variation across institutional and temporal contexts. The powerless petitioner sees pure coordination (Rope) — the writ arsenal removes remedy-shopping barriers. The moderate violation-victim also sees coordination (Rope) — writs vindicate violated rights. The institutional court system sees mixed coordination and extraction (Tangled Rope) — genuine coordination function, but institutional power grows through expanded jurisdiction. The civilizational observer sees the formal writs as degraded ritual (Piton) — performative persistence amid PIL's functional displacement. The analytical observer at global scope sees pure coordination (Rope) — the writ arsenal is a complete and appropriate toolkit. The gap between the piton and rope perspectives reveals the tension between formal doctrine and PIL practice: the five writs remain doctrinal categories and institutional ritual, but actual enforcement increasingly bypasses strict writ doctrine through PIL principles.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading attributes low to moderate extractiveness and no victim set because it treats Article 32 as pure coordination. The petitioner-beneficiary has mobile exit options (can choose among writs) and experiences low experienced extraction. The victim of the violation (secondary agent) also experiences coordination (the writs are available to vindicate the violated right) rather than extraction. The constitutional court system (institutional perspective) experiences tangled_rope, not rope, because institutional power grows through expanded writ jurisdiction — the court both coordinates remedies and benefits from the expanded role. The analytical observer sees rope (pure coordination), but the piton perspective at civilizational timescale reveals degradation: the formal writ machinery persists through institutional inertia while PIL mechanisms increasingly handle actual remedy-granting. No directionality override needed because the derivation chain (beneficiaries = petitioners, no victims in the pure-coordination reading, low suppression) produces accurate d values from the base structural declaration.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pil_drift_absorption,
    'Does PIL''s epistolary creativity and standing-relaxation absorb remedy-seeking energy that would flow through formal writs, or does PIL coexist with formal writ procedures as complementary pathways?',
    'Corpus analysis of Article 32 petitions: trend in per-capita writ filings post-PIL; analysis of whether PIL petitions cite specific writ doctrines or bypass doctrinal categories; judicial commentary on PIL as supplementing vs. replacing writ jurisdiction',
    'If PIL absorbs: the formal writ arsenal''s functional extractiveness is higher than measured (the coordination function is displaced). If coexistent: extractiveness remains as measured (both pathways function). If PIL forecloses formal writs: reading relationship to pil_epistolary_reading shifts from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pil_drift_absorption, empirical, 'Whether PIL absorption of remedy-seeking energy displaces formal writs').

omega_variable(
    overreach_criteria_ambiguity,
    'Where is the boundary between Article 32 coordination (enforcing existing rights) and PIL overreach (creating new entitlements or administering policy)? Is the boundary epistemic (what counts as a justiciable remedy) or institutional (what the courts actually do)?',
    'Comparative analysis of PIL cases that critics cite as overreach: do they derive from Article 32 writ doctrines (suggesting the arsenal''s scope permits the extension) or from PIL''s extra-constitutional creativity (suggesting overreach is orthogonal to the writ reading)? Courts'' own demarcation logic.',
    'If epistemic boundary: Article 32 writs themselves permit PIL extension, and the writ_arsenal_reading coexists naturally with pil_overreach_critique_reading. If institutional boundary: overreach is a fact about court behavior, not about the writ arsenal as formal mechanism — the reading relationship remains coexists_with but the critique does not attach to Article 32 itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overreach_criteria_ambiguity, conceptual, 'Boundary between writ coordination and PIL overreach').

omega_variable(
    writ_arsenal_completeness,
    'Does the five-writ set genuinely cover all structural violations of legal authority, or do gaps exist where no single writ provides a complete remedy?',
    'Systematic mapping of violation types (unlawful detention, denial of statutory entitlement, abuse of discretion, jurisdictional excess, procedural unfairness) to writs; identification of cases where petitioner had to file multiple writs or combine writs with other remedies',
    'If complete: the writ arsenal validates the rope classification (pure coordination, no remedy-shopping extraction). If gaps exist: extractiveness rises (petitioners must incur multiple filings or coordinate remedies), and classification shifts toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(writ_arsenal_completeness, empirical, 'Whether five-writ set completely covers all justiciable violations').

omega_variable(
    contested_kernel_ambiguity,
    'Is Article 32 fundamentally a ''writ arsenal'' (coordinating multiple specific remedies) as this reading holds, an ''epistolary gateway'' (democratized access to judicial relief) as PIL reading holds, or a ''overreach risk'' (expanded beyond legitimate adjudication) as the critique holds? Or does the constitutional text permit all three readings as simultaneously valid?',
    'Doctrinal analysis of Article 32''s language and constitutional history; comparison with predecessor Anglo-Indian writ jurisprudence vs. PIL-era judicial reasoning; whether courts themselves recognize these as distinct readings or treat them as variations on a single principle',
    'If simultaneously valid: all three readings coexist_with each other. If one reading''s core premise forecloses another: reading relations change to forecloses. If readings are institutional positions held by different actors: coexists_with confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_ambiguity, conceptual, 'Whether Article 32 kernel admits three distinct readings or one unfolding practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remedies_article_32__writ_arsenal_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(writ_arsenal_theater_t0, remedies_article_32__writ_arsenal_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(writ_arsenal_theater_t25, remedies_article_32__writ_arsenal_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(writ_arsenal_theater_t50, remedies_article_32__writ_arsenal_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(writ_arsenal_extractiveness_t0, remedies_article_32__writ_arsenal_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(writ_arsenal_extractiveness_t25, remedies_article_32__writ_arsenal_reading, base_extractiveness, 25, 0.24).
narrative_ontology:measurement(writ_arsenal_extractiveness_t50, remedies_article_32__writ_arsenal_reading, base_extractiveness, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remedies_article_32__writ_arsenal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remedies_article_32__writ_arsenal_reading, remedies_article_32__pil_epistolary_reading).
narrative_ontology:affects_constraint(remedies_article_32__writ_arsenal_reading, remedies_article_32__pil_overreach_critique_reading).

% DUAL FORMULATION NOTE:
% The Article 32 kernel admits three structurally distinct readings, each modeling a different constraint. The writ_arsenal_reading treats Article 32 as a pure coordination mechanism (Rope) with low extractiveness (0.28). The pil_epistolary_reading treats Article 32 as a democratized gateway to judicial relief through epistolary accessibility and standing relaxation (higher extractiveness reflecting institutional expansion). The pil_overreach_critique_reading treats Article 32 as a constraint whose expanded application risks displacing legislative and administrative functions (highest extractiveness reflecting institutional extraction). All three share the same kernel (Article 32 text and institutional role) but instantiate different constraints because they emphasize different aspects of the mechanism and effects. The ε-invariance principle applies: these are not 'different measurements' of one constraint; they are structurally distinct claims with different ε values, different beneficiary/victim sets, and different temporal trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
