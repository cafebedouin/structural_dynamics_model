% ============================================================================
% CONSTRAINT STORY: basic_rights_catalog__objective_values_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_rights_catalog__objective_values_order, []).

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
 *   constraint_id: basic_rights_catalog__objective_values_order
 *   human_readable: Rights Catalog as Objective Order of Values (Lüth Doctrine)
 *   domain: constitutional_law/private_law
 *
 * SUMMARY:
 *   The Lüth decision (1958) established that the German Basic Law's catalog
 *   of fundamental rights does not confine rights to the vertical relation
 *   between citizen and state, but instead radiates into private law —
 *   governing every legal relation, including contracts, employment,
 *   landlord-tenant, and creditor-debtor relations. This reading instantiates
 *   the objective values order interpretation: the rights catalog is not a
 *   bill of negative liberties against the state, but a material constitution
 *   of all social ordering. The constraint this creates is genuinely hybrid —
 *   it coordinates private law around rights principles while extracting from
 *   the autonomy of private actors, especially the powerful. The doctrine
 *   suppresses one alternative (pure private ordering unmediated by rights),
 *   benefiting rights-bearers in weaker positions but constraining powerful
 *   private actors. From different structural positions, the same doctrinal
 *   content appears as natural law (the analytical observer's view), as pure
 *   extraction (the weaker party's view), as coordination (the constitutional
 *   court's view), as degraded ritual (the private law academy's view), and
 *   as a constraint to organize against (the pure autonomy coalition's view).
 *
 * KEY AGENTS:
 *   - Weaker Parties in Private Relations: Primary victims (powerless/trapped) — historically unconstrained by rights doctrine, now nominally protected but enforcement is slow and costly
 *   - Rights-Bearing Public: Primary beneficiaries (moderate/constrained) — gain justiciable protection in horizontal relations but must pursue costly litigation; organized rights advocates (organized/constrained) drive enforcement
 *   - Constitutional Court: Institutional beneficiary (institutional/arbitrage) — doctrine expands jurisdiction and provides coherent framework for reviewing private law
 *   - Powerful Private Actors: Secondary victims (powerful/constrained) — contractual freedom and operational discretion constrained by radiating rights; organized defense through business associations
 *   - Pure Private Autonomy Advocates: Organized opposition (organized/constrained) — legal scholars, libertarian doctrine, contract-freedom positions work toward doctrinal reversal or limits on horizontal application
 *   - Traditional Private Law Academy: Institutional degradation (institutional/arbitrage) — positivist frameworks persist performatively despite structural subordination to constitutional values
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the objective values order as inherent law when it is a contingent doctrinal choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_rights_catalog__objective_values_order, 0.38).
domain_priors:suppression_score(basic_rights_catalog__objective_values_order, 0.52).
domain_priors:theater_ratio(basic_rights_catalog__objective_values_order, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_rights_catalog__objective_values_order, extractiveness, 0.38).
narrative_ontology:constraint_metric(basic_rights_catalog__objective_values_order, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_rights_catalog__objective_values_order, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_rights_catalog__objective_values_order, tangled_rope).
narrative_ontology:human_readable(basic_rights_catalog__objective_values_order, "Rights Catalog as Objective Order of Values (Lüth Doctrine)").
narrative_ontology:topic_domain(basic_rights_catalog__objective_values_order, "constitutional_law/private_law").

domain_priors:requires_active_enforcement(basic_rights_catalog__objective_values_order).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_rights_catalog__objective_values_order, '4b3f9570-b863-4de9-866c-003212a39511').
narrative_ontology:cs_kernel_codification('4b3f9570-b863-4de9-866c-003212a39511', formalized).
narrative_ontology:cs_authority_grounding('4b3f9570-b863-4de9-866c-003212a39511', lineage).
narrative_ontology:cs_interpretation_layer_present('4b3f9570-b863-4de9-866c-003212a39511').
narrative_ontology:cs_reading_relation('4b3f9570-b863-4de9-866c-003212a39511', basic_rights_catalog__essence_guarantee, coexists_with).
narrative_ontology:cs_reading_relation('4b3f9570-b863-4de9-866c-003212a39511', basic_rights_catalog__informational_self_determination, coexists_with).
narrative_ontology:cs_reading_relation('4b3f9570-b863-4de9-866c-003212a39511', basic_rights_catalog__proportionality_doctrine, influences).
narrative_ontology:cs_axiom('4b3f9570-b863-4de9-866c-003212a39511', foundational, rights_radiate_horizontally).
narrative_ontology:cs_axiom_status(rights_radiate_horizontally, holdable).
narrative_ontology:cs_axiom_grounding('4b3f9570-b863-4de9-866c-003212a39511', rights_radiate_horizontally, deontological).
narrative_ontology:cs_axiom('4b3f9570-b863-4de9-866c-003212a39511', foundational, fundamental_rights_constrain_private_autonomy).
narrative_ontology:cs_axiom_status(fundamental_rights_constrain_private_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4b3f9570-b863-4de9-866c-003212a39511', fundamental_rights_constrain_private_autonomy, deontological).
narrative_ontology:cs_reference_frame('4b3f9570-b863-4de9-866c-003212a39511', private_law_vertical_rights_framework).
narrative_ontology:cs_drift_state('4b3f9570-b863-4de9-866c-003212a39511', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b3f9570-b863-4de9-866c-003212a39511', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(basic_rights_catalog__objective_values_order, basic_rights_catalog).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_rights_catalog__objective_values_order, constitutional_rights_holders).
narrative_ontology:constraint_beneficiary(basic_rights_catalog__objective_values_order, weaker_private_parties).
narrative_ontology:constraint_victim(basic_rights_catalog__objective_values_order, pure_private_autonomy_doctrine).
narrative_ontology:constraint_victim(basic_rights_catalog__objective_values_order, contractual_freedom_absolutism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAKER PARTY (SNARE) — Trapped in private contractual relations where the stronger party (employer, landlord, creditor) has historically been unconstrained by rights doctrine. The rights catalog's radiation into private law creates nominal protection but enforces through constitutional courts, not immediate mechanism. Suppression remains high: the weaker party cannot exit the relation without severe cost, and judicial remedies are slow and contingent. Maximum experienced extraction from asymmetric private power.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL RIGHTS ADVOCATES (TANGLED ROPE) — Benefit from the objective values order doctrine — rights are now justiciable in private disputes, extending the protection apparatus beyond state action. But also constrained: judicial enforcement remains costly, doctrine is contested, and private actors (especially wealthy enterprises) can absorb litigation costs that individual rights-bearers cannot. Genuine coordination function (horizontal rights enforcement) coupled with asymmetric extraction (litigation burden, access barriers).
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (ROPE) — Benefits from the doctrine of objective values: the rights catalog provides a coherent framework for reviewing private law, expanding the court's jurisdiction and interpretive authority. The constraint serves coordination: clarifying which rights apply in horizontal relations and how to balance them. The court experiences this as pure coordination, not extraction — the doctrine solves an adjudicatory problem.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POWERFUL PRIVATE ACTOR (TANGLED ROPE) — Experiences the objective values order as an extraction mechanism: rights constraints on contractual freedom reduce profit margins and operational discretion. But also benefits from coordination: the doctrine provides settled rules for balancing rights, reducing uncertainty about which private-law claims will be upheld. The powerful actor is constrained (cannot ignore rights in drafting contracts) but has organizational resources to navigate compliance. Experienced extraction is lower than for weaker parties but real.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PURE AUTONOMY ADVOCATES (SCAFFOLD) — Organized actors (libertarian legal scholars, business associations, contract-freedom advocates) see the objective values order as a temporary constraint on their preferred framework of contractual freedom. They work toward a sunset: a doctrine-shift that reinstates pure private ordering unmediated by rights. Their exit is organized action (jurisprudential argument, legislative reform, transnational arbitration) — medium effective extraction because they have organized capacity.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PRIVATE LAW POSITIVISTS (PITON) — The traditional view that private law is a realm of autonomous ordering, bracketed from constitutional rights, persists through institutional inertia. Law schools teach private law as a self-contained system; judges cite positivist doctrine even while applying rights constraints. The old framework is maintained performatively even as its functional authority has degraded — courts now routinely impose rights constraints, but doctrine pretends private law remains autonomous. Theater ratio high: the ritual of 'autonomous private ordering' continues despite structural subordination to constitutional values.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some constraints on contractual freedom are inherent to rights-bearing systems: if agents possess fundamental rights by nature or transcendent principle, those rights cannot be contracted away absolutely. This perspective sees the objective values order as discovering an immutable principle, not constructing a contingent doctrine. However, structural data contradicts this — beneficiaries (rights-holders) and victims (pure autonomy doctrine) are identifiable, making this a false summit: the 'inherent to rights systems' framing naturalizes a doctrinal choice.
constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_rights_catalog__objective_values_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_rights_catalog__objective_values_order, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(basic_rights_catalog__objective_values_order, TR),
    TR >= 0.70.

:- end_tests(basic_rights_catalog__objective_values_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The objective values order does extract from private autonomy — contractual freedom is constrained, operational discretion is limited, compliance costs are imposed on private actors. But extraction is not severe: the doctrine serves a genuine coordination function (clarifying how rights apply in private law), provides settled rules that reduce uncertainty, and is justified by rights protection (not arbitrary rent-seeking). The extractiveness has grown over 20 years (0.22 → 0.38) as jurisprudence has expanded the scope of radiating rights and courts have become more aggressive in imposing rights constraints. Suppression (0.52): Moderate-high. Alternatives to the objective values order exist (pure autonomy doctrine, proportionality-first approach, essence-protection framework) but are suppressed through doctrinal authority and constitutional court enforcement. Pure autonomy doctrine is not legally prohibited — it exists as a theoretical position — but cannot win in constitutional adjudication. The suppression is structural rather than coercive: doctrine constrains what judges can legally say and decide. Theater ratio (0.48): Moderate. The constraint requires genuine enforcement through constitutional courts, creating real friction in private law. But some performative element exists: private law positivism continues as ritual (law schools teach autonomous private ordering, judges cite autonomous private law doctrine) even while courts impose rights constraints. The theater ratio has risen (0.35 → 0.48) as the gap between the performative autonomy doctrine and actual constitutional constraint has widened.
 *
 * PERSPECTIVAL GAP:
 *   The objective values order produces radical perspectival divergence. The weaker party sees extraction with minimal coordination benefit — courts are slow, litigation is costly, and the constraint depends on the weaker party's ability to access justice. The constitutional court sees pure coordination — the doctrine solves the problem of how rights apply in private disputes. The powerful private actor sees asymmetric constraint — their freedom is limited while weaker parties cannot effectively exercise their new rights. The pure autonomy coalition sees a temporary constraint they can organize against — their exit involves legal reform, jurisdictional arbitrage, or doctrinal shift. The private law academy sees degraded ritual — the old autonomy doctrine persists performatively while its functional authority has collapsed. The analytical observer sees natural law — the catalog's structure inherently radiates rights — but this is revealed as a false summit by the identifiable beneficiaries and victims. Each perspective emerges from a distinct structural position and none is 'wrong' — they collectively reveal the hybrid nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The objective values order's directionality flows from the rights catalog's material structure toward beneficiaries (rights-holders in weaker positions) and away from victims (pure autonomy doctrine). The powerful private actor is constrained (high d, high experienced extraction) but retains significant organizational resources. The weaker party benefits nominally but cannot extract value (trapped in horizontal relation, cannot exit to assert rights without cost). The court benefits from expanded authority and settled doctrine. The pure autonomy coalition is suppressed but organized (can push back through legal scholarship and political reform). Directionality is not uniform — it differentiates by power level, exit options, and structural position relative to the radiating rights.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy through doctrinal positioning. The objective values order is not coordination disguised as extraction, nor extraction disguised as coordination. It is genuinely hybrid: the doctrine coordinates private law around rights principles (real coordination function) while extracting from contractual autonomy and private power (real extraction). The mandatrophy resolution is perspectival: from the court's position it appears as coordination; from the powerful actor's position it appears as extraction; from the weaker party's position it appears as nominal protection with limited enforcement. The constraint is not misclassified as Rope when it should be Snare — it is correctly classified as Tangled Rope because both the coordination and extraction functions are structurally real, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_doctrine_grounding,
    'Does the objective values order derive from the rights catalog''s essential structure, or does it represent a doctrinal choice by the court that could have decided differently?',
    'Comparative constitutional law analysis: how other jurisdictions (US, Canada, Australia) handle rights in private law; documentary evidence of Lüth court''s reasoning and acknowledged alternatives; counterfactual: what would private law look like under a pure autonomy doctrine without radiating rights',
    'If essential: mountain classification holds (true natural law). If doctrinal choice: false summit confirmed; Tangled Rope / Snare / Rope are correct depending on observer position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_doctrine_grounding, conceptual, 'Whether objective values order is essential or chosen doctrine').

omega_variable(
    horizontal_enforcement_effectiveness,
    'Do constitutional courts actually enforce rights radiating into private law, or is the doctrine performative while real power remains with private ordering?',
    'Empirical study of constitutional court decisions in private-law disputes: what percentage of rights claims succeed; time-to-resolution and cost; comparison of outcomes for well-resourced vs under-resourced parties; longitudinal change in private-law jurisprudence post-Lüth',
    'If effective: doctrine achieves real suppression of unlimited private power (Tangled Rope / Rope correct). If performative: theater ratio rises, classification shifts toward Piton, and the constraint becomes degraded ritual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_enforcement_effectiveness, empirical, 'Whether constitutional courts enforce rights in private disputes or doctrine is largely performative').

omega_variable(
    pure_autonomy_doctrine_viability,
    'Is pure private autonomy doctrine (the victim set in this reading) still a live jurisprudential position, or has it been foreclosed by the objective values order?',
    'Doctrinal survey of current legal scholarship and appellate decisions; identification of any remaining jurisdictions or doctrinal contexts where pure autonomy persists; evidence of attempted jurisprudential reversal',
    'If live: coexists_with relationship to pure autonomy advocates is correct (different factions hold competing readings). If foreclosed: forecloses relationship applies — objective values order is the only tenable framework within this constitutional tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pure_autonomy_doctrine_viability, conceptual, 'Whether pure private autonomy doctrine remains live or has been foreclosed').

omega_variable(
    rights_catalog_content_determinacy,
    'Does the rights catalog itself determine how rights radiate into private law, or is the radiation doctrine a second-order interpretive layer that could instantiate different rights selections?',
    'Analysis of which rights are treated as radiating (dignity, property, freedom of contract, personality) and which are not (political rights, social benefits); evidence of doctrine''s gatekeeping function; comparison to other frameworks that would expand or contract the set of radiating rights',
    'If determinative: the catalog''s content fixes which private relations are constrained (specificity). If interpretive layer: doctrine leaves room for competing readings about scope of horizontal application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_catalog_content_determinacy, conceptual, 'Whether rights catalog content determines radiation or radiation is independent interpretive layer').

omega_variable(
    sibling_reading_kernel_contest,
    'Which sibling reading of the basic_rights_catalog kernel is structurally dominant in current jurisprudence: essence_guarantee (the untouchable core), informational_self_determination (the new right minted from old text), objective_values_order (this reading), or proportionality_doctrine (the balancing method)?',
    'Docket analysis of constitutional court cases citing each doctrine; measurement of citation frequency and doctrinal priority; analysis of how courts resolve conflicts when two readings point to different outcomes',
    'Determines which reading is the ''master'' frame and how the others are subordinated. High dominance = this reading''s axioms are unchallengeable; low dominance = sibling readings remain coordinate live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, empirical, 'Structural dominance of objective_values_order among sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_rights_catalog__objective_values_order, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ovo_tr_t0, basic_rights_catalog__objective_values_order, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ovo_tr_t10, basic_rights_catalog__objective_values_order, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ovo_tr_t20, basic_rights_catalog__objective_values_order, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ovo_be_t0, basic_rights_catalog__objective_values_order, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ovo_be_t10, basic_rights_catalog__objective_values_order, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(ovo_be_t20, basic_rights_catalog__objective_values_order, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_rights_catalog__objective_values_order, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_rights_catalog__objective_values_order, basic_rights_catalog__essence_guarantee).
narrative_ontology:affects_constraint(basic_rights_catalog__objective_values_order, basic_rights_catalog__informational_self_determination).
narrative_ontology:affects_constraint(basic_rights_catalog__objective_values_order, basic_rights_catalog__proportionality_doctrine).

% DUAL FORMULATION NOTE:
% The objective_values_order reading is part of a constraint family (basic_rights_catalog kernel with four sibling readings). All siblings share the same kernel (the rights catalog) but instantiate structurally different constraints because they emphasize different aspects of the catalog's doctrinal authority. The objective_values_order reading emphasizes radiating values into private law; essence_guarantee emphasizes untouchable core; informational_self_determination emphasizes discovery of new rights; proportionality_doctrine emphasizes balancing method. Each has its own ε value and perspectives. The network links document how interpretations compete and influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_rights_catalog__objective_values_order, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
