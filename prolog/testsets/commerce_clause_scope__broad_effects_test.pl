% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Scope: Broad Effects Test (Federal Aggregation Doctrine)
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   The Commerce Clause scope constraint models how a single constitutional
 *   text can be interpreted to extract federal power over state sovereignty
 *   through an aggregation doctrine. Under the broad effects test, virtually
 *   any economic activity can be federal commerce regulation if its effects
 *   on interstate commerce, aggregated across similar actors, are
 *   'substantial.' This reading crystallized in Wickard v. Filburn (wheat for
 *   personal consumption affects interstate wheat markets through
 *   aggregation) and became the dominant doctrine by mid-20th century,
 *   enabling federal civil rights enforcement, environmental regulation, and
 *   labor standards that would otherwise remain state matters. The constraint
 *   exhibits the full range of DR classifications: state legislators see a
 *   snare (trapped by aggregation), states see tangled rope (mixed
 *   coordination and extraction), federal regulators see rope (coordination
 *   benefit), civil rights coalitions see rope (enables justice enforcement),
 *   businesses see tangled rope (mixed), and analytical observers risk seeing
 *   natural law (aggregation is inevitable in modern economies). The false
 *   summit risk is substantial: the doctrine is frequently framed as
 *   necessary consequence of economic integration rather than as a contingent
 *   interpretive choice that subordinates federalism to federal regulatory
 *   capacity.
 *
 * KEY AGENTS:
 *   - Federal Regulatory Apparatus: Primary beneficiary (institutional/arbitrage) — captures authority over all economic regulation via aggregation doctrine; can override state autonomy when interstate effect is demonstrated
 *   - State Legislatures: Primary victim (powerless/trapped) — subject to federal override; cannot rely on local regulatory autonomy when economic characterization is available
 *   - State Governments (Institutional): Secondary institutional actor (organized/constrained) — see mixed benefit (interstate commerce coordination) and extraction (subordination to federal authority)
 *   - Civil Rights Enforcement Agencies: Secondary beneficiary (organized/arbitrage) — federal commerce power enabled civil rights enforcement where states would not act
 *   - National Coalitions Seeking Uniform Policy: Secondary beneficiary (organized/arbitrage) — business groups, environmental coalitions, labor unions benefit from federal standard-setting via commerce clause
 *   - Federalism as Structural Principle: Victim (powerless/trapped) — the constitutional constraint limiting federal power is itself subordinated by the broad effects reading
 *   - Analytical Observer: (analytical/analytical) — risks treating aggregation logic as natural necessity rather than interpretive doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.58).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.72).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Scope: Broad Effects Test (Federal Aggregation Doctrine)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'c9dc3fa3-6151-4df2-b98d-91cee557c321').
narrative_ontology:cs_kernel_codification('c9dc3fa3-6151-4df2-b98d-91cee557c321', formalized).
narrative_ontology:cs_authority_grounding('c9dc3fa3-6151-4df2-b98d-91cee557c321', lineage).
narrative_ontology:cs_interpretation_layer_present('c9dc3fa3-6151-4df2-b98d-91cee557c321').
narrative_ontology:cs_reading_relation('c9dc3fa3-6151-4df2-b98d-91cee557c321', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('c9dc3fa3-6151-4df2-b98d-91cee557c321', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('c9dc3fa3-6151-4df2-b98d-91cee557c321', foundational, aggregation_nexus_suffices).
narrative_ontology:cs_axiom_status(aggregation_nexus_suffices, holdable).
narrative_ontology:cs_axiom_grounding('c9dc3fa3-6151-4df2-b98d-91cee557c321', aggregation_nexus_suffices, empirically_contingent).
narrative_ontology:cs_axiom('c9dc3fa3-6151-4df2-b98d-91cee557c321', secondary, federalism_subordinate_to_commerce_coordination).
narrative_ontology:cs_axiom_status(federalism_subordinate_to_commerce_coordination, holdable).
narrative_ontology:cs_axiom_grounding('c9dc3fa3-6151-4df2-b98d-91cee557c321', federalism_subordinate_to_commerce_coordination, deontological).
narrative_ontology:cs_reference_frame('c9dc3fa3-6151-4df2-b98d-91cee557c321', integrated_national_commerce).
narrative_ontology:cs_drift_state('c9dc3fa3-6151-4df2-b98d-91cee557c321', contemporary_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('c9dc3fa3-6151-4df2-b98d-91cee557c321', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_policy_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_experimentation_capacity).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_structural_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE LEGISLATOR / LOCAL AUTONOMY (SNARE) — Trapped by aggregation doctrine. Any local economic regulation, no matter how locally motivated, can be recharacterized as affecting interstate commerce in the aggregate. Exit options zero: the federal commerce power subsumes local autonomy whenever economic effects are claimed and demonstrated through economic data. Maximum suppression — the fed can always reframe local regulation as interstate commerce regulation.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENT (TANGLED ROPE) — Constrained but not entirely trapped. States retain some police powers and can regulate if they can show non-economic primary purposes (rational basis scrutiny). But the broad effects test shifts the burden: federal authority can overtake state regulation if the economic nexus chain is established. States do benefit from federal coordination in interstate commerce regulation — they avoid the tragedy of commons in setting tariffs or discriminatory regulations. But the constraint extracts state sovereignty asymmetrically: coordination benefit is shared; extraction flows toward federal authority.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL REGULATORY APPARATUS (ROPE) — Benefits from the broad effects test as a coordination mechanism for interstate commerce. The aggregation doctrine solves genuine collective action problems: states cannot unilaterally prevent interstate commerce barriers, and interstate competition in regulation creates races to the bottom in environmental and labor standards. Federal authority coordinates these problems. The constraint appears to the federal regulator as legitimate coordination, not extraction — though the beneficiary status is clear.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONAL CIVIL RIGHTS COALITIONS (ROPE) — Beneficiary of federal commerce power aggregation. The broad effects test enabled federal civil rights enforcement under the Commerce Clause when state legislatures would not act (Heart of Atlanta Motel, Katzenbach v. McClung). The constraint solves the coordination problem of state-level discrimination in interstate commerce. Civil rights coalitions see this as legitimate coordination, even though they benefit from the federal override of state autonomy.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSTATE COMMERCE PARTICIPANT / BUSINESS (TANGLED ROPE) — Mixed experience. Broad effects test provides coordination benefit: uniform federal standards reduce compliance fragmentation. But businesses operating primarily intrastate face extraction — they can be regulated under federal commerce power despite minimal interstate footprint if aggregation doctrine applies. Exit options are constrained: they cannot simply go local without federal commerce jurisdiction following them through the economic nexus chain.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — Sees the broad effects test as natural law: given modern economies with integrated national commerce, any substantial economic activity has interstate effects. The aggregation doctrine is inherent to regulating a modern national economy. Treating this as natural law (mountain) risks naturalizing what is actually a contingent interpretive choice about how to measure 'substantial effect.' This perspective will likely classify as false-summit — the structural data shows beneficiaries and asymmetric extraction, contradicting the natural-law framing.
constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_scope__broad_effects_test, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The broad effects test extracts federal regulatory authority over domains that would otherwise remain state matters. The extraction is not total (states retain police powers for non-economic regulation) but is substantial. The measurement trajectory shows rise from 0.35 (pre-1937) to peak of 0.62 (late 20th century) settling at 0.58 (contemporary), reflecting the doctrine's entrenchment and persistent federalism critiques. Suppression (0.72): High. The doctrine requires suppression of federalism objections. States have no meaningful exit option from the aggregation calculus — the fed can always point to interstate commerce nexus chains. The suppression is structural: it operates through the doctrinal logic itself, not merely through enforcement. Theater ratio (0.65): Moderate-high. Courts perform rational basis scrutiny in commerce cases, reliably finding 'substantial effect' on interstate commerce. The ritual of articulating the aggregation logic has become largely performative — the outcome (upholding federal regulation) is rarely in doubt. The measurement shows rise from 0.40 (substantive federalism debate pre-1937) to 0.68 (peak theater in Civil Rights era) to 0.65 (contemporary stable level).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single constitutional doctrine can appear as natural law (analytical mountain), coordination mechanism (federal rope), legitimate power extraction (institutional rope for beneficiaries), mixed coordination-extraction (tangled rope for states), and pure extraction (snare for local autonomy). The gap reveals the doctrine's dual nature: it solves genuine interstate commerce coordination problems (the rope function) while simultaneously extracting state sovereignty (the snare function). The false summit risk is particularly acute because the coordination function is real — the broad effects test did solve the tragedy-of-commons problem in interstate regulation — making it easy to frame as natural law. But the extraction function is equally real — state legislatures are genuinely trapped by the aggregation doctrine's measurement logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory apparatus (institutional/arbitrage) derives d ≈ 0.15 (beneficiary with strong exit options — can always frame new regulations under commerce clause without constraint). State legislatures (powerless/trapped) derive d ≈ 0.95 (victims with zero exit options — aggregation logic subsumes all intrastate activity). State governments as institutional actors (organized/constrained) derive d ≈ 0.55 (mixed: they benefit from interstate coordination but are constrained by federal override). Civil rights coalitions (organized/arbitrage) derive d ≈ 0.20 (beneficiaries; aggregation doctrine enabled enforcement). The perspectival gap follows from these d values: low d produces rope classification; high d produces snare; middle d produces tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that all six classifications are perspectivally correct. The constraint simultaneously coordinates interstate commerce AND extracts state sovereignty. The beneficiaries genuinely experience coordination benefit (federal civil rights enforcement solved state-level discrimination problems that markets alone could not address). The victims genuinely experience extraction (local autonomy subordinated to federal aggregation logic). The analytical observer risks mandatrophy by trying to determine which is the 'real' nature of the constraint — but the real answer is that the constraint is a tangled rope: genuine coordination layered with asymmetric extraction. The false summit classification for the analytical observer is the diagnostic signal that the doctrine is frequently naturalized as necessary to modern economics, when it is actually a contingent constitutional choice about how to balance federalism and federal regulatory capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_effects_measurement_ambiguity,
    'What constitutes ''substantial effect'' on interstate commerce? Is the threshold empirically determined or interpretively constructed by federal courts?',
    'Historical analysis of Commerce Clause cases: correlation between economic magnitude of claimed effect and judicial outcomes. Does the same effect-size produce consistent classifications across eras and issue domains?',
    'If empirically determined and consistent: the broad effects test is a discoverable structural fact (supports mountain classification). If interpretively constructed and variable: the test is a doctrine that changes based on federal court policy preferences (supports snare/tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantial_effects_measurement_ambiguity, empirical, 'Whether ''substantial effects'' is an objective threshold or interpretive doctrine').

omega_variable(
    aggregation_doctrine_necessity,
    'Is the aggregation logic (cumulative effects of individual intrastate actions) necessary to regulate modern interstate commerce, or is it a choice among possible constitutional doctrines?',
    'Comparative constitutional analysis: how do other federal systems (EU, Canada, Australia) handle comparable federalism tensions without equivalent aggregation doctrines? Counterfactual analysis: could pre-1942 commerce power (requiring direct substantial effect) still govern modern economies?',
    'If necessary: supports mountain or rope classification (structural inevitability). If contingent choice: supports tangled_rope/snare classification (reveals the doctrine as a power allocation choice, not a natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_necessity, conceptual, 'Whether aggregation doctrine is structurally necessary or a contingent interpretive choice').

omega_variable(
    federalism_as_competing_reading,
    'The narrow_originalist reading invokes federalism as a structural principle limiting commerce power. Does the broad_effects_test logically foreclose federalism-based limits, or do the two coexist as live doctrinal positions held by different courts/eras?',
    'Historical jurisprudence: trace cases where both readings have held force simultaneously. Identify whether the switch from narrow to broad effects test was a logical resolution or a political/institutional shift in judicial philosophy.',
    'If forecloses: the broad effects test is incompatible with federalism doctrine (the kernel reading itself contains a contradiction requiring resolution). If coexists: both readings remain live, and the kernel is genuinely contested (current state of play).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_as_competing_reading, conceptual, 'Whether broad effects doctrine logically forecloses federalism-based limits or whether both coexist as live readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is the broad effects test presented as a natural consequence of modern economics (suggesting mountain classification), or is it recognized as a discretionary constitutional doctrine?',
    'Jurisprudential analysis: examine whether courts frame the doctrine as ''what the Constitution requires given modern commerce'' vs ''our interpretive choice about how to balance federalism and national regulation.'' Compare rhetoric in early (Wickard, Filburn) vs contemporary cases.',
    'If courts present as natural law: the false-summit detection signature will identify beneficiaries and flag for reclassification. If courts acknowledge discretion: the tangled_rope classification is structurally transparent, not a cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether courts frame broad effects as natural law or as interpretive doctrine').

omega_variable(
    reading_boundary_with_intermediate_channels,
    'The intermediate_channels reading likely invokes a middle path: some intrastate activities can be regulated via commerce power based on channels/instrumentalities logic, without committing to full aggregation. Where is the boundary between broad_effects_test and intermediate_channels?',
    'Doctrinal comparison: identify which cases each reading would classify identically and which it would classify differently. Locate the empirical threshold (effect-size, proximity to interstate nexus) where intermediate_channels diverges from broad_effects.',
    'If clear boundary exists: the two readings can coexist with differentiated application domains. If boundary is blurry: the readings are not distinct enough to model as separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_with_intermediate_channels, conceptual, 'Boundary between broad effects and intermediate channels readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccs_broad_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ccs_broad_tr_t1, commerce_clause_scope__broad_effects_test, theater_ratio, 1, 0.55).
narrative_ontology:measurement(ccs_broad_tr_t2, commerce_clause_scope__broad_effects_test, theater_ratio, 2, 0.68).
narrative_ontology:measurement(ccs_broad_tr_t3, commerce_clause_scope__broad_effects_test, theater_ratio, 3, 0.65).
narrative_ontology:measurement(ccs_broad_tr_t4, commerce_clause_scope__broad_effects_test, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ccs_broad_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccs_broad_be_t1, commerce_clause_scope__broad_effects_test, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(ccs_broad_be_t2, commerce_clause_scope__broad_effects_test, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ccs_broad_be_t3, commerce_clause_scope__broad_effects_test, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(ccs_broad_be_t4, commerce_clause_scope__broad_effects_test, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(ccs_broad_be_t5, commerce_clause_scope__broad_effects_test, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ccs_broad_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ccs_broad_su_t1, commerce_clause_scope__broad_effects_test, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(ccs_broad_su_t2, commerce_clause_scope__broad_effects_test, suppression_requirement, 2, 0.75).
narrative_ontology:measurement(ccs_broad_su_t3, commerce_clause_scope__broad_effects_test, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(ccs_broad_su_t4, commerce_clause_scope__broad_effects_test, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__broad_effects_test, 0.18).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, dormant_commerce_clause_discrimination_strict_scrutiny).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_civil_rights_enforcement_authority).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, state_police_power_environmental_regulation).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three structurally distinct readings: broad_effects_test (this file), narrow_originalist, and intermediate_channels. Each reading has its own ε, its own beneficiary/victim structure, and its own classification type. The narrow_originalist reading will show lower extractiveness (states retain autonomy) and narrower federal authority. The intermediate_channels reading will show moderate extractiveness (some federal authority via channels logic but not full aggregation). All three are linked via network.affects_constraints and represent the contested interpretive territory of the Commerce Clause. The broad_effects_test reading is the contemporary dominant doctrine; the other two represent persistent constitutional counterreadings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
