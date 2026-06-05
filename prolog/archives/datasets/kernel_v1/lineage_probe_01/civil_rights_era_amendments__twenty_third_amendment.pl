% ============================================================================
% CONSTRAINT STORY: civil_rights_era_amendments__twenty_third_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_rights_era_amendments__twenty_third_amendment, []).

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
 *   constraint_id: civil_rights_era_amendments__twenty_third_amendment
 *   human_readable: The Twenty-Third Amendment: Partial Enfranchisement of the District of Columbia
 *   domain: political/legal/constitutional_amendments
 *
 * SUMMARY:
 *   The Twenty-Third Amendment (1961) grants the District of Columbia
 *   electoral votes for president, establishing a compromise position between
 *   total disenfranchisement and full representation as a state. The
 *   constraint instantiates one specific reading of the civil-rights-era
 *   amendments kernel: enfranchisement through limited electoral
 *   participation without legislative representation. D.C. residents number
 *   over 700,000 — larger than several states — yet possess no congressional
 *   representation and face federal supremacy through the District Clause.
 *   The amendment represents a partial resolution of the 'taxation without
 *   representation' problem: D.C. residents pay federal taxes and can now
 *   influence presidential elections, but lack any voice in the Congress that
 *   controls D.C.'s budget, governance, and territory. The constraint
 *   exhibits all six classification types across different perspectives: for
 *   the powerless disenfranchised, it functions as a snare (recognition
 *   without power); for organized advocates, a tangled rope (partial benefit
 *   with asymmetric extraction); for the federal government, pure
 *   coordination (rope); for civil rights activists, a scaffold (temporary
 *   stepping stone); for the constitutional authority structure, a piton
 *   (performative recognition maintained by inertia); and for the analytical
 *   observer, a false summit (naturalized political contingency presented as
 *   constitutional law). The extractiveness metric increases from 0.28 (1961,
 *   immediate post-amendment recognition) to 0.38 (2021, accumulated
 *   frustration as D.C. population grows faster than its political power
 *   grows), reflecting the growing asymmetry between D.C.'s demographic and
 *   fiscal weight and its electoral powerlessness.
 *
 * KEY AGENTS:
 *   - District of Columbia Residents: Primary victims (powerless/trapped) — experience both the gain (presidential electoral votes) and the permanent loss (no congressional representation, federal control of local budget)
 *   - Federal Government / Congressional Majority: Primary beneficiary (institutional/arbitrage) — maintains control of capital territory and federal supremacy while granting symbolic electoral recognition
 *   - D.C. Statehood / Autonomy Movements: Organized agents (organized/constrained) — seek full representation but constrained by need for congressional consent and the amendment's foreclosure of the question
 *   - Civil Rights Coalition (1961): Activist agents (organized/mobile) — view the amendment as a step toward full enfranchisement, not a permanent settlement
 *   - Constitutional Formalists: Institutional actors (institutional/constrained) — defend the amendment as the constitutional settlement and resist further changes on formalist grounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_rights_era_amendments__twenty_third_amendment, 0.38).
domain_priors:suppression_score(civil_rights_era_amendments__twenty_third_amendment, 0.52).
domain_priors:theater_ratio(civil_rights_era_amendments__twenty_third_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_third_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_third_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_third_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_rights_era_amendments__twenty_third_amendment, tangled_rope).
narrative_ontology:human_readable(civil_rights_era_amendments__twenty_third_amendment, "The Twenty-Third Amendment: Partial Enfranchisement of the District of Columbia").
narrative_ontology:topic_domain(civil_rights_era_amendments__twenty_third_amendment, "political/legal/constitutional_amendments").

domain_priors:requires_active_enforcement(civil_rights_era_amendments__twenty_third_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(civil_rights_era_amendments__twenty_third_amendment, 'b302a94f-826f-48db-8e15-8c7e4c885ac9').
narrative_ontology:cs_kernel_codification('b302a94f-826f-48db-8e15-8c7e4c885ac9', formalized).
narrative_ontology:cs_authority_grounding('b302a94f-826f-48db-8e15-8c7e4c885ac9', lineage).
narrative_ontology:cs_interpretation_layer_present('b302a94f-826f-48db-8e15-8c7e4c885ac9').
narrative_ontology:cs_reading_relation('b302a94f-826f-48db-8e15-8c7e4c885ac9', civil_rights_era_amendments__twenty_fourth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('b302a94f-826f-48db-8e15-8c7e4c885ac9', civil_rights_era_amendments__twenty_fifth_amendment, influences).
narrative_ontology:cs_reading_relation('b302a94f-826f-48db-8e15-8c7e4c885ac9', civil_rights_era_amendments__twenty_sixth_amendment, influences).
narrative_ontology:cs_axiom('b302a94f-826f-48db-8e15-8c7e4c885ac9', foundational, partial_enfranchisement_is_constitutional_remedy).
narrative_ontology:cs_axiom_status(partial_enfranchisement_is_constitutional_remedy, holdable).
narrative_ontology:cs_axiom_grounding('b302a94f-826f-48db-8e15-8c7e4c885ac9', partial_enfranchisement_is_constitutional_remedy, deontological).
narrative_ontology:cs_axiom('b302a94f-826f-48db-8e15-8c7e4c885ac9', foundational, federal_supremacy_over_capital_is_intrinsic).
narrative_ontology:cs_axiom_status(federal_supremacy_over_capital_is_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('b302a94f-826f-48db-8e15-8c7e4c885ac9', federal_supremacy_over_capital_is_intrinsic, instrumental).
narrative_ontology:cs_reference_frame('b302a94f-826f-48db-8e15-8c7e4c885ac9', partial_capital_enfranchisement_framework).
narrative_ontology:cs_drift_state('b302a94f-826f-48db-8e15-8c7e4c885ac9', contemporary_demographic_disparity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b302a94f-826f-48db-8e15-8c7e4c885ac9', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(civil_rights_era_amendments__twenty_third_amendment, civil_rights_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_third_amendment, district_of_columbia_residents).
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_third_amendment, federal_government_centralization).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_third_amendment, full_representation_claims).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_third_amendment, local_democratic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: D.C. RESIDENTS SEEKING FULL REPRESENTATION (SNARE) — The Twenty-Third Amendment grants only presidential electoral votes, explicitly denying representation in Congress ('as if a state'). D.C. residents experience perpetual structural subordination: they fund federal government operations through taxation while lacking legislative voice. The amendment is presented as a gain (enfranchisement) but operates as a suppression mechanism preventing full democratic membership. Exit is impossible — leaving D.C. terminates the pathological privilege. Experienced extraction is severe.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: D.C. AUTONOMY MOVEMENTS / STATEHOOD ADVOCATES (TANGLED ROPE) — These organized agents benefit from the coordination function of the amendment (it partially suppresses total disenfranchisement) while bearing extraction (it forecloses the pathway to full statehood by establishing a constitutional precedent of partial enfranchisement). The constraint requires active enforcement: Congress maintains special authority over D.C. through the District Clause (Article I, Section 8). Advocates see both genuine benefit (president is not irrelevant) and asymmetric extraction (statehood is barred). Exit is costly but possible — move the capital or amend the Constitution.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT / CONGRESSIONAL MAJORITY (ROPE) — The amendment is experienced as pure coordination: it resolves the crisis of a capital city with substantial population (>700,000 residents) holding no electoral voice. The solution creates presidential electoral power for D.C. residents while preserving federal supremacy through explicit Congressional authority retention. This is coordination in the classical sense: a collective action problem (how to acknowledge D.C.'s population while keeping the capital under federal control) is solved through a mechanism that all parties accept as legitimate. No extraction is perceived — the constraint benefits the federal system's stability.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS REFORMERS / AMENDMENT ACTIVISTS (SCAFFOLD) — The Twenty-Third Amendment represents a temporary solution to total disenfranchisement, with the explicit understanding that it is a scaffold toward fuller enfranchisement. The amendment's framing ('as if a state') creates a logical pathway to the next step (statehood or congressional representation). Theater is moderate — the amendment is performative in that it grants electoral votes without legislative power, but the activists see it as a stepping stone with a sunset horizon. Exit appears available: future amendments can revise or supersede this one.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AUTHORITY STRUCTURE / FORMALIST VIEW (PITON) — The amendment persists as an institutional arrangement despite its increasingly performative character. D.C. residents now outnumber voters in several states, yet the amendment grants only electoral votes equivalent to the smallest state. The constitutional framework has calcified around this solution: Congress has no incentive to revise (it retains control); D.C. residents lack the formal standing to initiate change (no representation in Congress); statehood requires congressional consent, which Congress will not grant. The amendment functions primarily as a theater of recognition — it signals that D.C. residents are counted as quasi-citizens while preserving the institutional subordination. Theater ratio is moderate to high because the electoral votes are numerically minimal and symbolically asymmetric to D.C.'s population and tax contribution.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL NATURAL LAW VIEW (MOUNTAIN) — From a formalist/originalist position, the Twenty-Third Amendment represents an immutable constitutional settlement: the people (via supermajority ratification, Framers' intent, formal amendment process) have definitively resolved the question of D.C.'s electoral role. The amendment is 'natural law' in the sense that it is the supreme law of the land and cannot be unilaterally revised. No agent can exit this constraint without violating the Constitution itself. However, the structural data contradicts the mountain classification — the amendment has identifiable beneficiaries (federal government maintaining control) and victims (D.C. residents denied full representation). The engine will identify this as a false summit, revealing that the 'constitutional law is immutable' framing naturalizes what is actually a contingent political settlement protected by an institutional veto (Congressional consent).
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_rights_era_amendments__twenty_third_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_third_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(civil_rights_era_amendments__twenty_third_amendment, TR),
    TR >= 0.70.

:- end_tests(civil_rights_era_amendments__twenty_third_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The amendment suppresses the total extraction of complete disenfranchisement but does not eliminate it. D.C. residents gain presidential electoral votes (approximately 3, tiny compared to large states) but lose any mechanism to influence Congress, which controls D.C.'s territory, budget, and local governance. The extraction increases over time as D.C.'s population grows (from 760,000 in 1961 to 700,000+ today, with fluctuations due to gentrification and demographic shifts) while electoral votes remain constant. The 'as if a state' framing intensifies extraction by creating an asymmetry: if D.C. had the population of Wyoming (~580,000), it would have 3 electoral votes exactly, yet D.C. exceeds this and is explicitly denied the next step (statehood). Suppression (0.52): Moderate-high. D.C. residents face multiple barriers to exit: they cannot vote congressional representatives; Congress controls the District's budget and territory; statehood requires congressional approval, which Congress will not grant; moving the capital is politically infeasible. The suppression is not absolute (the amendment does grant electoral voice), but it is structural and durable. Theater ratio (0.58): Moderate-high. The amendment's performative character increases over time. In 1961, the grant of electoral votes was meaningful recognition; by 2021, D.C.'s electoral votes are a tiny fraction of its population's voting power relative to smaller states. The performative gap widens as the asymmetry becomes visible. The 'as if a state' language is itself theater — D.C. is explicitly not a state and the amendment forecloses the pathway to becoming one by establishing a constitutional precedent of partial enfranchisement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classic perspectival disparity across power levels. The federal government sees a coordination success (rope): the capital city now has electoral voice and the federal system's legitimacy is enhanced. Organized D.C. advocates see mixed coordination and extraction (tangled rope): they gain some agency (presidential electoral votes) while losing all leverage on the question that matters most (congressional representation). The powerless residency sees suppression (snare): the amendment is recognition without power, transforming 'no voice at all' into 'voice only in a domain the residents cannot control.' The analytical observer at the civilizational level risks seeing a constitutional natural law (mountain) — the Twenty-Third Amendment is the Supreme Law of the Land and therefore immutable — but the structural data reveals this as a false summit: the amendment's durability depends on congressional veto of statehood, which is a political choice, not a constitutional requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps the agent's structural position relative to the constraint. D.C. residents as victims of foreclosed full representation have high d (approximately 0.75–0.85): they are targets of the extraction (denied congressional voice). The federal government as beneficiary of preserved supremacy has low d (approximately 0.10–0.20): it benefits from the constraint's preservation of federal control. The organized statehood movements have intermediate d (approximately 0.50–0.60): they benefit from some electoral recognition but bear the cost of foreclosed full representation. The sigmoid f(d) applies these context values to derive experienced extraction (chi). High-d agents experience more extraction because they cannot arbitrage (leave D.C., lose their voice entirely). Low-d agents experience less extraction because they can exit (Congress can ignore D.C.'s interests with impunity). The piton perspective at the institutional level (constrained exit) has higher d than the rope perspective at the federal level (arbitrage exit) because the piton sees institutional inertia as binding, while the rope sees voluntary coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The Twenty-Third Amendment resolves the mandatrophy by showing that the apparent contradiction between 'partial enfranchisement' and 'continued disenfranchisement' reflects the reading's own embedded ambiguity. The amendment says 'as if a state' — presenting D.C. as quasi-equal — while explicitly denying Congress representation and federal legislative voice. This is the amendment's designed form: it solves the political problem (acknowledging the capital's population) without solving the structural problem (federal supremacy over D.C.). The constraint therefore exhibits all six types because it IS all six types simultaneously from different observational positions. The snare perspective (powerless, trapped) captures the reality that D.C. residents face a permanent boundary condition: they can vote in presidential elections but cannot remove that boundary through congressional representation. The tangled rope perspective (organized, constrained) captures the reality that advocates for statehood benefit from the amendment's recognition (it establishes D.C. residents as voters) while bearing extraction (the amendment forecloses the next step by creating a constitutional precedent). The rope perspective (institutional, arbitrage) captures the reality that the federal system genuinely solved a coordination problem: acknowledging a capital city with substantial population without disrupting federal supremacy. All three are correct simultaneously because they measure from different structural positions. The mandatrophy is resolved not by choosing one but by recognizing that the indexical tuple fully determines the classification outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partial_enfranchisement_as_settlement_or_suppression,
    'Is the Twenty-Third Amendment a genuine resolution of the capital disenfranchisement problem, or a suppression mechanism that forecloses the pathway to full representation by establishing a constitutional precedent of partial enfranchisement?',
    'Historical legislative analysis: examine congressional debates on statehood proposals post-1961; track whether Congress treats the amendment as settling the question (full representation is off-table) or as a transitional step. Compare statehood support before and after the amendment. Analyze whether the amendment''s existence is cited as a reason to reject statehood bills.',
    'If settlement: the constraint is genuinely cordial (Rope from institutional perspective stands). If suppression: the constraint is extractive (Snare from powerless perspective is correct, and the rope perspective misses the foreclosure mechanism). This changes whether D.C. residents face an insurmountable obstacle (mountain-like suppression) or merely a high-cost one (tangled rope constraint with exit pathway).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partial_enfranchisement_as_settlement_or_suppression, empirical, 'Whether the amendment resolves or suppresses the full representation question').

omega_variable(
    taxation_without_representation_reduction,
    'Does the Twenty-Third Amendment actually reduce the ''taxation without representation'' extraction, or merely relocate it from presidential elections to Congress while intensifying it through the contradiction between D.C.''s federal tax contributions and legislative powerlessness?',
    'Fiscal analysis: compare D.C. federal tax revenue vs. federal funding allocated to D.C. Compare electoral power (electoral votes) to tax burden (federal income tax paid). Calculate the ratio of voting power per tax dollar for D.C. residents vs. state residents. Track whether the contradiction (largest tax burden + no congressional voice) increases perceived extraction over time.',
    'If extraction is genuinely reduced: the tangled_rope classification holds — there is real benefit alongside extraction. If extraction is merely transformed: the snare classification may be correct — the amendment creates the appearance of enfranchisement while preserving and intensifying the underlying disenfranchisement in the legislative domain where power actually resides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxation_without_representation_reduction, empirical, 'Whether the amendment reduces or relocates taxation-without-representation extraction').

omega_variable(
    congressional_veto_on_statehood_as_constitutional_or_political,
    'Is the congressional requirement for D.C. statehood a constitutional constraint (inherent to the system) or a political veto by a particular congressional majority that could be overridden by a different majority or by constitutional amendment?',
    'Constitutional law analysis: Does the Constitution explicitly require congressional consent for D.C. statehood, or is this a statutory/procedural constraint? Examine the District Clause and amendment procedures. Model what would happen if Congress refused to authorize a statehood referendum vs. if the Supreme Court required Congress to allow one.',
    'If constitutional: D.C. statehood requires a second amendment (exit appears possible but faces a supermajority gate). If political: Congress is exercising a veto that could theoretically be overridden by political will or judicial intervention — the constraint is more contingent than it appears. This affects whether the piton classification (institutional inertia) or the tangled_rope classification (active enforcement) better captures the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_veto_on_statehood_as_constitutional_or_political, conceptual, 'Whether the congressional statehood veto is constitutional or political').

omega_variable(
    kernel_reading_contest_over_amendment_scope,
    'Is this reading of the civil-rights-era amendments kernel the one that interprets enfranchisement narrowly (electoral votes only, ''as if a state'') or one that interprets it as a stepping stone toward full representation (Congress eventually grants statehood)?',
    'This is not an empirical question but a hermeneutical one: what is the canonical interpretation of the Twenty-Third Amendment''s intent? The amendment text says ''as if a state'' (suggesting a permanent partial status), but historical context (civil rights era, expansion of voting rights) suggests stepping-stone intent. Different constitutional readings (originalist, living constitutionalist) produce different answers. This omega documents the reading contest at the kernel level.',
    'The snare perspective assumes the amendment is a permanent foreclosure. The scaffold perspective assumes it is transitional. The mountain perspective assumes it is constitutional settlement. Which interpretation is correct determines whether D.C. residents face a mountain (immutable), a snare (permanent extraction), a tangled rope (contested boundary with some benefits), or a scaffold (temporary with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_over_amendment_scope, conceptual, 'Hermeneutical contest over the amendment''s scope and intent within the civil-rights-era amendments kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_rights_era_amendments__twenty_third_amendment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(t23a_theater_1961, civil_rights_era_amendments__twenty_third_amendment, theater_ratio, 0, 0.4).
narrative_ontology:measurement(t23a_theater_1991, civil_rights_era_amendments__twenty_third_amendment, theater_ratio, 30, 0.55).
narrative_ontology:measurement(t23a_theater_2021, civil_rights_era_amendments__twenty_third_amendment, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(t23a_extract_1961, civil_rights_era_amendments__twenty_third_amendment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(t23a_extract_1991, civil_rights_era_amendments__twenty_third_amendment, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(t23a_extract_2021, civil_rights_era_amendments__twenty_third_amendment, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(t23a_supp_1961, civil_rights_era_amendments__twenty_third_amendment, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(t23a_supp_1991, civil_rights_era_amendments__twenty_third_amendment, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(t23a_supp_2021, civil_rights_era_amendments__twenty_third_amendment, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_rights_era_amendments__twenty_third_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_third_amendment, civil_rights_era_amendments__twenty_fourth_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_third_amendment, civil_rights_era_amendments__twenty_fifth_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_third_amendment, civil_rights_era_amendments__twenty_sixth_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_third_amendment, district_of_columbia_statehood_foreclosure).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_third_amendment, federal_district_supremacy).

% DUAL FORMULATION NOTE:
% The Twenty-Third Amendment reading is one constraint within the civil-rights-era amendments kernel family. Related constraints include the Twenty-Fourth Amendment (poll tax elimination, ε ≈ 0.25, Rope), the Twenty-Fifth Amendment (succession clarity, ε ≈ 0.15, Rope), and the Twenty-Sixth Amendment (age enfranchisement, ε ≈ 0.20, Rope). The Twenty-Third Amendment differs structurally: it grants limited electoral participation (ε ≈ 0.38) alongside foreclosure of full representation, making it a Tangled Rope rather than a pure Rope. The constraint also affects downstream questions about D.C. statehood and federal supremacy by establishing a constitutional precedent of partial enfranchisement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civil_rights_era_amendments__twenty_third_amendment, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
