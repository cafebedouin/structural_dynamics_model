% ============================================================================
% CONSTRAINT STORY: failed_amendments__dc_voting_rights_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failed_amendments__dc_voting_rights_amendment, []).

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
 *   constraint_id: failed_amendments__dc_voting_rights_amendment
 *   human_readable: D.C. Voting Rights Amendment — Constitutional Disfranchisement of the Capital
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The D.C. Voting Rights Amendment embodies one reading of the 'failed
 *   amendments' kernel — a constitutional commitment to expand representation
 *   that passed both chambers of Congress in 1978 but stalled in state
 *   legislatures at sixteen ratifications, sixteen votes short of the
 *   thirty-eight needed. This constraint instantiates the specific reading:
 *   D.C. residents as full constituents with House and Senate representation.
 *   The refusal to ratify suppresses the disfranchisement of the capital —
 *   the condition of federal taxation without electoral voice continues. The
 *   extractiveness lies in the continued asymmetry: D.C. residents pay
 *   federal income tax, obey federal law, and host the federal government,
 *   but hold no voting power in Congress. The suppression mechanism is
 *   structural: no D.C. resident can exit the capital; the constraint
 *   requires 38 state legislatures to break, giving 13 states the power to
 *   block permanent disfranchisement. The amendment's failure reveals that
 *   the federal structure is defended by beneficiaries with material
 *   interests in D.C. disfranchisement: state legislatures (who would lose
 *   relative House power), the federal executive (which retains uncontested
 *   dominion over the capital), and suburban delegations (whose power is
 *   preserved by the absence of D.C. House representation). The sibling
 *   readings (Balanced Budget, Child Labor, Equal Rights Amendments) are
 *   co-readings of the same failed-amendments kernel — all passed Congress
 *   but failed ratification, all represent competing constitutional visions,
 *   all reveal the supermajority requirement as the actual constitution of
 *   amendment possibility.
 *
 * KEY AGENTS:
 *   - D.C. Residents: Primary victims (powerless/trapped) — disfranchised despite federal taxation and law; cannot exit the capital; bear full cost of representation asymmetry
 *   - State Legislatures (Southern, Suburban Blocs): Primary beneficiaries (powerful/arbitrage) — preserve legislative power by blocking ratification; maintain relative House representation; can exit by simply not voting
 *   - Federal Executive Branch: Secondary beneficiary (institutional/arbitrage) — retains uncontested governance authority over the capital; needs no D.C. House representation to exercise power
 *   - D.C. Voting Rights Coalition: Organized victim group (organized/constrained) — mobilizes ratification campaigns but constrained by needing 38 state legislatures; has agency but faces asymmetric coalition-building burden
 *   - Constitutional Amendment Mechanism: Structural actor (institutional/arbitrage) — the supermajority requirement itself becomes extractive; theoretically allows constitutional evolution, actually preserves status quo
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice (state power) as inevitable federal logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failed_amendments__dc_voting_rights_amendment, 0.62).
domain_priors:suppression_score(failed_amendments__dc_voting_rights_amendment, 0.72).
domain_priors:theater_ratio(failed_amendments__dc_voting_rights_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failed_amendments__dc_voting_rights_amendment, extractiveness, 0.62).
narrative_ontology:constraint_metric(failed_amendments__dc_voting_rights_amendment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(failed_amendments__dc_voting_rights_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failed_amendments__dc_voting_rights_amendment, snare).
narrative_ontology:human_readable(failed_amendments__dc_voting_rights_amendment, "D.C. Voting Rights Amendment — Constitutional Disfranchisement of the Capital").
narrative_ontology:topic_domain(failed_amendments__dc_voting_rights_amendment, "political/legal/constitutional").

domain_priors:requires_active_enforcement(failed_amendments__dc_voting_rights_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failed_amendments__dc_voting_rights_amendment, '015cfcf4-c191-4947-b700-b242436bb1a5').
narrative_ontology:cs_kernel_codification('015cfcf4-c191-4947-b700-b242436bb1a5', formalized).
narrative_ontology:cs_authority_grounding('015cfcf4-c191-4947-b700-b242436bb1a5', lineage).
narrative_ontology:cs_interpretation_layer_present('015cfcf4-c191-4947-b700-b242436bb1a5').
narrative_ontology:cs_reading_relation('015cfcf4-c191-4947-b700-b242436bb1a5', failed_amendments__balanced_budget_amendment, coexists_with).
narrative_ontology:cs_reading_relation('015cfcf4-c191-4947-b700-b242436bb1a5', failed_amendments__child_labor_amendment, coexists_with).
narrative_ontology:cs_reading_relation('015cfcf4-c191-4947-b700-b242436bb1a5', failed_amendments__equal_rights_amendment, coexists_with).
narrative_ontology:cs_axiom('015cfcf4-c191-4947-b700-b242436bb1a5', foundational, representation_requires_consent_of_governed).
narrative_ontology:cs_axiom_status(representation_requires_consent_of_governed, holdable).
narrative_ontology:cs_axiom_grounding('015cfcf4-c191-4947-b700-b242436bb1a5', representation_requires_consent_of_governed, deontological).
narrative_ontology:cs_axiom('015cfcf4-c191-4947-b700-b242436bb1a5', foundational, federal_structure_permits_capital_representation).
narrative_ontology:cs_axiom_status(federal_structure_permits_capital_representation, holdable).
narrative_ontology:cs_axiom_grounding('015cfcf4-c191-4947-b700-b242436bb1a5', federal_structure_permits_capital_representation, empirically_contingent).
narrative_ontology:cs_reference_frame('015cfcf4-c191-4947-b700-b242436bb1a5', constitutional_amendment_as_democratic_renewal).
narrative_ontology:cs_drift_state('015cfcf4-c191-4947-b700-b242436bb1a5', contemporary_post_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('015cfcf4-c191-4947-b700-b242436bb1a5', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(failed_amendments__dc_voting_rights_amendment, failed_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failed_amendments__dc_voting_rights_amendment, suburban_and_state_legislatures).
narrative_ontology:constraint_beneficiary(failed_amendments__dc_voting_rights_amendment, federal_executive_branch).
narrative_ontology:constraint_victim(failed_amendments__dc_voting_rights_amendment, d_c_residents).
narrative_ontology:constraint_victim(failed_amendments__dc_voting_rights_amendment, capital_city_polity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: D.C. RESIDENT (SNARE) — Trapped within the capital with no exit option. Pays federal income tax, subject to federal law, has no House representative and no Senate voice. The constraint extracts political participation and tax revenue while refusing voting power. Zero degrees of freedom. Powerless agent in trap structure.
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: D.C. VOTING RIGHTS COALITION (ROPE) — Organized actors (local government, civil rights groups, residents' associations) who have agency to mobilize ratification campaigns. Constrained by needing 38 state legislatures to cooperate. The coalition sees the amendment as pure coordination: it solves a distribution problem (bring D.C. into the federal constitutional structure). From their perspective, the amendment itself is functional coordination with minimal coercive overhead. The failure to ratify is the extraction mechanism, not the amendment's structure.
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE LEGISLATURES / CONGRESSIONAL DELEGATIONS (TANGLED ROPE) — State legislatures that declined or refused ratification experienced mixed coordination and extraction. The amendment would rebalance House representation (giving D.C. two House seats), which coordinates the principle of one-person-one-vote across state lines. But it also extracts from state-apportionment logic: states would lose relative power in the House. The constraint from this perspective is hybrid — genuine coordination function (equalizing representation) embedded within asymmetric extraction (shifting power away from state delegations). Powerful actors with arbitrage options (states can exit by not ratifying).
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL EXECUTIVE BRANCH (ROPE) — The presidency and executive agencies benefit from the disfranchisement of D.C. residents: the capital remains effectively under executive dominion without the countervailing power of D.C. congressional representation. From the executive perspective, the constraint is pure coordination of governance — the federal government can make policy for its capital without negotiating with House members from D.C. This perspective sees the ratification failure as preserving a functional coordination mechanism. Institutional power with arbitrage options (executive branch does not need to ratify; it benefits from state non-ratification).
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC PRINCIPLE / CONSTITUTIONAL EQUALITY (SNARE) — The structural principle that constitutional authority derives from the consent of the governed experiences extraction. The disfranchisement of D.C. residents violates the foundational premise of democratic legitimacy. This is a snare from the perspective of systemic democratic theory: the principle is constrained by the amendment's failure; it cannot exit the structure; and it bears the cost of the extraction (undermining the legitimacy claim of federal democracy). Moderate power because democratic principle has institutional champions but no sovereign enforcement mechanism.
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL SUPREMACY DOCTRINE (PITON) — The foundational legal doctrine that the Constitution is the supreme law and that amendment procedures guarantee periodic constitutional renewal has degraded into theater. The 22nd Amendment (two-term presidency) passed; the 26th Amendment (eighteen-year-old suffrage) passed. But the Voting Rights Amendment stalled at 16/38 ratifications, revealing that the amendment mechanism is performative rather than functional. The doctrine persists through ritualized amendment attempts, but the mechanism's actual function (constitutional evolution through democratic ratification) has atrophied. Theater ratio (0.55) reflects that public debate, congressional passage, and state ratification campaigns are substantial activities, but the outcome is predetermined by structural interests unwilling to ratify.
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FEDERAL STRUCTURE AS NATURAL LAW (MOUNTAIN) — From a civilizational vantage, the disfranchisement of D.C. appears as an inherent limit of federal structure: a capital city by definition sits outside normal state boundaries, and the federal structure is designed to preserve state power against centralization. This perspective sees the amendment's failure as natural law — the federal system's own logic prevents its capital from having equal representation. However, this classification contradicts the structural data: D.C. residents are not metaphysically barred from voting; they are legally excluded by state choice. The mountain framing naturalizes what is actually an institutional choice backed by suppression.
constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failed_amendments__dc_voting_rights_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failed_amendments__dc_voting_rights_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failed_amendments__dc_voting_rights_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failed_amendments__dc_voting_rights_amendment, TR),
    TR >= 0.70.

:- end_tests(failed_amendments__dc_voting_rights_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. D.C. residents bear a clear cost — federal taxation without representation. But the extraction is not maximal because some federal services and programs are provided, and D.C. governance is not entirely unilateral. The value reflects persistent asymmetry (1978–2026) without escape. Suppression (0.72): High. The constraint is enforced through constitutional gatekeeping: the supermajority requirement (38 of 50 states) makes it nearly impossible to override. D.C. residents have no material exit option (cannot leave the capital without losing property and community). State legislatures that support disfranchisement are numerous and organized enough to maintain the veto. Theater ratio (0.55): Moderate. The amendment process involves genuine democratic activity — congressional debate, state campaigns, public advocacy — but the outcome has been predetermined since ratification stalled. The ritual of amendment attempts (multiple re-introduction in subsequent Congresses) and state campaigns is performative relative to the institutional gatekeeping that prevents success. The theater increased slightly over time (0.52 → 0.55) as D.C. population grew and federal spending per capita diverged from state averages, making the representation gap more salient but not more remediable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural fact (D.C. residents lack House/Senate representation) classifies as snare (to powerless D.C. residents), tangled rope (to state legislatures balancing coordination with power preservation), rope (to the coalition seeing functional amendment), rope (to the executive preserving its authority), snare (to democratic principle), piton (to constitutional amendment doctrine), and mountain (to the analytical observer naturalizing federal structure). The perspectival gaps reveal the extractive mechanism: beneficiaries experience the constraint as functional coordination (executive has clean authority over the capital; states preserve relative power). Victims experience it as pure extraction (trapped residents; constrained democratic principle). The analytical mountain threatens to naturalize what is actually institutional choice defended by organized suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   D.C. residents (powerless + trapped) derive d ≈ 0.95, producing high f(d) ≈ 1.42 → high χ even with moderate base ε. State legislatures (powerful + arbitrage) derive d ≈ 0.30, producing low f(d) ≈ 0.15 → negative χ (they benefit from the constraint). Federal executive (institutional + arbitrage) derives d ≈ 0.10, producing negative f(d) ≈ -0.08 → negative χ (clear beneficiary). The coalition (organized + constrained) derives d ≈ 0.62, producing moderate f(d) ≈ 0.95 → moderate χ reflecting mixed experience. Scope modifiers: D.C. residents experience the constraint at local scope (σ = 0.8), dampening their measured χ; state legislative blocs experience it at national scope (σ = 1.0); the federal executive experiences it at national scope (σ = 1.0). The calculation reveals the structural advantage: beneficiaries have organized power and arbitrage options; victims have neither.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint cleanly resolves the mandatrophy (the apparent paradox that pure extraction could persist without a coordination function) by identifying the coordination function: the federal structure itself. The constraint is not pure snare from the executive perspective (coordination of capital governance), not pure snare from state legislative perspective (coordination of federalism), but pure snare from the D.C. residents' perspective (no coordination benefit to them). The mandatrophy is resolved by recognizing that chi scales per observer: chi_snare (D.C. residents) is high and unambiguous; chi_rope (executive/state perspective) is high in the negative direction (beneficiaries). The two perspectives have opposite sign on chi, not opposite signs on classification. The classification is correct: the constraint IS a snare from the victims' position, even though beneficiaries experience it as coordination. This is the classic pattern of extractive coordination masquerading as functional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_structure_necessity,
    'Is D.C. disfranchisement a necessary feature of federalism, or a contingent institutional choice?',
    'Comparative constitutional analysis: examine federal systems that grant capitals full representation (Germany, Austria, Australia) vs those that don''t (U.S., India); identify whether representation is logically incompatible with federal structure or merely reflects power distribution',
    'If necessary: mountain classification correct; amendment''s failure reflects structural law, not politics. If contingent: false summit — the mountain framing naturalizes what is actually institutional choice backed by suppression. D.C. disfranchisement becomes a snare rather than a mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_structure_necessity, empirical, 'Whether D.C. disfranchisement is intrinsic to federalism or contingent').

omega_variable(
    state_legislature_heterogeneity,
    'Why did ratification stall at exactly sixteen states? What blocked the critical thirty-eighth state?',
    'Historical analysis of non-ratifying state legislatures'' voting records, campaign finance data, state-level interest group opposition, and explicit statements by legislators. Identify whether failure was due to federalism principle or to specific interests (suburban bloc, Southern resistance to D.C. representation, anti-federal sentiment).',
    'If federalism principle: the failure represents structural consensus that federal capitals should not have representation. If specific interests: the suppression mechanism is clear — organized opposition from beneficiaries (suburban legislators, federal executive allies) blocked ratification through standard legislative gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_legislature_heterogeneity, empirical, 'State-level sources of ratification failure').

omega_variable(
    extractiveness_temporal_trajectory,
    'Has the extractiveness of federal taxation without D.C. representation increased or remained stable since 1978?',
    'Time-series analysis of D.C. tax revenue as percentage of municipal budget, federal spending per capita in D.C. vs comparable cities, and per-capita representation deficit in Congress. Track whether the discrepancy has widened, narrowed, or remained flat.',
    'Rising extractiveness suggests the constraint''s suppressive force is accumulating; stable extractiveness suggests the constraint is structural but not intensifying. This affects piton vs snare classification over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_temporal_trajectory, empirical, 'Long-term trajectory of D.C. tax extraction without representation').

omega_variable(
    amendment_kernel_interpretation_gap,
    'Is the failed D.C. amendment a reading of the ''failed amendments'' kernel based on the same legitimacy framework as the ERA, or does it rely on a fundamentally different constitutional principle?',
    'Textual and theoretical comparison: analyze whether D.C. amendment arguments (federalism, representation, taxation without representation) share a common legitimacy foundation with ERA arguments (individual rights, equal protection) or invoke distinct constitutional principles. Identify whether the readings can coexist within a single constitutional framework or represent incompatible interpretations of constitutional authority.',
    'If common framework: readings coexist_with (different sibling readings of the same kernel). If distinct frameworks: this reading forecloses or influences others depending on which principle takes priority in any given constitutional era.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_kernel_interpretation_gap, conceptual, 'Whether D.C. amendment shares constitutional legitimacy framework with other failed amendments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failed_amendments__dc_voting_rights_amendment, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcvra_theater_1978, failed_amendments__dc_voting_rights_amendment, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dcvra_theater_1993, failed_amendments__dc_voting_rights_amendment, theater_ratio, 15, 0.54).
narrative_ontology:measurement(dcvra_theater_2026, failed_amendments__dc_voting_rights_amendment, theater_ratio, 48, 0.55).

% Extraction over time
narrative_ontology:measurement(dcvra_extractiveness_1978, failed_amendments__dc_voting_rights_amendment, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dcvra_extractiveness_1993, failed_amendments__dc_voting_rights_amendment, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(dcvra_extractiveness_2026, failed_amendments__dc_voting_rights_amendment, base_extractiveness, 48, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(dcvra_suppression_1978, failed_amendments__dc_voting_rights_amendment, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(dcvra_suppression_1993, failed_amendments__dc_voting_rights_amendment, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(dcvra_suppression_2026, failed_amendments__dc_voting_rights_amendment, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failed_amendments__dc_voting_rights_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(failed_amendments__dc_voting_rights_amendment, failed_amendments__equal_rights_amendment).
narrative_ontology:affects_constraint(failed_amendments__dc_voting_rights_amendment, failed_amendments__balanced_budget_amendment).
narrative_ontology:affects_constraint(failed_amendments__dc_voting_rights_amendment, failed_amendments__child_labor_amendment).

% DUAL FORMULATION NOTE:
% The D.C. Voting Rights Amendment stalled at 16/38 ratifications. This constraint family decomposes the 'failed amendments kernel' into four distinct constraint readings, each representing a different constitutional interpretation that passed Congress but failed state ratification. The D.C. reading shares the constitutional gatekeeping structure (supermajority requirement) with the others, but has distinct beneficiary/victim sets and distinct ε values. D.C. amendment: ε ≈ 0.62 (extraction of taxation without representation). The network links capture that these are sibling readings of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(failed_amendments__dc_voting_rights_amendment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
