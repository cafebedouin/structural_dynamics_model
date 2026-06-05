% ============================================================================
% CONSTRAINT STORY: strict_scrutiny_tier__fatal_in_fact_trajectory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strict_scrutiny_tier__fatal_in_fact_trajectory, []).

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
 *   constraint_id: strict_scrutiny_tier__fatal_in_fact_trajectory
 *   human_readable: Strict Scrutiny Tier: Fatal-in-Fact Trajectory (Kernel Reading)
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   This constraint instantiates the 'fatal_in_fact_trajectory' reading of
 *   the strict_scrutiny_tier kernel — one of three structurally distinct
 *   readings of how strict scrutiny doctrine governs race-conscious
 *   government action. The fatal_in_fact reading traces the historical
 *   trajectory from Justice Gunther's 1976 quip ('strict in theory, fatal in
 *   fact') through the affirmative action era (1978–2023, when race-conscious
 *   remediation survived some strict scrutiny reviews despite the tier's
 *   theoretical lethality) to the post-SFFA era (2024–present, when any
 *   racial classification is categorically suppressed). This reading
 *   emphasizes that the tier's history is the gradual completion of Gunther's
 *   original observation: what appeared as hyperbole in 1976 became literal
 *   doctrine by 2024. The constraint exhibits the structure of a snare —
 *   race-conscious remediation programs and implementing institutions face
 *   categorical suppression with no meaningful escape route; alternatives are
 *   closed off; the doctrine permits no tailoring that would survive if the
 *   race-based remedy itself is the objected-to feature. The beneficiary is
 *   the colorblind-rule coalition (advocates, judges, scholars committed to
 *   prohibition of all race-conscious action); the victim set is the class of
 *   race-conscious remediation programs and the affected racial minorities
 *   those programs were designed to serve. The extractiveness value (0.88)
 *   reflects near-total suppression of the remedial pathway post-SFFA; the
 *   suppression value (0.92) reflects that alternatives to race-conscious
 *   remediation are normatively constructed as constitutionally compelled
 *   rather than merely permitted.
 *
 * KEY AGENTS:
 *   - Race-Conscious Remediation Programs (powerless/trapped): Universities, employers, government agencies implementing affirmative action; no exit route post-SFFA
 *   - Institutional Administrators (moderate/constrained): Colleges, agencies, corporations managing compliance; face choice between program elimination and litigation with near-certain loss
 *   - Colorblind Doctrine Advocates (institutional/arbitrage): Constitutional scholars, Supreme Court majority, Federalist Society judges committed to race-neutral equal protection; net beneficiaries from the tier's enforcement
 *   - Affected Racial Minorities (powerless/trapped): Groups that benefited from race-conscious remediation during the affirmative action era; lose access to the remedial pathway post-SFFA
 *   - Strict Scrutiny Doctrine as Institutional Practice (institutional/generational): The doctrinal apparatus itself; shift from genuine uncertainty (1978–2023) to predetermined outcome (post-SFFA)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strict_scrutiny_tier__fatal_in_fact_trajectory, 0.88).
domain_priors:suppression_score(strict_scrutiny_tier__fatal_in_fact_trajectory, 0.92).
domain_priors:theater_ratio(strict_scrutiny_tier__fatal_in_fact_trajectory, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strict_scrutiny_tier__fatal_in_fact_trajectory, extractiveness, 0.88).
narrative_ontology:constraint_metric(strict_scrutiny_tier__fatal_in_fact_trajectory, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(strict_scrutiny_tier__fatal_in_fact_trajectory, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strict_scrutiny_tier__fatal_in_fact_trajectory, snare).
narrative_ontology:human_readable(strict_scrutiny_tier__fatal_in_fact_trajectory, "Strict Scrutiny Tier: Fatal-in-Fact Trajectory (Kernel Reading)").
narrative_ontology:topic_domain(strict_scrutiny_tier__fatal_in_fact_trajectory, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(strict_scrutiny_tier__fatal_in_fact_trajectory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(strict_scrutiny_tier__fatal_in_fact_trajectory, '3484c1b0-dfe1-4334-b57a-c26afe8957d3').
narrative_ontology:cs_kernel_codification('3484c1b0-dfe1-4334-b57a-c26afe8957d3', formalized).
narrative_ontology:cs_authority_grounding('3484c1b0-dfe1-4334-b57a-c26afe8957d3', extraction).
narrative_ontology:cs_interpretation_layer_present('3484c1b0-dfe1-4334-b57a-c26afe8957d3').
narrative_ontology:cs_reading_relation('3484c1b0-dfe1-4334-b57a-c26afe8957d3', strict_scrutiny_tier__compelling_interest_jurisprudence, influences).
narrative_ontology:cs_reading_relation('3484c1b0-dfe1-4334-b57a-c26afe8957d3', strict_scrutiny_tier__narrow_tailoring_mechanics, coexists_with).
narrative_ontology:cs_axiom('3484c1b0-dfe1-4334-b57a-c26afe8957d3', foundational, strict_scrutiny_is_categorically_fatal_to_race_consciousness).
narrative_ontology:cs_axiom_status(strict_scrutiny_is_categorically_fatal_to_race_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('3484c1b0-dfe1-4334-b57a-c26afe8957d3', strict_scrutiny_is_categorically_fatal_to_race_consciousness, empirically_contingent).
narrative_ontology:cs_axiom('3484c1b0-dfe1-4334-b57a-c26afe8957d3', secondary, the_affirmative_action_era_was_a_holding_pattern_not_genuine_doctrinal_flexibility).
narrative_ontology:cs_axiom_status(the_affirmative_action_era_was_a_holding_pattern_not_genuine_doctrinal_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('3484c1b0-dfe1-4334-b57a-c26afe8957d3', the_affirmative_action_era_was_a_holding_pattern_not_genuine_doctrinal_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('3484c1b0-dfe1-4334-b57a-c26afe8957d3', strict_scrutiny_applied_with_meaningful_outcome_variance).
narrative_ontology:cs_drift_state('3484c1b0-dfe1-4334-b57a-c26afe8957d3', post_sffa_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3484c1b0-dfe1-4334-b57a-c26afe8957d3', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(strict_scrutiny_tier__fatal_in_fact_trajectory, strict_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__fatal_in_fact_trajectory, colorblind_rule_advocates).
narrative_ontology:constraint_victim(strict_scrutiny_tier__fatal_in_fact_trajectory, race_conscious_remediation_programs).
narrative_ontology:constraint_victim(strict_scrutiny_tier__fatal_in_fact_trajectory, affected_racial_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RACE-CONSCIOUS REMEDIATION PROGRAMS (SNARE) — Any affirmative action program faces categorical suppression under strict scrutiny. The tier offers no escape route: even race-conscious remediation justified by identified historical discrimination faces lethal scrutiny post-SFFA. The program cannot modify itself to survive — narrowing tailoring fails because the remedy itself (race as proxy) is foreclosed. Maximum extraction: the constraint extracts full compliance cost and program dissolution. Experiences the strict scrutiny tier as pure extraction with no alternative.
constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL ADMINISTRATORS (SNARE) — Universities, employers, and government agencies implementing race-conscious remediation face a constrained exit: they can modify or eliminate programs (high institutional cost, political cost, values conflict, mission damage) or defend them in court (catastrophic financial and reputational cost post-SFFA). Neither exit is costless. The constraint extracts compliance costs and institutional burden regardless of choice. Suppression is near-total — alternatives framed as identity-violating or institutionally impermissible.
constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COLORBLIND DOCTRINE ADVOCATES (ROPE) — Constitutional scholars, judicial coalitions, and policy advocates committed to colorblind equal protection see the strict scrutiny tier as coordination around a durable principle. They experience the constraint as enabling their preferred jurisprudence: race-conscious remediation is incompatible with equal protection values. The tier extracts nothing from them; it coordinates their normative commitment with doctrinal structure. Arbitrage options available: the colorblind frame has generated alternative policy pathways (race-neutral admissions, socioeconomic preference) that this constituency benefits from developing. Net beneficiary — the constraint's enforcement advances their institutional interests.
constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRICT SCRUTINY AS PROCEDURAL THEATER (PITON) — From a generational institutional view, the strict scrutiny tier is now substantially performative. The substantive outcome (race-conscious programs lose) was settled long before SFFA closed the formal tier. The tier's theater_ratio (0.68) reflects that the 'review' of compelling interest and narrow tailoring had become scripted: SFFA completed the transition from genuine uncertainty to predetermined outcome. The procedure persists — opinions recite the tiers — but the functional decision was made at the level of whether race can be a remedy at all, not within the scrutiny framework.
constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the strict scrutiny tier appears as an immutable doctrine of equal protection law: the principle that race-based government action requires overwhelming justification is foundational to constitutional structure. This perspective sees the tier as a structural necessity of equal protection itself — not as a contingent doctrinal choice but as a natural law of constitutional meaning. However, the structural data contradicts the mountain classification. The tier's trajectory from Gunther's quip ('strict in theory, fatal in fact') through the affirmative action era (where remediation survived some strict scrutiny reviews) to post-SFFA (where any race-conscious line is categorically suppressed) reveals that what appears as natural law is actually the completion of a specific doctrinal trajectory shaped by judicial personnel change and strategic framing of the colorblind axiom.
constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strict_scrutiny_tier__fatal_in_fact_trajectory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strict_scrutiny_tier__fatal_in_fact_trajectory, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strict_scrutiny_tier__fatal_in_fact_trajectory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strict_scrutiny_tier__fatal_in_fact_trajectory, TR),
    TR >= 0.70.

:- end_tests(strict_scrutiny_tier__fatal_in_fact_trajectory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): The trajectory shows progressive extraction from 0.35 (pre-affirmative action, when strict scrutiny existed but race-conscious action was less common) through 0.62 (affirmative action era, when some programs survived strict scrutiny but with increasing difficulty) to 0.88 (post-SFFA, when all race-conscious classifications are categorically suppressed regardless of remedial purpose). The post-SFFA value reflects near-total extraction — the constraint offers no legitimate pathway for race-conscious remediation. Suppression (0.92): The trajectory shows enforcement intensification from 0.45 to 0.92. Pre-affirmative action, alternatives existed (race-consciousness was not yet normalized into constitutional conflict). During the affirmative action era, suppression increased as courts recognized race-conscious remediation as falling under strict scrutiny, but some programs survived. Post-SFFA, suppression converges on categorical — no race-conscious classification survives regardless of narrow tailoring. Theater ratio (0.68): Reflects the piton observation — the doctrine maintains the appearance of reviewing compelling interest and narrow tailoring, but post-SFFA the outcome is predetermined. The affirmative action era showed lower theater (0.55) because the scrutiny was functional; pre-affirmative action showed much lower theater (0.32) because race-consciousness was not yet subjected to the tier. The trajectory reveals that as the tier was applied more uniformly, its theater increased because the functional question (race-consciousness survival) was decided before doctrinal review occurred.
 *
 * PERSPECTIVAL GAP:
 *   The fatal_in_fact trajectory reveals that the perspectival gap between colorblind advocates and remediation programs has expanded over time. During the affirmative action era (1978–2023), the gap was smaller — the doctrine permitted debate about whether compelling interests and narrow tailoring could save programs. The trajectory shows this gap closing: SFFA eliminated the functional gap by making race-consciousness itself the decision point, not the scrutiny tiers. The theater ratio increase (0.32 → 0.68) measures this closing — the doctrine's performative content grows as the functional outcome becomes predetermined.
 *
 * DIRECTIONALITY LOGIC:
 *   Colorblind advocates (beneficiaries with arbitrage options) derive d ≈ 0.10–0.15: low material extraction, high institutional benefit from the tier's enforcement. They experience the tier as coordination, not suppression. Race-conscious programs (victims with trapped exit) derive d ≈ 0.95–1.0: maximum extraction, no exit options, categorical suppression. Institutional administrators (moderate agents with constrained exit) derive d ≈ 0.80–0.88: high extraction through forced choice between institutional costs and near-certain litigation loss. The analytical observer (analytical/analytical) derives d ≈ 0.72 in the mountain perspective, but this is revealed as false summit: the high d indicates the observer is seeing the constraint from the perspective of those it suppresses, not from a neutral vantage point.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. The fatal_in_fact reading resolves mandatrophy by showing that the trajectory from Gunther's quip to SFFA's categorical suppression represents the completion of a doctrinal choice — specifically, the choice to adopt the colorblind-rule reading of equal protection as categorical law rather than as one live position in an ongoing doctrinal contest. The constraint does not exhibit mandatrophy (mixed coordination-extraction) at the post-SFFA endpoint: extractiveness 0.88, suppression 0.92, beneficiary and victim sets clearly demarcated. The mandatrophy appears in the historical trajectory (affirmative action era showed some coordination features — debate about how to balance remediation with equal protection) and in the perspectival gap (colorblind advocates see rope; remediation programs see snare). The resolution is that the current state (post-SFFA) is not mixed; it is pure snare. The transition from mixed to pure was driven by Supreme Court majority coalitional change and explicit doctrinal reorientation (SFFA decision). This is a case where mandatrophy resolution shows not a constraint maintaining ambiguity but a constraint crystallizing into a predetermined form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_variability_across_eras,
    'Was ''compelling interest'' genuinely indeterminate during the affirmative action era (1978–2023), or was the variance in what courts accepted as compelling actually suppressed contestation that appeared as doctrinal flexibility?',
    'Systematic analysis of interest characterizations across circuits and time periods: how often did courts actually find compelling interests in remediation cases vs. how often did they deny strict scrutiny application entirely? Was the apparent doctrinal play a genuine doctrine, or a holding pattern before categorical rejection?',
    'If interests were genuinely indeterminate: the current trajectory is a legitimate doctrinal completion within strict scrutiny logic. If variance was suppressed contestation: SFFA represents closure of a previously open field, making the trajectory one of categorical suppression rather than doctrinal development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelling_interest_variability_across_eras, empirical, 'Whether ''compelling interest'' allowed genuine doctrinal play or merely appeared to').

omega_variable(
    narrow_tailoring_as_escape_route_feasibility,
    'During the affirmative action era, could any race-conscious program have been designed to survive strict scrutiny through narrow tailoring alone, independent of the court''s acceptance of the compelling interest?',
    'Doctrinal history of programs that were upheld post-strict-scrutiny-application; analysis of their design features and how they were characterized in judicial opinions. Were upheld programs genuinely narrowly tailored, or did courts find compact compelling interests to avoid the tailoring question?',
    'If narrow tailoring was a feasible escape: the tier''s current lethality is judicial choice, not doctrinal necessity. If narrow tailoring was never sufficient without favorable interest characterization: the tier was always extractive; the affirmative action era just provided occasional stay of execution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_tailoring_as_escape_route_feasibility, empirical, 'Whether narrow tailoring could independently save a race-conscious program under strict scrutiny').

omega_variable(
    colorblind_axiom_foreclosure_of_remediation_reading,
    'Does the colorblind-rule reading of equal protection (prohibiting all race-conscious government action) logically foreclose the remediation-centered reading (permitting race-conscious remediation of identified discrimination)?',
    'Jurisprudential analysis: are these readings incompatible within any single legal framework, or can they coexist as live doctrinal positions (as they did during the affirmative action era when courts applied strict scrutiny but sometimes upheld remediation)?',
    'If they foreclose each other: SFFA''s result was logically predetermined by the colorblind axiom; the trajectory is doctrinal necessity. If they can coexist: SFFA represents a choice to adopt the colorblind reading exclusively; the trajectory is institutional politics materialized as doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_axiom_foreclosure_of_remediation_reading, conceptual, 'Whether colorblind and remediation readings are logically incompatible').

omega_variable(
    extraction_mechanism_identity_suppression_or_doctrinal_logic,
    'Is the near-total suppression of race-conscious remediation (suppression = 0.92) a doctrinal achievement (strict scrutiny tier''s logic correctly applied) or an extraction mechanism (the tier exists to suppress remediation as a normative matter)?',
    'Comparative analysis: apply strict scrutiny logic to other government classifications (gender, alienage, disability) and measure whether the tier functions as a consistent doctrine or as a mechanism selective to race. Do gender-conscious remediation programs face the same categorical suppression, or does the tier operate asymmetrically?',
    'If asymmetric application to race: suppression is an extraction mechanism, and SFFA represents its completion. If applied consistently: suppression is a doctrinal artifact, and the trajectory is logical consequence of tier structure, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_identity_suppression_or_doctrinal_logic, empirical, 'Whether suppression reflects consistent doctrine or selective extraction').

omega_variable(
    kernel_reading_committer_status,
    'This constraint instantiates the ''fatal_in_fact_trajectory'' reading of the strict_scrutiny_tier kernel. Is the trajectory from Gunther''s quip to SFFA''s categorical suppression a reading that represents a genuine alive position in constitutional law, or is it a post-hoc narrative describing a historical closure that is no longer contested?',
    'Doctrinal mapping: identify scholars, judges, and advocates who currently hold each reading (compelling_interest, narrow_tailoring, fatal_in_fact_trajectory) as live positions. Is the trajectory reading maintained as a live position by any current institutional actor, or is it now purely historical?',
    'If live: the kernel contest remains open; multiple readings coexist. If historical: the fatal_in_fact reading describes a closed trajectory; the kernel has foreclosed to the colorblind reading post-SFFA.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_status, conceptual, 'Whether the fatal-in-fact reading is a live position or historical closure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strict_scrutiny_tier__fatal_in_fact_trajectory, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssft_theater_pre_affirmative_action, strict_scrutiny_tier__fatal_in_fact_trajectory, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ssft_theater_mid_affirmative_action_era, strict_scrutiny_tier__fatal_in_fact_trajectory, theater_ratio, 5, 0.55).
narrative_ontology:measurement(ssft_theater_post_sffa, strict_scrutiny_tier__fatal_in_fact_trajectory, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ssft_extractiveness_pre_affirmative_action, strict_scrutiny_tier__fatal_in_fact_trajectory, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ssft_extractiveness_mid_affirmative_action_era, strict_scrutiny_tier__fatal_in_fact_trajectory, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ssft_extractiveness_post_sffa, strict_scrutiny_tier__fatal_in_fact_trajectory, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(ssft_suppression_pre_affirmative_action, strict_scrutiny_tier__fatal_in_fact_trajectory, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ssft_suppression_mid_affirmative_action_era, strict_scrutiny_tier__fatal_in_fact_trajectory, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(ssft_suppression_post_sffa, strict_scrutiny_tier__fatal_in_fact_trajectory, suppression_requirement, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strict_scrutiny_tier__fatal_in_fact_trajectory, enforcement_mechanism).
narrative_ontology:affects_constraint(strict_scrutiny_tier__fatal_in_fact_trajectory, strict_scrutiny_tier__compelling_interest_jurisprudence).
narrative_ontology:affects_constraint(strict_scrutiny_tier__fatal_in_fact_trajectory, strict_scrutiny_tier__narrow_tailoring_mechanics).

% DUAL FORMULATION NOTE:
% The strict_scrutiny_tier kernel decomposes into three structurally distinct readings: fatal_in_fact_trajectory (this story, ε=0.88, emphasizes historical closure), compelling_interest_jurisprudence (ε=0.65–0.75, emphasizes what counts as compelling), narrow_tailoring_mechanics (ε=0.55–0.68, emphasizes tailoring as operational filter). Each reading has different extractiveness, different beneficiary/victim structures, and different classification profiles. They are linked through the shared kernel but represent different structural positions within the same doctrinal apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strict_scrutiny_tier__fatal_in_fact_trajectory, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
