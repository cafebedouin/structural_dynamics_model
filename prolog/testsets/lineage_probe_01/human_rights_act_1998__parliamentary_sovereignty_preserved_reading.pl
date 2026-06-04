% ============================================================================
% CONSTRAINT STORY: human_rights_act_1998__parliamentary_sovereignty_preserved_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hra_1998_parliamentary_sovereignty, []).

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
 *   constraint_id: human_rights_act_1998__parliamentary_sovereignty_preserved_reading
 *   human_readable: Human Rights Act 1998: Parliamentary Sovereignty Preserved Reading
 *   domain: legal/constitutional/human_rights
 *
 * SUMMARY:
 *   The Human Rights Act 1998 represents the UK's incorporation of the
 *   European Convention on Human Rights into domestic law. Under this
 *   reading, the HRA's defining feature is what it withheld: Parliament
 *   retained the power to ignore judicial declarations of incompatibility.
 *   While courts gained the power to declare statutes incompatible with
 *   Convention rights (section 4), they lost the power to strike them down or
 *   suspend them. Parliament's remedial obligation is political, not legal.
 *   This reading frames the HRA as a constitutional architecture that
 *   preserves parliamentary sovereignty by design, creating a structured
 *   asymmetry between judicial authority (to declare) and parliamentary
 *   authority (to decide remedy). The constraint emerges from this asymmetry:
 *   human rights claimants obtain judicial validation of their rights claims
 *   but no binding remedy; the incompatible statute persists until Parliament
 *   acts; the claimant's only leverage is reputational pressure on Parliament
 *   and the threat of Strasbourg intervention. This is the
 *   parliamentary_sovereignty_preserved reading — one of three competing
 *   readings of the same HRA kernel. The other readings reframe the same
 *   architecture as either functional incorporation (incorporation_reading)
 *   or effective judicial power (judicial_power_grab_reading). This reading
 *   claims the HRA's genius is structural preservation of sovereignty through
 *   design; the other readings claim the design was either incomplete or
 *   misleading.
 *
 * KEY AGENTS:
 *   - Human Rights Claimants: Primary victim (powerless/trapped) — obtain judicial declarations but no binding remedy; extraction persists until Parliament acts
 *   - Parliamentary Sovereignty Doctrine: Primary beneficiary (institutional/arbitrage) — preserved as ultimate authority through the declaration-without-force mechanism
 *   - Executive Government: Secondary beneficiary (powerful/mobile) — can defend policies through legislative amendment or explicit statutory authorization
 *   - The Judiciary: Institutional actor (institutional/arbitrage) — retains interpretive authority but lacks enforcement power; sees own authority as degraded (piton perspective)
 *   - Strasbourg Court and International System: Organized actor (organized/constrained) — provides backstop remedy but only after UK exhausts domestic options
 *   - Parliament: Institutional actor (institutional/arbitrage) — retains ultimate decision-making power; receives coordination signals from courts
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent design choice as a structural necessity of rights incorporation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, 0.52).
domain_priors:suppression_score(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, 0.68).
domain_priors:theater_ratio(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, tangled_rope).
narrative_ontology:human_readable(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, "Human Rights Act 1998: Parliamentary Sovereignty Preserved Reading").
narrative_ontology:topic_domain(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, "legal/constitutional/human_rights").

domain_priors:requires_active_enforcement(human_rights_act_1998__parliamentary_sovereignty_preserved_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, '7f852809-a327-4476-9ad5-93454ea7f279').
narrative_ontology:cs_kernel_codification('7f852809-a327-4476-9ad5-93454ea7f279', fixed_text).
narrative_ontology:cs_authority_grounding('7f852809-a327-4476-9ad5-93454ea7f279', lineage).
narrative_ontology:cs_interpretation_layer_present('7f852809-a327-4476-9ad5-93454ea7f279').
narrative_ontology:cs_reading_relation('7f852809-a327-4476-9ad5-93454ea7f279', human_rights_act_1998__incorporation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f852809-a327-4476-9ad5-93454ea7f279', human_rights_act_1998__judicial_power_grab_reading, coexists_with).
narrative_ontology:cs_axiom('7f852809-a327-4476-9ad5-93454ea7f279', foundational, judicial_declaration_is_not_strike_down).
narrative_ontology:cs_axiom_status(judicial_declaration_is_not_strike_down, holdable).
narrative_ontology:cs_axiom_grounding('7f852809-a327-4476-9ad5-93454ea7f279', judicial_declaration_is_not_strike_down, deontological).
narrative_ontology:cs_axiom('7f852809-a327-4476-9ad5-93454ea7f279', foundational, parliament_retains_political_discretion).
narrative_ontology:cs_axiom_status(parliament_retains_political_discretion, holdable).
narrative_ontology:cs_axiom_grounding('7f852809-a327-4476-9ad5-93454ea7f279', parliament_retains_political_discretion, conventional).
narrative_ontology:cs_reference_frame('7f852809-a327-4476-9ad5-93454ea7f279', parliamentary_sovereignty_intact_through_retained_remedial_discretion).
narrative_ontology:cs_drift_state('7f852809-a327-4476-9ad5-93454ea7f279', post_twenty_five_years_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f852809-a327-4476-9ad5-93454ea7f279', '').
narrative_ontology:cs_kernel_id(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, human_rights_act_1998).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_beneficiary(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, executive_government).
narrative_ontology:constraint_victim(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, human_rights_claimants).
narrative_ontology:constraint_victim(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, convention_rights_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE HUMAN RIGHTS CLAIMANT (SNARE) — Faces a court system that can declare a statute incompatible with Convention rights but cannot require Parliament to remedy the incompatibility. Structural extraction: the claimant obtains a judicial declaration with no binding remedy. Parliament's silence leaves the incompatible statute operative. The claimant is trapped: no domestic remedy, and the long road to Strasbourg becomes necessary. Maximum experienced extraction because the judicial remedy is symbolic, not substantive.
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENT (ROPE) — Retains ultimate legislative power. A declaration of incompatibility is a coordination signal, not an order. Parliament can amend, repeal, or ignore the incompatible statute. Parliament experiences the constraint as a coordination mechanism: the court has clarified the Convention rights position, and Parliament can choose to respond. Net beneficiary of the architecture — the last word remains Parliament's. No coercion, only an informational signal that can be acted on or not.
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXECUTIVE GOVERNMENT (TANGLED ROPE) — Benefits from preserved parliamentary sovereignty (can defend policies through legislative amendment or explicit statutory authorization) while bearing some cost of declarations of incompatibility (political pressure, reputational damage, international scrutiny). Mobile exit options because government can seek legislative remedy. Mixed benefit and cost — genuine coordination function (courts clarify rights; government can respond legislatively) alongside asymmetric extraction (claimants absorb delay and uncertainty).
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE JUDICIARY (PITON) — The court's power to declare incompatibility is largely performative. The declaration carries no binding force; its only mechanism is political pressure on Parliament and reputational signal to international bodies. Over 25 years, most declarations result eventually in legislative amendment, but some persist for years or decades. The judiciary sees its own power as degraded — they can identify violations but cannot remedy them. The theatrical performance is the declaration itself: a symbolic pronouncement with no binding effect. Theater ratio is high because the declaration's force depends on Parliament's voluntary response, not judicial authority.
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STRASBOURG AND INTERNATIONAL OVERSIGHT (TANGLED ROPE) — The European Court of Human Rights retains ultimate authority; a UK declaration of incompatibility that Parliament ignores eventually triggers Strasbourg intervention and a judgment binding on the UK. The international system provides a coordination function (clarifies UK obligations) while also extracting UK authority (Strasbourg can override UK sovereignty). The UK is constrained by international law obligations but has avenues for response (legislative amendment, derogation, formal notice). Moderate extraction because the international remedy exists but is costly and reputationally damaging.
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational perspective, any system that incorporates international human rights law into domestic courts while preserving ultimate legislative authority necessarily produces this constraint: the court declares but cannot enforce; Parliament retains last word. This is not contingent institutional design but a structural feature of how sovereignty and rights incorporation interact. The declaration-without-enforcement mechanism is an immutable property of the HRA's architecture. However, the structural data (beneficiaries in sovereignty doctrine, victims in claimants, significant suppression and extractiveness) contradicts the mountain gate — the engine will flag this as a false summit, revealing that 'structural necessity' naturalizes what is actually a contingent design choice.
constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_rights_act_1998__parliamentary_sovereignty_preserved_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, TR),
    TR >= 0.70.

:- end_tests(human_rights_act_1998__parliamentary_sovereignty_preserved_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from claimants through delay, uncertainty, and the need to pursue dual remedies (UK courts + Strasbourg). However, the extraction is not total because claimants do obtain judicial validation and some political leverage. Parliament's theoretical 'last word' is exercised in most cases eventually, but many claimants absorb indefinite extraction while waiting. The trajectory shows rising extractiveness over 25 years: early cases (t=0) produced quicker parliamentary responses; contemporary cases (t=25) show lengthening timelines and higher rates of persistent incompatibility. Suppression (0.68): High. The suppression consists of the structural impossibility of domestic judicial remedy. Claimants cannot obtain a binding resolution at home; they are suppressed to seeking an informational declaration and political remedy. The suppression persists because it is engineered into the statute — Parliament must act, and Parliament may choose not to. Rising slightly over time (0.62 → 0.68) as claimants discover the mechanism's limits. Theater ratio (0.55): Moderate. The declaration of incompatibility is partially performative — its force depends on Parliament's voluntary response — but it is not purely theatrical because it does trigger parliamentary amendment in most cases and carries reputational cost internationally. The ratio rises slightly over time (0.48 → 0.55) as Parliament's reluctance to amend increases and the performative aspects of the declaration become more obvious.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. Parliament sees coordination (Rope) — the court clarifies rights, Parliament responds. The claimant sees extraction (Snare) — the court declares but does not remedy; Parliament may never respond. The executive sees mixed coordination and extraction (Tangled Rope) — politically valuable to have a remedial mechanism but also valuable to retain flexibility. The judiciary sees its own degradation (Piton) — authority without enforcement, declaring violations that may persist indefinitely. Strasbourg sees its role as backstop coordination (Tangled Rope) — providing remedy when domestic systems fail. The analytical observer risks seeing immutable structure (Mountain) when the actual structure is contingent design. The gap reveals that 'parliamentary sovereignty preserved' is a reading that forecloses or coexists with the other readings depending on how one interprets section 4's legal force and Parliament's political obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality reflects the agent's structural position relative to this reading. Human rights claimants are trapped victims: no exit from domestic courts, no enforcement power, suppressed to political remedy or Strasbourg. Parliament and the doctrine of sovereignty experience low directionality (beneficiaries with arbitrage exit options — Parliament can choose to remedy or not). The judiciary experiences moderate directionality despite institutional power because their authority is constrained by design — they can declare but not enforce, making them functionally targets of the claimant's failed remedial expectations. The international system experiences moderate directionality because UK sovereignty suppresses its direct power (Strasbourg is backstop only, after UK exhausts domestic remedies). The analytical observer experiences the constraint as mountain-class (universal, civilizational, analytical, analytical exit) and risks naturalizing the design choice as inevitable, but the structural data (beneficiaries in sovereignty doctrine, victims in claimants, suppression-by-design) reveals this as a false summit: the preservation of sovereignty is a choice, not a structural law.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: This reading avoids the mandatrophy by denying that the HRA transferred judicial power to strike down statutes. The constraint is not a conflict between judicial and parliamentary authority but a division of labor: courts declare, Parliament decides remedy. However, the mandatrophy resurfaces at the level of whether this design actually preserves sovereignty or merely performs preservation while functional incorporation occurs through section 3 and political pressure. The competing readings offer different resolutions: the incorporation_reading resolves by claiming the design does incorporate (declarations are effective through political and international pressure). The judicial_power_grab_reading resolves by claiming section 3 effective rewording makes declarations backstop. The parliamentary_sovereignty_preserved_reading resolves by insisting the distinction between declaration (non-binding) and strike-down (binding) preserves real sovereignty despite functional similarities. Each reading resolves mandatrophy differently by choosing which aspect of the design to emphasize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaration_compliance_rate,
    'What proportion of declarations of incompatibility result in legislative amendment, and within what timeframe?',
    'Longitudinal audit of all section 4 declarations since 1998; correlation with parliamentary remedial action; classification of amendments as substantive vs perfunctory vs absent',
    'If compliance rate > 90% within 2 years: the ''last word'' is nearly always exercised remedially, making the constraint approximate to incorporation (judicial power is effective through political pressure). If compliance rate < 50% or timescale > 5 years: Parliament''s theoretical last word is preserved but many claimants absorb indefinite extraction, making snare the stable classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(declaration_compliance_rate, empirical, 'Compliance rate and timeline for parliamentary remedial action after declarations').

omega_variable(
    section_3_interpretive_constraint_scope,
    'Does section 3 interpretive obligation to read statutes compatibly with Convention rights amount to an effective judicial rewording of statutes, or does it remain a constrained interpretive method?',
    'Case law analysis: frequency and magnitude of section 3 reinterpretations that diverge from statutory plain meaning; parliamentary response to section 3 readings; comparison with explicit amendment timelines',
    'If section 3 produces effective rewording functionally equivalent to judicial amendment: the judicial_power_grab reading is structurally correct, and declarations of incompatibility are backstop rather than primary mechanism. This would shift the beneficiary from Parliament to the judiciary and reduce extraction for claimants. If section 3 remains a constrained interpretive tool: the parliamentary_sovereignty reading is preserved, and declarations remain the primary constraint mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_3_interpretive_constraint_scope, empirical, 'Whether section 3 reinterpretation amounts to effective judicial amendment').

omega_variable(
    strasbourg_backchannel_leverage,
    'Does the threat of Strasbourg intervention sufficiently incentivize UK parliamentary amendment that declarations function as de facto binding remedies?',
    'Counterfactual analysis: compare amendment rates for statutes with extant declarations vs hypothetical amendment rates without Strasbourg threat; survey of parliamentary attitudes toward Strasbourg judgments; longitudinal tracking of declarations followed by Strasbourg judgments',
    'If Strasbourg threat is highly effective (> 75% amendment rate within 3 years): the architecture is functionally incorporation despite formal preservation of sovereignty — the snare classification becomes less stable. If Strasbourg threat is weak (< 50% of declarations prevent eventual Strasbourg judgment): Parliament''s authority is genuinely preserved but claimants face dual extraction (declaration + Strasbourg judgment), making snare the primary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strasbourg_backchannel_leverage, empirical, 'Effectiveness of Strasbourg threat in inducing parliamentary remedial action').

omega_variable(
    reading_versus_incorporation_empirical_test,
    'Are human rights claimants functionally better or worse off than they would be if the UK had simply incorporated the Convention into statute law with direct effect?',
    'Comparative analysis: UK under HRA 1998 vs hypothetical direct incorporation regime (modeled on Canadian or Australian approaches) using metrics: time to remedy, claimant success rate, finality of judicial remedies, parliamentary amendment frequency',
    'If outcomes are substantially equivalent: the readings diverge in form but converge in function, and the parliamentary_sovereignty reading is a theoretical preservation without practical effect — the incorporation_reading would be structurally more accurate. If outcomes differ materially: the reading correctly identifies a real structural difference in extraction and remedial capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_incorporation_empirical_test, empirical, 'Comparative outcome analysis: HRA 1998 vs hypothetical direct incorporation').

omega_variable(
    reading_choice_committer_ambiguity,
    'Which reading of the HRA kernel — incorporation, judicial power grab, or parliamentary sovereignty preserved — is the one the HRA''s drafters and primary sponsors intended the courts to adopt?',
    'Legislative history analysis (Hansard records of parliamentary debate 1997-1998); judicial interpretation in early cases (R v Lord Chancellor ex p Witham, R (Alconbury) v Secretary of State); law commission reports and white papers preceding the HRA',
    'If evidence points strongly to one reading: the competing readings are misreadings or aspirational reframings, and this constraint''s classification is either vindicated or refuted by authorial intent. If evidence is ambiguous or contradictory: the kernel is genuinely contested from inception, and the three readings coexist without foreclosure. This resolves the foundational ambiguity about whether this is a neutral architecture preserving sovereignty or a sovereignty-suppressing design dressed in formalist language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_choice_committer_ambiguity, conceptual, 'Authorial intent regarding the HRA''s structural relationship to parliamentary sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hra_parl_theater_t0, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hra_parl_theater_t8, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(hra_parl_theater_t25, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(hra_parl_extractiveness_t0, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hra_parl_extractiveness_t8, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(hra_parl_extractiveness_t25, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hra_parl_suppression_t0, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(hra_parl_suppression_t8, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(hra_parl_suppression_t25, human_rights_act_1998__parliamentary_sovereignty_preserved_reading, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, european_court_of_human_rights_domestic_sovereignty).
narrative_ontology:affects_constraint(human_rights_act_1998__parliamentary_sovereignty_preserved_reading, section_3_statutory_interpretation_rewriting).

% DUAL FORMULATION NOTE:
% The HRA 1998 kernel decomposes into three distinct constraints with different ε values corresponding to the three readings. The parliamentary_sovereignty_preserved_reading (this file) models the design as preserving sovereignty through the declaration-without-enforcement mechanism, with moderate-high extractiveness (0.52) reflecting the claimant's inability to obtain binding domestic remedy. The incorporation_reading (separate file) models the design as functional incorporation through combined section 3 interpretation + political pressure, with lower extractiveness reflecting that declarations are effectively binding. The judicial_power_grab_reading (separate file) models the design as de facto judicial supremacy through section 3 rewriting, with higher extractiveness reflecting that claimants obtain de facto relief but courts claim they are not striking down. Each reading is ε-invariant within its own frame; the three readings coexist as competing interpretations of the same statutory text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
