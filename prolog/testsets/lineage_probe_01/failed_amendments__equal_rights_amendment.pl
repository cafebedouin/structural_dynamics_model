% ============================================================================
% CONSTRAINT STORY: failed_amendments__equal_rights_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failed_amendments__equal_rights_amendment, []).

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
 *   constraint_id: failed_amendments__equal_rights_amendment
 *   human_readable: The Failed Equal Rights Amendment: Sex Equality Doctrine Left to Judicial Discretion
 *   domain: political/legal/constitutional_amendment
 *
 * SUMMARY:
 *   The Equal Rights Amendment (passed by Congress in 1972, requiring 38
 *   state ratifications) represents a failed constitutional effort to make
 *   sex-based equality an explicit textual command. The constraint examined
 *   here is not the ERA itself but the consequence of its failure:
 *   sex-equality doctrine remains grounded in judge-made intermediate
 *   scrutiny (Reed v. Reed, Craig v. Boren) rather than textual prohibition
 *   on sex classification. This reading of the failed-amendments kernel
 *   focuses on the extraction mechanism specific to sex equality—the
 *   doctrinal discretion courts retain to permit sex-based classifications
 *   that express text would foreclose. The absence of ratification is not
 *   merely procedural failure; it is a structural outcome that preserves
 *   judicial authority over sex classification doctrine and denies claimants
 *   a textual anchor for strict scrutiny. The constraint exhibits genuine
 *   coordination alongside extraction: the doctrine has advanced sex equality
 *   significantly through litigation and judicial reasoning, yet claimants
 *   remain dependent on a lower scrutiny tier that permits sex
 *   classifications under circumstances the ERA would have prohibited
 *   absolutely. The theater ratio has increased over time as ratification
 *   efforts have shifted from substantive state-level campaigns to ceremonial
 *   re-ratifications and deadline manipulations (extensions, deadline
 *   removal), performative rituals that no longer connect to amendment of the
 *   constitutional text.
 *
 * KEY AGENTS:
 *   - Sex-Equality Claimants Without Textual Anchor (powerless/trapped) — persons seeking strict scrutiny for sex-based classifications but lacking express constitutional text; bear full cost of intermediate scrutiny standard
 *   - Sex-Equality Advocacy Coalition (moderate/constrained) — litigation-focused organizations (NOW, ACLU, Center for Constitutional Rights) that have advanced doctrine through case work but remain dependent on judicial discretion
 *   - Judicial Authority Over Sex Classification (institutional/arbitrage) — courts that retain doctrinal flexibility and arbitage to determine scrutiny tier, categorical exceptions, and doctrine evolution
 *   - Sex-Hierarchy Beneficiaries Under Doctrine (institutional/constrained) — military, selective service, single-sex military academies, pregnancy classifications that benefit from intermediate scrutiny permitting sex classifications
 *   - Ratification Apparatus (organized/mobile) — state legislatures, congressional sponsors, amendment campaign organizations that participate in formal ratification process now largely ceremonial
 *   - Analytical Observer (analytical/analytical) — constitutionalism grounded in textual authority vs. common-law judicial evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failed_amendments__equal_rights_amendment, 0.58).
domain_priors:suppression_score(failed_amendments__equal_rights_amendment, 0.68).
domain_priors:theater_ratio(failed_amendments__equal_rights_amendment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failed_amendments__equal_rights_amendment, extractiveness, 0.58).
narrative_ontology:constraint_metric(failed_amendments__equal_rights_amendment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(failed_amendments__equal_rights_amendment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failed_amendments__equal_rights_amendment, tangled_rope).
narrative_ontology:human_readable(failed_amendments__equal_rights_amendment, "The Failed Equal Rights Amendment: Sex Equality Doctrine Left to Judicial Discretion").
narrative_ontology:topic_domain(failed_amendments__equal_rights_amendment, "political/legal/constitutional_amendment").

domain_priors:requires_active_enforcement(failed_amendments__equal_rights_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failed_amendments__equal_rights_amendment, '8e50110e-ec75-4557-93e9-4f3f2dd2fc97').
narrative_ontology:cs_kernel_codification('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', fixed_text).
narrative_ontology:cs_authority_grounding('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', lineage).
narrative_ontology:cs_interpretation_layer_present('8e50110e-ec75-4557-93e9-4f3f2dd2fc97').
narrative_ontology:cs_reading_relation('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', failed_amendments__balanced_budget_amendment, coexists_with).
narrative_ontology:cs_reading_relation('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', failed_amendments__child_labor_amendment, coexists_with).
narrative_ontology:cs_reading_relation('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', failed_amendments__dc_voting_rights_amendment, coexists_with).
narrative_ontology:cs_axiom('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', foundational, textual_equality_mandate_required).
narrative_ontology:cs_axiom_status(textual_equality_mandate_required, holdable).
narrative_ontology:cs_axiom_grounding('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', textual_equality_mandate_required, deontological).
narrative_ontology:cs_axiom('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', secondary, doctrine_preserves_judicial_discretion_over_classification).
narrative_ontology:cs_axiom_status(doctrine_preserves_judicial_discretion_over_classification, holdable).
narrative_ontology:cs_axiom_grounding('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', doctrine_preserves_judicial_discretion_over_classification, empirically_contingent).
narrative_ontology:cs_reference_frame('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', constitutional_authority_grounded_in_ratified_text).
narrative_ontology:cs_drift_state('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', contemporary_post_ratification_failure, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('8e50110e-ec75-4557-93e9-4f3f2dd2fc97', '').
narrative_ontology:cs_kernel_id(failed_amendments__equal_rights_amendment, failed_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failed_amendments__equal_rights_amendment, sex_equality_doctrine_as_institutional_authority).
narrative_ontology:constraint_victim(failed_amendments__equal_rights_amendment, sex_equality_claimants_without_textual_anchor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-EQUALITY CLAIMANTS WITHOUT EXPRESS TEXT (SNARE) — Trapped in reliance on intermediate scrutiny (Reed v. Reed standard) rather than strict scrutiny that express ERA text would provide. Maximum experienced extraction: claims must survive a lower constitutional standard, with no textual anchor to reset the bar. No exit from this lower tier without amendment or Supreme Court reversal.
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEX-EQUALITY ADVOCACY COALITION (TANGLED ROPE) — Genuinely coordinated efforts to advance doctrine through litigation have succeeded in raising scrutiny tiers and precedent-building (Craig v. Boren, Mississippi University for Women v. Hogan). But constrained by the higher burden of proving sex-based classifications deserve strict review without textual mandate. Mixed: real coordination/progress alongside extraction (must fight the battle repeatedly for each issue category).
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL DISCRETION OVER SEX CLASSIFICATION DOCTRINE (ROPE) — Benefits from absence of express constitutional command. The judiciary retains arbitage to calibrate scrutiny tier, create categorical exceptions, and maintain doctrinal flexibility. The refusal of the ERA sustains judicial authority over sex classification doctrine. Low experienced extraction (relative to the beneficiary): the constraint enables this actor's function.
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RATIFICATION APPARATUS AND AMENDMENT PROCESS (PITON) — The formal amendment process (2/3 congressional passage, 3/4 state ratification) persists as a procedural requirement. Ratification efforts for the ERA produced performative ritual: state campaigns, deadline extensions (1982 deadline pushed to 1992, then effective date removed in 2017–2020), ceremonial re-ratifications after the artificial deadline passed. The apparatus itself generates theater without functional change to constitutional text. Organized actors (state legislatures, congressional sponsors) participate in a process that has ceased to do what it formally claims (amend the Constitution) yet persists through institutional inertia.
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SEX-HIERARCHY BENEFICIARIES UNDER DOCTRINE (TANGLED ROPE) — Institutional actors (military conscription exemptions, pregnancy classifications, protective labor laws, single-sex military academies) benefit from intermediate scrutiny standard that permits sex classifications for certain purposes. Genuine coordination function exists: the doctrine allows sex-based categories when (per official doctrine) truly important governmental objectives are advanced by substantially related means. But the constraint is also extractive: the absence of strict scrutiny allows discriminatory classifications to survive that would be invalid under express textual equality. Constrained from direct exit (cannot repeal intermediate scrutiny) but benefit from maintaining doctrinal flexibility.
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective grounded in constitutional originalism or textualism, the Constitution's legitimate authority derives from its written text as ratified. Absent an express textual mandate for sex equality, sex-based classifications remain constitutionally permissible as a structural matter. The absence of the ERA's text is not extraction—it is the baseline constitutional condition. This view naturalizes the contingent institutional arrangement (failure to ratify) as structural law. The engine will identify this as a false summit: the constraint's beneficiaries and doctrinal structure reveal that 'constitutional authority through text only' is itself a reading that benefits actors who prefer judge-made doctrine to textual mandates.
constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failed_amendments__equal_rights_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failed_amendments__equal_rights_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failed_amendments__equal_rights_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failed_amendments__equal_rights_amendment, TR),
    TR >= 0.70.

:- end_tests(failed_amendments__equal_rights_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The absence of express textual equality of rights creates extraction through doctrinal discretion. Sex-equality claimants must satisfy intermediate scrutiny rather than strict scrutiny, a measurable legal burden that denies them a categorical protection available if the ERA had been ratified. The 0.58 value reflects that genuine doctrinal progress has occurred through litigation (Reed to Craig to Mississippi), so the extraction is not maximal—but it remains substantial because each new sex classification must be fought through full litigation, and categories of sex-based law (military, pregnancy, protective classifications) retain doctrinal exceptions that an express ERA prohibition would eliminate. Suppression (0.68): High. Significant barriers to alternative doctrinal development include: (1) the closed text of the Constitution limits judicial revision without amendment; (2) a Court majority could overturn Reed/Craig precedents at any time without express textual constraint; (3) state ratification failure means no path to textual revision except a new amendment; (4) the formal amendment process creates high exit costs for actors seeking textual change. Theater ratio (0.55): Moderate. The 1972 congressional passage was substantive (genuine legislative choice to propose a sex-equality amendment). But subsequent activity has increasingly theatrical: the 1982 ratification deadline was ceremonial when it passed (35 of 38 states), then extended to 1992, then the deadline was removed in 2017–2020 with states undertaking ceremonial re-ratifications that do not change the underlying failure. The ratio rises over time as ratification efforts shift from actual state-level substantive engagement to symbolic re-ratification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a full perspectival divergence. Sex-equality claimants experience pure extraction (Snare) because they face a structural disability without remedy. The advocacy coalition experiences mixed coordination and extraction (Tangled Rope) because litigation has advanced doctrine but remaining dependent on judicial discretion. The judiciary experiences a Rope constraint—the absence of textual constraint enables their doctrinal authority and function. Hierarchy beneficiaries experience Tangled Rope—genuine coordination of complex sex classifications through doctrine, but also extraction of flexibility. The amendment process apparatus performs Piton—ratification theater persists despite losing functional connection to constitutional change. The analytical observer risks seeing this as immutable constitutional authority grounded in text-only legitimacy (Mountain), but the structural data reveals that this is a false summit: the absence of textual equality is not a law of nature but a contingent institutional outcome that benefits actors (judiciary, sex-hierarchy institutions) who prefer doctrine to text.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. Sex-equality claimants without textual anchor are the victim group (trapped exit, maximum d ~0.95). Judicial discretion over doctrine is the beneficiary (arbitrage exit, d ~0.15). The extraction flow runs from victims (claimants forced to litigate under lower scrutiny) to beneficiaries (courts retaining doctrinal authority). The sex-equality advocacy coalition is secondary victim (constrained exit, d ~0.65) because they benefit from doctrinal progress but constrained by need to relitigate sex classifications. Sex-hierarchy institutions are secondary beneficiaries (constrained exit, d ~0.40) because they retain classifications that express ERA text would eliminate. The directionality profile is asymmetric: high d for victims (powerless, trapped) produces high f(d); low d for beneficiaries (institutional, arbitrage) produces negative or near-zero f(d). This asymmetry justifies the tangled_rope classification—the constraint has both coordination (doctrinal development) and extraction (doctrinal discretion).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_doctrinal_primacy,
    'Is constitutional equality of rights properly grounded in express text (ERA reading) or in judicial doctrine developed through case-by-case adjudication (doctrine reading)?',
    'Comparative analysis: do textual mandates (14th Amendment''s Equal Protection Clause) produce more stable and comprehensive equality protections than doctrine alone? Are protections doctrinal lose their force after a change in Court composition or interpretive philosophy?',
    'If textual mandates more stable: ERA failure is extractive (denied a durable protection). If doctrine equally stable: ERA failure is merely a different institutional choice (Rope). If doctrine destabilizes rapidly: ERA failure is catastrophic extraction (Snare from broader perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_vs_doctrinal_primacy, empirical, 'Whether textual or doctrinal constitutional equality more stable').

omega_variable(
    intermediate_scrutiny_adequacy,
    'Has intermediate scrutiny (Craig/Mississippi standard) produced sex-equality protections sufficient to capture the ERA''s intended scope, or does the doctrine systematically permit sex-based classifications the ERA would have foreclosed?',
    'Doctrinal comparison: identify sex-based classifications that survive intermediate scrutiny but would be invalid under express ERA text. Count and categorize exemptions. Compare with jurisdictions that treat sex as suspect class (quasi-strict scrutiny).',
    'If intermediate scrutiny adequate: ERA failure is theatrical, not extractive (Piton or Rope). If systematic gaps: ERA failure is extractive (Snare or Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intermediate_scrutiny_adequacy, empirical, 'Whether intermediate scrutiny captures ERA''s scope').

omega_variable(
    ratification_ceiling_contingency,
    'Would a three-state swing (from 35 ratifications in 1982 to 38 of 50 required) have materially changed the sex-equality doctrine landscape, or is the doctrinal floor so entrenched that textual confirmation would be largely ceremonial?',
    'Counterfactual doctrinal analysis: model how Supreme Court jurisprudence would have evolved if ERA had been ratified in 1982 vs. the actual path without it. Assess whether strict scrutiny for sex would have foreclosed or merely accelerated doctrinal developments.',
    'If ratification would be largely ceremonial: constraint is closer to Piton (theater). If ratification would have foreclosed certain doctrinal moves: constraint is closer to Snare (real extraction by denial of textual bar).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ratification_ceiling_contingency, conceptual, 'Counterfactual doctrinal impact of ERA ratification').

omega_variable(
    kernel_reading_contest,
    'Is the failure to ratify the ERA best understood as a reading of the broader failed-amendment kernel, or as a distinct constraint about sex equality doctrine specifically?',
    'Structural comparison with sibling readings (balanced budget, child labor, DC voting): do all four failed amendments exhibit the same extraction mechanism (procedural foreclosure of a particular doctrinal binding), or does the ERA have a structurally distinct mechanism (sex-equality claimants forced to rely on doctrine vs. text)?',
    'If structurally parallel: constraint is instance of general ''failed amendment extraction'' type. If structurally distinct: constraint has unique extractiveness profile (sex hierarchy maintained through doctrinal discretion rather than explicit refusal). This affects network.affects_constraints weight and kernel reading_relations typing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether ERA failure is instance of general amendment failure or distinct constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failed_amendments__equal_rights_amendment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fail_tr_t0, failed_amendments__equal_rights_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fail_tr_t10, failed_amendments__equal_rights_amendment, theater_ratio, 10, 0.48).
narrative_ontology:measurement(fail_tr_t20, failed_amendments__equal_rights_amendment, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(fail_be_t0, failed_amendments__equal_rights_amendment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fail_be_t10, failed_amendments__equal_rights_amendment, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(fail_be_t20, failed_amendments__equal_rights_amendment, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fail_su_t0, failed_amendments__equal_rights_amendment, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fail_su_t10, failed_amendments__equal_rights_amendment, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fail_su_t20, failed_amendments__equal_rights_amendment, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failed_amendments__equal_rights_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(failed_amendments__equal_rights_amendment, failed_amendments__balanced_budget_amendment).
narrative_ontology:affects_constraint(failed_amendments__equal_rights_amendment, failed_amendments__child_labor_amendment).
narrative_ontology:affects_constraint(failed_amendments__equal_rights_amendment, failed_amendments__dc_voting_rights_amendment).
narrative_ontology:affects_constraint(failed_amendments__equal_rights_amendment, sex_classification_doctrine_intermediate_scrutiny).
narrative_ontology:affects_constraint(failed_amendments__equal_rights_amendment, constitutional_amendment_ratification_process).

% DUAL FORMULATION NOTE:
% The ERA failure is both a reading of the general failed-amendments kernel and a distinct constraint about sex-equality doctrine specifically. It is linked to sibling amendment failures through the shared institutional mechanism (ratification foreclosure) but has a unique extraction structure (doctrinal discretion over sex classification). It is downstream of the ratification process constraint (which enables the failure) and upstream of specific sex-classification doctrines (which develop without textual mandate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(failed_amendments__equal_rights_amendment, institutional, 0.25).
constraint_indexing:directionality_override(failed_amendments__equal_rights_amendment, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
