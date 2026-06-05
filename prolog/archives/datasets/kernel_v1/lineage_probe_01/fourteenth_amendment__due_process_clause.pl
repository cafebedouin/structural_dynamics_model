% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment__due_process_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment__due_process_clause, []).

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
 *   constraint_id: fourteenth_amendment__due_process_clause
 *   human_readable: Fourteenth Amendment Due Process Clause: Suppression of Arbitrary State Deprivation
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Due Process Clause operates as a vehicle for
 *   imposing procedural and substantive constraints on state power to deprive
 *   persons of life, liberty, or property. Ratified in 1868 as a
 *   Reconstruction-era response to arbitrary deprivation of the newly
 *   emancipated, the clause has functioned as the constitutional engine of
 *   incorporation (extending Bill of Rights protections against states) and
 *   the source of judicially-recognized substantive liberty rights (marriage,
 *   procreation, bodily autonomy, intimate association). The constraint
 *   exhibits a complex structural hybridity: it coordinates federalism
 *   (creates hierarchy, establishes federal judicial review authority) while
 *   simultaneously extracting state sovereignty and imposing federal limits
 *   on state discretion. The suppression of alternatives (state-level
 *   remedies, legislative appeal, unreviewable executive action) is
 *   substantial, particularly for the poorest litigants who cannot access
 *   federal courts. The theater ratio (0.58) reflects that due process
 *   hearings often satisfy formal procedural requirements while remaining
 *   substantively hollow for the powerless—the promise of 'due process' often
 *   means notice and opportunity to be heard, but without counsel, without
 *   funds, without access to evidence, the hearing is more ritual than
 *   remedy. This reading instantiates ONE interpretation of the contested
 *   Fourteenth Amendment kernel; sibling readings emphasize the Citizenship
 *   Clause (membership protection), Equal Protection Clause (anti-caste
 *   function), or Privileges or Immunities Clause (substantive rights engine
 *   that the Slaughter-House Cases hollowed). The due process reading has
 *   become dominant in constitutional doctrine, but this dominance is itself
 *   a structural choice that influences and partially forecloses alternative
 *   readings.
 *
 * KEY AGENTS:
 *   - Rights-Holders Against States (powerless/trapped): Primary beneficiaries of the clause's protection, but experience maximum extraction due to access barriers; include formerly enslaved persons and contemporary subjects of arbitrary state action
 *   - Unreviewable State Process (abstract): Primary victim—the deprivation mechanism that loses legitimacy once due process review becomes mandatory
 *   - Federal Judiciary (institutional/arbitrage): Institutional beneficiary with interpretive authority; see the clause as pure coordination (establishes their supremacy in substantive liberty questions)
 *   - State Governments (powerful/arbitrage): Experience mixed coordination (procedural regularity has value) and extraction (federal override of state discretion); have arbitrage options but face supremacy ceiling
 *   - Civil Rights Advocacy Organizations (organized/mobile): See the clause as coordination mechanism enabling systematic challenge to arbitrary state action; have significant mobility
 *   - Reconstruction-Era Framers (organized/constrained): Original authors intended the clause as scaffolding—temporary federal override until states internalized the norm; embedded sunset assumption
 *   - Slaughter-House Doctrine Inheritors (institutional/arbitrage): Beneficiaries of the hollowing of Privileges or Immunities Clause; due process dominance marginalizes competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment__due_process_clause, 0.38).
domain_priors:suppression_score(fourteenth_amendment__due_process_clause, 0.52).
domain_priors:theater_ratio(fourteenth_amendment__due_process_clause, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment__due_process_clause, extractiveness, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment__due_process_clause, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fourteenth_amendment__due_process_clause, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment__due_process_clause, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment__due_process_clause, "Fourteenth Amendment Due Process Clause: Suppression of Arbitrary State Deprivation").
narrative_ontology:topic_domain(fourteenth_amendment__due_process_clause, "legal/constitutional").

domain_priors:requires_active_enforcement(fourteenth_amendment__due_process_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment__due_process_clause, '97bfdbb3-1350-4d29-8f7b-83ebbbb31540').
narrative_ontology:cs_kernel_codification('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', formalized).
narrative_ontology:cs_authority_grounding('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', lineage).
narrative_ontology:cs_interpretation_layer_present('97bfdbb3-1350-4d29-8f7b-83ebbbb31540').
narrative_ontology:cs_reading_relation('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', fourteenth_amendment__citizenship_clause, coexists_with).
narrative_ontology:cs_reading_relation('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', fourteenth_amendment__equal_protection_clause, influences).
narrative_ontology:cs_reading_relation('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', fourteenth_amendment__privileges_or_immunities_clause, forecloses).
narrative_ontology:cs_axiom('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', foundational, federal_review_of_deprivation_mandatory).
narrative_ontology:cs_axiom_status(federal_review_of_deprivation_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', federal_review_of_deprivation_mandatory, deontological).
narrative_ontology:cs_axiom('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', foundational, substantive_liberty_protected_from_state_action).
narrative_ontology:cs_axiom_status(substantive_liberty_protected_from_state_action, holdable).
narrative_ontology:cs_axiom_grounding('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', substantive_liberty_protected_from_state_action, deontological).
narrative_ontology:cs_reference_frame('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', federal_review_of_state_deprivation).
narrative_ontology:cs_drift_state('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97bfdbb3-1350-4d29-8f7b-83ebbbb31540', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment__due_process_clause, fourteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment__due_process_clause, rights_holders_against_states).
narrative_ontology:constraint_victim(fourteenth_amendment__due_process_clause, unreviewable_state_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECTED INDIVIDUAL (SNARE) — A person facing arbitrary state deprivation (detention without hearing, property seizure without notice) has no exit. The Due Process Clause promises review but enforcement depends on access to federal courts, which many lack. The constraint suppresses alternatives (state-level remedy, legislative appeal) and extracts compliance through coercive power backed by the threat of deprivation itself. Maximum experienced extraction.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REPRESENTED LITIGANT WITH COUNSEL (TANGLED ROPE) — A person with resources to hire counsel experiences genuine coordination (the due process hearing mechanism does provide a forum for contesting arbitrary deprivation) alongside extraction (litigation costs, delay, procedural complexity, risk of losing and having state power used against them). Benefits from the coordination function but pays real costs.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS ADVOCACY ORGANIZATIONS (ROPE) — These actors see the Due Process Clause as a coordination mechanism that enables systematic challenge to arbitrary state action. They have mobility (can litigate in federal courts, can relocate advocacy focus), and the constraint provides the vehicle for their coordination function. Experience the clause as pure coordination with minimal extraction — it is their tool.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE GOVERNMENTS (TANGLED ROPE) — States experience the Due Process Clause as a constraint on their administrative discretion. They benefit from some coordination (procedural regularity reduces litigation costs in the long term, creates predictability). But they face extraction: the clause limits their ability to act rapidly, forecloses certain expedient deprivations, creates federal oversight. States have arbitrage options (they can appeal to the federal courts, argue for narrow interpretations) but are bound by the supremacy of federal constitutional review.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL JUDICIARY (ROPE) — The federal courts are the primary institutional beneficiary of the Due Process Clause. It grants them the authority to review state action, establishing federal judicial supremacy over substantive liberty determinations. The coordination function is genuine: the clause coordinates federalism by creating a clear hierarchy (federal courts interpret, states comply). The judiciary has arbitrage options (narrow or broad construction) but no real exit. Pure coordination from this structural position.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RECONSTRUCTION-ERA FRAMERS (SCAFFOLD) — The Due Process Clause was authored as a temporary fix to a crisis (post-Civil War arbitrary deprivation of the freedmen's persons and property). From this generational view, it functions as scaffolding: a temporary constitutional vehicle intended to last until states accepted the legitimacy of equal citizenship and substantive liberty protection. The sunset was always implicit — once states internalized the norm, the federal override should become unnecessary. Low effective extraction because the original frame saw this as a time-limited coordination solution.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, some minimal constraint on arbitrary deprivation is inherent to any coherent legal order: no society can function if state power is absolutely unreviewable. The Due Process Clause might be seen as recognizing an immutable principle of legal order itself. However, this perspective risks false summitry — the specific content of 'due process,' what counts as arbitrary, and which persons deserve this protection are entirely contingent institutional choices, not natural laws.
constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment__due_process_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment__due_process_clause, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(fourteenth_amendment__due_process_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Due Process Clause extracts state autonomy by imposing federal review of deprivation decisions. From the rights-holder perspective, it promises liberation but often delivers theater—formal process without material remedy (hence theater_ratio elevation). Extractiveness increases from Reconstruction (0.28, where the novelty of the constraint was high but enforcement capacity limited) through the Lochner era (0.35, substantive due process expanding) and Civil Rights era (0.42, incorporation doctrine broadening), then stabilizes at contemporary levels (0.38) as doctrinal maturity reduces aggressive expansion. Suppression (0.52): Moderate-high. The clause suppresses alternatives to federal review (state-level remedy, legislative appeal, unreviewable executive dispatch). Suppression was highest in Reconstruction (0.65) when the clause was new and enforcement relied on military occupation; declined during Lochner-era rationality review (0.48, courts gave states broader deference); rose again during Civil Rights (0.52) as courts strictly scrutinized race-dependent deprivation. Contemporary suppression remains high because access-to-courts barriers remain: counsel is not guaranteed, litigation is expensive, federal courts are overburdened. Theater Ratio (0.58): Moderate-high. Due process hearings satisfy formal procedural requirements yet often remain substantively hollow for the powerless. Reconstruction-era theater was lower (0.35) because the clause was novel and courts took its promise seriously. Theater rose during the Lochner era (0.55) as the clause became routinized and courts developed predictable patterns. Civil Rights era theater peaked (0.62) as courts absorbed vast incorporation obligations and created formulaic review standards. Contemporary theater (0.58) is slightly lower due to some expansion of access-to-justice mechanisms (in forma pauperis, public defense requirements via Gideon), but the general pattern persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence rooted in structural position. The powerless person facing arbitrary state deprivation sees a Snare—the clause promises process but delivers theater, and they have no exit. The represented litigant sees Tangled Rope—genuine coordination function (the hearing forum exists and sometimes succeeds) mixed with extraction (costs, delay, litigation risk). Civil rights organizations see pure Rope—the clause is their coordination tool, their vehicle for systematic challenge. States see Tangled Rope—they benefit from procedural regularity (coordination) but face federal override (extraction). Federal courts see pure Rope—the clause grants them authority and creates hierarchy. The Reconstruction framers see Scaffold—temporary scaffolding intended to last until states internalize the norm, with implicit sunset. The analytical observer risks Mountain—seeing arbitrary deprivation as inherently illegitimate, thus making the clause a natural law. The engine will evaluate the mountain perspective as a false summit, revealing that the 'inherent illegitimacy' of arbitrary deprivation naturalizes a specific Reconstruction-era institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint tracks the agent's structural relationship to the extraction flow—whether they benefit from or bear costs under the clause. Rights-holders against arbitrary deprivation have high d (0.92) because they are the primary targets of state power, yet the clause's promise to protect them often fails due to access barriers; they are nominal beneficiaries but practical victims, making them targets of extraction. Federal courts have low d (0.08) because they benefit from the authority the clause grants without bearing extraction costs. State governments have moderate d (0.45) because the clause both constrains (they lose some autonomy) and protects (procedural regularity reduces litigation costs). These d values feed into the sigmoid f(d) to produce effective extractiveness (chi) relative to each perspective: powerless agents experience high chi (extraction is painful); institutional courts experience low chi (authority is pleasant); states experience moderate chi (constraint is real but manageable). The directionality derivation is automatic from the beneficiary/victim declarations and power atoms; no overrides are needed because the structural relationships are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends entirely on the observer's structural position and exit options. The Snare classification (powerless/trapped) reflects the ground truth that the powerless face extraction—the clause's promise is real but inaccessible. The Tangled Rope classifications (represented litigant, states) reflect genuine hybridity: coordination functions exist alongside extraction. The Rope classification (federal courts, civil rights organizations) reflects pure coordination from the beneficiary position. The Scaffold classification (Reconstruction framers) reflects the original intent: temporary scaffolding with embedded sunset assumption. The Mountain classification (analytical observer) is a false summit—it naturalizes a contingent institutional choice as inherent to legal order. The mandatrophy is not 'what type is the clause?' but 'what does the clause do to you, and what structural position are you in?' The constraint's core function—suppressing arbitrary state deprivation while creating federal judicial authority—is the same across all perspectives. The different classifications emerge from different experiences of that function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_procedural_boundary,
    'Is the Due Process Clause fundamentally a procedural constraint (requiring process before deprivation) or a substantive constraint (protecting specific liberties from deprivation regardless of process)?',
    'Historical analysis of original framing intent vs. contemporary doctrinal practice (Lochner era, substantive due process revival, rationality review). Examination of whether courts have consistently applied the clause as procedure-only or as protecting substantive rights (marriage, procreation, bodily autonomy).',
    'If procedural-only: clause is lower extractiveness (ε ≈ 0.20), focuses enforcement on hearing access (coordination function). If substantive: clause is higher extractiveness (ε ≈ 0.50+), courts dictate which liberties states must protect (extraction function from state perspective). This reading treats it as both (tangled rope hypothesis) but the boundary remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_procedural_boundary, empirical, 'Whether Due Process Clause enforces procedure or substantive liberty').

omega_variable(
    incorporation_mechanism_legitimacy,
    'Is the incorporation of Bill of Rights protections against states (via the Fourteenth Amendment Due Process Clause) a faithful reading of the clause''s original meaning, or does it represent judicial overreach that reads a procedural constraint as a vehicle for imposing federal limits on state substantive law?',
    'Comparative constitutional history (other federations'' approaches to rights limitation); textual analysis of ''due process'' in 1868 usage; examination of whether incorporation doctrine has created unsustainable doctrinal tensions (e.g., Second Amendment incorporation, incorporation of Excessive Bail Clause).',
    'If original legitimate: incorporation is proper exercise of federal authority (rope/coordination). If overreach: incorporation is extraction of state sovereignty (snare/tangled rope with states as victims). The reading instantiated here assumes legitimate but contested incorporation — tangled rope captures this hybridity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incorporation_mechanism_legitimacy, conceptual, 'Legitimacy of incorporation doctrine as reading of Due Process Clause').

omega_variable(
    access_to_courts_implementation,
    'The Due Process Clause promises review of state deprivation, but does it require states to provide counsel, subsidize litigation, or guarantee federal court access for those unable to afford representation?',
    'Doctrinal analysis of Gideon v. Wainwright, Miranda, and in forma pauperis rules; empirical data on success rates of unrepresented litigants in due process challenges; analysis of whether Due Process Clause''s promise is illusory for the poorest subjects of state power.',
    'If courts read the clause as requiring access-to-justice infrastructure: suppression drops (barriers lower), snare perspective shifts toward tangled rope (more exit options). If courts read the clause narrowly (process required but access not guaranteed): suppression stays high, snare perspective deepens (no real exit despite procedural promise). Current doctrine is mixed, creating uncertainty about the clause''s practical function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_to_courts_implementation, empirical, 'Whether Due Process Clause guarantees practical access to courts').

omega_variable(
    federalism_extraction_vs_protection,
    'From states'' perspective, does the Due Process Clause function as extraction (federal courts overriding state judgment) or protection (courts preventing arbitrary fellow-states from deprivation through interstate comity)?',
    'Comparative analysis of state perspectives across eras: Reconstruction (clause as federal imposition), Lochner era (states chafing at substantive due process), Civil Rights era (states resisting incorporation), contemporary (some states embracing broader due process protections than federal minimum). Assessment of whether states experience federal oversight as extraction or as protection against race-to-the-bottom.',
    'If extraction dominant: state victims perspective strengthens (snare/tangled rope). If protection dominant: state cooperation perspective strengthens (rope). The reading assumes extraction-with-coordination (tangled rope) but the balance varies historically and among states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_extraction_vs_protection, empirical, 'Whether Due Process Clause functions as federalism extraction or protection').

omega_variable(
    reading_contested_within_tradition,
    'This reading of the Fourteenth Amendment as a due process vehicle for substantive liberty is itself contested within constitutional law. Does this reading coexist with equal protection and privileges or immunities readings, or does adoption of this reading foreclose the others?',
    'Doctrinal history: Slaughter-House Cases privileged this reading over privileges/immunities; contemporary scholarship debates whether equal protection or due process is the proper vehicle for individual rights; examination of whether the three readings (due process, equal protection, privileges/immunities) can be held simultaneously by a single adjudicator.',
    'If coexist: all three readings remain live in different litigational contexts (coexists_with relation). If this reading forecloses the others: due process absorption of individual rights protection marginalizes equal protection and privileges/immunities readings (forecloses relation). Current doctrine allows some simultaneity but due process has become dominant, suggesting influences relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contested_within_tradition, conceptual, 'Contest between due process, equal protection, and privileges/immunities readings').

omega_variable(
    false_summit_natural_law,
    'Is the Due Process Clause''s prohibition on arbitrary deprivation a natural law of legal systems (immutable requirement of any coherent legal order) or a contingent institutional choice embodied in constitutional text?',
    'Examination of whether the specific content of ''due process'' and ''liberty'' varies across legal systems and historical periods; analysis of whether the arbitrariness prohibition is logically entailed by law-as-such or is a particular political settlement. Investigation of whether calling this a ''natural law'' serves to naturalize a specific Reconstruction-era policy choice.',
    'If natural law: mountain classification appropriate. If contingent institutional choice: mountain classification is false summit (engine reclassifies to tangled rope or snare via FSM). The beneficiary declaration (rights_holders_against_states) triggers FSM evaluation in the engine; this omega documents the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether Due Process Clause prohibition reflects natural law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment__due_process_clause, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fp14dp_theater_reconstruction, fourteenth_amendment__due_process_clause, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fp14dp_theater_lochner_era, fourteenth_amendment__due_process_clause, theater_ratio, 50, 0.55).
narrative_ontology:measurement(fp14dp_theater_civil_rights, fourteenth_amendment__due_process_clause, theater_ratio, 100, 0.62).
narrative_ontology:measurement(fp14dp_theater_contemporary, fourteenth_amendment__due_process_clause, theater_ratio, 150, 0.58).

% Extraction over time
narrative_ontology:measurement(fp14dp_extractiveness_reconstruction, fourteenth_amendment__due_process_clause, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fp14dp_extractiveness_lochner_era, fourteenth_amendment__due_process_clause, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(fp14dp_extractiveness_civil_rights, fourteenth_amendment__due_process_clause, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(fp14dp_extractiveness_contemporary, fourteenth_amendment__due_process_clause, base_extractiveness, 150, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fp14dp_suppression_reconstruction, fourteenth_amendment__due_process_clause, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fp14dp_suppression_lochner_era, fourteenth_amendment__due_process_clause, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(fp14dp_suppression_civil_rights, fourteenth_amendment__due_process_clause, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(fp14dp_suppression_contemporary, fourteenth_amendment__due_process_clause, suppression_requirement, 150, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment__due_process_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment__due_process_clause, fourteenth_amendment__equal_protection_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__due_process_clause, fourteenth_amendment__citizenship_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__due_process_clause, fourteenth_amendment__privileges_or_immunities_clause).

% DUAL FORMULATION NOTE:
% The Fourteenth Amendment is a composite kernel containing four structurally distinct readings. This story instantiates the due process reading. The ε-invariance principle requires separate constraint stories for each reading because they have different base extractiveness values (due process ε ≈ 0.38, equal protection ε ≈ 0.42, citizenship ε ≈ 0.15, privileges/immunities ε ≈ 0.52 if revived). All four are linked via network.affects_constraints to reflect their constitutional kinship and their documented contest in doctrinal history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
