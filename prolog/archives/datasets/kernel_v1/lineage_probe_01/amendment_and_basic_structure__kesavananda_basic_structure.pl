% ============================================================================
% CONSTRAINT STORY: amendment_and_basic_structure__kesavananda_basic_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_and_basic_structure__kesavananda_basic_structure, []).

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
 *   constraint_id: amendment_and_basic_structure__kesavananda_basic_structure
 *   human_readable: Kesavananda Basic Structure Doctrine: Constitutional Unamendability
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   The Kesavananda Bharati judgment (1973) established the doctrine of
 *   'basic structure' — a constitutional ceiling below which Parliament
 *   cannot descend even by amendment. The doctrine declares that judicial
 *   review, federalism, secularism, and democracy are foundational and
 *   unamendable. This constraint instantiates ONE READING of a contested
 *   kernel that spans the Constitution's entire amendment history: the First
 *   Amendment (1951) showed parliament amending against early court
 *   interpretations; the 42nd Amendment (1976) showed parliament overriding
 *   courts entirely; the 44th Amendment (1978) showed court-protective reform
 *   and mutual realignment; and Kesavananda (1973, crystallized post-1978)
 *   shows courts claiming final veto over amendment itself. The constraint
 *   exhibits the full range of perspectival classification: to constituent
 *   power claims, it appears as an absolute snare; to Parliament, as
 *   constrained coordination; to courts, as enabling rope; to organized
 *   reform movements, as mixed coordination-extraction; to the formal
 *   amendment procedure, as degraded theater; and to the analytical observer,
 *   as a false summit (appears as immutable law but reveals structural
 *   beneficiaries and suppression mechanisms). The extractiveness value
 *   (0.38) reflects that the doctrine performs genuine coordination
 *   (Parliament and courts both need a stable reference frame for
 *   constitutional interpretation) while also enabling judicial veto over
 *   Parliamentary will in the name of protecting 'basic structure.' The
 *   suppression value (0.62) reflects the high institutional barriers to
 *   challenging the doctrine itself — any amendment would be struck down as
 *   itself attempting to destroy basic structure, creating a logical ceiling.
 *   Theater ratio (0.55) reflects that the amendment procedure remains
 *   formally sovereign but substantively constrained by judicial
 *   interpretation of what counts as 'basic'; the procedure is neither fully
 *   performative nor fully functional. This is Kesavananda's own
 *   institutional reality: a constraint that stabilizes constitutional
 *   governance while foreclosing certain transformative possibilities.
 *
 * KEY AGENTS:
 *   - Constitutional Continuity (Beneficiary): The abstract continuity of the Constitution itself is institutionally protected by the basic structure doctrine; the Constitution as an entity cannot be abolished through amendment, only reinterpreted.
 *   - Judicial Review Authority (Beneficiary): The courts gain institutional power and legitimacy from being custodians of the basic structure; the doctrine grounds judicial review of amendments in principle rather than mere power.
 *   - Constituent Power Claims (Victim): Any agent or movement seeking to fundamentally transform the Constitution's foundational architecture (abolish judicial review, dissolve federalism, end secularism, overturn democracy) faces absolute suppression.
 *   - Parliamentary Amendment Supremacy (Victim): Parliament loses the classical unlimited amendment power; its sovereignty is bounded by a judicially-policed ceiling.
 *   - The Parliament: Institutional actor constrained by the doctrine but also benefiting from its coordination function — Parliament knows the boundaries within which amendments will be upheld.
 *   - The Judiciary: Institutional actor that enforces the doctrine and derives authority from it; carries burden of continuously interpreting what counts as basic.
 *   - Organized Constitutional Reform Movements: Organized agents that seek structural change but must navigate the Kesavananda ceiling; some proposals succeed (within basic structure), others fail (attempt to destroy it).
 *   - The Analytical Observer: Risks naturalizing a contingent institutional arrangement (judicial veto over amendment) as an immutable constitutional law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_and_basic_structure__kesavananda_basic_structure, 0.38).
domain_priors:suppression_score(amendment_and_basic_structure__kesavananda_basic_structure, 0.62).
domain_priors:theater_ratio(amendment_and_basic_structure__kesavananda_basic_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_and_basic_structure__kesavananda_basic_structure, extractiveness, 0.38).
narrative_ontology:constraint_metric(amendment_and_basic_structure__kesavananda_basic_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(amendment_and_basic_structure__kesavananda_basic_structure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_and_basic_structure__kesavananda_basic_structure, tangled_rope).
narrative_ontology:human_readable(amendment_and_basic_structure__kesavananda_basic_structure, "Kesavananda Basic Structure Doctrine: Constitutional Unamendability").
narrative_ontology:topic_domain(amendment_and_basic_structure__kesavananda_basic_structure, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(amendment_and_basic_structure__kesavananda_basic_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_and_basic_structure__kesavananda_basic_structure, '8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f').
narrative_ontology:cs_kernel_codification('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', formalized).
narrative_ontology:cs_authority_grounding('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', extraction).
narrative_ontology:cs_interpretation_layer_present('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f').
narrative_ontology:cs_reading_relation('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', amendment_and_basic_structure__first_amendment_1951, forecloses).
narrative_ontology:cs_reading_relation('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', amendment_and_basic_structure__forty_second_amendment_1976, coexists_with).
narrative_ontology:cs_reading_relation('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', amendment_and_basic_structure__forty_fourth_amendment_1978, influences).
narrative_ontology:cs_axiom('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', foundational, constitutional_core_is_unamendable).
narrative_ontology:cs_axiom_status(constitutional_core_is_unamendable, holdable).
narrative_ontology:cs_axiom_grounding('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', constitutional_core_is_unamendable, deontological).
narrative_ontology:cs_axiom('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', foundational, judicial_stewardship_of_core).
narrative_ontology:cs_axiom_status(judicial_stewardship_of_core, holdable).
narrative_ontology:cs_axiom_grounding('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', judicial_stewardship_of_core, instrumental).
narrative_ontology:cs_reference_frame('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', constitutional_immutability_through_judicial_veto).
narrative_ontology:cs_drift_state('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', contemporary_expansive_basic_structure_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8e2b9c6f-54a7-4773-bc25-27d6eefc6d8f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(amendment_and_basic_structure__kesavananda_basic_structure, amendment_and_basic_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_and_basic_structure__kesavananda_basic_structure, constitutional_continuity).
narrative_ontology:constraint_beneficiary(amendment_and_basic_structure__kesavananda_basic_structure, judicial_review_authority).
narrative_ontology:constraint_victim(amendment_and_basic_structure__kesavananda_basic_structure, constituent_power_claims).
narrative_ontology:constraint_victim(amendment_and_basic_structure__kesavananda_basic_structure, parliamentary_amendment_supremacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENT POWER CLAIMS (SNARE) — Agents seeking to amend the Constitution's foundational architecture face absolute suppression. The Kesavananda doctrine forecloses entire categories of amendment (abolishing judicial review, dismantling federalism, striking secularism, overturning democratic structure) from the parliament's jurisdiction. No exit mechanism exists short of a revolution. Highest experienced extraction — structural ceiling on constitutional transformation.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENT (TANGLED ROPE) — Parliament retains broad amendment power (Articles can be added, ordinary provisions rewritten, even fundamental rights reallocated) but faces a structural ceiling enforced by courts. The constraint both enables (unlimited amendment within the basic structure) and constrains (the basic structure itself is untouchable). Parliament experiences genuine coordination benefit — the ceiling prevents its own amendments from being recursively amended away — but also experiences extraction: courts hold veto power over parliament's conception of constitutional change.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (ROPE) — The courts benefit from the Kesavananda doctrine as a coordination mechanism: it provides a stable frame for review (courts need a fixed reference point to evaluate amendments as constitutional or not). The judiciary's power derives from being the custodian of the basic structure, but this is also a burden — courts must continuously interpret what counts as 'basic.' Net beneficiary through institutional authority; experiences this as coordination of constitutional order rather than extraction.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (TANGLED ROPE) — Organized groups seeking structural constitutional change (e.g., movements for greater state autonomy, proposals to alter the secular-democratic character, efforts to redistribute power between center and states) face real but not absolute barriers. They must navigate the Kesavananda ceiling but can still operate within it — most reform proposals touch derivative structures, not the basic structure itself. Mixed extraction and coordination: the constraint channels reform into permissible paths while blocking others.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMENDMENT PROCESS (PITON) — The formal amendment procedure under Article 368 has become substantially performative. Parliament passes amendments; courts either ratify them (if within basic structure) or strike them (if not). The elaborate procedure creates an appearance of parliamentary sovereignty while actual gatekeeping power resides in the judiciary's interpretation of 'basic structure.' Theater ratio reflects the gap between the amendment's formal legitimacy and the court's ultimate veto. The ritual persists through constitutional habit rather than functional amendment sovereignty.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY (MOUNTAIN) — From a civilizational vantage, the Kesavananda doctrine appears to identify an immutable core to constitutional systems themselves: the claim is that any constitution that loses judicial review, federalism, secularism, or democracy ceases to be itself. This perspective sees the basic structure as emerging naturally from the logic of constitutional government rather than as a judicially imposed constraint. However, the structural data (beneficiaries who benefit from the doctrine's enforcement, victims who are suppressed, suppression requiring active institutional effort) reveals this as a false summit.
constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_and_basic_structure__kesavananda_basic_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_and_basic_structure__kesavananda_basic_structure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(amendment_and_basic_structure__kesavananda_basic_structure, TR),
    TR >= 0.70.

:- end_tests(amendment_and_basic_structure__kesavananda_basic_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The doctrine performs genuine coordination — both Parliament and courts benefit from having a stable frame for evaluating amendments (Parliament knows which changes will be upheld; courts have a principled basis for review beyond pure discretion). But the doctrine also enables extraction: courts hold veto power over Parliament's amendment agenda, and the definition of 'basic structure' is continuously re-interpreted by courts, creating uncertainty and leverage. The value reflects this mix. Extractiveness has risen from 0.12 (at Kesavananda's announcement in 1973) to 0.38 (contemporary) as the doctrine has crystallized and courts have applied it to increasingly diverse areas. Suppression (0.62): High. The doctrine creates a logical and institutional ceiling: any attempt to amend the 'basic structure' is itself an amendment attempting to destroy the Constitution, which courts will strike down. This creates a paradox where challenging the ceiling IS the boundary violation, making the suppression self-enforcing and nearly absolute. No exit mechanism exists except revolution. The suppression reflects that agents seeking fundamental constitutional transformation cannot operate within the legal system; they must exit it entirely. Theater ratio (0.55): Moderate-high. The formal amendment procedure under Article 368 retains full parliamentary sovereignty — Parliament votes, amends, the Constitution is formally amended. But substantive gatekeeping power resides in the judiciary's interpretation of 'basic structure.' The procedure is neither purely performative (amendments do occur and do shift constitutional meaning) nor purely functional (courts hold veto power). The theater has increased over time (from 0.40 in 1973 to 0.62 in contemporary jurisprudence) as an interpretive layer has thickened — courts now give elaborate reasoning about what counts as basic structure, creating the appearance of rigorous doctrine while actually performing flexible re-interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives exhibit the full range of DR classification for this constraint. Constituent power claims (powerless/trapped) see pure extraction (Snare) — absolute suppression with no exit. Parliament (powerful/constrained) sees mixed coordination and extraction (Tangled Rope) — the doctrine both enables and constrains amendment. The judiciary (institutional/arbitrage) sees coordination (Rope) — the doctrine provides the frame the courts need to adjudicate amendments. Organized reform movements (organized/constrained) see constrained coordination (Tangled Rope) — they can operate within the basic structure, but certain transformations are foreclosed. The amendment procedure itself (institutional/arbitrage) sees a degraded ritual (Piton) — the formal procedure persists through institutional habit, but substantive power has migrated to judicial interpretation. The analytical observer (analytical/analytical) risks seeing immutable constitutional law (Mountain) — the claim that any constitution that loses judicial review, federalism, secularism, or democracy ceases to be itself. This last perspective is a false summit. The structural data clearly shows beneficiaries (courts, constitutional continuity as institutional concept), victims (constituent power claims, parliamentary amendment supremacy), and active suppression requiring institutional enforcement. This is not a natural law; it is a judicially-enforced doctrine that distributes power and forecloses alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) emerges from the agent's structural position within the constraint. Constituent power claims are full targets (d≈0.95, trapped exit → victim status → high f(d) ≈ 1.42, maximum experienced extraction). Parliament is a mixed beneficiary-target (d≈0.50, constrained exit + powerful position, benefits from coordination but bears veto cost, moderate f(d) ≈ 0.65). The judiciary is a beneficiary (d≈0.10, arbitrage exit + institutional position, derives authority from doctrine, low f(d) ≈ -0.01, negative effective extraction — courts experience the doctrine as enabling rather than extractive). Organized reform movements are secondary targets (d≈0.65, constrained exit + organized position, some agency but fundamental goals foreclosed, moderate f(d) ≈ 1.00). The amendment procedure is a beneficiary-in-ritual (d≈0.15, arbitrage exit + institutional position, but experiencing degradation through thickening interpretive layer, low f(d) ≈ -0.01). The analytical observer is approximately at the center of the constraint's impact distribution (d≈0.72, analytical exit means external position, perceives the full structure, f(d) ≈ 1.15, sees the structural extraction even if not personally subject to it). These directionality values derive from the beneficiary/victim declarations and exit options; they are not arbitrary.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION via perspectival decomposition: The Kesavananda doctrine resolves the mandatrophy — the tension between claiming both coordination AND extraction — by showing that both are true from different perspectives and that the constraint exhibits genuinely different classifications from different structural positions. This is not an inconsistency or a classification error; it is the architecture of the constraint itself. Constituent power claims experience pure extraction (Snare) because they bear 100% of the suppression cost and have zero exit options. Parliament experiences coordination with extraction (Tangled Rope) because the doctrine both enables broad amendment (coordination benefit) and prevents certain amendments (extraction cost). The judiciary experiences coordination (Rope) because the doctrine provides the frame courts need to perform review without being merely discretionary. The amendment procedure experiences degraded ritual (Piton) because the formal sovereignty persists while substantive power has shifted. And the analytical observer risks seeing immutable law (Mountain) — naturalizing what is actually an institutional arrangement. The doctrine is best understood as a successful tangled rope that has institutionally benefited judges and the Constitution-as-entity while suppressing agents seeking fundamental transformation. It performs genuine coordination (Parliament and courts both need stable rules) while extracting from those pursuing alternatives. The classification does NOT change; the doctrine IS a tangled rope from most structural positions. The mandatrophy is resolved by recognizing that this is exactly what a tangled rope should look like from the vantage of a powerless target (snare experience) versus a beneficiary (rope experience).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_structure_definition_contestation,
    'What constitutes the ''basic structure'' of the Constitution? Is it fixed and discoverable, or is it continuously redefined by judicial interpretation?',
    'Historical analysis of which features the courts have declared basic (judicial review, federalism, secularism, democracy clearly basic; separation of powers, constitutional supremacy, sovereign democratic republic contested; property rights, state power over certain sectors, amendment procedure itself debated). Comparative examination of whether courts'' understanding of basic structure has changed across decades.',
    'If fixed and discoverable: basic structure is a genuine ceiling (mountain-like). If continuously reinterpreted: basic structure is a shifting target (extractive, vulnerable to judicial capture). If path-dependent: basic structure crystallizes through precedent (piton-like, inertial). Different classifications emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_structure_definition_contestation, conceptual, 'Whether basic structure is fixed or continuously reinterpreted by courts').

omega_variable(
    majoritarian_vs_countermajoritarian_framing,
    'Does the Kesavananda doctrine protect constitutional democracy against majoritarian destruction (countermajoritarian rationale) or does it entrench judicial power at the expense of constituent authority (majoritarian rights rationale)?',
    'Examination of how the doctrine was invoked in historical moments: the post-Emergency (44th Amendment) case where courts protected democracy against authoritarian amendment; versus ordinary amendment proposals where courts have blocked parliamentary will on matters not directly threatening democratic structure. Assess whether the doctrine correlates with protecting democracy or protecting judicial prerogative.',
    'If countermajoritarian protection dominant: Kesavananda is a scaffold (temporary protection against a specific threat). If judicial entrenchment dominant: it is a snare (permanent veto by unelected authority). Framing determines classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_vs_countermajoritarian_framing, conceptual, 'Whether basic structure doctrine protects democracy or entrench judicial power').

omega_variable(
    amendment_as_abolition_empirical_threshold,
    'How much of the basic structure must be altered for a purported amendment to count as abolition rather than amendment? Is there a quantitative or purely qualitative threshold?',
    'Comparative study of proposed amendments the courts have evaluated: at what point of structural change does an amendment shift from permissible to impermissible? Hypothetical tests (if Parliament amended the preamble to remove ''democratic'' but kept judicial review + federalism, is that abolition? If Parliament curbed judicial review powers without formally removing the institution, is that abolition?). Analysis of courts'' actual holdings for consistency in threshold application.',
    'If threshold is clear and consistently applied: the doctrine provides a stable frame (rope-like coordination). If threshold is vague or inconsistently applied: the doctrine is weaponizable (snare-like, unpredictable). Current evidence suggests threshold is post-hoc and reasoned from outcome (extraction signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_abolition_empirical_threshold, empirical, 'Empirical threshold distinguishing amendment from abolition').

omega_variable(
    kernel_reading_contest,
    'Which reading of the amendment-and-basic-structure kernel is institutionally dominant at specific historical moments: the 1951 First Amendment (parliament amending against courts), the 1976 Emergency (parliament suppressing courts), the 1978 post-Emergency (courts and parliament realigning), or the Kesavananda doctrine (courts holding final veto)?',
    'Historical tracing of which reading frames constitutional discourse at each moment. The First Amendment represents unambiguous parliamentary supremacy; the 42nd Amendment represents parliamentary override of courts; the 44th Amendment represents court-protective reform and court restoration; Kesavananda represents judge-protective doctrine. At any given moment, which reading is the lived constitutional practice? How do courts themselves cite the doctrine to justify their particular holdings?',
    'This is the kernel-level omega. Different readings coexist but with shifting institutional force. Kesavananda is currently dominant in post-1978 jurisprudence, but its dominance is not inevitable — parliamentary supermajorities and constituent assembly could resurrect the 1951 reading or impose a 1976-like override. Uncertainty about which reading will remain institutionally binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of amendment-and-basic-structure kernel is institutionally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_and_basic_structure__kesavananda_basic_structure, 1973, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbs_theater_1973_nascent_doctrine, amendment_and_basic_structure__kesavananda_basic_structure, theater_ratio, 0, 0.4).
narrative_ontology:measurement(kbs_theater_1978_post_emergency_stabilization, amendment_and_basic_structure__kesavananda_basic_structure, theater_ratio, 5, 0.55).
narrative_ontology:measurement(kbs_theater_contemporary_interpretive_layer_thickening, amendment_and_basic_structure__kesavananda_basic_structure, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(kbs_extractiveness_1973_kesavananda_judgment, amendment_and_basic_structure__kesavananda_basic_structure, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(kbs_extractiveness_1976_emergency_42nd_amendment_challenge, amendment_and_basic_structure__kesavananda_basic_structure, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(kbs_extractiveness_1978_post_emergency_44th_amendment, amendment_and_basic_structure__kesavananda_basic_structure, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(kbs_extractiveness_1990_2000_doctrine_crystallization, amendment_and_basic_structure__kesavananda_basic_structure, base_extractiveness, 8, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(kbs_suppression_1973_doctrine_announcement, amendment_and_basic_structure__kesavananda_basic_structure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kbs_suppression_1976_emergency_test, amendment_and_basic_structure__kesavananda_basic_structure, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(kbs_suppression_contemporary_institutional_entrenchment, amendment_and_basic_structure__kesavananda_basic_structure, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_and_basic_structure__kesavananda_basic_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(amendment_and_basic_structure__kesavananda_basic_structure, amendment_and_basic_structure__first_amendment_1951).
narrative_ontology:affects_constraint(amendment_and_basic_structure__kesavananda_basic_structure, amendment_and_basic_structure__forty_second_amendment_1976).
narrative_ontology:affects_constraint(amendment_and_basic_structure__kesavananda_basic_structure, amendment_and_basic_structure__forty_fourth_amendment_1978).

% DUAL FORMULATION NOTE:
% The amendment-and-basic-structure kernel family consists of four constraint stories, one per reading. Each story instantiates a different structural understanding of the Constitution's amendment power: parliamentary supremacy (1951), parliamentary override of courts (1976), court-protective reform (1978), and judicial veto via basic structure doctrine (Kesavananda). Each reading has its own ε value (1951: ≈0.15, rope; 1976: ≈0.72, snare; 1978: ≈0.35, tangled_rope; Kesavananda: 0.38, tangled_rope) reflecting different empirical understandings of what the Constitution's amendment rules actually permit. These are not the same constraint viewed from different angles; they are distinct structural claims about where amendment power resides. All four stories are linked via network.affects_constraints. The Kesavananda reading is currently institutionally dominant (post-1978), but its dominance is contingent — a sufficiently large parliamentary majority with sufficient political will could invoke the 1976 reading or attempt to establish the 1951 reading's pure parliamentary supremacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_and_basic_structure__kesavananda_basic_structure, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
