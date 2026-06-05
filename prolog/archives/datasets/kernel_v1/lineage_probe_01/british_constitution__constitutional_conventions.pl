% ============================================================================
% CONSTRAINT STORY: british_constitution__constitutional_conventions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_constitution__constitutional_conventions, []).

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
 *   constraint_id: british_constitution__constitutional_conventions
 *   human_readable: British Constitutional Conventions: Binding-but-Unenforceable Usage
 *   domain: political/legal
 *
 * SUMMARY:
 *   The British constitution lives in its conventions:
 *   binding-but-unenforceable usages that do the daily work no statute
 *   describes. This constraint instantiates one specific reading of the
 *   contested british_constitution kernel — the reading that privileges
 *   conventions as the living mechanism of constitutional governance, in
 *   contrast to sibling readings that ground authority in foundational
 *   charters, modern judicialization, parliamentary supremacy statutes, or
 *   the revolution settlement. This reading holds that the British
 *   constitution is not a written document, not a set of cases, not a
 *   codified statute, but a corpus of shared understandings about what is
 *   permitted and what is forbidden. These understandings have no legal force
 *   — breach of a convention is not illegal — yet they constrain all
 *   constitutional actors with near-absolute power. A PM with formal legal
 *   authority to dissolve Parliament at will is bound by the convention
 *   (increasingly formalized but still conventionally rooted) that they must
 *   call elections periodically. A minister with legal power to make policy
 *   is bound by the convention of collective Cabinet responsibility, which
 *   nullifies individual dissent. Parliament with formal legal sovereignty is
 *   bound by the convention that it does not override devolved powers. The
 *   mechanism of enforcement is not law but expectation: breach triggers
 *   scandal, loss of office, or constitutional crisis. This makes the
 *   constraint a tangled_rope at the center (genuine coordination functions)
 *   but a snare at the periphery (actors who lack fluency in the conventions
 *   are trapped by invisible rules). The interval [0, 40] represents roughly
 *   the 1980s to 2020s, during which the theater_ratio of conventions has
 *   risen (more explicit articulation of conventions, more judicial tiptoeing
 *   toward adjudication, more constitutional commentary) while
 *   suppression_requirement has also risen (the cost of convention breach has
 *   increased as constitutional consciousness has grown and media has
 *   amplified scandal). The extractiveness has accumulated as newer
 *   governance actors (devolved governments, independence movements, populist
 *   disruptors) have discovered that formal legal power is constrained by
 *   conventions they were not fluent in.
 *
 * KEY AGENTS:
 *   - Executive Insiders (Cabinet, Crown Office): Primary beneficiary (institutional/arbitrage) — fluent in conventions, navigate them as coordination, experience low extraction
 *   - Constitutional Elites (senior civil service, experienced ministers, constitutional scholars): Secondary beneficiary (institutional/constrained) — gatekeepers of convention knowledge, reproduce insider advantage through mentorship
 *   - Actors Mistaking Legal Power for Permitted Power (new MPs, regional actors, populist challengers): Primary victim (powerless/trapped) — hold formal authority but discover it is nullified by conventions they do not understand
 *   - Constitutional Outsiders (previously excluded groups, novel governance models, non-traditional politicians): Secondary victim (moderate/constrained) — face high costs to learning and respecting conventions they did not write
 *   - Parliamentary Institution (House of Commons, House of Lords): Mixed actor (organized/constrained) — has agency to reshape conventions through organized pressure but faces coordination costs to doing so
 *   - Formal Legal System (Judiciary, statute-based law): Piton actor (institutional/constrained) — maintains performative abstention from convention adjudication through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as universal laws of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_constitution__constitutional_conventions, 0.38).
domain_priors:suppression_score(british_constitution__constitutional_conventions, 0.62).
domain_priors:theater_ratio(british_constitution__constitutional_conventions, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_constitution__constitutional_conventions, extractiveness, 0.38).
narrative_ontology:constraint_metric(british_constitution__constitutional_conventions, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(british_constitution__constitutional_conventions, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_constitution__constitutional_conventions, tangled_rope).
narrative_ontology:human_readable(british_constitution__constitutional_conventions, "British Constitutional Conventions: Binding-but-Unenforceable Usage").
narrative_ontology:topic_domain(british_constitution__constitutional_conventions, "political/legal").

domain_priors:requires_active_enforcement(british_constitution__constitutional_conventions).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_constitution__constitutional_conventions, '3946cdb4-2a2e-4038-83e1-280b2046d3ec').
narrative_ontology:cs_kernel_codification('3946cdb4-2a2e-4038-83e1-280b2046d3ec', implicit).
narrative_ontology:cs_authority_grounding('3946cdb4-2a2e-4038-83e1-280b2046d3ec', lineage).
narrative_ontology:cs_interpretation_layer_present('3946cdb4-2a2e-4038-83e1-280b2046d3ec').
narrative_ontology:cs_reading_relation('3946cdb4-2a2e-4038-83e1-280b2046d3ec', british_constitution__foundational_charters, coexists_with).
narrative_ontology:cs_reading_relation('3946cdb4-2a2e-4038-83e1-280b2046d3ec', british_constitution__modern_judicialization, influences).
narrative_ontology:cs_reading_relation('3946cdb4-2a2e-4038-83e1-280b2046d3ec', british_constitution__parliamentary_supremacy_statutes, coexists_with).
narrative_ontology:cs_reading_relation('3946cdb4-2a2e-4038-83e1-280b2046d3ec', british_constitution__revolution_settlement, coexists_with).
narrative_ontology:cs_axiom('3946cdb4-2a2e-4038-83e1-280b2046d3ec', foundational, unwritten_usage_constitutes_legitimate_authority).
narrative_ontology:cs_axiom_status(unwritten_usage_constitutes_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('3946cdb4-2a2e-4038-83e1-280b2046d3ec', unwritten_usage_constitutes_legitimate_authority, conventional).
narrative_ontology:cs_axiom('3946cdb4-2a2e-4038-83e1-280b2046d3ec', foundational, convention_enforcement_by_expectation_not_law).
narrative_ontology:cs_axiom_status(convention_enforcement_by_expectation_not_law, holdable).
narrative_ontology:cs_axiom_grounding('3946cdb4-2a2e-4038-83e1-280b2046d3ec', convention_enforcement_by_expectation_not_law, conventional).
narrative_ontology:cs_reference_frame('3946cdb4-2a2e-4038-83e1-280b2046d3ec', continuous_transmission_of_political_practice).
narrative_ontology:cs_drift_state('3946cdb4-2a2e-4038-83e1-280b2046d3ec', contemporary_judicialization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3946cdb4-2a2e-4038-83e1-280b2046d3ec', '').
narrative_ontology:cs_kernel_id(british_constitution__constitutional_conventions, british_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_constitution__constitutional_conventions, executive_insiders).
narrative_ontology:constraint_beneficiary(british_constitution__constitutional_conventions, constitutional_elites).
narrative_ontology:constraint_victim(british_constitution__constitutional_conventions, actors_mistaking_legal_power_for_permitted_power).
narrative_ontology:constraint_victim(british_constitution__constitutional_conventions, constitutional_outsiders).
narrative_ontology:constraint_victim(british_constitution__constitutional_conventions, novel_governance_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL OUTSIDER (SNARE) — An actor with statutory legal power but no fluency in the binding conventions discovers their authority is nullified by invisible usage. A backbench MP with formal legislative power finds themselves bound by convention against defying the Cabinet. A newly empowered regional actor (post-devolution) discovers the convention of Westminster supremacy constrains their agency. No escape: breach triggers constitutional crisis and political destruction. The outsider bears the full cost of the convention's suppression while receiving none of its coordination benefits.
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL INSIDER LEARNING THE GAME (TANGLED ROPE) — A minister, career civil servant, or senior party figure learns the conventions through apprenticeship. The conventions do coordinate genuine governance needs: the convention against partisan Cabinet appointments coordinates executive stability; the convention of ministerial accountability coordinates Parliamentary oversight without formal procedure; the Sewel convention coordinates devolved governance. But learning the conventions requires years of access and mentorship, and violating them (even inadvertently) triggers scandal and removal. Moderate experienced extraction — genuine coordination functions exist, but entry costs and exit costs are both high.
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE INSTITUTION / CABINET-IN-BEING (ROPE) — From the perspective of the continuously governing executive, the conventions are pure coordination. The convention of collective Cabinet responsibility coordinates policy coherence across multiple departments; the convention of individual ministerial accountability coordinates delegation from the PM; the convention that the Crown acts on PM advice coordinates the formal apparatus with political reality. The executive experiences these as the baseline rules that enable governance. No extraction is perceived — the institution is the beneficiary whose power is coordinated and stabilized.
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENTARY COALITION (TANGLED ROPE) — Parliament as an institution sees conventions as both coordinate and extractive. The conventions of executive accountability enable Parliamentary oversight without formal procedures; the conventions of ministerial responsibility coordinate delegation. But the conventions also constrain Parliament's formal legal sovereignty — the convention against dissolving Parliament without PM consent, the convention that legislative time is controlled by the executive, the convention that the PM sets the parliamentary agenda. Parliament has agency (can threaten convention breach, can organize to demand convention respect) but faces high costs to exercising it. Generational time horizon: conventions feel durable but organized Parliamentary pressure does reshape them (see Fixed-term Parliaments Act 2011, later repealed; see pressure on PM resignation conventions).
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL LEGAL SYSTEM / JUDICIARY (PITON) — The courts have largely abstained from adjudicating conventions (doctrine of nonjusticiability). The legal system's role is to enforce the statutes that create the formal skeleton; conventions are treated as external to law. This abstention is increasingly performative: recent cases (R v Barclay; R v Prorogation) have shown willingness to test convention boundaries, yet the courts maintain the fiction that they are not adjudicating conventions themselves — they are reading the statutes' implications. The legal system persists in the convention of non-justiciability through institutional inertia and legitimacy anxiety (if courts adjudicate conventions, who will prevent judicial overreach?). Theater ratio: high. The abstention ritual is maintained despite visible strain.
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, unwritten constitutional conventions are an irreducible feature of all stable governing systems: no written constitution can specify every legitimate action and prohibition. The gap between formal authority and permitted authority must be filled by usage, or the system cannot function. This perspective sees the British convention system as demonstrating a universal structural law: all constitutions live in their conventions. However, the structural data reveals this as a false summit — the British convention system is not universal, it is a contingent institutional arrangement that benefits insiders fluent in the usages.
constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_constitution__constitutional_conventions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_constitution__constitutional_conventions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_constitution__constitutional_conventions, TR),
    TR >= 0.70.

:- end_tests(british_constitution__constitutional_conventions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The conventions coordinate genuine governance needs (Cabinet coherence, ministerial accountability, parliamentary procedure), so extractiveness is not the 0.72 of a pure snare. But the conventions also extract a significant premium from outsiders and novel governance actors who are punished for breaking conventions they did not understand. The extractiveness accumulates during the interval (0.28 → 0.38) as the theater of conventions has increased and the cost of breach (scandal, removal, constitutional crisis) has become more visible and more severe. Suppression (0.62): High. Conventions are enforced not by law but by expectation and scandal — a subtler but often more effective mechanism than formal legal prohibition. Breach does not result in imprisonment or fine, but in loss of career, public shame, forced resignation, or constitutional crisis. This produces high suppression despite zero formal legal consequences. The suppression has increased over the interval (0.55 → 0.62) as constitutional consciousness has grown and media amplification of convention breaches has intensified (see Boris Johnson's prorogation scandal; see the intensifying scrutiny of prime ministerial honors conventions). Theater Ratio (0.68): High. The formal legal system treats conventions as non-justiciable, yet courts increasingly adjudicate convention implications without admitting they are doing so (R v Barclay on parliamentary privilege; R v Prorogation on the implied limits on PM power). The convention of non-justiciability is increasingly performative — maintained through institutional anxiety about judicial overreach rather than through genuine uncertainty about whether courts are already adjudicating conventions. The theater ratio has risen over the interval (0.52 → 0.68) as the performance has become more strained and more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The executive insider sees pure coordination (Rope) — the conventions enable stable, efficient governance. The constitutional outsider sees pure extraction and entrapment (Snare) — the conventions are invisible rules that trap them despite formal legal power. The analytical observer at civilizational scale risks seeing an immutable natural law (Mountain) — 'all constitutions live in conventions' — but the structural data reveals this as a false summit: the British convention system is a specific, extractive institutional arrangement that benefits people already inside the system. The perspectival gap is diagnostic: it shows that the same structural phenomenon (binding-but-unenforceable usage) appears as pure coordination to the beneficiary, pure snare to the victim, and risk-laden natural law to the distant observer. The gap also reveals the mechanism: the extractiveness flows through the learning and apprenticeship barrier. Insiders who spent years learning the conventions experience them as coordination. Outsiders who discover the conventions too late experience them as snares.
 *
 * DIRECTIONALITY LOGIC:
 *   The conventions reading produces high directionality differentiation. Executive insiders (beneficiaries with arbitrage exit options) derive d ≈ 0.05-0.15, producing f(d) ≈ -0.10 to 0.00 — they experience negative or near-zero effective extractiveness despite the constraint's moderate base extractiveness. Constitutional outsiders (victims with trapped or constrained exit) derive d ≈ 0.85-0.95, producing f(d) ≈ 1.10 to 1.40 — they experience high effective extractiveness. The scope modifier σ(S) is 1.0 (national scope), so χ = ε × f(d) × 1.0 = 0.38 × f(d). For the insider, χ ≈ -0.04 to 0.00 (no extraction experienced). For the outsider, χ ≈ 0.42 to 0.53 (high extraction experienced). This gap is the core diagnostic feature of the constraint. The beneficiary-victim relationship is not symmetric: the insiders' arbitrage option (they can move laterally between government and private sector, carrying their convention knowledge) is structurally unavailable to outsiders (they cannot earn convention fluency without years of access, which is gatekept). The overrides array is empty: the derivation chain from beneficiary/victim + exit options produces the correct d values without manual override.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_breach_consequence_threshold,
    'What distinguishes a convention breach that triggers a constitutional crisis from a convention breach that is absorbed as mere scandal?',
    'Historical case analysis: correlate convention breaches with parliamentary confidence votes, PM resignations, electoral consequences, and constitutional reform. Identify the threshold for ''crisis'' vs. ''scandal.''',
    'If threshold is low (breach → crisis): conventions are more suppressive than measured, extractiveness should rise to 0.50+. If threshold is high (only catastrophic breaches → crisis): conventions are more flexible than measured, extractiveness should fall to 0.25.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_breach_consequence_threshold, empirical, 'Threshold between convention breach scandal and constitutional crisis').

omega_variable(
    convention_learning_as_extraction,
    'Is the multi-year apprenticeship required to learn conventions a necessary coordination cost or an extractive barrier that benefits insiders?',
    'Comparative analysis: track career outcomes for actors who enter governance with and without prior access to convention-fluent mentors. Measure time-to-influence and scandal risk. Compare with other legislative systems'' onboarding costs.',
    'If learning is pure coordination cost: extractiveness should fall, suppression remains high. If learning is extractive barrier: extractiveness should rise, beneficiary group shrinks to true insiders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convention_learning_as_extraction, empirical, 'Whether convention learning represents coordination cost or extractive barrier').

omega_variable(
    written_vs_unwritten_equivalence,
    'Would formalizing the major conventions into written statute preserve their coordinate functions or destroy them?',
    'Counterfactual analysis: examine cases where conventions were formalized (Fixed-term Parliaments Act 2011, attempts at devolution clarity) and track whether coordination improved or degraded. Test whether flexibility is a feature or a bug.',
    'If formalization preserves function: conventions are not intrinsically unwritten, extractiveness becomes contingent (could fall with formalization). If formalization degrades function: unwritten form is essential to the coordinate mechanism, extraction index is locked by the constraint''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_vs_unwritten_equivalence, conceptual, 'Whether formalizing conventions would preserve or degrade their functions').

omega_variable(
    kernel_reading_contest,
    'Which sibling reading of the british_constitution kernel is closest to being foreclosed by the conventions reading?',
    'Examine whether this reading of conventions logically rules out sibling readings: Does conventions-primacy foreclose foundational_charters (if conventions govern now, are medieval charters still binding)? Does it coexist with modern_judicialization (can conventions and judicialized rights coexist)? Does it influence parliamentary_supremacy (can statute override convention, or do conventions override statute)?',
    'If conventions foreclose foundational_charters or parliamentary_supremacy: kernel contest has asymmetric logical structure (some readings rule out others). If all readings coexist: kernel is genuinely underdetermined. If conventions influence but don''t foreclose: kernel is layered (multiple readings stack).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationships between conventions reading and sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_constitution__constitutional_conventions, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcc_tr_t0, british_constitution__constitutional_conventions, theater_ratio, 0, 0.52).
narrative_ontology:measurement(bcc_tr_t20, british_constitution__constitutional_conventions, theater_ratio, 20, 0.58).
narrative_ontology:measurement(bcc_tr_t40, british_constitution__constitutional_conventions, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(bcc_be_t0, british_constitution__constitutional_conventions, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bcc_be_t20, british_constitution__constitutional_conventions, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(bcc_be_t40, british_constitution__constitutional_conventions, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bcc_su_t0, british_constitution__constitutional_conventions, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bcc_su_t20, british_constitution__constitutional_conventions, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(bcc_su_t40, british_constitution__constitutional_conventions, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_constitution__constitutional_conventions, identity_coordination).
narrative_ontology:affects_constraint(british_constitution__constitutional_conventions, british_constitution__foundational_charters).
narrative_ontology:affects_constraint(british_constitution__constitutional_conventions, british_constitution__modern_judicialization).
narrative_ontology:affects_constraint(british_constitution__constitutional_conventions, british_constitution__parliamentary_supremacy_statutes).
narrative_ontology:affects_constraint(british_constitution__constitutional_conventions, british_constitution__revolution_settlement).

% DUAL FORMULATION NOTE:
% The british_constitution kernel decomposes into five structurally distinct readings, each with its own constraint story and ε value. The conventions reading (this story, ε=0.38, tangled_rope) is downstream of and influences all sibling readings. The foundational_charters reading (ε unknown, likely mountain or rope) grounds authority in documents; conventions provide the interpretive layer that makes charters actionable. The modern_judicialization reading (ε unknown) asserts that courts now adjudicate constitutional limits; conventions become subject to judicial review or displacement. The parliamentary_supremacy reading (ε unknown) asserts that statute overrides convention; conventions become subordinate to parliamentary enactment. The revolution_settlement reading (ε unknown) fixes the constitution at 1688-1701; conventions are interpreted as continuous with that settlement, not innovations. Each reading has different beneficiary and victim groups, different suppression mechanisms, and different terminal classifications. The network edges link them as members of a constraint family, not as logical implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
