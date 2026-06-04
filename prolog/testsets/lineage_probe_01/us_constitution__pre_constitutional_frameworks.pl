% ============================================================================
% CONSTRAINT STORY: us_constitution__pre_constitutional_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution__pre_constitutional_frameworks, []).

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
 *   constraint_id: us_constitution__pre_constitutional_frameworks
 *   human_readable: Pre-Constitutional Frameworks Reading of the US Constitution
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The pre-constitutional-frameworks reading interprets the US Constitution
 *   as a compact among already-sovereign states, modifying but not displacing
 *   the Articles of Confederation baseline. Under this reading, states retain
 *   fundamental sovereignty in their own territories; the Constitution grants
 *   enumerated federal powers for specific purposes (commerce, defense,
 *   dispute resolution) but does not create a new sovereign people with
 *   general legislative authority. The reading privileges the founding-era
 *   framing: states ratified the Constitution as sovereign entities, not as
 *   constituent parts of a new nation. This constraint exhibits the
 *   structural hallmarks of Rope at the state level (coordination, low
 *   extraction, states retain arbitrage options) but generates Tangled Rope
 *   and Snare perspectives from creditors and the Union itself, which
 *   experience the weakness of central authority as an extraction mechanism —
 *   not because anyone intends to extract, but because the reading's logical
 *   structure denies the Union enforcement power. The reading's theater ratio
 *   rises sharply over time (from 0.30 to 0.72) as federal supremacy doctrine
 *   becomes the actual governing principle while the pre-constitutional
 *   language persists rhetorically in tenth amendment and state sovereignty
 *   invocations. By the modern era, the reading is substantially
 *   performative: it is ritually invoked in doctrine without actually
 *   governing practice (federal law is supreme, federal courts have final
 *   jurisdiction, federal power extends far beyond enumerated powers). The
 *   reading coexists with four competing interpretations of the
 *   constitutional kernel (bill_of_rights_1791, failed_amendments,
 *   later_amendment_eras, original_constitution_1787), each addressing
 *   different aspects of how the Constitution's authority is grounded and
 *   where its operative meaning is located.
 *
 * KEY AGENTS:
 *   - State Political Classes (Founding Era): Institutional beneficiaries (institutional/arbitrage) — coordinate through Constitution while retaining domestic supremacy. This is their baseline reading.
 *   - State Legislatures: Institutional beneficiaries (institutional/constrained over time) — begin with negotiating power (founding) but experience declining leverage as federal supremacy doctrine takes hold.
 *   - Creditors and Commercial Interests: Powerless victims (powerless/trapped) — seek uniform commercial law and enforcement across state lines but are trapped in fragmented legal space under this reading.
 *   - The Union Itself: Structural victim (powerless/trapped) — given responsibilities (common defense, dispute resolution) without sufficient enforcement power to discharge them.
 *   - Nationalist Reformers: Powerful agents (powerful/mobile) — experience this reading as blocking federal authority and can exercise mobility by adopting competing readings (original_constitution_1787, later_amendment_eras).
 *   - Federal Courts: Institutional mediators (institutional/constrained) — must manage the tension between the pre-constitutional reading's claim (states retain sovereignty) and federal supremacy doctrine (Constitution is supreme law). Over time, courts resolve in favor of supremacy, rendering the pre-constitutional reading subordinate.
 *   - Analytical Observers: Across positions — at founding, the reading is substantive (states genuinely retain options); by modern era, it is substantially performative (invoked rhetorically but subordinate to federal supremacy in practice).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution__pre_constitutional_frameworks, 0.18).
domain_priors:suppression_score(us_constitution__pre_constitutional_frameworks, 0.35).
domain_priors:theater_ratio(us_constitution__pre_constitutional_frameworks, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution__pre_constitutional_frameworks, extractiveness, 0.18).
narrative_ontology:constraint_metric(us_constitution__pre_constitutional_frameworks, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(us_constitution__pre_constitutional_frameworks, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution__pre_constitutional_frameworks, rope).
narrative_ontology:human_readable(us_constitution__pre_constitutional_frameworks, "Pre-Constitutional Frameworks Reading of the US Constitution").
narrative_ontology:topic_domain(us_constitution__pre_constitutional_frameworks, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution__pre_constitutional_frameworks, '3537fd1f-f02c-4660-9ba4-4ec6ba212aef').
narrative_ontology:cs_kernel_codification('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', fixed_text).
narrative_ontology:cs_authority_grounding('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', lineage).
narrative_ontology:cs_interpretation_layer_present('3537fd1f-f02c-4660-9ba4-4ec6ba212aef').
narrative_ontology:cs_reading_relation('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', us_constitution__bill_of_rights_1791, influences).
narrative_ontology:cs_reading_relation('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', us_constitution__original_constitution_1787, coexists_with).
narrative_ontology:cs_reading_relation('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', us_constitution__later_amendment_eras, influences).
narrative_ontology:cs_reading_relation('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', us_constitution__failed_amendments, coexists_with).
narrative_ontology:cs_axiom('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', foundational, pre_constitutional_state_sovereignty).
narrative_ontology:cs_axiom_status(pre_constitutional_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', pre_constitutional_state_sovereignty, conventional).
narrative_ontology:cs_axiom('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', foundational, enumerated_powers_as_ceiling).
narrative_ontology:cs_axiom_status(enumerated_powers_as_ceiling, overridden).
narrative_ontology:cs_axiom_grounding('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', enumerated_powers_as_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', confederal_sovereign_states).
narrative_ontology:cs_drift_state('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', contemporary_federal_supremacy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3537fd1f-f02c-4660-9ba4-4ec6ba212aef', '').
narrative_ontology:cs_kernel_id(us_constitution__pre_constitutional_frameworks, us_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution__pre_constitutional_frameworks, state_political_classes).
narrative_ontology:constraint_beneficiary(us_constitution__pre_constitutional_frameworks, state_legislatures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE POLITICAL CLASSES — Coordinate through the Constitution to solve inter-state conflicts while retaining state sovereignty as the baseline. This reading preserves state supremacy in their own territories and sees the Constitution as a coordination mechanism for federal questions only. States benefit from coordination (dispute resolution, common defense) without surrendering domestic power. Rope classification: low extraction, genuine coordination function, states have arbitrage options (can maintain Articles confederation logic or negotiate separately).
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDITORS AND COMMERCIAL INTERESTS — The pre-constitutional reading leaves states retaining power to regulate commerce, issue currency, and impose tariffs. This reading renders central authority weak and unable to enforce contracts across state lines or protect property rights at scale. Creditors are trapped in a fragmented legal space. Tangled Rope classification: the reading coordinates inter-state commerce mechanically (gives Congress power to regulate) but extracts by leaving enforcement power thin and state-level veto preserved. Extraction arises from the reading's incapacity, not intent — states retain blocking power that creditors cannot overcome.
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE UNION AS COLLECTIVE ENTITY — The pre-constitutional reading denies the Union enforceable powers. The Constitution, read as a compact among sovereign states rather than a creation of a new sovereign, cannot compel state compliance with federal law, cannot levy taxes directly, cannot command militia or commerce regulation with teeth. The Union experiences this reading as a trap: it has responsibilities (common defense, dispute resolution) but insufficient authority to discharge them. Snare classification: suppression of federal authority (states retain veto), extraction of union authority (responsibilities without power), no meaningful exit option (the union cannot dissolve itself unilaterally in this reading).
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONALIST REFORMERS — Agents who believe the Constitution's real power lies in federal supremacy (contrary to the pre-constitutional reading) experience this reading as Rope at the surface but detect an embedded extraction. The reading coordinates discourse through the fiction of state sovereignty while nationalist intent seeks to establish federal supremacy. These agents have mobile exit: they can adopt competing readings (original_constitution_1787 or later_amendment_eras) and push interpretations toward federal power. Rope classification: the reading provides a coordination mechanism for constitutional discourse, but the reformers can exercise mobility by pushing interpretation toward nationalist readings.
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER — From a civilizational perspective, the pre-constitutional reading is a performative invocation of a baseline that never existed: the United States was never actually constituted as a pure league of sovereign states with the Articles as its law. The reading theatrically references the Articles while actually operating under the 1787 Constitution's federal supremacy. Theater arises from the gap between what the reading claims (states are sovereign, compact-based) and what the institutional practice enforces (federal law is supreme, interpretation flows through federal courts). Piton classification: the reading persists through doctrinal invocation and rhetorical tradition despite being functionally superseded by federal supremacy doctrine. Theater_ratio reflects this gap.
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW / LOGICAL STRUCTURE (MOUNTAIN VIEW) — From a position claiming access to the inherent logical structure of political compacts, one could argue that any constitution born from sovereign states must, by logical necessity, preserve some baseline of state sovereignty. This reading treats the pre-constitutional baseline as a logical entailment of how political unions form. However, this perspective risks naturalizing a historically contingent interpretation as a logical law. The engine's false summit detector may flag this as naturalization of a contested reading.
constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution__pre_constitutional_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution__pre_constitutional_frameworks, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution__pre_constitutional_frameworks, TR),
    TR >= 0.70.

:- end_tests(us_constitution__pre_constitutional_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18, rising to 0.28): Low at founding, rising over time. At the founding, the reading accurately reflects the actual distribution of power — states do retain substantial authority, and the Constitution does coordinate rather than subordinate them. Extraction is minimal because the reading describes reality. Over time, extractiveness rises because federal supremacy doctrine diverges from what the pre-constitutional reading claims, yet the reading persists in doctrine and is invoked (especially in tenth amendment and federalism cases) to constrain federal power beyond what the reading's own logic would permit. The gap between rhetoric (states are sovereign) and practice (federal law is supreme) is the extractive mechanism — the reading's invocation now serves to limit federal power in selective domains (privacy, commerce clause outer boundaries, federalism limits) without fully reversing the supremacy doctrine. Suppression (0.35, rising to 0.42): Moderate. At founding, suppression is low because the pre-constitutional reading reflects actual state power. Suppression rises over time as federal enforcement mechanisms (tax collection, interstate commerce courts, federal law supremacy) accumulate, making state sovereignty increasingly difficult to exercise in practice. Theater ratio (0.55, rising to 0.72): The reading becomes increasingly performative. At founding (0.30), it is mostly substantive — states really do coordinate and retain authority. By mid-19th century (0.55), the reading has become substantially rhetorical — civil war and reconstruction amendments establish federal supremacy, yet the pre-constitutional language persists in tenth amendment jurisprudence. By modern era (0.72), the reading is predominantly theatrical: it is invoked to defend state police powers and tenth amendment limits, but actual federal authority extends far beyond the reading's logical scope, federal courts are supreme arbiters, and federal law overrides state law. The rise in theater ratio indicates that the primary function of the reading has shifted from describing federal structure to performing federalism symbolism.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates perspectival divergence primarily along the axis of time horizon and exit options. State political classes (immediate/biographical time, institutional power, arbitrage exit) experience stable Rope classification across the founding era. The Union's perspective (civilizational time, powerless position, trapped exit) exhibits Snare classification throughout. Creditors (biographical time, powerless position, constrained exit) experience Tangled Rope. The piton perspective reflects the reading's own historical position: it was substantive in 1787, became mixed (partially accurate, partially performative) by the mid-19th century, and is now substantially theatrical by design — invoked selectively to defend federalism constraints that the reading's own logic would not support. The natural law perspective (universal/civilizational) risks treating the pre-constitutional baseline as a logical necessity rather than a historically contingent interpretation — the engine's false summit detector would flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   State political classes as beneficiaries enter the derivation with institutional power and arbitrage-level exit options (can negotiate separately, maintain prior confederal structure, or coordinate through the Constitution). They derive d ≈ 0.15, producing low f(d) ≈ -0.01, resulting in negative or near-zero effective extraction chi. They experience Rope. The Union as a structural entity has powerless power in this reading (cannot compel compliance) and trapped exit options (cannot dissolve or redistribute authority without amendment). It derives d ≈ 0.95, producing high f(d) ≈ 1.42, resulting in high effective extraction chi. It experiences Snare. Creditors have powerless power and trapped exit (fragmented legal space with no central adjudicator in the pre-constitutional reading), deriving d ≈ 0.90, producing f(d) ≈ 1.32, resulting in high chi and Tangled Rope classification (some coordination function exists for commerce regulation, but enforcement power is insufficient). The nationalist reformers have powerful power and mobile exit options (can adopt competing readings), deriving d ≈ 0.45, producing f(d) ≈ 0.50, and experiencing Rope at the surface but with the option to exercise interpretive mobility toward federal supremacy readings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    articles_as_actual_baseline,
    'Did the Articles of Confederation constitute the actual legal baseline from which the 1787 Constitution departed, or is this reading a retrospective interpretive choice?',
    'Historical analysis of founding-era texts, ratification debates, and explicit language in the Constitution regarding the Articles'' legal status. Examination of whether the Constitution formally voids or supersedes the Articles.',
    'If Articles were the actual legal baseline: this reading''s claim to represent the historical ground is upheld, extractiveness may be lower (reading accurately describes pre-existing structure). If Articles were merely precedent but not binding legal baseline: this reading is a retrospective interpretive choice, extractiveness increases (reading naturalizes a constructed framing), piton classification may apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(articles_as_actual_baseline, empirical, 'Whether Articles of Confederation were the actual legal baseline or a retrospective reference point').

omega_variable(
    federal_supremacy_foreclosure,
    'Does the Supremacy Clause (Article VI, Clause 2) logically foreclose the pre-constitutional reading''s claim that the Constitution is a compact among sovereign states?',
    'Textual analysis of the Supremacy Clause''s language and founding-era interpretation. Examination of whether ''This Constitution... shall be the supreme law of the land'' forecloses treating states as retaining fundamental sovereignty.',
    'If Supremacy Clause forecloses: this reading coexists with others as a rejected interpretive option but does not logically rule out the original_constitution_1787 reading (which also accepts Supremacy but interprets it differently). If Supremacy is ambiguous: the reading remains live. If Supremacy logically forbids the pre-constitutional baseline: the reading is foreclosed by its own axioms, reclassifying to piton (performative tradition without logical foundation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_supremacy_foreclosure, conceptual, 'Whether Supremacy Clause logically forecloses the pre-constitutional reading').

omega_variable(
    state_ratification_performance,
    'When states ratified the Constitution, did they act as sovereign entities delegating specific powers, or did they act as constituent parts of a new sovereign people?',
    'Analysis of ratification rhetoric and official state positions. Examination of whether state legislatures treated ratification as a sovereign act or a constituent act. Historical record of whether states claimed reserved sovereignty afterward.',
    'If states acted as sovereign delegates: this reading is strengthened (states did constitute a compact). If states acted as constituent parts: this reading is weakened (the ''sovereign state'' baseline is not historically grounded). If ambiguous: the reading remains contested and coexists with competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_ratification_performance, empirical, 'State ratification performance: sovereign delegation or constituent constitution').

omega_variable(
    founding_intent_counterfactual,
    'What would the founding generation have done if the Constitution''s enumerated powers proved insufficient? Would they have construed federal power expansively or amended to add powers?',
    'Historical records of constitutional amendment efforts in early republic. Analysis of how the founding generation responded to federal authority gaps (e.g., Louisiana Purchase, Alien and Sedition Acts debates). Comparison of early practice with later practice.',
    'If founding intent was to amend for new powers: this reading is upheld (enumerated powers remain baseline, expansion requires amendment). If founding intent allowed expansive construction: this reading is foreclosed or weakened (baseline sovereignty was already being overridden in practice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_counterfactual, empirical, 'Founding generation''s counterfactual response to federal authority gaps').

omega_variable(
    reading_as_kernelcontest_position,
    'This reading is one of five interpretations of the constitutional kernel. Does it coexist with the other four readings, or does its core axiom about pre-constitutional sovereignty foreclose or influence them?',
    'Comparative logical analysis of axioms: does asserting pre-constitutional state sovereignty necessarily contradict or weaken the bill_of_rights, original_constitution, or later_amendment readings? Do the readings speak to the same question or different questions about the Constitution''s authority?',
    'If readings coexist: the constraint is a live option in constitutional interpretation, classification stable across readings. If this reading forecloses others: it is a binding interpretation, not a mere option. If other readings foreclose this one: this reading is deprecated, reclassifies toward piton (performative tradition). If readings are cross-cutting (address different constitutional questions): they influence rather than foreclose, and the kernel is genuinely contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernelcontest_position, conceptual, 'Logical relationship between pre-constitutional-frameworks reading and sibling readings of the constitution kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution__pre_constitutional_frameworks, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscf_theater_founding_1787, us_constitution__pre_constitutional_frameworks, theater_ratio, 0, 0.3).
narrative_ontology:measurement(uscf_theater_mid_19c, us_constitution__pre_constitutional_frameworks, theater_ratio, 50, 0.55).
narrative_ontology:measurement(uscf_theater_modern, us_constitution__pre_constitutional_frameworks, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(uscf_extract_founding_1787, us_constitution__pre_constitutional_frameworks, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(uscf_extract_mid_19c, us_constitution__pre_constitutional_frameworks, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(uscf_extract_modern, us_constitution__pre_constitutional_frameworks, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(uscf_suppress_founding_1787, us_constitution__pre_constitutional_frameworks, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(uscf_suppress_mid_19c, us_constitution__pre_constitutional_frameworks, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(uscf_suppress_modern, us_constitution__pre_constitutional_frameworks, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution__pre_constitutional_frameworks, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution__pre_constitutional_frameworks, us_constitution__bill_of_rights_1791).
narrative_ontology:affects_constraint(us_constitution__pre_constitutional_frameworks, us_constitution__original_constitution_1787).
narrative_ontology:affects_constraint(us_constitution__pre_constitutional_frameworks, us_constitution__later_amendment_eras).
narrative_ontology:affects_constraint(us_constitution__pre_constitutional_frameworks, us_constitution__failed_amendments).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional kernel us_constitution. The five sibling readings (bill_of_rights_1791, failed_amendments, later_amendment_eras, original_constitution_1787, pre_constitutional_frameworks) form a constraint family where each reading instantiates a distinct ε value and beneficiary/victim structure. The pre_constitutional_frameworks reading has low extractiveness (0.18, rising to 0.28) and preserves state beneficiary status. Other readings will have different ε values reflecting their different structural claims about where the Constitution's authority is located. The network edges indicate that this reading structurally influences the others by establishing the baseline they either preserve, modify, or reject.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
