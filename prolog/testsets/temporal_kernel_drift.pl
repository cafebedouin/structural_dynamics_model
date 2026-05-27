% ============================================================================
% CONSTRAINT STORY: temporal_kernel_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_kernel_drift, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_kernel_drift
 *   human_readable: Constitutional Meaning Fixed at Ratification (Originalist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the US Constitution binds constitutional
 *   meaning to the historical moment of ratification, foreclosing
 *   post-ratification practice, evolving social values, and democratic
 *   amendment (absent formal constitutional change via Article V). This
 *   constraint operates as a rigid interpretive methodology enforced by
 *   federal courts and adopted by a significant scholarly and judicial
 *   movement beginning in the 1980s. The originalist constraint exhibits
 *   tangled rope structure: it provides genuine coordination (a rule-bound,
 *   predictable methodology) while simultaneously extracting value from
 *   rights claimants whose interests are not grounded in 18th/19th-century
 *   historical practice. The constraint has increased in institutional
 *   authority over the measured interval (1994–2024), reflected in rising
 *   theater_ratio and extractiveness as originalist judges dominate Supreme
 *   Court appointments. The measurement trajectory shows practice_drift:
 *   originalism's theatrical legitimacy has increased (more elaborate
 *   historical evidence rituals, more citations to Framers' intent) while its
 *   actual constraint function has become more rigid (fewer post-ratification
 *   doctrines are sustained by originalist reasoning). This reading is one of
 *   three structurally distinct constraint stories arising from different
 *   interpretations of the same kernel—the Constitution's text and its
 *   authority. Sibling readings (living constitutionalism, legal positivism)
 *   would produce different ε values and different beneficiary/victim
 *   structures from the same constitutional text.
 *
 * KEY AGENTS:
 *   - Originalist Legal Movement: Primary beneficiary (institutional/arbitrage) — provides coherent methodology that appeals to judges and ideologically aligns with conservative jurisprudence; benefits from institutional dominance via judicial appointments
 *   - Conservative Property-Rights Doctrine: Primary beneficiary (institutional/arbitrage) — originalism systematically favors historical understandings of property and contract rights over modern regulatory reinterpretations
 *   - Contemporary Rights Claimants: Primary victim (powerless/trapped) — persons asserting constitutional rights not grounded in 18th-century public understanding are categorically foreclosed by originalist constraint
 *   - Progressive Constitutional Scholars: Secondary victim (organized/constrained) — experience the constraint as forcing engagement with originalist methodology while systematically disadvantaging progressive rights claims
 *   - Democratic Majority: Secondary victim (moderate/constrained) — can amend Constitution but faces high institutional barriers; originalist constraint prevents evolutionary constitutional interpretation
 *   - Federal Judiciary: Institutional enforcer (institutional/arbitrage) — maintains constraint through selective citation of historical evidence and performative invocation of original meaning; benefits from legitimacy that rule-bound methodology provides
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement (beneficiary-backed interpretive dominance) as a law of semantics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_kernel_drift, 0.58).
domain_priors:suppression_score(temporal_kernel_drift, 0.72).
domain_priors:theater_ratio(temporal_kernel_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_kernel_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(temporal_kernel_drift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(temporal_kernel_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_kernel_drift, tangled_rope).
narrative_ontology:human_readable(temporal_kernel_drift, "Constitutional Meaning Fixed at Ratification (Originalist Reading)").
narrative_ontology:topic_domain(temporal_kernel_drift, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(temporal_kernel_drift).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_kernel_drift, 'c728e056-b855-4891-9e0f-28b8fe7ea180').
narrative_ontology:cs_created_at('c728e056-b855-4891-9e0f-28b8fe7ea180', '').
narrative_ontology:cs_kernel_codification('c728e056-b855-4891-9e0f-28b8fe7ea180', fixed_text).
narrative_ontology:cs_authority_grounding('c728e056-b855-4891-9e0f-28b8fe7ea180', lineage).
narrative_ontology:cs_interpretation_layer_present('c728e056-b855-4891-9e0f-28b8fe7ea180').
narrative_ontology:cs_kernel_id(temporal_kernel_drift, us_constitution_text).
narrative_ontology:cs_reading_relation('c728e056-b855-4891-9e0f-28b8fe7ea180', living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c728e056-b855-4891-9e0f-28b8fe7ea180', positivist_reading, influences).
narrative_ontology:cs_axiom('c728e056-b855-4891-9e0f-28b8fe7ea180', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c728e056-b855-4891-9e0f-28b8fe7ea180', meaning_fixed_at_ratification, empirically_contingent).
narrative_ontology:cs_axiom('c728e056-b855-4891-9e0f-28b8fe7ea180', foundational, post_ratification_practice_irrelevant).
narrative_ontology:cs_axiom_status(post_ratification_practice_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('c728e056-b855-4891-9e0f-28b8fe7ea180', post_ratification_practice_irrelevant, deontological).
narrative_ontology:cs_reference_frame('c728e056-b855-4891-9e0f-28b8fe7ea180', constitutional_meaning_as_semantic_fact).
narrative_ontology:cs_drift_state('c728e056-b855-4891-9e0f-28b8fe7ea180', contemporary_2024, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_kernel_drift, originalist_legal_movement).
narrative_ontology:constraint_beneficiary(temporal_kernel_drift, property_rights_doctrine).
narrative_ontology:constraint_beneficiary(temporal_kernel_drift, federalist_institutional_framework).
narrative_ontology:constraint_victim(temporal_kernel_drift, contemporary_rights_claimants_without_historical_grounding).
narrative_ontology:constraint_victim(temporal_kernel_drift, adaptive_constitutional_doctrine).
narrative_ontology:constraint_victim(temporal_kernel_drift, democratic_majoritarian_amendment_processes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTEMPORARY RIGHTS CLAIMANT (SNARE) — A person asserting a constitutional right not explicitly grounded in 18th-century public meaning is trapped: the originalist constraint forecloses their claim categorically. No historical evidence = no constitutional basis, period. High suppression (courts categorically reject non-historical arguments) and no exit path (cannot change the past).
constraint_indexing:constraint_classification(temporal_kernel_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC MAJORITY (TANGLED ROPE) — Can amend the Constitution (exit exists, but at high cost: supermajority in both houses + state ratification). The originalist constraint both enables and constrains: it provides a stable, predictable legal foundation for majority action, but it also forecloses unamended constitutional evolution. Moderate power with significant institutional barriers to exit.
constraint_indexing:constraint_classification(temporal_kernel_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST LEGAL MOVEMENT (ROPE) — Benefits from the constraint through institutional dominance: originalism provides a coherent, rule-bound methodology that appeals to judges skeptical of judicial discretion. The constraint functions as coordination mechanism: originalism coordinates judicial behavior around a shared epistemic standard. High exit optionality (can deploy originalism or abandon it) and net beneficiary status.
constraint_indexing:constraint_classification(temporal_kernel_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE CONSTITUTIONAL SCHOLARS (TANGLED ROPE) — Experience the constraint as mixed: originalism provides a common analytical language that enables debate (coordination function), but it systematically disadvantages rights claims not grounded in historical practice (extraction). Can exit via alternative interpretive methodologies, but faces institutional pressure to engage originalism's terms.
constraint_indexing:constraint_classification(temporal_kernel_drift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL JUDICIARY (PITON) — Maintains the constraint through institutional inertia and theater. Originalist methodology requires extensive historical research and archival evidence-gathering, much of which is performative: judges often cite Framers' intent selectively, and historical evidence is contestable (see Balkin, Sunstein). The ritual of originalist reasoning persists because it legitimizes judicial decisions as constrained by law rather than judicial will, but the actual constraint is weaker than the theater suggests.
constraint_indexing:constraint_classification(temporal_kernel_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist view, the Constitution's meaning is inherently fixed at the moment of ratification by logical necessity: meaning cannot exist independent of an act of authorial intention. To change the meaning without amendment is logically impossible — it is a law of semantics. However, this perspective naturalizes what is actually a contested normative claim about how constitutional authority operates. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(temporal_kernel_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_kernel_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_kernel_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_kernel_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_kernel_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_kernel_drift, TR),
    TR >= 0.70.

:- end_tests(temporal_kernel_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The originalist constraint systematically forecloses rights claims that lack historical grounding, regardless of contemporary justice or democratic will. Contemporary beneficiaries of this foreclosure (property owners, federalists, conservative institutions) benefit substantially. The extractiveness is not maximal (0.70+) because the constraint legitimately provides coordination value—originalism enables predictable judicial behavior—and because the extraction is mediated through a coherent normative theory (original public meaning) rather than pure coercion. Suppression (0.72): High. Significant barriers prevent departure from the constraint: (1) institutional: federal courts treat originalism as the binding interpretive methodology; (2) epistemic: originalist scholars dominate constitutional law academia; (3) normative: the constraint is framed as fidelity to the Constitution itself, making departure seem like constitutional violation. Exit requires either amending the Constitution (supermajority threshold) or displacing originalism from institutional authority (multi-decade effort). Theater ratio (0.68): Moderate-high. Originalist methodology requires extensive historical research (archives, ratification debates, contemporaneous usage), much of which serves legitimating function rather than epistemic necessity. Judges often cite Framers' intent selectively; historical evidence is contested and incomplete; the interpretive practice produces outcomes correlated with ideological preferences. The theater has increased over the interval as originalism has become institutionalized—more elaborate evidentiary rituals, more ceremonial citation of historical sources.
 *
 * PERSPECTIVAL GAP:
 *   The originalist constraint produces maximal perspectival divergence. The originalist legal movement sees coordination: a shared epistemic standard that enables predictable judicial behavior and constrains judicial discretion. Contemporary rights claimants see categorically foreclosed paths: their constitutional claims are structurally impossible within the constraint. The democratic majority sees a high-cost amendment process: exit is available but prohibitively expensive. Progressive scholars see a mixed constraint: originalism provides analytical tools and common language (enabling debate) but systematically disadvantages their jurisprudential positions. The federal judiciary sees a degraded ritual: historical evidence is performatively invoked but interpretation remains contestable. The analytical observer sees a natural law of semantics: meaning is inherently fixed at ratification by logical necessity. This last perspective is a false summit—it naturalizes what is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from the base extractiveness (ε=0.58), the agent's structural position (d derived from beneficiary/victim status + exit options), and the scope modifier (national scope → σ=1.0). Originalist legal movement: beneficiary + arbitrage exit → low d (~0.15) → low/negative f(d) → low chi → experiences constraint as rope (coordination benefit). Contemporary rights claimants: victim + trapped exit → high d (~0.95) → high f(d) → high chi → experiences constraint as snare (pure extraction). Democratic majority: victim + constrained exit (amendment possible but costly) → moderate d (~0.65) → moderate f(d) → moderate chi → experiences constraint as tangled rope (mixed coordination and extraction). Progressive scholars: victim + constrained exit (can use alternative methodologies but face institutional pressure) → moderate-high d (~0.70) → moderate-high f(d) → moderate-high chi → experiences constraint as tangled rope. Federal judiciary: beneficiary (legitimacy from rule-bound method) + arbitrage exit → low d → low chi → experiences constraint as rope/piton (coordination + inertia). The perspectival gap is substantial: beneficiaries perceive coordination, victims perceive extraction, scholars perceive mixed constraint, judiciary perceives degraded ritual.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading resolves its own mandatrophy by recognizing that the constraint is NOT a mountain (natural law of semantics) but rather a tangled rope (mixed coordination and extraction). The coordination function is genuine: originalism enables predictable judicial behavior and constrains discretionary interpretation. The extraction function is also genuine: it systematically forecloses rights claims not grounded in historical practice. The constraint requires active enforcement (federal courts continuously applying originalist methodology) and has identifiable beneficiaries (originalist legal movement, conservative doctrine) and victims (contemporary rights claimants). The mandatrophy dissolves when we recognize that originalism is a legitimate interpretive choice adopted by an institutional coalition—not a law of nature. The false summit detector will identify this: beneficiaries are declared (originalist_legal_movement, property_rights_doctrine, federalist_institutional_framework) and the constraint claims mountain-like characteristics (fixed meaning) without the natural-law preconditions (accessibility_collapse, resistance, emerges_naturally are not declared).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_sufficiency,
    'What evidentiary standard determines whether historical evidence adequately establishes original public meaning?',
    'Meta-analysis of originalist decisions: correlation between strength of historical evidence (documented in Framers'' papers, ratification debates, contemporaneous usage) and consistency of application across cases; identification of cases where weak evidence is treated as sufficient or strong evidence is disregarded',
    'If evidentiary standard is stringent: originalism appears coherent but forecloses many historical claims (high suppression). If permissive: originalism appears flexible but loses its distinguishing constraint function (extractiveness rises).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_sufficiency, empirical, 'Evidentiary sufficiency standard for original public meaning').

omega_variable(
    public_meaning_vs_framers_intent,
    'Is the relevant historical baseline the Framers'' private intentions or the publicly understood meaning at ratification?',
    'Comparison of originalist opinions: do they cite Framers'' personal correspondence/private documents (intent) or public ratification debates/contemporaneous newspaper usage (public meaning)? Frequency of cases where these diverge and how courts resolve the divergence.',
    'If public meaning is authoritative: scope of admissible evidence expands (extractiveness may fall, suppression may rise as more actors can claim historical understanding). If Framers'' intent is authoritative: scope narrows and constraint becomes more rigid (higher suppression, higher extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_meaning_vs_framers_intent, conceptual, 'Framers'' intent vs. public meaning distinction').

omega_variable(
    unamended_constitutional_change_possibility,
    'Can constitutional meaning change through practice and interpretation without formal amendment?',
    'Historical case studies: identification of constitutional doctrines that have substantially shifted without amendment language (e.g., Commerce Clause scope, Presidential executive authority). Determination of whether such shifts are justifiable within originalist framework or represent breach of constraint.',
    'If such change is impossible: originalism is an absolute constraint (mountain-like). If such change is possible: originalism is porous and less suppressive (extractiveness may rise, constraint becomes tangled rope). Core theoretical outcome: determines whether this reading forecloses the living constitutionalist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unamended_constitutional_change_possibility, empirical, 'Whether unamended constitutional meaning can change through practice').

omega_variable(
    kernel_vs_committer_framing,
    'Is the Constitution''s authority grounded in fixed semantic meaning (kernel—what it actually says) or in the authority-structure that treats it as fixed (committer—the judicial and legislative institutions that enforce the original-meaning reading)?',
    'Distinguish between: (a) a genuine constraint on meaning (semantic/historical fact: the Constitution meant X in 1787); (b) an institutional decision to treat it as having meant X (normative/political choice: we will enforce the original-meaning interpretation). Identify which one is actually driving the constraint.',
    'If kernel: the constraint derives from semantic fact and is structurally more rigid (mountain-like). If committer: the constraint derives from institutional adoption and is structurally contingent (tangled rope or snare, depending on who benefits). This omega instantiates the core interpretive contest between originalism and its rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_committer_framing, conceptual, 'Kernel (semantic fact) vs. committer (institutional choice) framing of original meaning').

omega_variable(
    originalism_as_false_summit,
    'Does the originalist constraint function as a genuine natural law of constitutional interpretation, or as a beneficiary-backed institutional arrangement naturalized as law?',
    'Cross-tradition analysis: identify beneficiaries of originalism (conservative legal movement, property-rights doctrine, federalist institutional framework). Correlate the rise of originalism''s institutional authority with these beneficiaries'' policy gains (originalist appointments to judiciary, originalist precedents favoring conservative jurisprudence). Determine whether the constraint would persist absent these beneficiary incentives.',
    'If genuine constraint: originalism is mountain. If naturalized institutional arrangement: originalism is tangled rope or snare. High confidence that the engine''s false summit detector will flag this—the beneficiaries are explicitly declared and correlation is empirically documented.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_as_false_summit, empirical, 'False summit detection: originalism as natural law vs. institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_kernel_drift, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temporal_kernel_drift, theater_ratio, 0, 0.42).
narrative_ontology:measurement(temp_tr_t15, temporal_kernel_drift, theater_ratio, 15, 0.58).
narrative_ontology:measurement(temp_tr_t30, temporal_kernel_drift, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temporal_kernel_drift, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(temp_be_t15, temporal_kernel_drift, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(temp_be_t30, temporal_kernel_drift, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_kernel_drift, enforcement_mechanism).
narrative_ontology:affects_constraint(temporal_kernel_drift, living_constitutionalist_reading).
narrative_ontology:affects_constraint(temporal_kernel_drift, legal_positivist_reading).
narrative_ontology:affects_constraint(temporal_kernel_drift, fourteenth_amendment_enforcement_power).
narrative_ontology:affects_constraint(temporal_kernel_drift, judicial_discretion_constraint).

% DUAL FORMULATION NOTE:
% The temporal_kernel_drift constraint represents one reading of the us_constitution_text kernel. It is downstream of the kernel's fundamental ambiguity about how constitutional authority and meaning operate. Sibling readings (living_constitutionalist_reading, legal_positivist_reading) would have different ε values reflecting different structural commitments about constitutional evolution and institutional authority. All three stories share the same kernel but produce different constraint types and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temporal_kernel_drift, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
