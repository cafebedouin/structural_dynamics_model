% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent Corpus as Evolutionary Framework
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   The common law precedent corpus serves as a framework for legitimate
 *   normative evolution within the judiciary. This reading — the evolutionary
 *   framework — treats precedent as an adaptive, reinterpretable resource
 *   that enables appellate courts to update doctrine as constitutional and
 *   social conditions change. Unlike strict stare decisis (which treats
 *   precedent as nearly immutable absent extraordinary circumstances), the
 *   evolutionary framework normalizes precedent reinterpretation as a
 *   legitimate judicial function. Unlike pluralist balancing (which
 *   distributes normative authority across multiple institutional actors),
 *   the evolutionary framework concentrates updating authority in the
 *   appellate judiciary while treating the precedent corpus as the medium
 *   through which evolutionary development occurs. This constraint exhibits
 *   the full range of common law classification: subordinate courts
 *   experience it as binding constraint (snare), litigants experience it as
 *   mixed predictability and uncertainty (tangled rope), appellate courts
 *   experience it as empowering coordination mechanism (rope), reform
 *   coalitions experience it as both opportunity and barrier (tangled rope),
 *   formalists experience it as doctrinal degradation (piton), comparative
 *   constitutionalists see it as a temporary staging point (scaffold), and
 *   the civilizational analytical observer risks naturalizing it as an
 *   immutable feature of legal systems (false summit).
 *
 * KEY AGENTS:
 *   - Appellate Judiciary: Primary beneficiary (institutional/arbitrage) — exclusive authority to reinterpret and update precedent; gains normative power under this reading
 *   - Subordinate Courts: Primary victim (powerless/trapped) — bound by appellate reinterpretation with no exit pathway; experience maximum constraint
 *   - Individual Litigants: Secondary victim (moderate/constrained) — experience constraint as mixed predictability and uncertainty; can litigate but face resource barriers
 *   - Legal Academy & Reform Coalitions: Organized secondary actor (organized/constrained) — use evolutionary framework to coordinate doctrinal critique and litigation strategy; constrained by time and institutional access
 *   - Parties Relying on Prior Precedent: Distributed victim class (powerless/trapped) — their reliance interests are sacrificed when precedent is reinterpreted
 *   - Doctrinal Stability / Precedent Reliance Baseline: Abstract victim collective (powerless/trapped) — the general expectation that precedent provides predictable constraint is degraded
 *   - Formalist Originalist School: Institutional secondary observer (institutional/arbitrage) — advocates for constraint of reinterpretation; experience operative constraint as degradation of ideal form (piton perspective)
 *   - Comparative Constitutional Movement: Organized secondary observer (organized/mobile) — view evolutionary framework as transitional toward global legal harmonization (scaffold perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.48).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent Corpus as Evolutionary Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '5ba616f5-a55b-4cc2-a9ad-61df0381d768').
narrative_ontology:cs_kernel_codification('5ba616f5-a55b-4cc2-a9ad-61df0381d768', formalized).
narrative_ontology:cs_authority_grounding('5ba616f5-a55b-4cc2-a9ad-61df0381d768', lineage).
narrative_ontology:cs_interpretation_layer_present('5ba616f5-a55b-4cc2-a9ad-61df0381d768').
narrative_ontology:cs_reading_relation('5ba616f5-a55b-4cc2-a9ad-61df0381d768', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('5ba616f5-a55b-4cc2-a9ad-61df0381d768', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('5ba616f5-a55b-4cc2-a9ad-61df0381d768', foundational, doctrine_must_evolve_normatively).
narrative_ontology:cs_axiom_status(doctrine_must_evolve_normatively, holdable).
narrative_ontology:cs_axiom_grounding('5ba616f5-a55b-4cc2-a9ad-61df0381d768', doctrine_must_evolve_normatively, deontological).
narrative_ontology:cs_axiom('5ba616f5-a55b-4cc2-a9ad-61df0381d768', foundational, appellate_courts_legitimate_doctrine_updaters).
narrative_ontology:cs_axiom_status(appellate_courts_legitimate_doctrine_updaters, holdable).
narrative_ontology:cs_axiom_grounding('5ba616f5-a55b-4cc2-a9ad-61df0381d768', appellate_courts_legitimate_doctrine_updaters, instrumental).
narrative_ontology:cs_reference_frame('5ba616f5-a55b-4cc2-a9ad-61df0381d768', adaptive_common_law_tradition).
narrative_ontology:cs_drift_state('5ba616f5-a55b-4cc2-a9ad-61df0381d768', contemporary_rights_expansion_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5ba616f5-a55b-4cc2-a9ad-61df0381d768', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, constitutional_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, normative_innovation).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, doctrinal_stability).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, precedent_reliance_baseline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE COURT SYSTEM (SNARE) — Lower courts are bound by appellate precedent with no meaningful exit. They cannot overrule prior binding authority. They experience maximum extractive constraint: appellate courts reserve normative authority and can reverse lower-court interpretations retroactively. Suppression is total — subordinate judges have no pathway to challenge the precedent framework itself within their institutional role.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LITIGANT CLASS (TANGLED ROPE) — Individual litigants benefit from precedent as predictability (coordination function) but are constrained by the fixed doctrinal baseline. The evolutionary framework permits litigants to challenge precedent through appellate litigation, but this pathway is costly and uncertain. Constrained exit: litigants can litigate but face resource barriers and success uncertainty. Mixed extraction and coordination.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: APPELLATE JUDICIARY (ROPE) — Appellate courts experience the precedent corpus as coordination mechanism for resolving doctrinal ambiguity and updating norms. The evolutionary framework empowers appellate courts to reinterpret or overrule precedent as normative conditions change. Institutional actors with arbitrage options (can exit particular doctrinal lines by reinterpreting or distinguishing them). Net beneficiary of the constraint — the judiciary's authority to update doctrine is the constraint's primary function.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL ACADEMY & REFORM COALITIONS (TANGLED ROPE) — Organized groups (law professors, civil rights organizations, public interest law firms) use the evolutionary framework to coordinate doctrinal critique and shape appellate litigation strategy. They benefit from precedent instability (provides opening for reform) but are constrained by the time required to shift doctrine through litigation. Constrained exit: reformers must work through the appellate system; they cannot impose doctrine directly. Active enforcement: law review articles, amicus briefs, litigation strategy all enforce this constraint.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMALIST ORIGINALIST SCHOOL (PITON) — Formalist originalists see the evolutionary framework as doctrinal degradation. They advocate for strict constraint of precedent reinterpretation to textual fidelity. Their perspective treats the constraint as a degraded version of what it should be (fixed textual meaning, stable doctrine). Theater ratio high: formalist school performs doctrinal consistency while accepting that their preferred constraint is not the operative one. Institutional actors with arbitrage options who experience the operative constraint as a fallen form of the ideal.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL TRANSPLANT MOVEMENT (SCAFFOLD) — International and comparative constitutional scholars view the evolutionary framework as a temporary staging point for global legal harmonization. They see the common law precedent corpus as gradually transitioning toward comparative constitutional principles and international human rights law. The evolutionary framework enables this transition — it creates doctrinal flexibility. Mobile exit: scholars and courts can exit to alternative systems (civil law, international law, constitutional transplants). Sunset logic: as global legal standards converge, the common law precedent corpus becomes less distinctive.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the evolutionary framework is a natural law of legal systems: all systems must balance precedent stability against normative change. The tension between constraint and innovation is inherent to any legal order. This perspective sees the common law precedent corpus as an immutable structural feature of jurisprudence — systems that prohibit precedent reinterpretation become brittle; systems that permit it create instability. The equilibrium between constraint and innovation is a structural necessity, not a contingent institutional choice. However, the beneficiary/victim declarations will trigger false summit detection.
constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_law_precedent_corpus__evolutionary_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, TR),
    TR >= 0.70.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The evolutionary framework permits doctrinal reinterpretation, which reduces the binding force of prior precedent. However, extractiveness is not high because the framework also provides litigants with a legitimate pathway to challenge adverse precedent through appellate litigation. Subordinate courts bear extraction as total constraint (no exit), but appellate courts and litigants with resources can arbitrage the system. The measurement progression (0.30→0.35→0.38) shows extractiveness rising over time as the practice of precedent reinterpretation becomes more frequent and normalized, increasing predictability failure for parties relying on prior doctrine. Suppression (0.48): Moderate. Lower courts and individual litigants face significant barriers to challenging precedent (cost, access, institutional hierarchy), but the framework explicitly permits appellate reinterpretation, so suppression is not total. The framework normalizes precedent challenge as legitimate, which reduces suppression relative to strict stare decisis. Theater ratio (0.52): Moderate. The evolutionary framework involves significant theater—judicial opinions invoke 'changed circumstances,' 'evolved understanding,' or 'correction of prior error' to justify reinterpretation. The theater increased over time (0.38→0.52) as the practice became routinized and courts developed more elaborate justifications for overruling. However, the theater is not maximal because reinterpretation is genuinely treated as an available tool, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   The evolutionary framework creates maximum perspectival divergence across the indexed contexts. Appellate courts see coordination and empowerment (rope) — reinterpreting precedent enables them to update doctrine in response to changed circumstances. Subordinate courts see binding constraint with no exit (snare) — they must follow appellate doctrine regardless of whether they believe prior precedent should be reinterpreted. Individual litigants see mixed coordination and extraction (tangled rope) — precedent provides predictability, but the framework permits courts to reinterpret, creating uncertainty. Formalists see the framework as doctrinal degradation (piton) — they believe precedent should be more strictly binding and invoke the evolutionary framework while criticizing its excess. Comparative constitutionalists see it as a temporary staging point (scaffold) — they expect global legal convergence to eventually supersede the common law precedent distinction. The analytical observer from civilizational scope risks seeing an immutable natural law (mountain) — all legal systems must balance precedent stability against normative change — but the structural data reveals this as a false summit: the evolutionary framework is a doctrine that concentrates normative authority in the appellate judiciary, not an inherent feature of jurisprudence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the constraint. Appellate courts occupy the beneficiary position with arbitrage options — they can exit any doctrinal line by reinterpreting or overruling it. Their derived d is low (~0.15), producing negative f(d), making their effective extractiveness near zero or beneficial. Subordinate courts occupy the victim position with trapped exit options — they cannot reinterpret binding precedent, only follow it. Their derived d is high (~0.95), producing maximum f(d) (~1.42), making their effective extractiveness maximum. Litigants occupy a mixed position — they can litigate to challenge precedent but face resource barriers (constrained exit, ~0.55 d), producing moderate extraction (~0.75 f(d)). The appellate judiciary's power to update doctrine is the constraint's primary function, so they are the beneficiary despite being the mechanism through which the constraint operates. This is the crucial structural distinction: the constraint does not extract from appellate courts (they benefit); it extracts from subordinate courts and litigants who must live with the consequences of reinterpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The evolutionary framework resolves mandatrophy by demonstrating that the constraint is genuinely a tangled rope at the analytical level: it coordinates doctrinal adaptation (genuine coordination function — appellate courts need a mechanism to update doctrine) while extracting from litigants and subordinate courts (asymmetric extraction — parties relying on prior precedent face retroactive loss of their bargain; subordinate courts face binding updates with no voice). The tension between coordination and extraction is not a misclassification problem; it is the structural reality. The framework succeeds in enabling normative evolution (coordination benefit) but does so by concentrating power in the appellate judiciary and making reliance on precedent riskier (extraction mechanism). The mandatrophy is resolved by accepting that both functions are genuine and that the constraint's persistence depends on the coordination benefit outweighing the extraction cost from the perspective of institutional actors who support the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reinterpretation_threshold_ambiguity,
    'What degree of doctrinal reinterpretation constitutes legitimate evolutionary development versus illegitimate precedent overruling that violates the precedent corpus''s own authority?',
    'Comparative analysis of overruling events across common law jurisdictions (US, UK, Canada, Australia); identification of patterns in how courts justify reinterpretation vs overruling; longitudinal tracking of ''evolution'' vs ''reversal'' framing in judicial opinions',
    'If threshold is high (strict constraint on reinterpretation): evolutionary framework collapses toward strict stare decisis; extractiveness decreases, constraint becomes mountain-like. If threshold is low (permissive reinterpretation): precedent corpus loses binding force; constraint becomes snare for lower courts relying on today''s precedent knowing it may be reversed tomorrow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reinterpretation_threshold_ambiguity, conceptual, 'What counts as legitimate evolutionary development vs illegitimate precedent overruling').

omega_variable(
    appellate_court_legitimacy_grounding,
    'Does the appellate judiciary''s authority to reinterpret doctrine rest on democratic representativeness, expert judgment, or the organic evolution of practice?',
    'Textual analysis of foundational common law authorities and constitutional texts grounding judicial power; comparison of legitimacy claims across different common law jurisdictions; empirical tracking of which legitimacy frame (democratic accountability, expertise, organic evolution) courts invoke when justifying reinterpretation',
    'If grounded in democratic representativeness: evolutionary framework risks extracting from litigants who lack political voice. If grounded in expertise: framework favors institutional power concentration. If grounded in organic evolution: framework distributes normative authority more broadly but becomes harder to stabilize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_court_legitimacy_grounding, conceptual, 'What legitimizes appellate reinterpretation of precedent').

omega_variable(
    precedent_reliance_sacrifice_necessity,
    'Is the sacrificing of reasonable reliance on prior precedent a necessary cost of evolutionary normative framework, or can precedent stability be maintained through alternative mechanisms (prospective overruling, legislative correction, safe-harbor doctrines)?',
    'Empirical study of reliance costs in overruled doctrine; comparative analysis of jurisdictions employing prospective overruling; measurement of legislative response rates to overruling; analysis of safe-harbor and grandfathering doctrines in US law',
    'If reliance sacrifice is necessary: evolutionary framework''s victim class (parties relying on prior precedent) cannot exit; extraction is structural and inherent. If alternatives exist: reliance sacrifice becomes a contingent policy choice, not an inevitable cost; the constraint is less inevitably extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_reliance_sacrifice_necessity, empirical, 'Whether precedent reliance sacrifice is necessary to evolutionary framework or avoidable via alternative mechanisms').

omega_variable(
    reading_contest_kernel_identity,
    'Is the common law precedent corpus itself stable enough to permit three coherent readings (evolutionary_framework, strict_stare_decisis, pluralist_balancing), or do the readings'' axioms imply that the kernel is fundamentally contested with no unified corpus?',
    'Meta-analysis: can a single legal system coherently instantiate all three readings, or must a jurisdiction choose one? Historical examination of jurisdictional drift across readings; analysis of whether judicial opinions invoke reading-switching as legitimacy mechanism',
    'If the kernel permits all three readings: evolutionary framework is one legitimate position among coexisting alternatives. If the readings are mutually exclusive: this reading partially forecloses the others; the common law precedent corpus is not unified but fragmented.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether common law precedent corpus can simultaneously instantiate all three readings or requires exclusive choice').

omega_variable(
    false_summit_natural_law_vs_doctrine,
    'Is the evolutionary framework a natural law of legal systems (constraint''s narrative logic), or is it a doctrine chosen by appellate courts who benefit from the normative authority it grants them (FSM trigger)?',
    'Historical analysis: do common law jurisdictions converge on evolutionary framework independently, or do they adopt it through doctrinal transplant from influential precedents (US/UK/Canada)? Comparative legal examination of non-common-law systems; analysis of whether civilizations lacking common law tradition show equivalent doctrinal evolution',
    'If natural law: mountain classification legitimate; beneficiaries are artifacts. If doctrine with beneficiaries: false summit; engine reclassifies to tangled_rope; appellate judiciary''s power to reinterpret doctrine is revealed as institutional extraction masked as jurisprudential necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_doctrine, conceptual, 'Whether evolutionary framework is natural law of legal systems or appointed doctrine benefiting appellate courts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpce_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.38).
narrative_ontology:measurement(clpce_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.48).
narrative_ontology:measurement(clpce_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(clpce_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clpce_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(clpce_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(clpce_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clpce_su_t10, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(clpce_su_t20, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, appellate_authority_consolidation).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, precedent_reliance_stability).

% DUAL FORMULATION NOTE:
% The common law precedent corpus is instantiated through three structurally distinct constraint stories representing competing readings of the same kernel. Each reading has its own extractiveness (ε), its own perspectives, and its own classification. The evolutionary_framework reading (this story) has ε=0.38 and claims tangled_rope type. The strict_stare_decisis reading has lower extractiveness and claims rope type (precedent provides stronger constraint). The pluralist_balancing reading distributes institutional authority and has a different beneficiary/victim structure. All three stories link to the same network neighbors because they address the same domain question: how should the precedent corpus function? But each reading decomposes that question differently. This is the canonical pattern for kernel-reading constraint families: one kernel, multiple readings with different ε values and structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, powerless, 0.95).
constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, moderate, 0.55).
constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, organized, 0.48).
constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
