% ============================================================================
% CONSTRAINT STORY: semantic_overload_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semantic_overload_friction, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: semantic_overload_friction
 *   human_readable: The Semantic Saturation Threshold
 *   domain: technological/social
 *
 * SUMMARY:
 *   The semantic saturation threshold represents the friction created as
 *   specialized domains mature and accumulate specialized vocabulary. Law,
 *   medicine, software engineering, theoretical physics, and academic
 *   disciplines across all fields face this constraint: the precision that
 *   makes expert communication efficient becomes a barrier to entry for
 *   newcomers and a tax on cross-disciplinary collaboration. The constraint
 *   embodies a fundamental tension — the same semantic density that enables
 *   expert coordination simultaneously extracts from those outside the circle
 *   and reduces the domain's ability to transmit knowledge to broader
 *   audiences. This story exhibits all six classification types from
 *   different structural positions, making it a diagnostic exemplar for how
 *   jargon density operates simultaneously as coordination mechanism (for
 *   insiders), extraction mechanism (for outsiders), and degraded ritual
 *   (where credentialing preserves jargon requirements even as their
 *   functional value decays). The extractiveness has increased over the
 *   observation interval (0.28 → 0.54) as institutional credentialing has
 *   become more rigid, while the theater ratio has also increased (0.38 →
 *   0.58) as jargon mastery is increasingly tested in isolation from
 *   functional competence verification.
 *
 * KEY AGENTS:
 *   - Domain Gatekeepers: Primary beneficiaries (institutional/arbitrage) — universities, professional societies, journal boards, licensing bodies that control entry through credentialing
 *   - Newcomers and Outsiders: Primary victims (powerless/trapped) — aspiring practitioners with no alternative to jargon acquisition; face bootstrapping paradox
 *   - Cross-Disciplinary Practitioners: Secondary victims (moderate/constrained) — researchers working across domains, bear translation tax and cognitive load
 *   - Open Terminology Movement: Organized agents (organized/constrained) — glossary projects, accessible documentation initiatives, plain-language advocates building alternative on-ramps
 *   - Credentialing Institutions: Institutional actor (institutional/arbitrage) — licensure boards, degree-granting institutions maintaining jargon-based competency testing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional jargon requirements as inherent to knowledge complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semantic_overload_friction, 0.54).
domain_priors:suppression_score(semantic_overload_friction, 0.62).
domain_priors:theater_ratio(semantic_overload_friction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semantic_overload_friction, extractiveness, 0.54).
narrative_ontology:constraint_metric(semantic_overload_friction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(semantic_overload_friction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semantic_overload_friction, tangled_rope).
narrative_ontology:human_readable(semantic_overload_friction, "The Semantic Saturation Threshold").
narrative_ontology:topic_domain(semantic_overload_friction, "technological/social").

domain_priors:requires_active_enforcement(semantic_overload_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semantic_overload_friction, domain_gatekeepers).
narrative_ontology:constraint_beneficiary(semantic_overload_friction, specialist_elites).
narrative_ontology:constraint_victim(semantic_overload_friction, newcomers_outsiders).
narrative_ontology:constraint_victim(semantic_overload_friction, cross_disciplinary_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING NOVICE (SNARE) — No exit from jargon density. Must accumulate specialized vocabulary to participate in the field, but accumulation itself becomes prohibitive. Career penalty for failure to master language; trapped in a bootstrapping paradox where fluency is prerequisite to learning what the terms mean in practice. Maximum extraction with no alternative pathway.
constraint_indexing:constraint_classification(semantic_overload_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CROSS-DISCIPLINARY PRACTITIONER (TANGLED ROPE) — Must translate between domain languages and lose both precision and time. Experiences both coordination benefit (shared terminology reduces miscommunication among specialists) and extraction cost (translation tax, cognitive load of context-switching, reduced ability to contribute to fast-moving debates within domains). Constrained exit — cannot leave the field entirely but can reduce cross-domain work.
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMAIN GATEKEEPER (ROPE) — University department, professional society, journal editorial board. Controls credentialing and access through language mastery requirements. Experiences semantic density as a coordination mechanism: shared jargon enables rapid, precise communication among established practitioners and acts as a coordination standard. Can arbitrage gatekeeping power (control who enters), so exit is voluntary. Net beneficiary — extraction flows toward this institutional actor.
constraint_indexing:constraint_classification(semantic_overload_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN TERMINOLOGY MOVEMENT (SCAFFOLD) — Organized efforts (glossaries, layered documentation, accessible-jargon-first writing, plain language acts, open-source documentation standards) represent a temporary coordination structure with a sunset clause. These initiatives reduce semantic friction by creating alternative on-ramps to specialized domains. As these practices mature and diffuse (estimated 15-25 year timeline), the gatekeeper advantage from opaque jargon diminishes. High suppression during transition but declining over interval.
constraint_indexing:constraint_classification(semantic_overload_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING RITUAL (PITON) — Professional licensure, academic degree requirements, and board certifications increasingly rely on jargon mastery as a proxy for competence. The theater has increased as credential inflation has decoupled from actual skill verification — fluency in technical terminology is measured but functional mastery is not. Credentialing persists through institutional inertia despite low correlation between jargon mastery and actual performance in many fields. Theater ratio high; primary function (identifying competent practitioners) has atrophied.
constraint_indexing:constraint_classification(semantic_overload_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, semantic density is inherent to knowledge accumulation: any domain with sufficient complexity must encode nuance in language, and there is an irreducible minimum vocabulary required to express distinctions that matter. This perspective sees jargon as an unavoidable consequence of domain maturation. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that contingent institutional practices (credentialing requirements, publication gatekeeping, resistance to simplification) are being naturalized as inherent complexity.
constraint_indexing:constraint_classification(semantic_overload_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_overload_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semantic_overload_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semantic_overload_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semantic_overload_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semantic_overload_friction, TR),
    TR >= 0.70.

:- end_tests(semantic_overload_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint extracts value from newcomers through credentialing requirements and from cross-disciplinary collaboration through translation costs. However, it is not maximal extraction (0.70+) because established practitioners do experience genuine coordination benefits from shared terminology, and some domains successfully maintain lower jargon density through institutional culture (e.g., parts of biology, software open-source communities). The increasing trajectory reflects credentialing creep — institutions adding jargon requirements not because they improve competency assessment but because they serve as status signals. Suppression (0.62): High. Significant barriers to bypassing jargon include: credentialing requirements for professional practice, publication gatekeeping by specialist journals, social prestige attached to fluency, absence of alternative on-ramps, and tacit knowledge embedded in jargon (some concepts genuinely require precise language). Theater ratio (0.58): Moderate-high. Jargon mastery is increasingly tested in isolation from practical competence — licensure exams test terminology, publications are judged on precision of language rather than clarity of ideas, degree requirements specify technical writing standards. The theater has increased over the interval as institutions have doubled down on credentialing through jargon rather than competency assessment.
 *
 * PERSPECTIVAL GAP:
 *   The domain gatekeeper sees the semantic density as a coordination mechanism (Rope) that enables precise, efficient communication among specialists. The beneficiary experience is real — shared vocabulary does solve coordination problems. The newcomer sees the same semantic density as a pure extraction mechanism (Snare) with no alternative pathway and no coordination benefit for them. The cross-disciplinary practitioner sees mixed coordination and extraction (Tangled Rope) — semantic standards enable within-domain efficiency but impose translation costs across domains. The open terminology movement sees a temporary problem (Scaffold) — if alternative knowledge encodings and accessible-first documentation practices diffuse widely, the gatekeeper advantage decays. The credentialing institution sees its jargon requirements as degraded ritual (Piton) — these requirements persist through institutional inertia despite weak correlation with actual practitioner performance. The civilizational analytical observer risks seeing jargon density as an immutable natural law of knowledge accumulation (Mountain), but the structural data reveals this as naturalization of contingent institutional choices: domains could maintain lower jargon density if they chose to prioritize accessibility over credentialing theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural position within the semantic extraction flow. Domain gatekeepers (institutional/arbitrage) are beneficiaries — they control entry and can manipulate jargon density to maintain gatekeeping power. Their low d (approximately 0.15) reflects arbitrage exit: they can choose to simplify or complicate jargon standards. Newcomers (powerless/trapped) are victims with zero exit options — they must accumulate jargon to participate. Their high d (approximately 0.95) reflects full targeting and maximum extraction. Cross-disciplinary practitioners (moderate/constrained) experience mixed costs and benefits: they gain access to specialized knowledge but bear translation tax. Their mid-range d (approximately 0.60) reflects constrained exit and moderate extraction. The open terminology movement (organized/constrained) sees the bottleneck as a solvable coordination failure with declining extraction over time — their d reflects organizing capacity and a visible sunset pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that semantic saturation operates simultaneously as genuine coordination and as gatekeeping extraction. The false summit (mountain) occurs when jargon density is naturalized as inherent to knowledge complexity rather than recognized as a contingent institutional choice. The solution to the mandatrophy is not to eliminate jargon (coordination requires precision) but to decouple competency assessment from jargon fluency. Domains that successfully maintain both precision and accessibility (e.g., open-source software, collaborative science platforms) show that the tight coupling between jargon and knowledge is institutional, not necessary. The scaffold perspective captures the real solution path: alternative knowledge encodings (visual, interactive, executable) can encode the same precision as jargon while reducing the entry barrier. As these alternatives mature (15-25 year timeline), the gatekeeper advantage from opaque jargon declines, and the constraint's extractiveness should decline correspondingly. The measurement trajectory (increasing theater ratio, stable extractiveness plateau at 0.54 rather than 0.70+) suggests the system is not yet capturing full rent from jargon gatekeeping, which indicates the scaffold sunset is beginning to form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessary_versus_gatekeeping_jargon,
    'What fraction of specialized vocabulary in a mature domain is necessary for precision versus parasitic for gatekeeping?',
    'Historical analysis of terminology introduction over domain lifetime; correlation between jargon density and publication citation rates; comparison of outcomes when practitioners use simplified vs. specialist language for the same concept',
    'If >70% is necessary: mountain classification strengthens (natural complexity). If <40% is necessary: snare classification strengthens (primarily extraction). If 40-70%: tangled_rope confirmed as accurate representation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessary_versus_gatekeeping_jargon, empirical, 'Proportion of jargon that is functionally necessary versus parasitic').

omega_variable(
    alternative_knowledge_encoding_viability,
    'Can complex domains encode knowledge via non-textual media (interactive diagrams, video tutorials, graphical models, executable code) at parity with jargon-dense publications?',
    'Comparative learning outcome studies; retention and transfer rates for specialist knowledge acquired via alternative media versus jargon-based sources; analysis of open-source and visual domains (mathematics visualization, software documentation) that have successfully reduced jargon barriers',
    'If viable: scaffold sunset is real and accelerating. If not viable: jargon-density is a genuine coordination requirement, not parasitic gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_knowledge_encoding_viability, empirical, 'Whether alternative knowledge encodings can replace jargon-dense communication').

omega_variable(
    credential_inflation_decoupling,
    'How much of the jargon requirement in professional credentialing is driven by actual performance benchmarks versus institutional inertia and status signaling?',
    'Comparison of on-the-job performance metrics for credentialed practitioners versus autodidacts/alternative pathway practitioners; analysis of licensing exam content drift over time; correlation between jargon-tested competencies and actual error rates in professional practice',
    'If decoupling >60%: piton classification confirmed, credentialing becomes increasingly theatrical. If decoupling <20%: jargon requirement is functionally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_decoupling, empirical, 'Whether credentialing jargon requirements correlate with actual performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semantic_overload_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semsat_tr_t0, semantic_overload_friction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(semsat_tr_t5, semantic_overload_friction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(semsat_tr_t10, semantic_overload_friction, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(semsat_be_t0, semantic_overload_friction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(semsat_be_t5, semantic_overload_friction, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(semsat_be_t10, semantic_overload_friction, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semantic_overload_friction, information_standard).
narrative_ontology:affects_constraint(semantic_overload_friction, credentialing_decoupling).
narrative_ontology:affects_constraint(semantic_overload_friction, knowledge_transfer_friction).
narrative_ontology:affects_constraint(semantic_overload_friction, interdisciplinary_synthesis_barriers).

% DUAL FORMULATION NOTE:
% Semantic saturation is a single constraint with multiple observable instantiations across domains. Its extractiveness is stable across domains (0.54 ± 0.08) but its perceived mechanism varies: practitioners see coordination (rope), newcomers see extraction (snare), institutions see ritual (piton). This is perspectival variation, not constraint decomposition. By contrast, credentialing_decoupling (a separate constraint) measures whether jargon requirements correlate with actual competence — that constraint's ε differs because it measures a different structural property. Both constraints are linked because high semantic saturation extraction enables high credentialing theater, but they are distinct constraints with distinct base properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semantic_overload_friction, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
