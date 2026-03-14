% ============================================================================
% CONSTRAINT STORY: female_epistemic_marginalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_female_epistemic_marginalization, []).

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
 *   constraint_id: female_epistemic_marginalization
 *   human_readable: Female Epistemic Marginalization
 *   domain: epistemology/social_structures/gender
 *
 * SUMMARY:
 *   Female epistemic marginalization is a structural constraint that
 *   systematically discounts and excludes women from recognition as knowledge
 *   producers, authorities, and definers of truth in formal epistemic
 *   systems. This constraint operates across academic disciplines,
 *   professional expertise, scientific authority, and philosophical
 *   canon-setting. The marginalization is not incidental to these systems but
 *   constitutive — women's exclusion from epistemic authority creates
 *   benefits for male gatekeepers (reduced competition, access to
 *   undercompensated labor, preserved authority) and costs for female
 *   knowledge producers (citation invisibility, assumption of incompetence,
 *   career penalties, intellectual isolation) and the epistemic commons (loss
 *   of diverse perspectives, reduced error-correction capacity, knowledge
 *   siloing). The constraint exhibits high suppression (0.72) through
 *   institutional barriers including: publication bias against women,
 *   gendered citation patterns (women cite men at higher rates than men cite
 *   women), pattern interruption in peer review (male reviewers flag
 *   uncertainty more readily for women authors), exclusion from key
 *   professional networks and collaborations, differential mentoring
 *   investment, assumption of caregiving availability that displaces from
 *   knowledge production, and reproductive penalties that male colleagues
 *   avoid. The theater ratio (0.68) reflects that diversity initiatives,
 *   women-in-science programs, and gender equity commitments in academia are
 *   increasingly performative — institutions adopt the language and metrics
 *   of equity without structural change to epistemic authority systems. The
 *   constraint's extractiveness (0.58) has increased over the 50-year
 *   measurement interval as diversity theater has expanded while the
 *   underlying marginalization mechanisms have remained stable, suggesting
 *   that institutional responses to female epistemic marginalization have
 *   primarily substituted performative for structural change.
 *
 * KEY AGENTS:
 *   - Female Knowledge Producers: Primary victim (powerless/trapped) — systematic discounting of epistemic authority, citation invisibility, network exclusion, assumption of incompetence
 *   - Male Epistemic Gatekeepers: Primary beneficiary (institutional/arbitrage) — access to epistemic authority, reduced competition, discounted labor from women scholars who must over-justify, preserved status hierarchies
 *   - Aligned Female Gatekeeping Minority: Secondary victim (moderate/constrained) — face assimilationist pressure, often reproduce marginalization against other women to preserve their own position
 *   - Feminist Epistemology Movement: Organized agent (organized/mobile) — building alternative epistemic infrastructure, mentoring networks, and validation structures with higher gender equity
 *   - Diversity and Inclusion Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative equity programs that serve legitimacy function without structural epistemic change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing marginalization as inherent to knowledge production itself rather than recognizing institutional construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(female_epistemic_marginalization, 0.58).
domain_priors:suppression_score(female_epistemic_marginalization, 0.72).
domain_priors:theater_ratio(female_epistemic_marginalization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(female_epistemic_marginalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(female_epistemic_marginalization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(female_epistemic_marginalization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(female_epistemic_marginalization, snare).
narrative_ontology:human_readable(female_epistemic_marginalization, "Female Epistemic Marginalization").
narrative_ontology:topic_domain(female_epistemic_marginalization, "epistemology/social_structures/gender").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(female_epistemic_marginalization, male_epistemic_gatekeepers).
narrative_ontology:constraint_victim(female_epistemic_marginalization, female_knowledge_producers).
narrative_ontology:constraint_victim(female_epistemic_marginalization, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE KNOWLEDGE PRODUCER (SNARE) — Trapped within an epistemic system that systematically discounts their contributions regardless of quality. Faces citation invisibility, assumption of incompetence, attribution errors (findings credited to male colleagues), exclusion from key networks, and persistent requirement to over-justify claims. No viable exit option without abandoning the knowledge-producing role itself. Maximum extraction experience — the constraint extracts epistemic authority, career recognition, and intellectual resources while providing minimal coordination benefit.
constraint_indexing:constraint_classification(female_epistemic_marginalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALIGNED FEMALE GATEKEEPING MINORITY (SNARE) — Female scholars in editorial, hiring, and funding positions face constrained options: maintain the system that enabled their advancement or challenge it and risk loss of position. Many adopt assimilationist strategies, reproducing the same marginalization patterns against other women. High extraction from the broader victim class; constrained rather than trapped because they retain professional mobility at cost of complicity.
constraint_indexing:constraint_classification(female_epistemic_marginalization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALE EPISTEMIC GATEKEEPER (ROPE) — Benefits from the constraint through reduced competition, access to discounted labor (women must over-justify to be heard), and preserved authority. Experiences the constraint primarily as coordination: maintaining group boundaries and status hierarchies. Arbitrage exit means the gatekeeper can leverage epistemic authority into funding, positions, and priority within their field. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(female_epistemic_marginalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEMINIST EPISTEMOLOGY MOVEMENT (TANGLED ROPE) — Organized agents (feminist scholars, women's networks, open-science communities, alternative publishing) are building parallel epistemic infrastructure with higher gender equity. This represents genuine coordination: creating shared standards, mentoring networks, and alternative validation mechanisms. Simultaneously, the movement faces ongoing extraction through resource limitations, institutional marginalization, and co-optation of its language without structural change. Mobile exit because organized agents can shift their epistemic investment toward alternative structures, though not without significant cost.
constraint_indexing:constraint_classification(female_epistemic_marginalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSITY AND INCLUSION THEATER (PITON) — Institutional commitments to diversity, gender equity, and inclusion in academia have become substantially performative. Diversity statements, women-in-science initiatives, and gender parity metrics persist despite evidence of minimal impact on epistemic marginalization. The theater has high maintenance cost (reporting, committees, initiatives) with low functional effect on the underlying constraint. Institutional inertia maintains these practices because they serve legitimacy functions even when structural extraction mechanisms remain unchanged.
constraint_indexing:constraint_classification(female_epistemic_marginalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, there is a risk of naturalizing female epistemic marginalization as inherent to knowledge production: cognitive differences, differential interest patterns, or biological factors that make male dominance of formal epistemology appear inevitable. This represents a false summit — the mountain classification will be flagged as misattribution of contingent institutional arrangements to natural law. The structural data reveals marginalization as enforced institutional practice, not natural property.
constraint_indexing:constraint_classification(female_epistemic_marginalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(female_epistemic_marginalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(female_epistemic_marginalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(female_epistemic_marginalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(female_epistemic_marginalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(female_epistemic_marginalization, TR),
    TR >= 0.70.

:- end_tests(female_epistemic_marginalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Female knowledge producers systematically lose epistemic authority, citation credit, and career mobility through marginalization mechanisms. The constraint extracts intellectual resources (women must over-document claims), epistemic visibility (citation invisibility), career recognition (promotions delayed, contributions misattributed), and network access (exclusion from key collaborations). The extraction is severe but not total — women remain capable of producing knowledge and some recognition is possible, though at higher cost than male colleagues. The trajectory shows increasing extractiveness as diversity theater substitutes for structural change, creating new extraction mechanisms (time cost of performative initiatives). Suppression (0.72): High. Barriers to exit from or resistance to marginalization include institutional career dependence, professional licensing tied to mainstream gatekeeping, exclusion from alternative structures (most alternative epistemic systems are under-resourced and have limited career mobility), and internalization of marginalization as individual failure rather than systemic constraint. Theater ratio (0.68): High and increasing. Institutional responses to female epistemic marginalization have increasingly taken the form of performative equity — diversity statements, women-in-science initiatives, gender parity metrics, and inclusion committees. These theater elements serve legitimacy functions (institutions appear committed to equity) without changing the epistemic authority mechanisms that marginalize women. Theater has increased from 0.45 to 0.68 as institutions have invested in visible equity programs while leaving fundamental gatekeeping structures intact.
 *
 * PERSPECTIVAL GAP:
 *   This constraint creates fundamental disagreement about what the constraint even is. The beneficiary (male gatekeeper) may not perceive extraction at all — they experience it as normal gatekeeping. The victim experiences systematic suppression of authority. The analytical perspective risks naturalizing this as inevitable feature of expertise rather than institutional construction. The feminist epistemology movement recognizes it as transformable through parallel structures. The diversity theater perspective claims it has been solved by performative initiatives. These perspectives disagree not just on classification but on basic facts: is marginalization a feature of knowledge production, an institutional practice, or a solved problem?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Male gatekeepers are positioned as beneficiaries with institutional power and arbitrage exit — they derive career and status benefits from female marginalization while maintaining ability to exit gatekeeper roles if desired (they have alternative high-status options). Female knowledge producers are positioned as victims with powerless status and trapped exit — marginalization removes epistemic authority and career options, with no viable alternative system offering equivalent resources or status (alternative epistemic structures are under-resourced). The organized feminist epistemology movement occupies a hybrid position: they benefit from the construction of alternative structures (coordination function) while suffering extraction through resource scarcity and institutional marginalization. The aligned female gatekeeping minority experiences constrained exit because they have gained some benefits from the mainstream system (position, authority) but must suppress their own marginalization experience to maintain that position. These structural differences in beneficiary/victim relationship and exit capacity produce differentiated experienced extractiveness (χ) through the directionality function f(d).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids mandatrophy through perspectival consistency. The constraint exhibits genuine mixed properties: real coordination function (maintaining knowledge community standards) alongside real extraction (systematic discounting of female authority). The coordination function does not justify the extraction, and the extraction is not masked by the coordination narrative. The false summit (mountain perspective) is explicitly flagged through the analytical observer — there is real structural choice in whether marginalization persists, making it contingent institutional practice rather than natural law. The female perspective (Snare) and male perspective (Rope) genuinely differ in experienced classification because they occupy different structural positions with different exit capacities and beneficiary status. This is not a naming problem or a measurement problem — it is an honest perspectival disagreement that the framework correctly captures. The theater dimension (rising from 0.45 to 0.68) reveals that institutional responses have increasingly substituted performance for structure, which the engine detects as degradation (theater ratio rising while core suppression and extractiveness persist or increase). The constraint resolves mandatrophy by acknowledging that female epistemic marginalization is institutionally constructed (not natural law), has real beneficiaries and victims (not pure coordination), and could be otherwise through deliberate structural change (not immutable). The diversity theater represents genuine degradation — the performative response creates appearance of change while extractive mechanisms persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained,
    'To what extent is the constraint maintained by internalized epistemic inferiority (identity_locked) versus structural barriers (constrained/trapped)?',
    'Comparative analysis of exit behavior: do women who exit to parallel epistemic structures maintain the internalized marginalization, or does it attenuate? Longitudinal tracking of epistemic confidence in alternative institutional contexts.',
    'If primarily identity_locked: the constraint persists through cognitive capture even when structural barriers are removed; exit requires identity reconstruction. If primarily constrained/trapped: removal of institutional barriers would enable exit; the binding is external.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained, empirical, 'Whether marginalization is maintained by internalized inferiority or structural barriers').

omega_variable(
    alternative_epistemology_viability,
    'Can feminist epistemology and alternative validation structures genuinely replace mainstream gatekeeping, or are they structurally relegated to subordinate status?',
    'Institutional power analysis: do researchers trained primarily in alternative epistemic structures achieve equivalent career mobility and funding access? Does citation authority in alternative structures translate to mainstream influence?',
    'If viable replacement: scaffold perspective confirmed — organized agents have real exit path. If relegated: constraint is snare for all agents; alternatives become ghettoized knowledge production.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemology_viability, empirical, 'Whether alternative epistemic structures can replace mainstream gatekeeping').

omega_variable(
    suppression_internalization_mechanism,
    'What proportion of the measured suppression (0.72) is structural (institutional barriers) versus internalized (epistemic self-doubt, identity fusion with marginalization)?',
    'Cognitive assessment in controlled contexts: do women''s epistemic confidence and contribution rates change when structural barriers (evaluation bias, attribution error feedback) are experimentally removed? Longitudinal analysis of epistemic confidence trajectory across career stages.',
    'If primarily internalized: the constraint persists through cognitive capture; exit requires identity and worldview reconstruction. If primarily structural: removal of barriers would enable immediate participation recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    male_beneficiary_awareness,
    'To what extent do male epistemic gatekeepers perceive and consciously maintain the marginalization versus operating within naturalized norms they don''t recognize as extractive?',
    'Comparative behavioral analysis: do male gatekeepers consciously defend the system when alternatives are proposed, or do they treat female marginalization as neutral background? Do interventions targeting conscious bias differ in effect from those targeting unconscious patterns?',
    'If conscious: the constraint is actively maintained snare. If unconscious: marginalization operates through naturalized norms and implicit bias, making it harder to challenge but more amenable to norm-shift interventions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(male_beneficiary_awareness, empirical, 'Degree of conscious maintenance of marginalization by beneficiaries').

omega_variable(
    intersectional_multiplication,
    'Does female epistemic marginalization operate uniformly across racial, class, disability, and sexuality categories, or does it multiply into distinct constraints?',
    'Comparative analysis of marginalization intensity: Black female scholars, disabled female scholars, working-class female scholars versus white middle-class female scholars. Does the extractiveness scale apply uniformly or does it increase for intersectionally marginalized women?',
    'If uniform: single constraint story applies across categories. If multiplicative: decompose into separate constraint stories for each intersectional configuration with different ε values. This determines whether the constraint family includes racial/class/disability variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersectional_multiplication, empirical, 'Whether marginalization operates uniformly or multiplies across identity categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(female_epistemic_marginalization, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fem_epi_tr_t0, female_epistemic_marginalization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fem_epi_tr_t25, female_epistemic_marginalization, theater_ratio, 25, 0.58).
narrative_ontology:measurement(fem_epi_tr_t50, female_epistemic_marginalization, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(fem_epi_be_t0, female_epistemic_marginalization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fem_epi_be_t25, female_epistemic_marginalization, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(fem_epi_be_t50, female_epistemic_marginalization, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(female_epistemic_marginalization, identity_coordination).
narrative_ontology:affects_constraint(female_epistemic_marginalization, academic_publishing_bias).
narrative_ontology:affects_constraint(female_epistemic_marginalization, citation_invisibility).
narrative_ontology:affects_constraint(female_epistemic_marginalization, women_leadership_ceiling).
narrative_ontology:affects_constraint(female_epistemic_marginalization, epistemicide_indigenous_knowledge).

% DUAL FORMULATION NOTE:
% Female epistemic marginalization decomposes into multiple structurally distinct constraints that share causal ancestry: academic_publishing_bias (publication gatekeeping specific to journals and peer review), citation_invisibility (downstream effect of marginalization in citation practices), women_leadership_ceiling (institutional promotion barriers), and epistemicide_indigenous_knowledge (broader knowledge system suppression). These are separate stories with different ε values and beneficiary/victim configurations but all influenced by the broader female_epistemic_marginalization structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(female_epistemic_marginalization, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
