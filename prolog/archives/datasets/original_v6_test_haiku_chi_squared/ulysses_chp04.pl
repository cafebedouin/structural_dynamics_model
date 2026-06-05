% ============================================================================
% CONSTRAINT STORY: ulysses_chp04
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp04, []).

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
 *   constraint_id: ulysses_chp04
 *   human_readable: The Domestic Calypso (7 Eccels Street)
 *   domain: social/economic/religious
 *
 * SUMMARY:
 *   Leopold Bloom's day begins at 7 Eccles Street in Dublin on June 16, 1904,
 *   within the domestic constraint of his marriage to Molly. The marriage
 *   exhibits the structural properties of a tangled rope: it provides genuine
 *   coordination function (household resource management, social stability,
 *   legitimate child-rearing) while simultaneously extracting from Bloom
 *   through Molly's infidelity (which Bloom suspects), her emotional control,
 *   and his inability to exit without social/economic catastrophe. The
 *   constraint exhibits different classifications depending on the observer's
 *   structural position: Molly sees coordination (rope) and security; Bloom
 *   sees extraction (snare); the Victorian institution sees its own
 *   degradation (piton); the analytical observer risks naturalizing the
 *   arrangement as inevitable human kinship structure (false
 *   summit/mountain). The metaphor of 'domestic Calypso' invokes the
 *   constraint-island imagery: Bloom is the Odysseus held by Calypso (Molly),
 *   offered immortality (household security) in exchange for freedom
 *   (autonomous erotic life, dignity). The 'loose brass quoits' image
 *   suggests worn-down, careworn domestic objects—the material evidence of an
 *   institution maintained through inertia rather than genuine function.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped/biographical) — bears household costs while enduring emotional extraction and infidelity. Experiences constraint as snare.
 *   - Molly Bloom: Primary beneficiary (institutional/arbitrage/immediate) — maintains economic security and social standing while exercising sexual agency outside the marriage. Experiences constraint as rope or negotiated partnership.
 *   - Blazes Boylan: Secondary actor (powerful/mobile) — extracts sexual access and social status while bearing minimal costs. Structural position as interloper gives him arbitrage without commitment.
 *   - Victorian Marriage Institution: Institutional actor (institutional/constrained/generational) — maintains performative function while losing real coordination capacity. Experiences its own process as degraded (piton).
 *   - Dublin Catholic Society: Structural enforcer (institutional/constrained/national) — provides suppression through religious condemnation of divorce, social scandal norms, property law.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent 1904 arrangement as an inherent feature of human kinship.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp04, 0.52).
domain_priors:suppression_score(ulysses_chp04, 0.68).
domain_priors:theater_ratio(ulysses_chp04, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp04, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp04, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp04, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp04, tangled_rope).
narrative_ontology:human_readable(ulysses_chp04, "The Domestic Calypso (7 Eccels Street)").
narrative_ontology:topic_domain(ulysses_chp04, "social/economic/religious").

domain_priors:requires_active_enforcement(ulysses_chp04).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp04, molly_bloom_economic_security).
narrative_ontology:constraint_beneficiary(ulysses_chp04, bloom_household_stability).
narrative_ontology:constraint_victim(ulysses_chp04, leopold_bloom_autonomy).
narrative_ontology:constraint_victim(ulysses_chp04, bloom_erotic_life).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Leopold Bloom (powerless/trapped/biographical/local) experiences the domestic arrangement as a snare. Molly's infidelity, her economic dependence masked as control, and his inability to exit create maximum extraction. He pays household costs (food, rent) while bearing emotional extraction (suspicion, emasculation). d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(ulysses_chp04, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Bloom (moderate/constrained/biographical/national) as wage-earner sees coordination function: his advertising work, meal provision, and financial management enable household function. His constrained exit (social scandal, loss of livelihood if marriage fails) masks genuine coordination labor. d≈0.65, f(d)≈0.98, σ=0.9 → χ≈0.48.
constraint_indexing:constraint_classification(ulysses_chp04, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Molly Bloom (institutional/arbitrage/immediate/local) as wife and beneficiary of household security, food provision, rent, and social standing. Her arbitrage: sexual access to Blazes Boylan outside the marriage while retaining Bloom's economic support. She experiences the constraint as coordination (marriage enables her comfort) with minimal suppression from her perspective—she has agency in her infidelities. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.04.
constraint_indexing:constraint_classification(ulysses_chp04, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Blazes Boylan (powerful/mobile/biographical/local) as Molly's lover occupies an extractive position: he gains sexual access and social status (conquest) while bearing minimal costs. His mobility (can exit relationship without scandal) and power (social standing, masculine authority) mean he extracts from both Molly and Bloom without reciprocal obligation. d≈0.20, f(d)≈0.10, σ=0.8 → χ≈0.04. His classification shifts depending on whether the constraint is 'marriage' or 'infidelity network'—here we focus on his role within the marriage structure.
constraint_indexing:constraint_classification(ulysses_chp04, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Victorian marriage (institutional/constrained/generational/national) as a formal institution appears functional (property transfer, social stability, child legitimacy) but increasingly operates as theater. The actual coordination function (resource pooling, child-rearing, sexual fidelity) has degraded while the performative aspects (respectability, property law, divorce scandal) dominate. Theater_ratio=0.65 reflects this: Bloom performs the role of faithful husband; Molly performs the role of respectable wife; both violate the actual terms while maintaining the ritual. The institution persists through inertia (legal/religious/social costs of exit) rather than because it delivers its promised function.
constraint_indexing:constraint_classification(ulysses_chp04, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational/universal perspective, some constraints on domestic life—biological asymmetry in reproductive investment, resource-pooling economies of scale, social coordination of child-rearing—are structural features of human kinship that transcend institutional forms. The observer risks naturalizing 1904 Dublin marriage law as an inevitable feature of human life rather than as a contingent historical arrangement. However, the base metrics (ε=0.52, suppression=0.68, theater=0.65) contradict a mountain classification: the constraint has high enforcement costs, victim/beneficiary asymmetry, and performative content—hallmarks of contingent institutional arrangement, not natural law. False summit.
constraint_indexing:constraint_classification(ulysses_chp04, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp04_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp04, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp04, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp04, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp04, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp04_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Bloom provides income (wages), household management, food procurement, rent payment. In return, he receives domestic service (meals, laundry), but also endures suspicion, emotional labor around Molly's infidelity, and sexual frustration. The extraction is not total (he retains some autonomy in his wanderings, his intellectual life, his fantasies), but it is real and asymmetric. The measurement traces increasing extractiveness from 0.35 to 0.52 as the day progresses: early morning optimism (theater_ratio=0.40, low extraction) degrades through afternoon suspicions (afternoon theater_ratio=0.52) into evening resignation (final theater_ratio=0.65, extraction at peak). Suppression (0.68): High. Legal barriers to divorce under Irish Catholic law are near-total. Social barriers (scandal, loss of respectable standing) make exit catastrophic for both spouses but asymmetrically costly for Bloom (loss of income source, reputation). Economic barriers: separation/divorce would require Bloom to support Molly independently—a cost that outweighs separation benefit. Religious suppression is severe: Catholic doctrine forbids divorce and condemns Molly's infidelity; Bloom's agency is suppressed by guilt and moral confusion. Theater ratio (0.65): High and increasing. The marriage is substantially performative: Bloom performs fidelity and concern; Molly performs the respectable wife; both pretend ignorance of the infidelity while both know. Bloom's thought-life is consumed with managing the performance (imagining confrontations, fantasies of escape, religious/philosophical justifications). The institutional form (marriage certificate, property law, social invitation) persists while the functional content (sexual fidelity, mutual trust, voluntary cooperation) has degraded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Molly sees a rope or negotiated partnership: she has security (household income), autonomy (sexual license), and light suppression (Bloom is passive). Bloom sees a snare: he has obligations (income, household work, emotional labor), constraint (cannot exit), and high suppression (legal, social, economic, religious). The Victorian institution sees itself as a piton: the form (marriage ritual, property transfer, legal status) persists, but the function (voluntary cooperation, sexual fidelity, mutual benefit) has degraded into theater. Boylan sees an extractive opportunity (rope or light snare from his perspective): he gains pleasure and status with minimal obligation. The analytical observer at the civilizational level is tempted to see the constraint as a mountain: marriage as a natural law of human kinship. But the structural data (ε=0.52, high suppression, high theater) indicates a contingent institutional arrangement, not a natural law. The false summit detector should flag the mountain perspective as naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Molly Bloom (beneficiary + arbitrage): d≈0.08, f(d)≈-0.10. Net beneficiary from the constraint. She has exit options (arbitrage through Boylan's attention, potential for separation if she chose it) and experiences the constraint as enabling her security while allowing her agency. Bloom (victim + trapped): d≈0.92, f(d)≈1.40. Maximum extraction from the victim side. He has minimal exit options (legal/social/economic barriers), bears costs (income, household labor, emotional extraction), and his autonomy is suppressed by the constraint structure. Boylan (interloper + mobile): d≈0.20, f(d)≈0.10. Beneficiary from an extractive position—he gains sexual access without commitment or cost. His mobility (can exit the affair without scandal, given his gender and class) and his power (masculine social authority) insulate him from suppression. Victorian Institution (institutional + constrained): d≈0.05, f(d)≈-0.12. Appears as beneficiary (property law favors husbands, institution maintains legitimacy through marriage rituals), but the constrainedness (must enforce suppression through law, religion, social pressure) and the institution's own perception of degradation (piton classification) suggest it bears costs. The institution is sustained by active enforcement despite having lost its functional justification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification (ε=0.52, suppression=0.68, requires_active_enforcement=true, beneficiaries=[molly, household_stability], victims=[bloom_autonomy, bloom_erotic_life]) correctly identifies a genuine hybrid. It is not pure coordination (rope) because the distribution is asymmetric—Molly benefits far more than Bloom, and her arbitrage option (Boylan) is not equally available to Bloom. It is not pure extraction (snare) because there is real coordination function: the household does pool resources, provide shelter, manage the household economy. The tangled rope classification captures both functions: the marriage coordinates household resource management (rope function) while extracting disproportionately from Bloom (snare function). The mandatrophy is resolved by declaring who benefits and who bears costs. Beneficiaries: Molly, household stability (collective good). Victims: Bloom's autonomy, Bloom's erotic life. The active enforcement is Victorian law (marriage indissolubility), Catholic doctrine (sexual restriction), and social convention (respectability norms). Without active enforcement, the constraint would dissolve into a simple negotiated partnership—this is why active enforcement is necessary to maintain the tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    molly_agency_question,
    'Is Molly''s infidelity an expression of genuine agency (arbitrage option, choosing supplemental pleasure) or a symptom of the snare (desperate escape attempt within a trapped structure)?',
    'Literary analysis of Molly''s interior monologue (Ch. 18); comparison of her descriptions of Bloom vs Boylan vs her own desires; assessment of her knowledge of separation/divorce options and her choice to remain',
    'If genuine agency: beneficiary position justified, constraint classifies as Rope from her perspective. If symptom: she is also a victim, constraint classifies as Snare, and the ''beneficial'' household stability is achieved through her suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(molly_agency_question, conceptual, 'Whether Molly''s infidelity represents agency or victimization').

omega_variable(
    bloom_exit_option_evaluation,
    'What are Bloom''s actual exit options? Is divorce/separation truly unavailable or merely economically/socially catastrophic in a way that preserves choice?',
    'Historical research on 1904 Dublin divorce law and costs; comparison to contemporary accounts of male exits from similar marriages; analysis of whether Bloom''s ''trapped'' classification derives from legal bars or economic/social pressure',
    'If truly trapped (legal bar): constraint is more purely extractive (Snare persists). If constrained (economic/social barrier): exit is theoretically available, which might downgrade the victim power classification or permit higher exit_options for Bloom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bloom_exit_option_evaluation, empirical, 'Whether Bloom''s trapped status reflects legal or economic barriers').

omega_variable(
    coordination_function_degradation,
    'Has the marriage''s coordination function genuinely degraded (Piton reading correct) or is the theater masking continued functional coordination (Rope reading)?',
    'Analysis of household management across the novel: does Molly''s infidelity disrupt food provision, shelter, child-rearing? How much of Bloom''s day involves ''keeping the household running'' vs ''managing his emotional extraction''? Comparison to period accounts of functional vs dysfunctional marriages',
    'If degraded: Piton is correct, institutional inertia explains persistence. If still functional: the constraint is Rope or Tangled Rope (coordination + extraction), and theater is a symptom of how coordination is experienced under asymmetric power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_degradation, empirical, 'Whether the marriage''s coordination function has degraded').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp04, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp04, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ulys_tr_t8, ulysses_chp04, theater_ratio, 8, 0.52).
narrative_ontology:measurement(ulys_tr_t16, ulysses_chp04, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp04, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ulys_be_t8, ulysses_chp04, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ulys_be_t16, ulysses_chp04, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp04, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp04, victorian_gender_norms_1904).
narrative_ontology:affects_constraint(ulysses_chp04, dublin_catholic_marriage_law).

% DUAL FORMULATION NOTE:
% The domestic Calypso is downstream of both Victorian gender norms and Irish Catholic marriage law. Those upstream constraints establish the legal/cultural suppression that makes Bloom's exit options 'trapped' rather than merely 'constrained'. The marriage itself is a coordination mechanism (resource pooling) that has become entangled with extraction (emotional labor asymmetry, sexual control). Separate stories should model the institutional constraints (Victorian norms, Catholic law) as mountains or rope-only coordination mechanisms; this story focuses on their embodiment in the individual marriage contract.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp04, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
