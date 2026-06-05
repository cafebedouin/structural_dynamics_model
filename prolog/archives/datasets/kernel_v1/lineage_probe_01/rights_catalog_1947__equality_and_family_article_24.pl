% ============================================================================
% CONSTRAINT STORY: rights_catalog_1947__equality_and_family_article_24
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_1947__equality_and_family_article_24, []).

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
 *   constraint_id: rights_catalog_1947__equality_and_family_article_24
 *   human_readable: Article 24: Constitutional Reconstitution of Family and Marriage Equality
 *   domain: legal/doctrinal/family_law
 *
 * SUMMARY:
 *   Article 24 of the 1947 Constitution reconstitutes the family unit from a
 *   patriarchal hierarchy (ie-headship doctrine) to a contractual
 *   relationship based on mutual consent and equal rights of the sexes. This
 *   constraint is one reading of the contested rights kernel established by
 *   the 1947 Constitution — specifically, the reading that prioritizes family
 *   equality and mutual consent as the core constitutional principle. The
 *   constraint operates at the intersection of three domains: constitutional
 *   doctrine (the formal legal principle), institutional implementation
 *   (courts interpreting and enforcing Article 24), and intimate practice
 *   (actual household dynamics). The tension between these domains generates
 *   the constraint's hybrid character: Article 24 is simultaneously a
 *   recognition of women's equal personhood (Rope from the judiciary's
 *   perspective), a suppression of patriarchal authority (Snare from the
 *   ie-headship doctrine's perspective), a mix of formal equality with
 *   material dependency (Tangled Rope from the post-1947 wife's perspective),
 *   and a natural-law principle of inalienable dignity (false-summit Mountain
 *   from the analytical perspective). The constraint's extractiveness
 *   declines over time as doctrine matures and enforcement mechanisms
 *   develop, but suppression remains high because the constraint requires
 *   active state enforcement against entrenched custom and economic
 *   structures.
 *
 * KEY AGENTS:
 *   - Wives as legal persons: Primary beneficiary (powerless→constrained/trapped) — gain formal equal rights and legal personhood through Article 24, but material dependency and enforcement gaps preserve significant extraction
 *   - Children as independent rights holders: Secondary beneficiary (powerless/constrained) — gain independent legal status rather than being subsumed under father's authority; custody and welfare rights become individualized
 *   - Patriarchal household authority (ie-headship doctrine): Primary victim (institutional/constrained) — loses legal force and legitimacy; suppressed by constitutional rank; may migrate to informal extra-legal domains
 *   - Husbands as prior beneficiaries: Institutional actor (powerful→mobile) — lose unilateral authority but retain material power and social authority; experience Article 24 as extraction of legal supremacy while coordination function remains opaque
 *   - Judiciary as interpreter: Institutional actor (institutional/arbitrage) — mediates implementation; sees Article 24 as pure coordination (rebalancing rights) from neutral position
 *   - Constitutional authority system: Institutional actor (institutional/analytical) — risks naturalizing Article 24 as immutable natural law (dignity, consent) rather than contingent legal achievement requiring enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_1947__equality_and_family_article_24, 0.38).
domain_priors:suppression_score(rights_catalog_1947__equality_and_family_article_24, 0.62).
domain_priors:theater_ratio(rights_catalog_1947__equality_and_family_article_24, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_1947__equality_and_family_article_24, extractiveness, 0.38).
narrative_ontology:constraint_metric(rights_catalog_1947__equality_and_family_article_24, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rights_catalog_1947__equality_and_family_article_24, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_1947__equality_and_family_article_24, tangled_rope).
narrative_ontology:human_readable(rights_catalog_1947__equality_and_family_article_24, "Article 24: Constitutional Reconstitution of Family and Marriage Equality").
narrative_ontology:topic_domain(rights_catalog_1947__equality_and_family_article_24, "legal/doctrinal/family_law").

domain_priors:requires_active_enforcement(rights_catalog_1947__equality_and_family_article_24).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_1947__equality_and_family_article_24, 'ab54b310-b143-4517-9080-3b617471390f').
narrative_ontology:cs_kernel_codification('ab54b310-b143-4517-9080-3b617471390f', formalized).
narrative_ontology:cs_authority_grounding('ab54b310-b143-4517-9080-3b617471390f', lineage).
narrative_ontology:cs_interpretation_layer_present('ab54b310-b143-4517-9080-3b617471390f').
narrative_ontology:cs_reading_relation('ab54b310-b143-4517-9080-3b617471390f', rights_catalog_1947__individual_dignity_article_13, coexists_with).
narrative_ontology:cs_reading_relation('ab54b310-b143-4517-9080-3b617471390f', rights_catalog_1947__social_minimum_article_25, influences).
narrative_ontology:cs_axiom('ab54b310-b143-4517-9080-3b617471390f', foundational, mutual_consent_grounds_marriage_validity).
narrative_ontology:cs_axiom_status(mutual_consent_grounds_marriage_validity, holdable).
narrative_ontology:cs_axiom_grounding('ab54b310-b143-4517-9080-3b617471390f', mutual_consent_grounds_marriage_validity, deontological).
narrative_ontology:cs_axiom('ab54b310-b143-4517-9080-3b617471390f', foundational, equal_legal_personhood_in_marriage).
narrative_ontology:cs_axiom_status(equal_legal_personhood_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('ab54b310-b143-4517-9080-3b617471390f', equal_legal_personhood_in_marriage, deontological).
narrative_ontology:cs_reference_frame('ab54b310-b143-4517-9080-3b617471390f', mutual_consent_and_equal_personhood_marriage).
narrative_ontology:cs_drift_state('ab54b310-b143-4517-9080-3b617471390f', contemporary_post_second_generation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab54b310-b143-4517-9080-3b617471390f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(rights_catalog_1947__equality_and_family_article_24, rights_catalog_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_1947__equality_and_family_article_24, wives_as_legal_persons).
narrative_ontology:constraint_beneficiary(rights_catalog_1947__equality_and_family_article_24, children_as_independent_rights_holders).
narrative_ontology:constraint_victim(rights_catalog_1947__equality_and_family_article_24, patriarchal_household_authority).
narrative_ontology:constraint_victim(rights_catalog_1947__equality_and_family_article_24, traditional_ie_headship_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WIFE UNDER PRIOR REGIME (SNARE) — Trapped by legal disability; the household code vested authority entirely in the husband as ie-head. No exit from marital status without social ruin. Article 24 is written TO eliminate this snare, but from the perspective of a woman bound under the pre-1947 code, the constraint appears as pure extraction with no coordination function. The extraction is the legal removal of personhood.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POST-1947 WIFE — CONSTRAINED IMPLEMENTATION (TANGLED ROPE) — Article 24 grants equal rights in principle, but implementation is constrained by custom, economic dependency, and unequal enforcement. Women gain legal personhood and equal marital rights (coordination function: reconstituted family now requires mutual consent rather than unilateral authority). But enforcement of equality is weak, and the material bases of dependency persist (economic, childcare, social). The wife experiences both genuine coordination gain and significant residual extraction through incomplete implementation.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL AUTHORITY — JUDICIARY (ROPE) — The courts interpret Article 24 as pure coordination: reformulating marriage from patriarchal headship to mutual contract, restoring both parties to equal legal personhood. The judiciary experiences this as a legitimate reordering of the legal architecture, extracting no benefit and bearing no cost — a neutral arbiter rebalancing rights. This is the perspective from which Article 24 appears as Rope.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATRIARCHAL AUTHORITY — IE HEADSHIP DOCTRINE (SNARE) — From the perspective of the pre-constitutional ie doctrine and institutions that administered it, Article 24 is a total extraction event: the entire legal basis of patriarchal headship is removed. The constraint suppresses the traditional authority mechanism entirely. No mutual consent, no negotiation — the authority structure is foreclosed. This perspective sees Article 24 as a snare that traps the old order.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE PATRIARCH — MOBILE RESISTANCE (TANGLED ROPE) — The husband retains significant material power (income control, property, social authority) even after Article 24 formalizes equality. He experiences Article 24 as extracting his legal authority while he still coordinates household resources. The constraint is hybrid: it coordinates around mutual consent (genuine coordination function — a marriage cannot function as pure authority any longer) while it extracts his unilateral command. Mobility derives from ability to migrate practice to informal spheres (household decision-making norms) where written law cannot reach.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Article 24 might appear to instantiate a natural law: that human dignity cannot be sublimated into household hierarchy, that equal personhood is immutable, that mutual consent cannot be overridden. This view sees Article 24 not as a constraint but as a recognition of an existing limit on valid authority. However, the structural data contradicts the mountain classification: Article 24 has beneficiaries (women gaining legal status) and victims (patriarchal authority losing force), active enforcement requirements, and suppression mechanisms. This is a false summit — the 'natural dignity' framing naturalizes what is actually a contingent legal and political achievement.
constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_1947__equality_and_family_article_24_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_1947__equality_and_family_article_24, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(rights_catalog_1947__equality_and_family_article_24_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 → down from 0.78 preconstitutionally): The constraint measures extraction of patriarchal authority and extraction experienced by wives under formal inequality. Before Article 24, the patriarchal household extracted authority, labor, and autonomy from wives with minimal restriction — extractiveness was very high (0.78). After Article 24 is codified and doctrine matures, extractiveness declines because the legal basis of unilateral authority is removed. However, extractiveness does not fall to near-zero (as it would for pure Rope) because enforcement gaps persist — wives remain materially dependent, custom preserves patriarchal norms, and courts do not uniformly enforce equal authority in household decisions. The measured value (0.38) reflects genuine progress on formal equality with persistent gaps in substantive implementation. Suppression (0.62): Moderate-high. Article 24 requires active suppression of patriarchal authority structures — through constitutional supremacy, court enforcement, and legal disability of ie-headship claims. But suppression is not total because patriarchal authority migrates to informal domains (custom, religious law, household practice) where state enforcement cannot easily reach. Suppression is a structural property of the constraint — it measures how much coercion is required to maintain the equal-rights regime against countervailing pressures. Theater ratio (0.48): Moderate. Article 24's functional content is genuine — mutual consent replaces unilateral authority, equal rights are enforceable through courts, women gain legal personhood. But a significant portion of implementation is performative — courts issue judgments enforcing Article 24 equality that are not complied with in practice, formal equal marital authority coexists with informal patriarchal decision-making, and enforcement mechanisms are weak relative to enforcement requirements.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. The wife under the prior regime sees pure extraction (Snare) — the constraint is designed to eliminate her condition. The post-1947 wife sees a mix (Tangled Rope) — genuine coordination gain through mutual consent, but persistent extraction through incomplete implementation. The judiciary sees pure coordination (Rope) — a neutral rebalancing of legal rights. The patriarchal authority sees its own elimination (Snare) — the constraint extracts its entire legal basis. The patriarch with mobile resistance sees hybrid (Tangled Rope) — legal authority is extracted but material power enables informal preservation of patriarchal arrangements. The analytical observer sees immutable principle (Mountain) — but this is a false summit, revealed by the structural data showing beneficiaries, victims, and active enforcement. The perspectival gap reflects that Article 24 is not a neutral coordination mechanism — it is a fundamental redistribution of authority from one party to another, with the gain for wives precisely matched to the loss for patriarchal structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in Article 24 is determined by structural position relative to the equality-authority extraction flow. Wives experience high d (0.85-0.95) — they are targets of the prior extraction, beneficiaries of Article 24's suppression of that extraction. The husband who loses unilateral authority but retains material power experiences moderate-to-high d (0.55-0.70) — he is partly extracted from, partly still benefiting. The patriarchal ie-headship doctrine experiences maximum d (approaching 1.0) — it is the victim whose authority is entirely suppressed. The judiciary experiences low d (0.15-0.25) — they are neutral interpreters deriving no benefit and bearing no cost. These directionality values feed into the chi formula: a wife with d≈0.90 and moderate power sees much higher chi than an institutional judge with d≈0.20. The perspectival gap emerges from how d varies across the agent set, not from variation in epsilon itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE DEGRADATION SIGNAL: Article 24's claimed_type is Tangled Rope, which requires three structural features: (1) genuine coordination function (mutual consent replacing hierarchy), (2) asymmetric extraction (wives gaining equal rights, patriarchy losing authority), (3) active enforcement. All three are present. However, the measurement trajectory shows theater_ratio rising (0.35→0.48) — the performative content of Article 24 enforcement is increasing even as formal doctrine matures. This is a classic mandatrophy pattern: the mandate (equal family authority) is institutionalized and legalized, but compliance gaps force resort to increasingly theatrical enforcement (court judgments that are not followed, legal doctrine that diverges from practice). The constraint does not degrade INTO a Piton (it remains Tangled Rope), but the theater-ratio rise signals that implementation is becoming increasingly decoupled from function. If theater_ratio continues rising to 0.70+, the constraint would reclassify as Piton — a rule about family equality that is maintained through institutional inertia rather than functional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_gap_doctrine_vs_practice,
    'Does Article 24''s formal equality translate into substantive equal authority within households, or does the constraint''s enforcement gap preserve patriarchal extraction under a veneer of equal rights?',
    'Longitudinal data on divorce outcomes, property division, and custody allocation; comparison of stated marital authority (mutual consent) vs actual household decision-making patterns; enforcement rates of women''s equal rights claims through courts over time',
    'If substantive equality achieved: constraint reclassifies toward Rope from the wife''s perspective. If enforcement gap persists: constraint remains Tangled Rope or reverts toward Snare, with Article 24 as theater masking continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_doctrine_vs_practice, empirical, 'Gap between formal equality in Article 24 and substantive equal authority in households').

omega_variable(
    patriarchal_authority_reformation_vs_suppression,
    'Does Article 24 suppress patriarchal household authority entirely, or does it force reformation of ie-headship doctrine into informal extra-legal authority (migration to custom, religious law, or domestic practice)?',
    'Analysis of post-1947 household law doctrine; ethnographic and historical evidence of whether ie-headship migrated to non-statutory domains; comparison of enforcement patterns for formal vs informal authority claims; state capacity to suppress vs merely regulate extra-legal patriarchal arrangements',
    'If suppression is complete: Article 24 fully eliminates the victim class. If reformation/migration occurs: patriarchal extraction persists in delocalized form, making the constraint''s suppression value artificially high (appears to suppress more than it actually does).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patriarchal_authority_reformation_vs_suppression, empirical, 'Whether Article 24 suppresses patriarchal authority or forces its reformation into informal domains').

omega_variable(
    mutual_consent_doctrine_enforcement,
    'Is Article 24''s mutual-consent requirement genuinely enforced, or does it function as theater while effective marital control remains asymmetric?',
    'Court records on marital consent disputes; enforcement rates of Article 24 claims; comparative analysis of case outcomes when wives claim violation of mutual-consent requirement vs when husbands do; state capacity and willingness to police household-level consent',
    'If genuinely enforced: mutual consent is a real coordination mechanism, suppressing unilateral authority. If theater: Article 24''s classification drifts toward Piton (false coordination), with suppression masked by performative enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_consent_doctrine_enforcement, empirical, 'Degree of actual enforcement of mutual-consent requirement in Article 24').

omega_variable(
    kernel_contest_foreclosure_vs_coexistence,
    'Does this reading (Article 24 equality-and-family) logically foreclose the sibling reading (Article 13 individual dignity as the root), or do both readings remain live in the same constitutional framework?',
    'Doctrinal analysis of how courts position Articles 13 and 24 in relation to one another; whether dignity-based privacy doctrine (from Article 13) is used to protect marital autonomy (compatible with Article 24) or to override Article 24''s equal-rights mandate (incompatible); historical trajectory of jurisprudence on both articles',
    'If foreclosure: Article 24 (equality-and-family) is the dominant reading and Article 13 (individual dignity) is secondary or subordinate. If coexistence: both readings remain live, creating potential tension or complementarity in doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_vs_coexistence, conceptual, 'Logical relationship between equality-and-family reading and individual-dignity reading of rights kernel').

omega_variable(
    social_minimum_implementation_coupling,
    'Does Article 24''s equality mandate couple with Article 25''s social-minimum promise (state-provided childcare, economic support) to enable substantive equal participation, or do they operate independently with Article 24 remaining formal without Article 25''s material support?',
    'Historical analysis of whether childcare provision, parental leave, welfare support, and economic security policies developed in tandem with or independently of Article 24 doctrine; comparative outcomes for marital equality in regimes with strong Article 25 implementation vs weak Article 25 implementation',
    'If coupled: Article 24''s extractiveness is genuinely leveled by material support, enabling both spouses to exercise equal authority. If independent: Article 24 without Article 25 support leaves wives formally equal but materially dependent, with extractiveness remaining high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_minimum_implementation_coupling, empirical, 'Coupling between Article 24 family equality and Article 25 social-minimum provisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_1947__equality_and_family_article_24, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article24_theater_t0_preconstitutional, rights_catalog_1947__equality_and_family_article_24, theater_ratio, 0, 0.35).
narrative_ontology:measurement(article24_theater_t5_early_implementation, rights_catalog_1947__equality_and_family_article_24, theater_ratio, 5, 0.42).
narrative_ontology:measurement(article24_theater_t10_mature_doctrine, rights_catalog_1947__equality_and_family_article_24, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(article24_extractiveness_t0_preconstitutional, rights_catalog_1947__equality_and_family_article_24, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(article24_extractiveness_t5_early_implementation, rights_catalog_1947__equality_and_family_article_24, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(article24_extractiveness_t10_mature_doctrine, rights_catalog_1947__equality_and_family_article_24, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(article24_suppression_t0_preconstitutional, rights_catalog_1947__equality_and_family_article_24, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(article24_suppression_t5_early_implementation, rights_catalog_1947__equality_and_family_article_24, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(article24_suppression_t10_mature_doctrine, rights_catalog_1947__equality_and_family_article_24, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_1947__equality_and_family_article_24, attachment_coordination).
narrative_ontology:affects_constraint(rights_catalog_1947__equality_and_family_article_24, rights_catalog_1947__individual_dignity_article_13).
narrative_ontology:affects_constraint(rights_catalog_1947__equality_and_family_article_24, rights_catalog_1947__social_minimum_article_25).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 1947 constitutional rights kernel. Article 24 equality-and-family focuses on family structure and mutual consent as the organizing principle. Article 13 individual dignity focuses on personhood and autonomy as the root principle. Article 25 social minimum focuses on material conditions and state obligation. These are not the same constraint viewed from different angles — they have different epsilon values (Article 24 measures extraction of household authority; Article 13 measures extraction of individual autonomy; Article 25 measures extraction of material entitlements), different beneficiary-victim structures, and different empirical status. However, they are coupled through the same constitutional text and the same authority system (judiciary). Decomposition is appropriate because the epsilon values differ and the observable (what aspect of the rights catalog is being evaluated) changes classification outcome. All three readings should be linked via network.affects_constraints as members of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rights_catalog_1947__equality_and_family_article_24, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
