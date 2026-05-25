% ============================================================================
% CONSTRAINT STORY: irish_womens_legal_disability_1904
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_womens_legal_disability_1904, []).

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
 *   constraint_id: irish_womens_legal_disability_1904
 *   human_readable: Irish Women's Legal Disability Under the 1904 Legal Code
 *   domain: legal/political/gender
 *
 * SUMMARY:
 *   In 1904 Ireland under the legal code inherited from English common law
 *   and colonial administration, married women possessed virtually no legal
 *   capacity. Coverture — the doctrine that a woman's legal personality was
 *   suspended upon marriage and merged with her husband's — remained the
 *   foundational principle of family law. An Irish married woman could not
 *   own property, sign contracts, testify independently in court, refuse
 *   marital relations, or retain custody of her own children. Unmarried women
 *   retained nominal property rights but faced absolute barriers to economic
 *   independence and professional employment. Widows with minor children
 *   could inherit property but held no automatic guardianship rights. This
 *   constraint describes the systematic legal architecture that stripped
 *   women of legal personhood. It is a diagnostic case of how suppression
 *   mechanisms can be formalized into law codes and maintained through
 *   institutional enforcement (judges, barristers, legislative bodies all
 *   composed of men) while being naturalized as inevitable properties of
 *   legal order itself. The constraint exhibits the full classification
 *   spectrum: pure extraction (snare) from the perspective of trapped women;
 *   coordination with extraction (tangled rope) from organized reform
 *   movements; pure coordination (rope) from male beneficiaries and the legal
 *   profession; and false natural law (mountain) from the dominant analytical
 *   perspective.
 *
 * KEY AGENTS:
 *   - Married Women (Powerless/Trapped): Primary victims — experience complete legal extinction through coverture; property, custody, and bodily autonomy transferred to husbands
 *   - Unmarried Women (Powerless/Trapped): Primary victims — retain nominal property rights but face absolute barriers to independence and employment; subject to family guardianship
 *   - Widows with Minor Children (Moderate/Constrained): Secondary victims — inherit property but lack automatic guardianship; experience high suppression through children as legal hostages
 *   - Male Heads of Household (Institutional/Arbitrage): Primary beneficiaries — concentrate property rights, inheritance, and household labor control; benefit from legal stability
 *   - Legal Profession and Colonial Administration (Institutional/Arbitrage): Secondary beneficiaries — benefit from concentrated legal authority, simplified contracting, reduced litigation among women
 *   - Women's Suffragist and Reform Coalition (Organized/Constrained): Mixed position — coordinate essential social welfare functions (poor relief, child protection) while operating within legal disability; provide unpaid labor that legitimizes state withdrawal from welfare
 *   - Analytical Observer (Analytical/Analytical): Risk naturalization — the dominant frame of 1904 jurisprudence treats legal disability as inevitable property of law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_womens_legal_disability_1904, 0.68).
domain_priors:suppression_score(irish_womens_legal_disability_1904, 0.82).
domain_priors:theater_ratio(irish_womens_legal_disability_1904, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_womens_legal_disability_1904, extractiveness, 0.68).
narrative_ontology:constraint_metric(irish_womens_legal_disability_1904, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(irish_womens_legal_disability_1904, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_womens_legal_disability_1904, snare).
narrative_ontology:human_readable(irish_womens_legal_disability_1904, "Irish Women's Legal Disability Under the 1904 Legal Code").
narrative_ontology:topic_domain(irish_womens_legal_disability_1904, "legal/political/gender").

domain_priors:requires_active_enforcement(irish_womens_legal_disability_1904).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_womens_legal_disability_1904, male_heads_of_household).
narrative_ontology:constraint_beneficiary(irish_womens_legal_disability_1904, legal_profession_patriarchal).
narrative_ontology:constraint_beneficiary(irish_womens_legal_disability_1904, colonial_administrative_apparatus).
narrative_ontology:constraint_victim(irish_womens_legal_disability_1904, married_women).
narrative_ontology:constraint_victim(irish_womens_legal_disability_1904, unmarried_women).
narrative_ontology:constraint_victim(irish_womens_legal_disability_1904, widows_with_minor_children).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARRIED WOMAN (SNARE) — Under coverture law, a married woman has no legal capacity. She cannot own property, sign contracts, divorce, retain custody of children, or testify independently in court. Her property upon marriage transfers to her husband. She cannot refuse marital relations. Her only exit from the constraint is widowhood or abandonment — both catastrophic. The constraint is absolute at the biographical horizon: she experiences complete legal extinction.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNMARRIED WOMAN (SNARE) — While retaining some property rights, unmarried women face absolute barriers to economic independence: no access to professional employment, no legal protection against sexual assault within family, no mechanism to prevent forced marriage, and complete guardianship by father or eldest brother. Her legal capacity exists as a permission that can be revoked at any moment by male relatives. She is trapped by family law and custom enforcement.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: WIDOW WITH MINOR CHILDREN (SNARE) — Upon her husband's death, a widow may inherit property but has no automatic guardianship rights over her own minor children. The child's property or earnings can be claimed by the father's estate or designated guardian. She must navigate male-controlled probate courts and guardianship law. She experiences high suppression (economic dependency, children as legal hostages) but slightly more agency than the married woman. Her timeline to exit is generational — when children reach majority or she remarries.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUFFRAGIST AND REFORM COALITION (TANGLED ROPE) — Organized women's groups (suffragists, temperance societies, charitable organizations) coordinate social welfare functions — poor relief, child protection, moral reform — that the colonial state has outsourced to private initiative. This coordination is genuine and essential. But the constraint also extracts: women reformers must operate within the legal disability framework, their organizational authority is never recognized in law, and their work legitimizes the state's refusal to establish public welfare structures. They see Tangled Rope — real coordination role coupled with systematic extraction of their unpaid labor.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MALE HEAD OF HOUSEHOLD (ROPE) — Experiences the legal disability regime as coordination: it coordinates household labor allocation, inheritance, and sexual/reproductive rights through a unified rule set (coverture and patria potestas). He has immediate arbitrage exit — he can modify his use of these rights at will (being more or less controlling). He experiences the constraint as beneficial but also as a coordination mechanism, not as extraction. His effective extraction is negative — he benefits from the rule set.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL PROFESSION AND COLONIAL ADMINISTRATION (ROPE) — Barristers, solicitors, judges, and colonial administrators benefit from the legal disability regime: it creates stable rule sets, reduces litigation (women cannot sue), and concentrates legal authority in male hands. The regime coordinates property transfer, inheritance, and contract enforcement efficiently for those who hold legal capacity. These institutions experience the constraint as a coordination mechanism with benefits — they have arbitrage exit (they can advocate for change) but choose not to because the status quo suits them.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the dominant analytical frame of 1904 jurisprudence, women's legal disability appears as a natural law derived from coverture doctrine, the unity of the marital relation, and the fundamental distinction between persons with legal capacity and those without. This perspective sees the constraint as immutable — rooted in the logic of property, contract, and domestic order itself. However, the structural data contradicts the mountain classification: the constraint is actively enforced, requires legislative codification, and benefits specific groups. This perspective naturalizes a contingent institutional arrangement as law of nature.
constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_womens_legal_disability_1904_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_womens_legal_disability_1904, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_womens_legal_disability_1904, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irish_womens_legal_disability_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically transfers women's property, earnings, guardianship rights, and bodily autonomy to male heads of household. The measurement captures that this extraction is not total (some women retain property before marriage, some wealthy women hire male trustees to limit extraction) but is severe and structural. Suppression (0.82): Very high. Women face absolute legal barriers (coverture law, statutory prohibitions on professional work), economic dependency (no independent income), social enforcement (family pressure, community ostracism of separated women), and institutional barriers (courts staffed by men, legislature all-male). Escape requires abandonment (catastrophic) or death of spouse (forcing remarriage for economic survival). Theater ratio (0.55): Moderate. The legal disability regime is partially performative (courts maintain elaborate rituals around coverture doctrine) but also genuinely functional (coverture law efficiently allocates property, inheritance, and labor within patriarchal households). The theater is lower than in purely extractive regimes because the extraction is direct and transparent — the law is written; women know their disability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the collapse of perspectival agreement at the boundary between beneficiaries and victims. Married women see pure extraction (Snare) — they cannot exit and bear complete cost. Male heads of household see pure coordination (Rope) — the same legal rules appear to solve household labor allocation efficiently. The reform coalition sees mixed coordination and extraction (Tangled Rope) — they organize essential social functions within the disability framework while being systematically exploited. The legal profession sees only coordination (Rope) — the rule set coordinates property and inheritance perfectly. The dominant analytical perspective (Natural Law view) risks seeing an immutable feature of legal order itself (Mountain), but the structural data reveals this as false: the constraint requires active enforcement (statute, judicial interpretation, social enforcement), concentrates benefits to specific groups (men with property, legal professionals), and is actively contested (suffragists are visibly fighting it). The perspectival gap between the mountain view and the snare view is one of the largest in the corpus — it reveals exactly how powerful naturalization narratives can be.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extraction. Married women (powerless + trapped) face maximum directionality toward extraction (d ≈ 0.95), yielding high effective extractiveness chi ≈ 0.95. Male heads of household (institutional + arbitrage) experience negative directionality (d ≈ 0.10), yielding negative chi ≈ -0.15, meaning they perceive the constraint as conferring benefits rather than extracting costs. The reform coalition (organized + constrained) experience moderate-high d ≈ 0.60, yielding moderate-high chi because they have some agency (organized power) but face high constraints (legal barriers to their coordination). The analytical observer at civilizational scope risks deriving d from a 'natural law' framing (d ≈ 0.00) that yields negative chi and thus perceives mountain classification, but this derivation naturalizes institutional choice as law of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how naturalization narratives ('women's legal disability is inevitable') prevent recognition of what the structural data reveals: an extractive regime actively maintained by law, judiciary, and custom. The mountain perspective (natural law) exists as a 'mandatrop' — it is the dominant frame but is contradicted by the empirical evidence of enforcement, legislative codification, institutional benefit concentration, and organized resistance. The true classification (Snare, with Tangled Rope for organized actors) is only visible when the constraint is measured from the perspective of those paying its costs. Mandatrophy resolution requires rejecting the false natural law narrative and recognizing contingent institutional choice. This is the historical resolution: by 1920s, Irish and British suffragists, feminists, and legal reformers successfully reframed the constraint as a 'legal disability' (a term itself!) rather than inevitable property of law, forcing legislative repeal and recoding of family law. The mandatrophy was resolved through epistemic and political struggle, not through new data alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coverture_vs_natural_subordination,
    'Is women''s legal disability an inevitable property of contract law (natural law) or a contingent institutional choice that concentrates power in male hands (extraction mechanism)?',
    'Comparative legal analysis: evidence that other legal systems functioned without coverture (Scots law provided some protections; Roman law provided extensive property rights to women); empirical test of whether coverture is logically necessary for contract enforcement vs politically convenient',
    'If natural law: mountain classification correct, extraction narrative invalid. If contingent institutional choice: snare classification correct, the constraint is actively enforced rather than inherited from logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coverture_vs_natural_subordination, conceptual, 'Whether legal disability is natural law or contingent institutional choice').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.82) is structural (legal barriers, enforcement) versus internalized (women''s acceptance of their disability, identity fusion with domestic role)?',
    'Historical evidence from women''s diaries, correspondence, and testimony; analysis of resistance vs compliance patterns; measurement of organized resistance vs passive acceptance; post-constraint (post-suffrage) behavioral patterns to identify persistent internalization',
    'If mostly structural: legal reform alone removes the constraint. If significantly internalized: cultural/educational reform required alongside legal change; suppression may persist beyond formal repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    reform_coalition_cooptation,
    'Do organized women''s reform movements (perspective 4) actually constitute genuine coordination (Tangled Rope) or are they coopted into providing extraction legitimacy (Snare from analytical view)?',
    'Institutional analysis: Did reform movements achieve legal reforms? Or did they provide welfare services that substituted for state provision, making government delegation to unpaid women''s labor permanent? Comparison of reform movement demands vs actual legislative outcomes.',
    'If genuine coordination: Tangled Rope classification supported. If coopted: snare extends to the organized level — the constraint co-opts resistance to provide legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_cooptation, empirical, 'Whether reform movements provide genuine coordination or cooptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_womens_legal_disability_1904, 1904, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iris_tr_t0, irish_womens_legal_disability_1904, theater_ratio, 0, 0.55).
narrative_ontology:measurement(iris_tr_t5, irish_womens_legal_disability_1904, theater_ratio, 5, 0.52).
narrative_ontology:measurement(iris_tr_t10, irish_womens_legal_disability_1904, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(iris_be_t0, irish_womens_legal_disability_1904, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(iris_be_t5, irish_womens_legal_disability_1904, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(iris_be_t10, irish_womens_legal_disability_1904, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_womens_legal_disability_1904, enforcement_mechanism).
narrative_ontology:affects_constraint(irish_womens_legal_disability_1904, irish_property_law_1904).
narrative_ontology:affects_constraint(irish_womens_legal_disability_1904, irish_marriage_consent_and_guardianship).
narrative_ontology:affects_constraint(irish_womens_legal_disability_1904, irish_womens_professional_employment_barriers).

% DUAL FORMULATION NOTE:
% Irish women's legal disability in 1904 is the overarching institutional constraint. It decomposes into three distinct constraint stories: (1) property law (married women cannot own or inherit), (2) marriage law (coverture doctrine plus lack of divorce), (3) employment law (statutory and customary barriers to professional work). Each has its own ε value and network relationships. The parent constraint described here (irish_womens_legal_disability_1904) has ε=0.68 and represents the integrated suppression regime. Downstream constraints model specific mechanisms (property transfer, consent doctrine, employment barriers) each with potentially different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_womens_legal_disability_1904, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
