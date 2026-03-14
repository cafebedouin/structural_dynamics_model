% ============================================================================
% CONSTRAINT STORY: patriarchal_marriage_contract
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patriarchal_marriage_contract, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: patriarchal_marriage_contract
 *   human_readable: Patriarchal Marriage Contract: Legal and Identity-Based Extraction
 *   domain: social/legal/interpersonal
 *
 * SUMMARY:
 *   The patriarchal marriage contract is a legal, economic, and
 *   identity-based constraint that extracts from wives while appearing to
 *   coordinate household functions. From the perspective of the wife as legal
 *   dependent, it is a pure snare: legal structures (property rights
 *   subordination, custody prejudice, divorce penalties), economic barriers
 *   (wage gap, unpaid labor, financial dependency), and social suppression
 *   (shame, ostracism, reputation damage) create multiple exit barriers. From
 *   the perspective of the wife with identity fusion, the constraint operates
 *   through internalized framing: her self-concept is constituted through
 *   wifehood and motherhood; exit would require abandoning the identity that
 *   makes her life meaningful from within her own worldview. From the
 *   husband's perspective, the constraint appears as rope: legal headship
 *   legitimizes household decisions, economic responsibility clarifies
 *   financial management, marital duties define reciprocal obligations. From
 *   the feminist reform perspective, it contains both genuine coordination
 *   functions (managing joint resources, coordinating child-rearing) and
 *   asymmetric extraction (unpaid labor, legal subordination). From the
 *   religious institutional perspective, it is a degraded practice (piton):
 *   theological justification for male headship persists through ritual and
 *   doctrine despite functional obsolescence in contexts of women's economic
 *   participation and legal equality. The theater ratio reflects that much
 *   enforcement is performative—preaching, counseling, and ritual
 *   affirmation—rather than structural coercion. The extractiveness has
 *   increased over the 20-year interval as the gap between legal equality
 *   norms and lived subordination has become more visible; theater ratio
 *   similarly increased as formal justification became more performative
 *   relative to material practice.
 *
 * KEY AGENTS:
 *   - Wife as Legal Dependent (powerless/trapped): Primary victim — bears full extraction cost through legal subordination, economic dependency, and social penalties. No structural exit option.
 *   - Wife with Identity Fusion (powerless/identity_locked): Primary victim via cognitive mechanism — structurally mobile but functionally trapped through identity fusion. Cannot imagine self outside the marriage role.
 *   - Husband as Legal Head (institutional/arbitrage): Primary beneficiary — captures legal authority, economic headship, and social status. Net beneficiary with favorable exit option.
 *   - Feminist Reform Movement (powerful/mobile): Secondary actor — perceives mixed coordination-extraction hybrid. Organized agents pushing toward Tangled Rope resolution or constraint removal.
 *   - Religious Institutional Authority (institutional/arbitrage): Maintains doctrine through theology and ritual. Theater ratio reflects performative enforcement of doctrine with declining functional justification.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as immutable law or biological necessity. False summit detector flags this perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patriarchal_marriage_contract, 0.68).
domain_priors:suppression_score(patriarchal_marriage_contract, 0.72).
domain_priors:theater_ratio(patriarchal_marriage_contract, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patriarchal_marriage_contract, extractiveness, 0.68).
narrative_ontology:constraint_metric(patriarchal_marriage_contract, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(patriarchal_marriage_contract, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patriarchal_marriage_contract, snare).
narrative_ontology:human_readable(patriarchal_marriage_contract, "Patriarchal Marriage Contract: Legal and Identity-Based Extraction").
narrative_ontology:topic_domain(patriarchal_marriage_contract, "social/legal/interpersonal").

domain_priors:requires_active_enforcement(patriarchal_marriage_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patriarchal_marriage_contract, husband_as_legal_head).
narrative_ontology:constraint_beneficiary(patriarchal_marriage_contract, patriarchal_institutional_structure).
narrative_ontology:constraint_victim(patriarchal_marriage_contract, wife_as_legal_dependent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WIFE AS LEGAL DEPENDENT (SNARE) — Trapped by legal structures (property rights, custody precedence, economic dependency, divorce penalties), social suppression (shame, community ostracism, loss of identity), and internalized framing. No meaningful exit option within biographical time. Bears full extraction cost with minimal coordination benefit.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WIFE WITH IDENTITY FUSION (SNARE VIA IDENTITY_LOCKED) — Structurally mobile relative to legal barriers (has income potential, can access courts, legal protections exist on paper), but functionally trapped through identity fusion. Wife's self-concept is constituted through wifehood and motherhood; exit would require abandoning the identity frame that makes her life meaningful. Internalized suppression exceeds external barriers. Identity frame makes exit literally unthinkable from within.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: HUSBAND AS LEGAL HEAD (ROPE) — Experiences the constraint as coordination mechanism: legal authority legitimizes household decision-making, economic headship clarifies financial management, marital duties are reciprocally defined. Net beneficiary with exit option (arbitrage) — can exit marriage with fewer legal/social penalties. Extraction runs toward this agent; perceived as fair coordination of household authority.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEMINIST REFORM MOVEMENT (TANGLED ROPE) — Powerful organized agents (feminist networks, legal reformers, international human rights bodies) perceive genuine coordination problems (household economics, property rights, child custody) alongside extractive mechanisms. Reform agenda includes both coordination improvements (clearer rights) and extraction reduction (marital property, custody equity). From this position, the constraint contains both functions — neither pure rope nor pure snare.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL RELIGIOUS AUTHORITY (PITON) — Religious institutions maintain patriarchal marriage doctrine through theological justification and ritual performance (wedding ceremonies, marital counseling, divorce restrictions). The functional justification for male headship has atrophied in contexts of women's economic participation and legal equality, but the doctrine persists through institutional inertia, scriptural citation, and therapeutic theater. Theater ratio reflects that much enforcement is performative (preaching compliance, ritual affirmation) rather than structural.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / BIOLOGICAL NATURALIZATION (MOUNTAIN) — From maximum universality, patriarchal marriage may be perceived as emerging from biological reproductive asymmetry, evolutionary selection for male hierarchy, or immutable human nature. This perspective risks naturalizing what is contingent institutional arrangement. Engine's false summit detector will identify this as naturalization of contingent social structures, not inherent law.
constraint_indexing:constraint_classification(patriarchal_marriage_contract, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patriarchal_marriage_contract_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patriarchal_marriage_contract, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patriarchal_marriage_contract, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patriarchal_marriage_contract, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patriarchal_marriage_contract, TR),
    TR >= 0.70.

:- end_tests(patriarchal_marriage_contract_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts labor (unpaid domestic work), legal rights (property subordination, custody prejudice), economic independence (wage gap, spousal control), and autonomy (decision-making authority). The husband captures marriage benefits (status, household labor, economic advantage) while the wife bears costs. The value reflects both direct extraction and opportunity costs. Suppression (0.72): Very high. Multiple barriers prevent exit: legal (divorce restrictions, custody loss risk, property division penalties), economic (financial dependency, wage gap disadvantage, re-entry barriers), social (shame, community ostracism, identity loss), and cognitive (internalized obligation, fear, religious framing). Theater ratio (0.65): Moderate-high. Patriarchal marriage doctrine is maintained through significant performative content: religious preaching, marital counseling emphasizing 'headship,' family-oriented institutional messaging, ritual affirmation of male leadership. Functional justification has atrophied as women's economic participation and legal status changed, but doctrine persists through institutional inertia and theological citation. The 20-year trajectory shows increasing theater ratio as formal legal equality has made informal enforcement more theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between wife and husband is maximal: the wife experiences snare (pure extraction, trapped exit); the husband experiences rope (coordination, beneficiary status). This gap reflects their opposite structural positions—she is the target, he is the beneficiary. The wife_identity_locked perspective produces the same snare classification as wife_trapped, but through a different mechanism: cognitive fusion rather than structural barriers. This gap reveals the extent to which identity-locking amplifies suppression beyond material barriers alone. The reform perspective (Tangled Rope) reflects that genuine coordination functions exist (managing joint resources, coordinating child-rearing) alongside asymmetric extraction—neither pure rope nor pure snare is adequate. The religious perspective (piton) reflects performative maintenance of doctrine whose functional justification has declined. The analytical perspective's mountain classification is a false summit: the constraint is contingent on institutional arrangements (law, custom, theology), not immutable natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The wife_trapped perspective derives d from victim status (high) + trapped exit options (highest) → f(d) → maximum experienced extraction. The wife_identity_locked perspective derives d from victim status + identity_locked exit (high but not maximum) → slightly lower chi than trapped, but suppression extends beyond material barriers into cognitive capture. The husband_beneficiary perspective derives d from beneficiary status + arbitrage exit → minimal or negative d → he experiences the constraint as coordination, not extraction. The reform_movement perspective derives d from powerful + mobile + mixed beneficiary/victim status → intermediate d → mixed classification (Tangled Rope) reflecting genuine mixed experience. Directionality overrides are not needed—the structural derivation captures the real relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates how the same legal and social arrangement can be classified as snare, rope, tangled_rope, piton, or mountain depending on the observer's structural position and time horizon. The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings. The wife_trapped reads snare (she is the target, structurally trapped). The wife_identity_locked reads snare via internalized suppression (structurally mobile but identity-fused). The husband reads rope (he is the beneficiary, experiences coordination). The reform movement reads tangled_rope (perceives both coordination and extraction). The religious institution reads piton (maintains doctrine through theater). The analytical observer risks mountain (naturalizing contingent arrangement). No single type is 'correct'—the presheaf over all positions IS the answer. The constraint's extractiveness (0.68) and suppression (0.72) are structurally genuine; the classification variance reflects real perspectival differences, not measurement error or definitional ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_decomposition,
    'What proportion of the wife''s suppression is structural (legal, economic, enforcement barriers) versus internalized (identity fusion, cognitive capture, internalized shame)?',
    'Post-exit trajectory analysis: track wives'' mental health, self-concept, autonomy markers in years 1-5 after divorce or separation. If suppression declines after barriers are removed, it was primarily structural. If suppression persists or re-emerges, significant component is internalized.',
    'If suppression is primarily structural (>70%): constraint classification remains Snare, but exit support protocols should target material barriers. If suppression is primarily internalized (>60%): identity-locking mechanism is primary; cognitive reframing and identity reconstruction are essential to exit efficacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_decomposition, empirical, 'Proportion of suppression that is structural vs internalized').

omega_variable(
    coordination_function_genuine,
    'Does the patriarchal marriage contract genuinely solve coordination problems (household economics, child-rearing, property management) that cannot be solved with egalitarian alternatives?',
    'Comparative institutional analysis: performance metrics (household economic stability, child welfare outcomes, dispute resolution effectiveness) in patriarchal vs egalitarian marriage regimes controlling for economic development level, education, and legal context.',
    'If patriarchal structure outperforms egalitarian on coordination metrics: constraint contains genuine rope component (Tangled Rope classification more accurate). If egalitarian regimes perform equally or better: patriarchal structure is pure extraction dressed as coordination (Snare confirmed, rope component is performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_genuine, empirical, 'Whether patriarchal structure genuinely solves coordination problems').

omega_variable(
    legal_reform_substitution_effectiveness,
    'When legal reforms remove formal patriarchal authority (marital property equity, no-fault divorce, custody equality), do informal extraction mechanisms persist or diminish?',
    'Longitudinal analysis of household power dynamics, domestic labor allocation, economic decision-making, and domestic violence rates in jurisdictions before and after legal reform; control for cultural/religious context.',
    'If informal extraction persists post-reform: identity-locking mechanism is robust and primary (institutional inertia alone insufficient explanation). If extraction declines: legal reform is effective and constraint is primarily structural rather than identity-based.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_reform_substitution_effectiveness, empirical, 'Effectiveness of legal reform in reducing extraction when formal patriarchal authority is removed').

omega_variable(
    identity_locked_threshold,
    'At what point in identity development (adolescence, courtship, marriage, motherhood, mid-life) does the wife''s identity lock most strongly into the patriarchal role?',
    'Developmental psychology research and women''s exit narratives: identify critical junctures where identity fusion deepens or can be interrupted. Track identity reconstruction difficulty relative to age at exit.',
    'If locking occurs early (adolescence/courtship): prevention and alternative identity development are critical. If locking occurs at motherhood: custody fears amplify identity lock (omega variable interaction). If locking is continuous and deepening: exit support must address sequential identity reconstruction across life stages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_threshold, empirical, 'Critical junctures of identity locking into patriarchal marriage role').

omega_variable(
    mandatrophy_classification_variance,
    'Why do different perspectives (wife_trapped, wife_identity_locked, husband_beneficiary, reform_movement) all warrant Snare or Tangled_Rope rather than producing a false summit Mountain?',
    'Confirm that no perspective successfully argues for immutable natural law status: biological asymmetry arguments fail when confronted with egalitarian outcomes; religious naturalization is shown to be interpretive choice not doctrinal necessity; evolutionary psychology arguments show equifinality across marriage regimes.',
    'If false summits exist: analytical observer is naturalizing contingent structures. If no false summits: classification variance reflects genuine perspectival differences, not measurement error, and mandatrophy is resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_classification_variance, conceptual, 'Verification that classification variance reflects perspectival genuineness, not false naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patriarchal_marriage_contract, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmc_tr_t0, patriarchal_marriage_contract, theater_ratio, 0, 0.5).
narrative_ontology:measurement(pmc_tr_t10, patriarchal_marriage_contract, theater_ratio, 10, 0.62).
narrative_ontology:measurement(pmc_tr_t20, patriarchal_marriage_contract, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(pmc_be_t0, patriarchal_marriage_contract, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pmc_be_t10, patriarchal_marriage_contract, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(pmc_be_t20, patriarchal_marriage_contract, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patriarchal_marriage_contract, attachment_coordination).
narrative_ontology:boltzmann_floor_override(patriarchal_marriage_contract, 0.12).
narrative_ontology:affects_constraint(patriarchal_marriage_contract, patriarchal_household_labor_allocation).
narrative_ontology:affects_constraint(patriarchal_marriage_contract, patriarchal_custody_precedent).
narrative_ontology:affects_constraint(patriarchal_marriage_contract, patriarchal_marital_property_regime).

% DUAL FORMULATION NOTE:
% Patriarchal marriage decomposes into at least three structurally distinct constraints with different ε values: (1) Emotional attachment coordination (ε~0.45, genuine coordination with embedded extraction), (2) Labor allocation (ε~0.72, largely extraction with minimal coordination), (3) Legal property rights (ε~0.68, institutional extraction via law). This story treats the integration—the marriage contract as a unified legal/emotional/economic system. Downstream stories address specific mechanisms. All three are linked: removing legal subordination without addressing identity-locking and labor dynamics leaves suppression mechanisms intact; conversely, identity-level change without legal/economic reform traps women in improved but still-subordinate positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
