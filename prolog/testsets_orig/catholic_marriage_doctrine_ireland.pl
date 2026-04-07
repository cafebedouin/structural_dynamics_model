% ============================================================================
% CONSTRAINT STORY: catholic_marriage_doctrine_ireland
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catholic_marriage_doctrine_ireland, []).

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
 *   constraint_id: catholic_marriage_doctrine_ireland
 *   human_readable: Catholic Marriage Doctrine in Ireland: Institutional Coordination and Identity Extraction
 *   domain: religion/culture/family_law
 *
 * SUMMARY:
 *   Catholic marriage doctrine in Ireland represents a hybrid constraint
 *   combining genuine community coordination (ritual, identity, social
 *   structure) with asymmetric extraction (reproductive control, divorce
 *   restriction, patriarchal enforcement). The constraint's character changed
 *   dramatically across the interval: from institutional monopoly over family
 *   law (1950-1980) to contested coordination within pluralist society
 *   (2000-2026). The extractiveness declined from 0.72 to 0.58 as
 *   institutional enforcement capacity weakened through constitutional
 *   reforms (divorce legalization 1996, contraception access, abortion access
 *   gains), but theater increased from 0.55 to 0.68 as the doctrine's
 *   performance role became more explicit — the institution now articulates
 *   doctrine as spiritual guidance while continuing to enforce reproductive
 *   and family norms through cultural inertia and identity mechanisms rather
 *   than legal force. The constraint decomposition reveals multiple
 *   structurally distinct mechanisms: the identity-locked binding of faithful
 *   adherents, the institutional beneficiary position of Catholic authority,
 *   the state legal embedding (piton), organized opposition (women's
 *   coalitions), and the naturalizing analytical perspective. The critical
 *   diagnostic signal is the perspectival gap between the institutional
 *   beneficiary (rope — pure coordination) and the identity-locked victim
 *   (snare — maximum suppression through identity fusion).
 *
 * KEY AGENTS:
 *   - Faithful Catholic Adherents: Primary victims (powerless/identity_locked) — identity constituted through doctrine acceptance; structurally mobile but identity-locked by internalized worldview
 *   - Catholic Institutional Authority: Primary beneficiary (institutional/arbitrage) — maintains doctrine coordination and enforcement; experiences no extraction costs; benefits from structural position
 *   - Irish State: Secondary institutional actor (institutional/constrained) — historically embedded Catholic doctrine in family law; increasingly constrained to reform by constitutional pressure and pluralism
 *   - Women's Rights Coalitions: Organized victims (organized/constrained) — bear disproportionate extraction through reproductive control and family labor expectations; constrained by cultural inertia
 *   - Divorced and Unmarried Catholics: Specific victim group (moderate/constrained) — face both legal and social barriers to recognition; experience mixed coordination benefits and extraction costs
 *   - LGBTQ Individuals: Specific victim group (moderate/trapped) — excluded from doctrine's family coordination framework; trapped by doctrine's moral framework regardless of legal status
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent doctrine as unchangeable natural law about marriage and sexuality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catholic_marriage_doctrine_ireland, 0.58).
domain_priors:suppression_score(catholic_marriage_doctrine_ireland, 0.72).
domain_priors:theater_ratio(catholic_marriage_doctrine_ireland, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catholic_marriage_doctrine_ireland, extractiveness, 0.58).
narrative_ontology:constraint_metric(catholic_marriage_doctrine_ireland, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catholic_marriage_doctrine_ireland, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catholic_marriage_doctrine_ireland, tangled_rope).
narrative_ontology:human_readable(catholic_marriage_doctrine_ireland, "Catholic Marriage Doctrine in Ireland: Institutional Coordination and Identity Extraction").
narrative_ontology:topic_domain(catholic_marriage_doctrine_ireland, "religion/culture/family_law").

domain_priors:requires_active_enforcement(catholic_marriage_doctrine_ireland).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catholic_marriage_doctrine_ireland, catholic_institutional_authority).
narrative_ontology:constraint_beneficiary(catholic_marriage_doctrine_ireland, conservative_family_structure_advocates).
narrative_ontology:constraint_victim(catholic_marriage_doctrine_ireland, divorced_catholics).
narrative_ontology:constraint_victim(catholic_marriage_doctrine_ireland, unmarried_cohabiting_couples).
narrative_ontology:constraint_victim(catholic_marriage_doctrine_ireland, lgbtq_individuals).
narrative_ontology:constraint_victim(catholic_marriage_doctrine_ireland, women_constraining_reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAITHFUL ADHERENT (SNARE) — Catholic identity is constituted through doctrine acceptance. Exit would require abandoning not merely religious practice but the foundational identity frame that structures family, community membership, and self-conception. Structurally mobile (can legally divorce, cohabitate, access contraception) but identity-locked by internalized doctrine. Experiences maximum suppression of alternatives because the identity frame makes exit literally unthinkable from within the worldview.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: PRACTICING CATHOLIC IN MODERN IRELAND (TANGLED ROPE) — Faces real coordination benefits (community, ritual structure, identity affiliation) alongside asymmetric extraction (reproductive control, divorce restriction, sexual autonomy constraint). Exit costs are high (social penalty, family rupture, identity disruption) but not absolute. Experiences both genuine coordination function and coercive extraction — neither dominates.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN'S RIGHTS COALITION (TANGLED ROPE) — Organized actor constrained by persistent Catholic doctrine influence on family law, reproductive autonomy, and gender roles. Experiences coordination (shared struggle for equality) alongside extraction (doctrine-imposed reproductive constraints). Exit options constrained by cultural inertia and political coalition dynamics but improving with generational change.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CATHOLIC INSTITUTIONAL AUTHORITY (ROPE) — Primary beneficiary experiencing the doctrine as pure coordination mechanism: organizing community, regulating family structure, maintaining institutional coherence. No exit costs; full arbitrage capability. The institution experiences coordination benefits without apparent extraction — the extraction runs toward this agent but is invisible from within institutional perspective.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IRISH STATE LEGAL FRAMEWORK (PITON) — Historically embedded Catholic doctrine in family law (divorce prohibition until 1996, contraception restrictions until 1980s, ongoing abortion restrictions). Theater ratio high: legal frameworks cite 'traditional values' and 'constitutional protection of family' while functioning primarily to enforce doctrine without transparent acknowledgment. State capacity to exit has increased (constitutional referenda, legislative change) but inertia and path dependency remain. The legal framework has become degraded — performing constitutional protection while actual function is doctrine enforcement.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks treating Catholic doctrine's claims about indissoluble marriage, sexual ethics, and family structure as unchangeable natural law rather than contingent institutional doctrine. This perspective naturalizes what is actually a historical, culturally-specific system of meaning. The engine's false summit detection will flag this classification as naturalization of contingent doctrine.
constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catholic_marriage_doctrine_ireland_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catholic_marriage_doctrine_ireland, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catholic_marriage_doctrine_ireland, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catholic_marriage_doctrine_ireland, TR),
    TR >= 0.70.

:- end_tests(catholic_marriage_doctrine_ireland_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine extracts reproductive autonomy, divorce freedom, and gender equality through identity-locking mechanisms. However, extractiveness declined over the interval as institutional enforcement capacity weakened through constitutional reforms. Initial value (0.72) reflects the period of near-total institutional monopoly over family law (1950-1980); current value (0.58) reflects pluralism and secular alternatives, yet extractiveness remains substantial because identity-locking perpetuates suppression even as legal barriers dissolve. Suppression (0.72): High. Multiple layered mechanisms: legal barriers (abolished for some but persistent for abortion), social sanctions (family rejection, community exclusion), institutional enforcement (confession, sacrament withholding), and internalized suppression (identity fusion, epistemic closure, shame). The high suppression reflects both structural enforcement and internalized cognitive binding — agents cannot see alternatives as legitimate because the identity frame makes exit unthinkable. Theater ratio (0.68): Moderate-high. Doctrine performance has increased as enforcement capacity declined. The institution now articulates doctrine as 'spiritual guidance' and 'moral teaching' while continuing to enforce reproductive norms and family structure expectations through cultural mechanisms rather than legal force. The gap between stated function (moral direction) and actual function (reproductive control, gender role maintenance) has widened, increasing the performative content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum perspectival divergence. The institutional beneficiary experiences pure coordination (rope) — they see doctrine as solving legitimate community problems. The faithful adherent experiences pure extraction with identity binding (snare) — they see no coordination benefit because their identity is constituted through the constraint, making exit unthinkable. The organized coalition experiences mixed coordination and extraction (tangled_rope) — they recognize community benefits alongside recognized extraction. The state legal framework experiences degradation (piton) — the institution performs constitutional family protection while actually enforcing doctrine through inertia rather than legitimate function. The analytical observer risks seeing immutable natural law (mountain) — naturalizing contingent doctrine as unchangeable. This six-way perspectival gap indicates high structural information content: the constraint's meaning radically depends on the observer's structural position. No single perspective is 'wrong' — each is genuinely grounded in structural reality from that position. The gap itself is diagnostic of a hybrid constraint with both real coordination and real extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint generates vastly different directionality values for different agents despite identical base extractiveness. The Catholic institutional authority (beneficiary/arbitrage) derives low d from their structural position — they capture benefits with no exit cost, producing negative effective extraction from their perspective (rope classification). The faithful adherent (victim/identity_locked) derives high d from their structural position — they bear extraction costs through identity fusion and cannot exercise exit options, producing high effective extraction (snare classification). The organized women's coalition (victim/constrained) derives moderate-high d — they can organize and exert pressure (raising power level) but face substantial exit costs (constrained exit), producing moderate effective extraction (tangled_rope). The analytical observer risks false directionality by naturalizing the constraint as unchangeable (mountain) when the structural data reveals contingent institutional arrangements. The key insight: identical base_extractiveness (0.58) produces six different experienced χ values across perspectives, with the perspectival gap revealing the asymmetry of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Catholic marriage doctrine genuinely coordinates community meaning and ritual while extracting reproductive autonomy and gender equality. The constraint is not purely extractive (snare) — faithful adherents do experience community benefits, identity grounding, and ritual structure. Nor is it purely coordinative (rope) — the asymmetry of who benefits and who bears costs, and the identity-locking mechanisms that prevent exit perception, clearly indicate extraction. The tangled_rope classification captures this hybrid function. The critical analytical move is separating the three mechanisms: (1) genuine coordination (ritual, meaning-making, community structure), (2) asymmetric extraction (reproductive control, gender role maintenance), and (3) identity-locking (internalized doctrine frame that prevents exit perception). The theater_ratio increase (0.55→0.68) indicates that coordination function has declined relative to performative maintenance as institutional enforcement capacity weakened. The constraint persists not because it coordinates irreplaceable functions but because identity-locking and cultural inertia maintain suppression even as legal barriers dissolve. This is the classic piton pattern in disguise — the coordination narrative masks what is increasingly an inertial institutional performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Where is the boundary between identity-locked adherence (cognitive/identity binding) and constrained exit (high-cost material barriers)?',
    'Post-exit trajectory analysis: adherents who leave Catholicism show whether suppression persists after institutional exit. If identity frame breaks, suppression should decline; if internalized, it persists.',
    'If primarily identity_locked: constraint''s effective suppression is higher than structural measure suggests — agent carries binding with them. If primarily constrained: exit path remains viable if cost barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Identity lock versus material constraint boundary').

omega_variable(
    intergenerational_transmission_mechanism,
    'Is Catholic doctrine transmission primarily through identity socialization or through institutional enforcement?',
    'Generational cohort analysis: comparison of doctrine adherence rates between cohorts raised with strong institutional enforcement (mandatory religious education, exclusionary social structures) versus cohorts with voluntary participation. Tracking of apostasy rates and identity drift across generations.',
    'If primarily socialization: doctrine becomes vestigial as institutional enforcement declines — constraints naturally weaken (scaffold sunset). If primarily enforcement: doctrine persistence requires active institutional maintenance regardless of generational preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Whether transmission is socialization-based or enforcement-based').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal barriers, social sanctions, institutional enforcement) or internalized (cognitive patterns, identity fusion, epistemic closure)?',
    'Granular measurement: distinguish between legal barriers (divorce prohibition until 1996, contraception access, abortion law), social sanctions (family rejection, community exclusion, employment discrimination), institutional enforcement (confession, sacrament withholding), and internalized suppression (belief in doctrine legitimacy, identity fusion, internalized shame around contraception/divorce). Track changes in each component over the interval.',
    'If primarily structural: institutional reform reduces constraint severity. If primarily internalized: post-institutional exit may maintain suppression as agents carry internalized doctrine frame. If mixed: different interventions target different components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    doctrine_coordination_function_vs_extraction_cover,
    'Does Catholic doctrine coordinate genuine community/meaning functions, or does the coordination narrative mask extraction mechanisms?',
    'Comparative institutional analysis: identify which coordination functions (meaning-making, community solidarity, ritual structure, social capital) persist in secular alternatives. Measure community satisfaction and social capital in Catholic vs non-Catholic Irish populations with equivalent income/education. Assess whether coordination functions are doctrine-specific or generic to any community structure.',
    'If genuine coordination: doctrine provides real benefits alongside extraction — tangled_rope classification confirmed. If mask for extraction: primary function is control; coordination is secondary — reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_coordination_function_vs_extraction_cover, empirical, 'Whether coordination benefits are genuine or exploitation cover story').

omega_variable(
    theater_ratio_measurement_validity,
    'Does the theater_ratio (0.68) accurately capture the gap between stated doctrine function (moral/spiritual guidance) and actual social function (reproductive control, family structure enforcement, patriarchal norm maintenance)?',
    'Content analysis of Catholic doctrine articulation versus institutional enforcement patterns. Discourse analysis: what doctrine claims to do versus what institutional practice reveals. Track correspondence between stated teaching and actual enforcement resources allocated.',
    'If theater underestimated: constraint is closer to piton (high performance, low function). If theater accurate: tangled_rope with both genuine and performative elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_measurement_validity, empirical, 'Theater ratio measurement validity and potential underestimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catholic_marriage_doctrine_ireland, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cath_tr_t0, catholic_marriage_doctrine_ireland, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cath_tr_t20, catholic_marriage_doctrine_ireland, theater_ratio, 20, 0.62).
narrative_ontology:measurement(cath_tr_t40, catholic_marriage_doctrine_ireland, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cath_be_t0, catholic_marriage_doctrine_ireland, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(cath_be_t20, catholic_marriage_doctrine_ireland, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cath_be_t40, catholic_marriage_doctrine_ireland, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catholic_marriage_doctrine_ireland, identity_coordination).
narrative_ontology:affects_constraint(catholic_marriage_doctrine_ireland, irish_abortion_restriction).
narrative_ontology:affects_constraint(catholic_marriage_doctrine_ireland, irish_contraception_access).
narrative_ontology:affects_constraint(catholic_marriage_doctrine_ireland, irish_family_law_patriarchy).
narrative_ontology:affects_constraint(catholic_marriage_doctrine_ireland, irish_lgbtq_exclusion).

% DUAL FORMULATION NOTE:
% Catholic marriage doctrine is upstream of multiple specific family law and reproductive constraints in Ireland. Each downstream constraint has its own ε value reflecting specific domain empirics (abortion law ε≈0.62, contraception access ε≈0.35, family law patriarchy ε≈0.51, LGBTQ exclusion ε≈0.68), but all share the identity-coordination type and identity_locked exit mechanisms. The constraint family can be analyzed as a presheaf over Irish family law domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catholic_marriage_doctrine_ireland, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
