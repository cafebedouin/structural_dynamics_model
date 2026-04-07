% ============================================================================
% CONSTRAINT STORY: sexual_suppression_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sexual_suppression_regime, []).

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
 *   constraint_id: sexual_suppression_regime
 *   human_readable: Sexual Suppression Regime
 *   domain: social/political/psychological
 *
 * SUMMARY:
 *   Sexual suppression regimes enforce normative control over human sexuality
 *   through legal prohibition, social stigma, medical pathologization, and
 *   institutional gatekeeping. The regime extracts sexual autonomy, enforces
 *   reproductive labor, and suppresses gender-nonconforming and
 *   non-reproductive sexuality while claiming to serve legitimate
 *   coordination functions: organizing reproduction, stabilizing kinship,
 *   clarifying inheritance, and managing population. This constraint
 *   demonstrates how extraction can be deeply intertwined with genuine
 *   coordination. The sexual suppression regime is neither pure extraction
 *   (snare) nor pure coordination (rope), but a hybrid structure where the
 *   coordination function is real and the extraction is also real, and they
 *   are structurally entangled. The regime exhibits mandatrophy: it cannot be
 *   classified as coordination without acknowledging the suppression, and it
 *   cannot be classified as extraction without acknowledging the coordination
 *   function. From the perspective of the marginalized person, the regime is
 *   a snare — pure extraction with no beneficial coordination. From the
 *   perspective of the state, it is a rope — pure coordination with no
 *   experienced extraction. From the perspective of the organized movement,
 *   it is a scaffold with a sunset clause — the regime is being dismantled
 *   through decriminalization, marriage equality, and destigmatization. The
 *   sexual rights movement perceives the constraint as temporary and actively
 *   works to degrade it. Extractiveness shows a declining trend (0.82 → 0.68)
 *   as legal suppression relaxes in some jurisdictions, but theater ratio
 *   shows an increasing trend (0.45 → 0.74), indicating that doctrinal
 *   suppression is increasingly decoupled from institutional enforcement —
 *   the regime persists through performative rhetoric and cultural inertia
 *   even as legal mechanisms are dismantled.
 *
 * KEY AGENTS:
 *   - Sexually Marginalized Groups: Primary victims (powerless/trapped) — LGBTQ+ individuals, people with non-normative sexualities, those who face criminalization, employment discrimination, family rejection, and medical pathologization
 *   - Gender-Nonconforming Agents: Primary victims (powerless/trapped) — those whose gender expression violates normative expectations and who face legal, social, medical suppression
 *   - Conforming Heterosexual Population: Secondary victim and partial beneficiary (moderate/constrained) — benefit from regime's coordination functions (stable pair-bonding, inheritance clarity) while bearing extraction (sexual shame, autonomy suppression, emotional authenticity sacrifice)
 *   - State Authority: Primary beneficiary (institutional/arbitrage) — maintains regime as instrument of population control, inheritance management, kinship legitimacy, and demographic stability
 *   - Patriarchal Family System: Primary beneficiary (powerful/mobile) — regime ensures male sexual access, female reproductive labor, legitimate succession, and property transfer through patrilineal kinship
 *   - Religious Institutions: Beneficiary with degraded authority (powerful/mobile) — maintain doctrinal suppression but face institutional-practice divergence; piton dynamic as enforcement capacity weakens
 *   - Medical Establishment: Institutional beneficiary (institutional/arbitrage) — pathologizes non-normative sexuality, profits from conversion therapy and psychiatric treatment, maintains diagnostic authority over sexuality
 *   - Sexual Rights Movement: Organized victim-advocate (organized/constrained) — scaffold perspective; actively works to decriminalize, destigmatize, and build alternative relationship/sexuality frameworks with perceived sunset to traditional regime
 *   - Ideological Gatekeepers: Institutional beneficiary (institutional/arbitrage) — educators, media producers, cultural authorities who enforce normative sexuality narratives and control access to sexuality information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sexual_suppression_regime, 0.68).
domain_priors:suppression_score(sexual_suppression_regime, 0.85).
domain_priors:theater_ratio(sexual_suppression_regime, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sexual_suppression_regime, extractiveness, 0.68).
narrative_ontology:constraint_metric(sexual_suppression_regime, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(sexual_suppression_regime, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sexual_suppression_regime, tangled_rope).
narrative_ontology:human_readable(sexual_suppression_regime, "Sexual Suppression Regime").
narrative_ontology:topic_domain(sexual_suppression_regime, "social/political/psychological").

domain_priors:requires_active_enforcement(sexual_suppression_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sexual_suppression_regime, patriarchal_institutions).
narrative_ontology:constraint_beneficiary(sexual_suppression_regime, control_authorities).
narrative_ontology:constraint_beneficiary(sexual_suppression_regime, ideological_gatekeepers).
narrative_ontology:constraint_victim(sexual_suppression_regime, sexually_marginalized_groups).
narrative_ontology:constraint_victim(sexual_suppression_regime, gender_nonconforming_agents).
narrative_ontology:constraint_victim(sexual_suppression_regime, autonomous_sexuality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEXUALLY MARGINALIZED PERSON (SNARE) — Trapped by legal prohibition, social ostracization, family rejection, employment discrimination, and internalized shame. No exit options within the regime. Bears maximum extraction: loss of relationship autonomy, identity erasure, psychological harm, economic penalty, and physical danger. The constraint is experienced as immutable and inescapable from within the marginalized position.
constraint_indexing:constraint_classification(sexual_suppression_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONFORMING HETEROSEXUAL (TANGLED ROPE) — Constrained by marriage norms, reproductive expectations, sexual shame education, and monitoring. Benefits from the regime's coordination function: state-enforced pair bonding reduces uncertainty in partnership formation and legitimizes family structures. Also bears extraction: sexual autonomy suppressed, desire channeled into reproductive service, infidelity criminalized or heavily penalized, emotional authenticity sacrificed. Moderate experience — genuine coordination benefits paired with real extraction.
constraint_indexing:constraint_classification(sexual_suppression_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE AUTHORITY (ROPE) — Experiences the regime as pure coordination: enforcing sexual norms solves the coordination problem of inheritance, property transfer, kinship legitimacy, and population management. The state can exit or modify enforcement without bearing personal cost. Net beneficiary — extraction flows toward this agent. The regime appears functional and beneficial from the institutional perspective.
constraint_indexing:constraint_classification(sexual_suppression_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SEXUAL RIGHTS MOVEMENT (SCAFFOLD) — Organized agents (LGBTQ+ movements, feminist organizing, sexual autonomy advocates) perceive the regime as a temporary constraint with a sunset clause. Decriminalization laws, marriage equality, destigmatization education, and bodily autonomy frameworks are building alternative pathways for relationship formation and sexuality expression. The movement has agency and sees an exit path. Suppression is high but declining as norms shift. Low effective extraction because the movement has organized capacity and perceives decreasing constraint force over generational time.
constraint_indexing:constraint_classification(sexual_suppression_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTION (PITON) — Formal religious doctrine nominally enforces sexual suppression (condemning same-sex relations, non-marital sexuality, contraception use), but institutional practice is increasingly diverging from doctrine. Many religious communities have adopted affirming stances while maintaining formal doctrinal positions. The regime persists through performative rhetoric and selective enforcement rather than actual functional control. Theater ratio (0.68) reflects the gap between doctrinal claim and institutional reality. The institution sees its own authority as partially degraded — the suppression mechanism has lost binding force but institutional inertia maintains the appearance.
constraint_indexing:constraint_classification(sexual_suppression_regime, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLOSETED PERSON (TANGLED ROPE) — Identity-locked exit: structurally mobile (could relocate, could disclose, has legal protections in many jurisdictions) but identity is constituted through hiddenness. Disclosure would require abandoning the closeted identity and reconstructing self-concept. The agent benefits from regime participation (social belonging, family stability, employment security from passing) while bearing extraction (constant self-monitoring, denial of authentic relationships, psychological fragmentation). Identity lock prevents exercise of structural mobility. Classification as tangled_rope rather than snare reflects that the binding mechanism is cognitive rather than purely material.
constraint_indexing:constraint_classification(sexual_suppression_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, the regime exhibits the full structure of tangled rope: genuine coordination function (organizing reproduction, kinship legitimacy, property inheritance through sexual partnership control) paired with asymmetric extraction (suppression of non-reproductive sexuality, enforcement of sexual shame, criminalization of sexual autonomy, disproportionate targeting of gender nonconforming and sexually marginalized agents). The coordination function is real but intertwined with extraction. The suppression mechanism (shame, criminalization, medical pathologization) sustains both coordination and extraction. Mandatrophy is resolved: this is not pure coordination disguised as extraction, nor pure extraction wearing coordination's mask — the regime genuinely coordinates inheritance and kinship while genuinely extracting autonomy.
constraint_indexing:constraint_classification(sexual_suppression_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sexual_suppression_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sexual_suppression_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sexual_suppression_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sexual_suppression_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sexual_suppression_regime, TR),
    TR >= 0.70.

:- end_tests(sexual_suppression_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. The regime extracts sexual autonomy, forces reproductive labor, suppresses non-normative sexualities, and imposes enforcement costs on targets. However, the extraction is not as severe as pure snares (0.80+) because the regime has a genuine coordination function — organizing inheritance, kinship, and reproduction is a real problem that requires solving, and the regime does solve it (though at tremendous cost to marginalized agents). The extractiveness value reflects that the regime extracts more than the coordination function requires, but the coordination function is genuine. Suppression (0.85): Very high. Multiple independent suppression mechanisms operate simultaneously: criminal law (in many jurisdictions), employment discrimination, family rejection, medical pathologization, shame internalization, and access denial to sexuality information and relationship formation pathways. Suppression is structural (external barriers) and internalized (identity fusion). Theater ratio (0.68, increasing trend): Moderate-high and rising. The regime relies increasingly on performative enforcement as legal suppression has been partially dismantled in many jurisdictions. Doctrinal suppression (religious teaching, medical diagnosis of non-normative sexuality as pathology) persists while institutional enforcement capacity declines. The gap between what the regime claims to enforce and what it actually enforces is widening — piton dynamic. Claimed type (Tangled Rope): The regime is claimed as tangled rope because it exhibits both genuine coordination (organizing reproduction and kinship) and asymmetric extraction (suppressing sexuality autonomy disproportionately affects marginalized agents). The coordination function is real; the extraction is real. They are structurally entangled.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same structural constraint is experienced as immutably oppressive (snare), functionally beneficial (rope), temporarily problematic (scaffold), institutionally degraded (piton), or genuinely hybrid (tangled rope), depending entirely on the observer's position within the constraint. The powerless marginalized agent and the institutional state authority are describing the same constraint from opposite structural positions and reach opposite conclusions about its nature. The gap is not a measurement error — it is the core diagnostic signal. The regime's true nature is that it is tangled rope: genuine coordination intertwined with extraction. But this tangled nature is invisible from any single perspective. Only the presheaf of all perspectives reveals the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Marginalized agents (powerless/trapped) derive maximum directionality (d ≈ 0.95) — they are victims with no exit. State authorities (institutional/arbitrage) derive minimum directionality (d ≈ 0.05) — they are beneficiaries with costless exit. Conforming heterosexuals (moderate/constrained) derive moderate directionality (d ≈ 0.65) — they are both beneficiaries (from coordination) and victims (from extraction), with high-cost exit. Closeted agents (identity_locked) derive d ≈ 0.80 — they are victims with structural exit options but identity-level binding. The analytical observer (analytical/analytical) derives d ≈ 0.72 from the mixed beneficiary/victim status across the population. These directionality values feed into the chi (effective extraction) calculation via f(d), producing the differential experienced extractiveness across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The sexual suppression regime resolves the mandatrophy by being genuinely tangled rope. The coordination function is real — organizing reproduction, clarifying kinship, enabling property inheritance, and stabilizing pair-bonding are genuine collective action problems that the regime solves. However, the regime solves these problems using suppression mechanisms that extract far beyond what the coordination function requires. Non-reproductive sexuality is suppressed not because reproduction coordination requires it, but because the regime extracts from the suppression itself (control, domination, enforcement authority). Gender-nonconforming expression is suppressed not because inheritance clarity requires it, but because the regime extracts from the suppression (categorical control, ideological uniformity). The mandatrophy is resolved by recognizing that both the coordination and the extraction are real, are structurally entangled, and cannot be separated. This is the textbook case of tangled rope: a constraint that genuinely coordinates a legitimate collective action problem while genuinely extracting beyond what the coordination requires. The false natural law perspective (mountain classification from the analytical observer in some contexts) naturalizes this entanglement as 'sexual norms are necessary for social stability' — but the structural data shows that alternative coordination mechanisms (marriage equality, legal kinship clarity independent of sexual conformity, reproductive decoupling from sexuality enforcement) can achieve the coordination function with radically reduced extraction. The mountain classification is a false summit: it naturalizes what is actually a contingent institutional arrangement that concentrates benefits and disperses costs asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reproduction_coordination_necessity,
    'How much of the regime''s coordinating function (kinship legitimacy, inheritance clarity, population stability) actually requires sexual suppression vs. alternative coordination mechanisms (marriage equality, legal parent/child clarity independent of parental sexuality, reproductive policy decoupled from sexual control)?',
    'Cross-national comparison: jurisdictions with minimal sexual suppression (Netherlands, Scandinavia, Canada) show whether stable kinship, inheritance, and reproduction coordination occur without regime enforcement',
    'If suppression is necessary: many aspects of the regime are genuine coordination costs, not extractive overhead. If alternative coordination works: the regime is using suppression to extract beyond what coordination requires, reclassifying substantial portions as pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reproduction_coordination_necessity, empirical, 'Necessity of sexual suppression for reproduction and kinship coordination').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of measured suppression (0.85) is structural (legal prohibition, employment discrimination, housing access denial, medical autonomy violation) vs. internalized (shame, self-censorship, identity fusion with closeted state, internalized pathologization)?',
    'Post-decriminalization trajectory analysis: jurisdictions that eliminated legal suppression show persistence of internalized suppression in subsequent generations; measurement of psychological harm beyond legal barriers',
    'If primarily structural: eliminating legal enforcement reduces suppression substantially. If primarily internalized: the regime''s suppression persists after legal enforcement ends, requiring additional identity-reconstruction work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Proportion of suppression that is structural vs. internalized').

omega_variable(
    identity_locked_exit_capacity,
    'For closeted agents claiming identity_locked exit, what percentage could achieve structural exit (relocation, disclosure, alternative community formation) if identity lock were addressed through therapeutic or community intervention?',
    'Longitudinal tracking of agents who undergo identity reconstruction (gender-affirming therapy, coming-out processes, community reintegration); comparison of actual exit rates post-intervention vs. pre-intervention',
    'If high (>70%): identity lock is the primary binding mechanism; structural mobility exists but identity prevents its exercise. If low (<30%): identity lock may be post-hoc rationalization of material barriers; agent is actually constrained or trapped rather than identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_capacity, empirical, 'Whether identity-locked agents can achieve structural exit with identity support').

omega_variable(
    regime_cohesion_across_institutions,
    'Is the sexual suppression regime a unified control structure maintained by coordinated institutional enforcement (state, church, medicine, family), or a fragmented set of independent institutional interests that happen to align?',
    'Historical analysis of institutional coordination: evidence of explicit coordination vs. independent institutional adoption of suppression norms; evidence of institutional conflict over suppression policy',
    'If unified: the regime is a coherent system designed for control; collective action against any single institution is less effective. If fragmented: institutional conflicts can be exploited; different institutions have different exit paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_cohesion_across_institutions, empirical, 'Whether sexual suppression is unified institutional system or fragmented institutional interests').

omega_variable(
    extraction_flow_direction,
    'Who actually benefits from the sexual suppression regime? Is extraction flowing primarily toward the state (population control, inheritance management, kinship legitimacy), toward the patriarchal family system (male sexual access, female reproductive labor), toward religious institutions (doctrinal authority, membership stability), or toward some combination?',
    'Power analysis: tracking who bears enforcement costs vs. who receives benefits; identification of divergent interests among nominal beneficiaries; historical evidence of institutional conflict over regime terms',
    'If state primarily benefits: regime serves demographic and administrative functions; changing state policy (decriminalization, recognition of alternative families) can degrade the regime. If patriarch primarily benefits: regime serves gender extraction; dismantling requires transforming family structure and male sexual access norms. If multiple beneficiaries: coalition-building among victims must target each beneficiary''s distinct interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_direction, conceptual, 'Identity of primary beneficiaries and extraction flow direction').

omega_variable(
    theater_ratio_trend,
    'Is the theater ratio (0.68) stable or changing? In particular, is the gap between doctrinal suppression and institutional practice widening (piton dynamic) or narrowing (regime becoming more functionally coherent)?',
    'Historical measurement of enforcement intensity: criminal conviction rates, employment discrimination rates, medical pathologization rates over 20-30 year periods; comparison with doctrinal statements from same period',
    'If widening: regime is degrading into piton status in some institutional contexts, reducing overall suppression. If narrowing: institutions are tightening doctrinal-practice alignment, potentially increasing coordinated suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trend, empirical, 'Trend in theater ratio: whether doctrinal suppression is decoupling from institutional practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sexual_suppression_regime, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sexsupp_tr_t0, sexual_suppression_regime, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sexsupp_tr_t15, sexual_suppression_regime, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sexsupp_tr_t30, sexual_suppression_regime, theater_ratio, 30, 0.68).
narrative_ontology:measurement(sexsupp_tr_t45, sexual_suppression_regime, theater_ratio, 45, 0.72).
narrative_ontology:measurement(sexsupp_tr_t60, sexual_suppression_regime, theater_ratio, 60, 0.74).

% Extraction over time
narrative_ontology:measurement(sexsupp_be_t0, sexual_suppression_regime, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(sexsupp_be_t15, sexual_suppression_regime, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(sexsupp_be_t30, sexual_suppression_regime, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sexsupp_be_t45, sexual_suppression_regime, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(sexsupp_be_t60, sexual_suppression_regime, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sexual_suppression_regime, identity_coordination).
narrative_ontology:affects_constraint(sexual_suppression_regime, reproductive_labor_extraction).
narrative_ontology:affects_constraint(sexual_suppression_regime, gender_norm_enforcement).
narrative_ontology:affects_constraint(sexual_suppression_regime, family_property_inheritance).
narrative_ontology:affects_constraint(sexual_suppression_regime, medicalization_of_sexuality).

% DUAL FORMULATION NOTE:
% The sexual suppression regime decomposes into multiple structurally distinct constraints: reproductive labor extraction (enforced childbearing), gender norm enforcement (penalties for non-conformity), family property inheritance (kinship-based succession), and medicalization of sexuality (pathologization of non-normative sexuality). Each has its own extractiveness profile and its own perspectives. The regime story models the unified suppression mechanism; downstream stories model the specific extraction mechanisms enabled by this unified suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sexual_suppression_regime, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
