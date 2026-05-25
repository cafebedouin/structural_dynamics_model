% ============================================================================
% CONSTRAINT STORY: greek_gender_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_greek_gender_norms, []).

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
 *   constraint_id: greek_gender_norms
 *   human_readable: Greek Gender Norms and Social Coordination
 *   domain: social/cultural/gender
 *
 * SUMMARY:
 *   Greek gender norms represent a persistent constraint on women's autonomy
 *   and economic participation that exhibits characteristics of both genuine
 *   social coordination and systematic extraction. These norms have deep
 *   historical and religious roots in Orthodox Christianity, Aristotelian
 *   philosophy, and Mediterranean kinship systems. The constraint coordinates
 *   legitimate household functions — childcare, elder care, food production,
 *   kinship maintenance — while simultaneously extracting unpaid labor from
 *   women, restricting their educational and professional opportunities, and
 *   enforcing patriarchal authority through family honor systems. The
 *   constraint's extractiveness has paradoxically increased over the 40-year
 *   interval despite (or because of) women's increased education and labor
 *   force participation: as women occupy more professional space, the norms
 *   must be actively maintained rather than passively accepted, creating more
 *   visible performance and enforcement activity. The suppression remains
 *   high through multiple mechanisms: economic dependency (unequal wages, job
 *   segregation), legal frameworks (family law privileges male authority in
 *   traditional family structures), cultural enforcement (honor/shame
 *   systems, relational identity fusion), and institutional inertia (church,
 *   state, family institutions reproduce gendered role assignments). The
 *   theater ratio has increased as actual practice (women working, delaying
 *   marriage, choosing single motherhood) has diverged from official ideology
 *   (women's role is the home), requiring more elaborate performative
 *   maintenance of the traditional narrative.
 *
 * KEY AGENTS:
 *   - Women (victims): Primary targets of extraction — experience identity_locked exit at immediate/biographical level; structurally mobile but cognitively unable to exercise exit; bear disproportionate unpaid labor, restricted economic opportunity, legal disadvantages
 *   - Male household heads (primary beneficiary): Institutional actors with arbitrage exit options; benefit from labor appropriation, authority consolidation, status maintenance; experience norms as natural coordination
 *   - Orthodox Church (institutional beneficiary): Maintains theological justification for patriarchal order; provides ritual and authority backup for family structures; benefits from women's unpaid reproductive and care labor that stabilizes society
 *   - Greek state institutions (institutional beneficiary): Laws embedded with gender discrimination (family law, property law, custody frameworks); benefit from unpaid women's labor reducing public expenditure on care infrastructure; maintain performative commitment to 'traditional values' while implementing incomplete gender equality policies
 *   - Educated urban women (moderate victim): Constrained exit option — can pursue education and careers at cost of family rupture, social ostracism, marriage market penalty; see coordination function but also extraction clearly
 *   - Gender equality advocates and feminist organizations (organized challengers): Mobile exit options; work to organize legal and cultural change; perceive norms as both extractive and historically contingent; face suppression from conservative institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greek_gender_norms, 0.58).
domain_priors:suppression_score(greek_gender_norms, 0.72).
domain_priors:theater_ratio(greek_gender_norms, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greek_gender_norms, extractiveness, 0.58).
narrative_ontology:constraint_metric(greek_gender_norms, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(greek_gender_norms, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(greek_gender_norms, tangled_rope).
narrative_ontology:human_readable(greek_gender_norms, "Greek Gender Norms and Social Coordination").
narrative_ontology:topic_domain(greek_gender_norms, "social/cultural/gender").

domain_priors:requires_active_enforcement(greek_gender_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(greek_gender_norms, male_household_heads).
narrative_ontology:constraint_beneficiary(greek_gender_norms, patriarchal_institutional_actors).
narrative_ontology:constraint_victim(greek_gender_norms, women).
narrative_ontology:constraint_victim(greek_gender_norms, gender_nonconforming_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A woman born into Greek society experiences these norms as inescapable structural constraints backed by family honor, legal frameworks, economic dependency, and identity fusion. Exit is structurally mobile (she could legally leave, relocate, or abandon the identity) but cognitively impossible — her self-concept, family relationships, and social identity are constituted through gender role compliance. The constraint appears unchangeable from within because challenging it means ceasing to be 'properly Greek' and severing relational identity with kin.
constraint_indexing:constraint_classification(greek_gender_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% Women with education, career paths, and urban residence experience the norms as high-cost constraints they can partially escape at professional/economic price (career limitations, social ostracism, family rupture). They see coordination function — the norms do organize household labor and kinship maintenance — alongside the extraction targeting them. Some agency exists but is expensive to exercise.
constraint_indexing:constraint_classification(greek_gender_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Male household heads experience gender norms as coordination mechanism solving household organization, resource allocation, and social positioning. The norms benefit them through authority consolidation, labor appropriation, and status maintenance. Their exit is minimal (arbitrage) because they can navigate or redefine norms to their advantage. The constraint appears as natural social order providing legitimate coordination.
constraint_indexing:constraint_classification(greek_gender_norms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Religious and state institutions maintain these norms through performative rhetoric (tradition, divine order, natural law) while the functional role has atrophied. Women's actual participation in labor markets, education, and civil society has increased dramatically, yet institutional language and formal structures remain unchanged. The theater ratio reflects the gap between performative maintenance of 'traditional values' and actual household dynamics where women work, negotiate, and exercise agency.
constraint_indexing:constraint_classification(greek_gender_norms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized actors working on gender equality see the constraints as extractive but also recognize genuine coordination functions the norms historically served (kinship maintenance, household stability). Their perspective bridges: they see extraction clearly but also understand why people internalize and defend the norms. They experience some agency through legal reform and cultural organizing, but face strong suppression from conservative institutional and family structures.
constraint_indexing:constraint_classification(greek_gender_norms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational perspective detached from Greek context, it may appear that gender differentiation is a natural, unchangeable law of human social organization. This perspective risks naturalizing contingent cultural arrangements as inherent features of human nature or inevitable social organization. However, this is a false summit — the structural data reveals Greek gender norms as historically contingent, culturally specific, and actively maintained through institutions.
constraint_indexing:constraint_classification(greek_gender_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greek_gender_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(greek_gender_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greek_gender_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(greek_gender_norms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(greek_gender_norms, TR),
    TR >= 0.70.

:- end_tests(greek_gender_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting asymmetric labor extraction (women provide 70-80% of unpaid household/care labor while also increasingly in paid labor markets), restricted opportunity (wage gaps, occupational segregation, unequal burden of caregiving), and legal disadvantages (family law, inheritance frameworks). The value increased from 0.35 to 0.58 over 40 years because women's expanded education and labor force participation reveals the extraction rather than hiding it — the norms must now actively justify why women should accept unequal load. Suppression (0.72): High. Economic mechanisms (wage gaps, job market segregation, unequal asset control), legal mechanisms (family law frameworks preserving male authority, inheritance rules), cultural mechanisms (honor/shame systems, religious authority, identity fusion), and institutional mechanisms (church and state enforcement) all work to prevent exit. The suppression is not absolute — women can and do exit through migration, education, workplace careers — but the cost is high. Theater ratio (0.65): Moderate-high. Institutional rhetoric about 'traditional values,' 'family preservation,' and 'natural gender roles' is increasingly performative — women's actual behavior contradicts official ideology. Labor force participation by women has risen sharply; marriage and fertility rates have declined; women's educational attainment now exceeds men's in some cohorts. Yet institutions (church, state, family) maintain elaborate performative commitment to traditional gender roles. The theater has increased over the interval as the gap between stated ideology and actual practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The male household head sees coordination (Rope) — organizing household production and kinship. The institutional church/state sees natural order (Mountain, false summit) — or degraded maintenance (Piton) if acknowledging the gap between ideology and practice. The educated urban woman sees tangled hybrid (Tangled Rope) — the norms do coordinate household work but extract from her specifically through opportunity restrictions. The powerless/identity-locked woman sees inescapable extraction (Snare) — the binding is cognitive (identity fusion with gender role, family duty) making structural exit impossible. The gender equality movement sees a contingent, changeable constraint (Tangled Rope or Scaffold perspective with sunset) — coordinated by historical accident and institutional inertia, not by necessity. The civilizational analyst at risk sees mountain — but the structural data contradicts this.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from beneficiary/victim status and exit options. Male household heads: beneficiary status + arbitrage exit → low d (~0.10-0.15) → low/negative chi. Women: victim status + identity_locked exit → high d (~0.85-0.95) → high chi. Identity_locked is critical here: women are not trapped by material barriers alone (legal mobility exists, geographic exit is possible) but by identity fusion — their self-concept is constituted through gender role compliance, family duty, and kinship relationships. The engine's derivation chain computes d upward from victim status + identity_locked exit, capturing that the cognitive binding is as real as material binding, and experienced extractiveness is accordingly high.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing genuine coordination (kinship maintenance, household labor organization) from its contingent patriarchal implementation. The constraint exhibits Tangled Rope classification at the core because it DOES coordinate essential household functions while ALSO extracting asymmetrically. The snare perspective (powerless/identity-locked woman) shows the extraction clearly. The rope perspective (beneficiary male) shows the coordination clearly. Neither perspective is false — they see different aspects of the same structural reality. The piton and mountain perspectives are false summits (performative maintenance and naturalization). The scaffold perspective could exist if Greek gender norms showed a genuine sunset clause (they do not currently — no policy or cultural mechanism has established a time-bound decline). The analytical observer risks naturalizing the contingent as necessary. The mandatrophy is resolved by showing that all non-false-summit readings (Rope, Tangled Rope, Snare) are legitimate from their respective positions, and the classification depends on which agent's position is being measured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_entrapment,
    'How much of women''s compliance with gender norms derives from internalized identity fusion versus material barriers to exit?',
    'Post-exit analysis: women who leave Greece or abandon gender roles — does suppression persist through internalized shame/guilt (identity lock) or does it dissolve once external barriers are removed (structural trap)? Longitudinal mental health and well-being metrics.',
    'If mostly identity-locked: the constraint''s binding mechanism is cognitive/cultural — targets carry internalization with them. If mostly trapped: external barriers (economic, legal, family control) are primary — removing barriers changes exit capacity immediately. The distinction affects intervention strategies and sunset timelines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_entrapment, empirical, 'Identity fusion versus material entrapment in women''s compliance').

omega_variable(
    coordination_function_necessity,
    'How much of the actual household coordination that gender norms accomplish is necessary (cannot be done otherwise) versus contingent (could be organized differently)?',
    'Cross-cultural comparison of household organization in contexts with different gender norms; analysis of gender-egalitarian households in Greece and elsewhere; simulation of alternative household governance structures.',
    'If mostly necessary: the constraint serves genuine Rope-level coordination and cannot be eliminated without replacement. If mostly contingent: the coordination function is performatively maintained but could be replaced — the constraint is more Snare than Rope, and the theater ratio is the primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether household coordination requires gendered division of labor').

omega_variable(
    institutional_enforcement_degradation,
    'Is the piton classification (high theater, low function) accurate? Do Greek institutions actively enforce gender norms through law and policy, or do they maintain performative tradition while de facto enforcement has shifted to family/social pressure?',
    'Legal analysis of Greek family law, custody law, property law; audit of actual enforcement priorities of courts and state agencies; comparison with actual household labor division and women''s economic participation.',
    'If institutions actively enforce: the constraint is more Snare than Piton — state machinery matters. If institutions are performative and enforcement is social/familial: the piton classification holds, and the sunset mechanism is cultural norm shift rather than legal change. Determines whether policy focus should be law or culture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_enforcement_degradation, empirical, 'Whether state institutions actively enforce or performatively maintain gender norms').

omega_variable(
    generational_cliff_timing,
    'At what generational horizon does identity_locked exit transition to mobile or arbitrage exit? Is there a specific age/cohort where women perceive the norms as changeable?',
    'Age-cohort analysis of Greek women''s self-reported constraint perception, exit capacity, and identity fusion; qualitative interviews tracking shift from identity_locked to constrained/mobile perspectives; measurement of women''s labor force participation and educational attainment by age cohort.',
    'If cliff exists (e.g., women born after 1990 classify norms as changeable): sunset is structural and predictable. If shift is gradual: sunset timeline is longer and requires cultural intervention. Affects whether constraint should have scaffold perspective with sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_cliff_timing, empirical, 'Generational transition from identity-locked to mobile exit perception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(greek_gender_norms, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ggn_tr_t0, greek_gender_norms, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ggn_tr_t20, greek_gender_norms, theater_ratio, 20, 0.62).
narrative_ontology:measurement(ggn_tr_t40, greek_gender_norms, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(ggn_be_t0, greek_gender_norms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ggn_be_t20, greek_gender_norms, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(ggn_be_t40, greek_gender_norms, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(greek_gender_norms, identity_coordination).
narrative_ontology:affects_constraint(greek_gender_norms, greek_labor_market_segregation).
narrative_ontology:affects_constraint(greek_gender_norms, mediterranean_family_law_frameworks).
narrative_ontology:affects_constraint(greek_gender_norms, orthodox_church_authority).

% DUAL FORMULATION NOTE:
% Greek gender norms decompose across multiple structural domains with different ε values. The household labor coordination function (coordination of childcare, elder care, food production) has ε≈0.25-0.30 (Rope). The opportunity restriction mechanism (wage gaps, occupational segregation, career penalties) has ε≈0.55-0.65 (Snare/Tangled Rope). The legal/inheritance framework has ε≈0.35-0.45 (Tangled Rope with enforcement). The religious authority mechanism has ε≈0.40-0.50 (Piton — performative maintenance). These stories should be decomposed per the ε-invariance principle but are typically discussed as a unified constraint. This story represents the aggregate extractiveness across all mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(greek_gender_norms, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
