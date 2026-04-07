% ============================================================================
% CONSTRAINT STORY: nil_endorsement_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nil_endorsement_gatekeeping, []).

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
 *   constraint_id: nil_endorsement_gatekeeping
 *   human_readable: Nil Endorsement Gatekeeping in Academic and Professional Credentialing
 *   domain: institutional/epistemic
 *
 * SUMMARY:
 *   Nil endorsement gatekeeping is the institutional practice of requiring
 *   formal endorsement from credentialed practitioners as a prerequisite for
 *   entry into a profession, where obtaining that endorsement requires
 *   already possessing credentials or memberships unavailable outside the
 *   profession itself. This creates a structural catch-22 that produces a
 *   snare-like constraint: aspiring professionals cannot gain access without
 *   endorsement; cannot obtain endorsement without access. The constraint
 *   operates across academic disciplines, licensed professions, credentialing
 *   bodies, and informal professional communities. It functions
 *   simultaneously as a quality control mechanism (legitimate coordination),
 *   as a market protection device (extractive gatekeeping), and as a degraded
 *   theater (credential inflation where formal endorsement increasingly
 *   poorly predicts actual competence). The extractiveness has grown over the
 *   measurement interval as credentialing requirements have proliferated
 *   while their predictive validity has declined, suggesting the constraint
 *   is transitioning from rope (genuine coordination) toward snare (pure
 *   extraction) as the theater ratio rises.
 *
 * KEY AGENTS:
 *   - Aspiring Professionals Without Endorsement: Primary victims (powerless/trapped) — face insurmountable circular barriers to entry
 *   - Marginal Communities: Secondary victims (moderate/constrained) — geographic, linguistic, or social distance from gatekeeping institutions compounds trapped status
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control credential issuance and endorse or withhold as strategic choice
 *   - Incumbent Professionals: Secondary beneficiaries (powerful/mobile) — protect market position through restricted supply and benefit from high entry barriers
 *   - Reform Movements: Organized opponents (organized/constrained) — build alternative credentialing systems but face active suppression and economic incentives favoring traditional pathways
 *   - Credential Theater System: Institutional actor (institutional/arbitrage) — maintains performative credentialing despite declining functional validity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent gatekeeping as immutable feature of professional knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nil_endorsement_gatekeeping, 0.58).
domain_priors:suppression_score(nil_endorsement_gatekeeping, 0.65).
domain_priors:theater_ratio(nil_endorsement_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nil_endorsement_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(nil_endorsement_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nil_endorsement_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nil_endorsement_gatekeeping, snare).
narrative_ontology:human_readable(nil_endorsement_gatekeeping, "Nil Endorsement Gatekeeping in Academic and Professional Credentialing").
narrative_ontology:topic_domain(nil_endorsement_gatekeeping, "institutional/epistemic").

domain_priors:requires_active_enforcement(nil_endorsement_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nil_endorsement_gatekeeping, credentialing_gatekeepers).
narrative_ontology:constraint_beneficiary(nil_endorsement_gatekeeping, incumbent_professionals).
narrative_ontology:constraint_victim(nil_endorsement_gatekeeping, aspiring_professionals_without_endorsement).
narrative_ontology:constraint_victim(nil_endorsement_gatekeeping, marginal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PROFESSIONAL WITHOUT ENDORSEMENT (SNARE) — Faces insurmountable barriers to entry. Cannot access credentialing opportunities without prior endorsement from gatekeepers; cannot gain endorsement without already possessing credentials. The catch-22 structure creates total suppression. No alternative pathways exist. Maximum experienced extraction.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL COMMUNITY MEMBER (SNARE) — Higher exit costs than the trapped perspective due to geographic or social isolation from gatekeeping institutions. Faces both structural barriers (no access to endorsers) and identity barriers (outsider status in credentialing communities). Constrained exit but significant suppression.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL GATEKEEPER (ROPE) — Experiences the nil endorsement requirement as a coordination mechanism for quality control and professional standards. Gatekeepers retain arbitrage: they can endorse or withhold, can migrate between institutions, can shift standards. Sees the constraint as functional cooperation, not extraction.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT PROFESSIONAL (TANGLED ROPE) — Benefits from gatekeeping (protects market value and excludes competition) while also genuinely participating in coordination (maintains professional standards). Mobile enough to switch fields or institutions if needed, but benefits accumulate from staying. Experiences both coordination function and extractive protection.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM MOVEMENT (TANGLED ROPE) — Organized actors (alternative credentialing systems, skill-based hiring advocates, open-source communities) can coordinate to build parallel pathways but face active suppression from existing gatekeepers. Possess agency to create alternatives but must operate against institutional inertia and economic incentives favoring traditional credentialing.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CREDENTIAL THEATER (PITON) — The formal endorsement system persists as ritual. Much of the functional quality control has atrophied (credentials don't reliably predict competence) but the theater persists through institutional inertia and employer risk-aversion. The 'nil endorsement' rule is maintained performatively: institutions claim it guarantees quality while evidence increasingly suggests it mainly reproduces existing hierarchies.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, some gatekeeping is inherent to any knowledge-bearing profession: someone must verify that practitioners meet minimum standards. From this view, nil endorsement appears as a natural law of credentialing — you cannot have trustworthy professionals without some filtering mechanism. However, the structural data contradicts this: the gatekeeper's arbitrage options, the reform movement's organizational capacity, and alternative credentialing systems' emergence all demonstrate that nil endorsement is contingent, not immutable.
constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nil_endorsement_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nil_endorsement_gatekeeping, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nil_endorsement_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nil_endorsement_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(nil_endorsement_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the interval. The constraint extracts significant value by restricting supply and protecting incumbent rents while offering aspirants only the legitimate benefit of quality signaling. However, the extractiveness is not maximal (0.80+) because some genuine quality control function remains and alternative credentialing pathways are beginning to erode the gatekeeper monopoly. The rising trajectory reflects credential inflation without corresponding validity gains. Suppression (0.65): High. Structural barriers include circular catch-22 logic, institutional monopoly on credential issuance, credentialing fees that compound poverty-based barriers, and social/cultural distance from gatekeeping communities. However, suppression is not total because organized reform movements and alternative platforms (online learning, skill-based hiring, portfolio assessment) are creating cracks in the institutional barrier. Theater ratio (0.68): High and rising. Formal endorsement rituals persist (letters of recommendation, thesis defenses, certification exams) despite declining correlation with actual job performance. Employers continue to use credentials as a proxy for trustworthiness despite evidence that gatekeeping increasingly sorts for demographic privilege rather than competence.
 *
 * PERSPECTIVAL GAP:
 *   Gap between institutional gatekeeper and powerless aspiring professional: The gatekeeper sees legitimate quality control (Rope); the aspirant sees insurmountable barrier (Snare). This gap is the diagnostic signature of pure extraction masked as coordination. Gap between incumbent professional and reform movement: Both see mixed coordination/extraction (Tangled Rope) but with opposite power trajectories — incumbents see stable protection, reformers see degrading barriers. Gap between credential theater system (Piton) and analytical observer (Mountain): The theater sees itself as degraded but necessary; the analytical observer risks naturalizing this degradation as immutable law, when the structural data (organizational reform capacity, alternative systems emergence) suggests the sunset logic of Scaffold may apply instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position in the extraction flow. Trapped aspiring professionals have no exit options (d ≈ 0.95) and bear the full cost → high f(d) → maximum experienced extractiveness. Gatekeeping institutions benefit from the constraint and have arbitrage options (d ≈ 0.05) → negative f(d) → they experience this as coordination, not extraction. Marginal community members have constrained exit but also trapped components (d ≈ 0.75-0.85) → high f(d) → strong experienced extraction. Reform movements have moderate power and some organizational capacity (d ≈ 0.50-0.60) → moderate f(d) → they experience the constraint as difficult but not insurmountable. The analytical observer (d ≈ 0.72) sees the full structure but risks naturalizing the beneficiary's framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in nil endorsement gatekeeping is the apparent paradox that the constraint simultaneously (a) maintains quality control (genuine coordination function), (b) extracts significant rents from aspirants, and (c) increasingly fails at quality control (theater ratio rising while predictive validity declines). The resolution: the constraint is real snare with vestigial rope components. The quality control narrative is the cover story that legitimizes pure extraction. The theater ratio (0.68, rising) reveals that the functional quality control component is atrophying while the extractive component persists. The reform movement perspective (Tangled Rope with agency) shows that if genuine coordination were the primary function, reformers would not face suppression — they would be welcomed as improving the system. The suppression of alternatives (0.65, high) is the diagnostic signal: gatekeepers suppress reform movements not because alternatives threaten quality, but because alternatives threaten their rent extraction. If this constraint were pure rope (legitimate coordination), alternative quality signals would be adopted. The fact that gatekeepers actively prevent adoption of alternatives that demonstrate equivalent quality indicates the constraint is snare, not rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endorsement_necessity_threshold,
    'What fraction of actual competence in a profession is genuinely predictable from formal endorsement versus socially reproduced through gatekeeping?',
    'Longitudinal competence studies comparing endorsed vs non-endorsed practitioners; prediction models using endorsement status vs actual performance metrics',
    'If endorsement predicts >70% of competence variance: gatekeeping may be legitimate coordination. If <40%: nil endorsement is primarily extractive status protection with minimal quality function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endorsement_necessity_threshold, empirical, 'Actual information content of formal endorsement in predicting competence').

omega_variable(
    alternative_signaling_sufficiency,
    'Can alternative credentialing systems (portfolio-based, skill-testing, community reputation, open-source contribution records) achieve equivalent or superior quality signals without institutional gatekeeping?',
    'Comparative analysis of failure rates, competence variance, and employer satisfaction across gatekept vs alternative-credentialed workers in same roles',
    'If alternatives match or exceed gatekept quality: snare classification confirmed — nil endorsement is pure extraction. If gatekept consistently outperforms: rope classification strengthened — gatekeeper coordination provides measurable value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_signaling_sufficiency, empirical, 'Effectiveness of alternative credentialing compared to institutional endorsement').

omega_variable(
    suppression_internalization_mechanism,
    'To what extent do aspirants internalize the gatekeeping logic versus recognize it as external structural barrier?',
    'Qualitative interviews and psychological assessment of aspiring professionals; measurement of identity fusion with gatekeeper standards vs perception of external constraint',
    'If internalized: many trapped agents experience identity_locked exit (believe they genuinely cannot meet standards). If perceived as external: exit_options may be constrained rather than trapped — revealing higher structural mobility than the constraint maintains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized through identity fusion').

omega_variable(
    gatekeeper_coalition_capture,
    'Do institutional gatekeepers constitute a unified coalition extracting rent, or do they genuinely disagree on standards with some gatekeepers favoring reduced barriers?',
    'Analysis of gatekeeper institution statements, reform proposals, and career patterns of gatekeepers who lower barriers vs those who raise them',
    'If unified: snare classification confirmed with high institutional power consolidation. If fragmented: some gatekeepers may shift to reform perspective, weakening suppression and shifting classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_coalition_capture, empirical, 'Degree of coordination versus fragmentation among gatekeeping institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nil_endorsement_gatekeeping, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nilgate_tr_t0, nil_endorsement_gatekeeping, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nilgate_tr_t15, nil_endorsement_gatekeeping, theater_ratio, 15, 0.6).
narrative_ontology:measurement(nilgate_tr_t30, nil_endorsement_gatekeeping, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(nilgate_be_t0, nil_endorsement_gatekeeping, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nilgate_be_t15, nil_endorsement_gatekeeping, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(nilgate_be_t30, nil_endorsement_gatekeeping, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nil_endorsement_gatekeeping, identity_coordination).
narrative_ontology:affects_constraint(nil_endorsement_gatekeeping, credential_inflation).
narrative_ontology:affects_constraint(nil_endorsement_gatekeeping, professional_licensing_barriers).
narrative_ontology:affects_constraint(nil_endorsement_gatekeeping, academic_hiring_monoculture).

% DUAL FORMULATION NOTE:
% Nil endorsement gatekeeping is the structural mechanism through which credential inflation, professional licensing barriers, and academic hiring monoculture operate. Each of these constraints has distinct ε values (credential_inflation ≈ 0.50 Tangled Rope, licensing_barriers ≈ 0.62 Snare, hiring_monoculture ≈ 0.45 Tangled Rope) but all depend on the core gatekeeping constraint. Nil endorsement is the upstream node in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nil_endorsement_gatekeeping, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
