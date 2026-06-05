% ============================================================================
% CONSTRAINT STORY: award_system_voter_diversity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_award_system_voter_diversity, []).

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
 *   constraint_id: award_system_voter_diversity
 *   human_readable: Award System Voter Diversity Constraint
 *   domain: cultural_institutional/governance
 *
 * SUMMARY:
 *   Award systems in cultural fields (literature, music, visual arts, film)
 *   function simultaneously as coordination mechanisms that establish shared
 *   excellence standards across dispersed creative communities and as
 *   extraction systems that concentrate gatekeeping power in homogeneous
 *   voter populations. The voter diversity constraint captures this dual
 *   function: the award system must converge voters on shared judgment
 *   criteria (coordination) while the actual voter coalition systematically
 *   excludes creators and aesthetic traditions outside its cultural frame
 *   (extraction). The constraint exhibits the full range of DR
 *   classifications from different structural positions. Established
 *   tradition gatekeepers experience pure coordination (Rope) — the award
 *   validates their aesthetic and reinforces their curatorial authority.
 *   Emerging creators outside the voter coalition experience pure extraction
 *   (Snare) — structural exclusion from nomination and voting mechanisms. The
 *   award institution itself maintains the appearance of addressing diversity
 *   through expanded nominator pools, demographic reporting, and diversity
 *   statements, but actual voting outcomes remain concentrated in the
 *   established coalition (Piton — high theater, persistent extraction). The
 *   diversity reform coalition sees this as a temporary institutional failure
 *   with a sunset (Scaffold) — expanding voter demographics and implementing
 *   blind evaluation are structural reforms with measurable success criteria.
 *   The civilizational analytical observer risks naturalizing this as an
 *   immutable law of aesthetic judgment (Mountain) — that convergent voting
 *   on shared standards necessarily excludes alternatives — when the specific
 *   standards are contingently defended, not universal.
 *
 * KEY AGENTS:
 *   - Established Tradition Gatekeepers: Primary beneficiary (institutional/arbitrage) — voter coalition controls nomination gatekeeping, defines excellence criteria, receives award validation that reinforces their curatorial authority
 *   - Emerging Creators Outside the Coalition: Primary victim (powerless/trapped) — excluded from nomination pathways, cannot self-select into voting, face career stigma of 'not award-winning' even when work meets objective criteria under blind evaluation
 *   - Insider Creators with Marginal Status: Secondary victim (moderate/constrained) — access to voting but constrained by aesthetic conformity pressure; career damage from innovation outside established preferences
 *   - Diversity Reform Coalition: Organized agents (organized/constrained) — social media accountability, alternative award bodies, emerging voters pushing expansion of nomination pools and voter demographics; see diversity as structural reform with sunset logic
 *   - Award Institution Administrative Body: Institutional actor (institutional/arbitrage) — maintains diversity rhetoric and performative expansion (expanded nominator categories, demographic reports) while core voter coalition remains structurally homogeneous
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent aesthetic preferences as universal standards; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(award_system_voter_diversity, 0.52).
domain_priors:suppression_score(award_system_voter_diversity, 0.58).
domain_priors:theater_ratio(award_system_voter_diversity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(award_system_voter_diversity, extractiveness, 0.52).
narrative_ontology:constraint_metric(award_system_voter_diversity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(award_system_voter_diversity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(award_system_voter_diversity, tangled_rope).
narrative_ontology:human_readable(award_system_voter_diversity, "Award System Voter Diversity Constraint").
narrative_ontology:topic_domain(award_system_voter_diversity, "cultural_institutional/governance").

domain_priors:requires_active_enforcement(award_system_voter_diversity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(award_system_voter_diversity, established_tradition_gatekeepers).
narrative_ontology:constraint_beneficiary(award_system_voter_diversity, homogeneous_voter_coalition).
narrative_ontology:constraint_victim(award_system_voter_diversity, emerging_creative_traditions).
narrative_ontology:constraint_victim(award_system_voter_diversity, underrepresented_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Trapped by structural exclusion from voter access and nomination gatekeeping. No mechanism to self-select into the award system; must prove work's worth via external validation that the system itself constrains. Maximum experienced extraction: career stigma of 'not award-winning' persists even when work objectively matches award criteria under blind evaluation.
constraint_indexing:constraint_classification(award_system_voter_diversity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: INSIDER CREATOR WITH MARGINAL STATUS (TANGLED ROPE) — Has access to nomination and voting but constrained by expectation of conformity to established aesthetic. Experiences both coordination benefit (award validation carries institutional weight) and extraction (must suppress innovative impulses to remain electable). Career damage from experimentation; marginal chance of award even if work is nominated.
constraint_indexing:constraint_classification(award_system_voter_diversity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED TRADITION GATEKEEPER (ROPE) — Benefits from coordination: award validates their aesthetic, reinforces their position as arbiter of excellence, and legitimates their curatorial selections. Experiences the constraint as pure coordination — they are solving the problem of recognizing excellence within their tradition. No perceived extraction because they occupy the beneficiary position.
constraint_indexing:constraint_classification(award_system_voter_diversity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIVERSITY REFORM COALITION (SCAFFOLD) — Organized pressure (emerging voters, alternative award bodies, social media accountability) sees the homogeneous-voter constraint as a temporary institutional failure with a sunset. Diversity initiatives (expanding nominator pools, blind evaluation, demographic quotas for voters) are structural reforms with clear success metrics. Extraction falls as voter demographic barriers erode. Sunset logic: 15-25 years for norms to institutionalize 'diversity' as a structural feature rather than a reform goal.
constraint_indexing:constraint_classification(award_system_voter_diversity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: AWARD INSTITUTION ITSELF (PITON) — The institution maintains the voter diversity ritual as theater: diversity statements, expanded nominator recruitment, demographic reports. But actual voter composition and award outcomes show structural inertia — the core voting coalition remains homogeneous, and 'diversity winners' often reproduce established aesthetic preferences. The theater ratio is high: the institution performs diversity work while extractive outcomes persist. This is inertial maintenance, not functional reform.
constraint_indexing:constraint_classification(award_system_voter_diversity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / AESTHETIC UNIVERSALITY VIEW (MOUNTAIN) — From a civilizational perspective, excellence in artistic fields has objective criteria that transcend demographics: great work is great work regardless of creator origin. This perspective sees diversity concerns as performative — the real constraint is that voter coalitions must converge on shared aesthetic standards, and such convergence necessarily excludes alternatives. However, this naturalizes what is actually a contingent institutional arrangement: the specific standards that gatekeepers defend are culturally contingent, not universal. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(award_system_voter_diversity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(award_system_voter_diversity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(award_system_voter_diversity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(award_system_voter_diversity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(award_system_voter_diversity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(award_system_voter_diversity, TR),
    TR >= 0.70.

:- end_tests(award_system_voter_diversity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The award system extracts significant value from emerging creators through structural exclusion (no nomination pathway) and normalization (emerging traditions appear inferior until they conform). However, extraction is not maximal (0.66+) because some mechanisms exist for outsider entry (social media platforms, alternative award bodies, grassroots recognition) that bypass the primary award system. The metric reflects that the primary system is substantially extractive but not monopolistic. Rising trajectory from 0.38 to 0.52 over 20 years reflects that as diversity rhetoric increases, the performative content of reform increases (theater ratio rises) while actual extraction persists or slightly worsens (emerging creators face higher normalization pressure as 'diversity' becomes an aesthetic category to conform to rather than a structural change). Suppression (0.58): Moderate-high and stable. Barriers to emerging creator participation include: nomination gatekeeping (established voters curate the eligible pool), aesthetic conformity pressure (innovation risks exclusion), information asymmetry about voting criteria, and career dependency on award status in some fields (literature, film, classical music). The stability of suppression reflects that diversity reforms have not substantially reduced these barriers — they have added rhetoric without removing mechanisms. Theater ratio (0.64): High and rising. The award institution performs diversity work extensively: diversity statements, expanded nominator categories, demographic tracking, public commitment to inclusion. But the performance outpaces structural change: core voter demographics remain concentrated, award outcomes persist in established aesthetic preferences, and the theater has increased as the gap between diversity commitment and extractive outcomes widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range within a single structural phenomenon. The established tradition gatekeeper sees Rope — pure coordination solving the problem of shared excellence standards. The emerging creator sees Snare — pure extraction with no escape mechanism. The insider with marginal status sees Tangled Rope — both coordination benefit and extraction cost. The diversity reform coalition sees Scaffold — a temporary institutional failure being solved through structural reforms with a sunset. The award institution sees Piton — maintains diversity ritual through performative expansion while extractive outcomes persist. The civilizational analytical observer risks Mountain — seeing aesthetic convergence as an immutable law — but the structural data reveals this as false summit: the specific standards that define excellence are culturally contingent, and the gatekeeping mechanism is institutional, not natural. The perspectival gap between Rope (gatekeeper) and Snare (excluded creator) is maximal — the same structural phenomenon appears as benign coordination from one position and as structural exclusion from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Established gatekeepers are beneficiaries with arbitrage options (d ≈ 0.15 → f(d) ≈ -0.01 → negative χ) — they experience the constraint as beneficial coordination. Emerging creators outside the coalition are victims with trapped exit (d ≈ 0.95 → f(d) ≈ 1.42 → high χ) — they experience maximum extraction. Insider creators with marginal status are both beneficiaries and victims with constrained exit (d ≈ 0.55 → f(d) ≈ 0.75 → moderate χ) — they experience mixed coordination and extraction. The diversity reform coalition is organized with constrained exit (d ≈ 0.45 → f(d) ≈ 0.50 → moderate χ) — they have agency but face resistance from entrenched gatekeepers. The award institution is a beneficiary with arbitrage (d ≈ 0.15 → f(d) ≈ -0.01 → negative χ) — it benefits from maintaining the system that validates its authority. The analytical observer faces directionality ambiguity (d ≈ 0.72 → f(d) ≈ 1.15) — they are not embedded in the extraction flow but risk naturalizing it as universal rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution: this constraint demonstrates why single-type classification fails. The award system IS a coordination mechanism (Rope) and IS an extraction mechanism (Snare) simultaneously — not from observational ambiguity, but from structural reality. Established traditions require shared judgment standards (the coordination problem Rope solves). But the actual mechanism for establishing these standards excludes emerging traditions and concentrates power (the extraction problem Snare identifies). The Tangled Rope classification resolves the mandatrophy by capturing both functions in a single type: genuine coordination function (shared excellence standards) AND asymmetric extraction (homogeneous gatekeeping). The diversity reform coalition sees the constraint as Scaffold — a tangled rope with a sunset clause, where structural reforms (voter demographic expansion, blind evaluation, alternative nomination pathways) are eroding the gatekeeping mechanism over a 15-25 year horizon. The Piton classification (institutional perspective) reveals that institutional responses to diversity pressure are performative: the theater ratio rises as rhetoric increases while extraction persists. The Mountain classification at civilizational scope is a false summit — it naturalizes the gatekeeping mechanism as an inherent property of aesthetic judgment when the mechanism is actually institutional and contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aesthetic_universality_contingency,
    'Are the established aesthetic standards that gatekeepers defend genuinely universal, or are they culturally contingent preferences that function as universal because they control the voting mechanism?',
    'Comparative analysis of award-winning work from homogeneous vs. diverse voter coalitions; identification of systematic difference in selected aesthetic properties; longitudinal tracking of which ''universal'' standards persist when voter demographics shift',
    'If universal: diversity reform is theater (Piton dominates). If contingent: diversity reform is structural necessity (Scaffold dominates, and award system reclassifies from Mountain to Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aesthetic_universality_contingency, empirical, 'Whether aesthetic standards are universal or contingently defended through gatekeeping').

omega_variable(
    voter_homogeneity_mechanism,
    'Is voter homogeneity a structural feature of how awards are administered (formal barriers, self-selection by existing voters, nomination gatekeeping) or an emergent outcome of meritocratic self-selection?',
    'Historical analysis of voter recruitment, nomination pathways, and demographic barriers; comparison of demographic composition in fields with explicit diversity mandates vs. laissez-faire selection; identification of veto points in nomination and voting pathways',
    'If structural: diversity reform can meaningfully alter outcomes (Scaffold). If emergent meritocratic outcome: diversity reform will be resisted or reversed as voters converge again on shared standards (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_homogeneity_mechanism, empirical, 'Whether voter homogeneity is structural or emergently meritocratic').

omega_variable(
    extraction_mechanism_locus,
    'Does the award system''s extractive power operate primarily through direct gatekeeping (excluding creators from nomination/voting) or through normalization (making emerging traditions appear inferior until they conform)?',
    'Comparative analysis of creator trajectories: those excluded from nomination vs. those nominated but marginalized; measurement of work quality perception before/after award outcomes; tracking of aesthetic innovation rates in nominated vs. non-nominated traditions',
    'If direct gatekeeping: extraction is structural and measurable (Snare classification strengthens). If normalization: extraction operates through internalized inferiority framing (identity_locked exit applies, classification shifts to Tangled Rope with identity-lock binding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_locus, empirical, 'Whether extraction operates through direct exclusion or normalization').

omega_variable(
    diversity_reform_capture_risk,
    'As award systems expand voter diversity, will emerging traditions be genuinely empowered, or will diversity measures be captured by the established coalition (diversity window-dressing) and new standards be synthesized that still exclude alternatives?',
    'Longitudinal tracking of award outcomes as voter demographics shift; identification of aesthetic standards that persist across diverse voter cohorts vs. standards that change; comparison of innovation rates in emerging traditions before/after diversity expansion',
    'If empowerment: Scaffold is real (constraint has sunset). If capture: Piton is the long-term attractor (theater ratio remains high, actual extraction persists, reform becomes maintenance ritual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diversity_reform_capture_risk, preference, 'Whether diversity reform will be captured or substantive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(award_system_voter_diversity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(award_div_tr_t0, award_system_voter_diversity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(award_div_tr_t10, award_system_voter_diversity, theater_ratio, 10, 0.58).
narrative_ontology:measurement(award_div_tr_t20, award_system_voter_diversity, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(award_div_be_t0, award_system_voter_diversity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(award_div_be_t10, award_system_voter_diversity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(award_div_be_t20, award_system_voter_diversity, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(award_div_su_t0, award_system_voter_diversity, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(award_div_su_t10, award_system_voter_diversity, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(award_div_su_t20, award_system_voter_diversity, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(award_system_voter_diversity, identity_coordination).
narrative_ontology:affects_constraint(award_system_voter_diversity, aesthetic_canon_stability).
narrative_ontology:affects_constraint(award_system_voter_diversity, emerging_tradition_gatekeeping).

% DUAL FORMULATION NOTE:
% The award system voter diversity constraint represents a single underlying structural phenomenon that decomposes into two distinct structural claims with different extractiveness values: (1) the coordination function — establishing shared excellence standards across creative fields (low extractiveness, Rope), and (2) the gatekeeping mechanism — the voter coalition's structural exclusion of emerging traditions (high extractiveness, Snare). These are not two observables of one constraint but two constraints with different ε values linked by causal structure. The present story models the unified constraint (tangled rope) that encompasses both functions. Downstream constraints track how diversity reform affects aesthetic canon stability and emerging tradition gatekeeping separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(award_system_voter_diversity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
