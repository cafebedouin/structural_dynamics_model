% ============================================================================
% CONSTRAINT STORY: linguistic_assimilation_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_linguistic_assimilation_policy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: linguistic_assimilation_policy
 *   human_readable: Linguistic Assimilation Policy
 *   domain: social/cultural_policy
 *
 * SUMMARY:
 *   Linguistic assimilation policy represents the state's formalized
 *   requirement that minority language speakers transition to the dominant
 *   national language, typically through education, administration, and labor
 *   market incentives. The constraint exhibits a genuine coordination
 *   function (reduced administrative complexity, enhanced civic participation
 *   capacity) alongside asymmetric extraction (minority speakers bear the
 *   burden of acquisition and cultural displacement while dominant speakers
 *   pay minimal friction costs). The constraint's temporal trajectory shows
 *   increasing extractiveness as diverse populations grow and assimilation
 *   pressure intensifies, yet simultaneously increasing theater ratio as the
 *   functional necessity declines and the policy becomes more performative
 *   (symbolic patriotism, identity management). The constraint operates
 *   through dual suppression mechanisms: structural (economic opportunity
 *   concentrated in dominant language, legal barriers to minority language
 *   education) and internalized (identity lock — the parent's fear of
 *   cultural discontinuity, the child's shame about heritage language,
 *   internalized inevitability of assimilation). The policy exhibits all six
 *   DR types from different structural positions, making it diagnostic for
 *   understanding how institutional arrangements can appear natural to
 *   beneficiaries while appearing extractive to targets.
 *
 * KEY AGENTS:
 *   - Minority Language Speakers: Primary victims (powerless/identity_locked) — bear burden of acquisition, face identity displacement, experience irreversible intergenerational transmission window. Structurally mobile (could relocate, could refuse schooling) but identity-locked to linguistic heritage.
 *   - Dominant Language Speakers: Primary beneficiaries (institutional/arbitrage) — pay minimal friction costs, benefit from coordination efficiency, experience the policy as natural and just.
 *   - State Administration: Coordinator (institutional/arbitrage) — solves genuine collective action problem of governance efficiency through unified language. Net beneficiary but serves legitimate state function.
 *   - Bilingual Community Organizations: Secondary organized actors (moderate/constrained) — face resource barriers and social sanction but can negotiate carve-outs and heritage programs once assimilation framework is formalized.
 *   - Education System: Institutional enforcer (institutional/arbitrage) — implements policy through curriculum, teacher training, testing standards. Maintains performative component (language standardization rituals) alongside functional component (skill transmission).
 *   - Official Language Institutions: Institutional maintainer (institutional/arbitrage) — preserves formal apparatus (language academies, standard dictionaries, official designation) long after functional necessity declines. Theater-dominant perspective.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy as immutable law of social organization despite comparative evidence of successful multilingualism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(linguistic_assimilation_policy, 0.58).
domain_priors:suppression_score(linguistic_assimilation_policy, 0.65).
domain_priors:theater_ratio(linguistic_assimilation_policy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(linguistic_assimilation_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(linguistic_assimilation_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(linguistic_assimilation_policy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(linguistic_assimilation_policy, tangled_rope).
narrative_ontology:human_readable(linguistic_assimilation_policy, "Linguistic Assimilation Policy").
narrative_ontology:topic_domain(linguistic_assimilation_policy, "social/cultural_policy").

domain_priors:requires_active_enforcement(linguistic_assimilation_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(linguistic_assimilation_policy, dominant_language_speakers).
narrative_ontology:constraint_beneficiary(linguistic_assimilation_policy, national_cohesion_institutions).
narrative_ontology:constraint_beneficiary(linguistic_assimilation_policy, state_administration).
narrative_ontology:constraint_victim(linguistic_assimilation_policy, minority_language_speakers).
narrative_ontology:constraint_victim(linguistic_assimilation_policy, linguistic_heritage_preservation).
narrative_ontology:constraint_victim(linguistic_assimilation_policy, cultural_identity_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY LANGUAGE SPEAKER (SNARE) — Structurally mobile (could relocate, could refuse assimilation) but identity-locked to ancestral language and cultural continuity. The biological clock of childhood language acquisition creates an irreversible window: if the child is assimilated, the parent's linguistic identity cannot be transmitted. Suppression is dual: external (school policies, labor market incentives favoring dominant language) and internalized (internalized shame about heritage language, belief that assimilation is inevitable or necessary for their child's success). Maximum extraction because the agent cannot exercise mobility without abandoning identity.
constraint_indexing:constraint_classification(linguistic_assimilation_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: BILINGUAL COMMUNITY ORGANIZATION (TANGLED ROPE) — Faces high costs to exit (defunding, social sanction, legal restrictions on mother-tongue instruction). But also benefits from the policy's recognition framework: once assimilation policy is formalized, advocacy organizations can negotiate for minority-language schools, cultural centers, and heritage programs as carve-outs or exceptions. The constraint provides a coordination mechanism (formalized recognition of language categories) alongside extractive coercion (pressure to assimilate). Moderate extractiveness because organized actors can partially capture benefits while bearing costs.
constraint_indexing:constraint_classification(linguistic_assimilation_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATION (ROPE) — Primary beneficiary. Solves genuine coordination problem: citizens who share a common language reduce transaction costs for law, commerce, administration, and civic participation. The state experiences the constraint as pure coordination: unified language infrastructure simplifies governance. Net beneficiary — extraction flows toward this actor through reduced administrative overhead and enhanced state capacity. The arbitrage option reflects the state's structural mobility: if linguistic unification becomes costly (diaspora flows, cultural backlash), the state can theoretically pivot to multilingual policy without existential threat.
constraint_indexing:constraint_classification(linguistic_assimilation_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATION REFORM COALITION (SCAFFOLD) — Organized actors (educators, parents, cultural organizations) see assimilation policy as a temporary institutional arrangement with a sunset clause: bilingual education, heritage language programs, and de-facto multilingualism in diverse urban areas are creating an alternative pathway where the policy's coercive function declines over time. The constraint is experienced as a transitional coordination failure — the policy was necessary for nation-building in the 19th/20th century but is becoming obsolete as diversity-aware policies mature. Sunset clause derives from demographic shifts and educational innovation making monolingual assimilation less functional.
constraint_indexing:constraint_classification(linguistic_assimilation_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OFFICIAL LANGUAGE INSTITUTION (PITON) — Maintains the formal apparatus of linguistic assimilation (official language designation, school curricula, public signage standards) long after the functional necessity has declined. Modern nation-states achieve governance coordination through administrative systems, digital infrastructure, and economic integration — linguistic unity is no longer structurally required. The institution persists through inertia: language academies, curriculum boards, and official designation ceremonies continue the ritual despite reduced functional purpose. Theater ratio (0.55) reflects the mixed character: some real coordination function remains (administrative efficiency gains are real), but the performative component has grown (symbolic patriotism, identity performance via language standardization).
constraint_indexing:constraint_classification(linguistic_assimilation_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some linguistic convergence is inherent to human social organization: languages naturally diverge and converge through contact, and pressures toward linguistic unity are immutable features of how societies function at scale. This perspective sees assimilation as an unchangeable law of social physics. However, this mountain classification risks false naturalization — the structural data reveals that assimilation policy is a contingent institutional arrangement, not an immutable law. Comparative evidence (multilingual nation-states, successful maintenance of minority languages, code-switching communities) contradicts the mountain reading.
constraint_indexing:constraint_classification(linguistic_assimilation_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(linguistic_assimilation_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(linguistic_assimilation_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(linguistic_assimilation_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(linguistic_assimilation_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(linguistic_assimilation_policy, TR),
    TR >= 0.70.

:- end_tests(linguistic_assimilation_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy extracts from minority speakers through acquisition burden, labor market discrimination, and identity displacement. However, the extraction is not total — some coordination function is genuine (administrative efficiency is real, though achievable through multilingual systems). The value increased from 0.35 to 0.58 over the interval because diverse immigration increased the functional pressure on assimilation and intensified the friction costs borne by minorities. Suppression (0.65): High. Dual suppression mechanisms create strong binding: external (school policies mandate dominant language, labor market rewards dominant language competence, legal barriers restrict minority language services) and internal (identity lock, internalized shame, fear of cultural discontinuity). Suppression is not total because some minority language communities successfully resist through private schooling, diaspora networks, and cultural institutions. Theater ratio (0.55): Moderate. The policy has genuine functional component (administrative transactions are more efficient in unified language) but increasingly performative component (symbolic patriotism, identity performance, language standardization ceremonies). Theater increased from 0.40 to 0.55 as the functional necessity declined (digital translation tools, multilingual administration proving viable) while symbolic utility increased (nationalism intensified, identity politics elevated language status).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. State administration sees rope (pure coordination) — they are solving a genuine collective action problem of administrative efficiency. Dominant language speakers see rope — the constraint is transparent to them, experienced as natural and just. Bilingual organizations see tangled rope — they bear costs (restricted funding, social sanction) but can exploit the framework (negotiate heritage programs, formalize minority recognition). Minority speakers see snare — they cannot exit without abandoning identity, and the identity lock makes escape unthinkable despite structural mobility. The educational institution sees piton — the apparatus (curricula, testing, teacher training) persists through inertia and symbolic utility despite reduced functional necessity. The analytical observer risks seeing mountain — naturalizing assimilation as immutable law of social organization. The perspectival gap reveals how the same policy appears as a solution to some actors and as coercion to others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim positions determine directionality values. State administration (beneficiary with arbitrage exit) derives low d — the policy extracts resources toward this actor through reduced administrative overhead. Minority speakers (victim with identity_locked exit) derive high d — they bear extraction without escape option. Bilingual organizations (beneficiary + victim with constrained exit) derive moderate d — they face barriers to exit but can partially exploit the policy framework. The dominant language speaker experiences low d despite being a beneficiary because they face minimal friction cost — the extraction mechanisms target minorities, so dominant speakers experience the constraint as pure coordination (rope). Suppression is unscaled (raw structural property): the 0.65 suppression applies uniformly; what differs across perspectives is how this suppression interacts with power and exit options to produce experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   Assimilation policy resolves the mandatrophy by showing that the same constraint simultaneously coordinates and extracts. The rope classification (state administration perspective) is genuine — unified language infrastructure does reduce administrative complexity. The snare classification (minority speaker perspective) is also genuine — the identity lock creates irreversible transmission window and makes exit unthinkable despite mobility. The constraint is both rope and snare, not because of measurement ambiguity but because the policy serves genuinely different functions for different agents: for the state, it solves coordination; for minorities, it enforces extraction. The tangled rope classification at the moderate/constrained level captures this hybrid: the constraint both coordinates (provides framework for minority organizations) and extracts (imposes friction costs and cultural pressure). The mandatrophy is resolved not by choosing one type but by recognizing that the presheaf of perspectives reveals the full extractive structure: a policy that appears coordinative from a beneficiary position is revealed as extractive from a target position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_mobility,
    'Is the minority language speaker''s inability to exit assimilation driven by external suppression (lack of economic opportunity, legal barriers, housing discrimination) or by internal identity fusion (internalized belief that assimilation is inevitable, shame about heritage language, fear of losing connection to ancestors)?',
    'Post-exit trajectory analysis: if speakers retain linguistic identity and community after barrier removal (e.g., economic opportunity, legal protection), suppression was structural. If speakers continue assimilation despite barrier removal (economic success, legal protection, geographic opportunity), suppression is internalized. Comparison with voluntarily assimilating speakers vs pressured speakers.',
    'If structural: constraint is primarily Snare (external coercion). If internalized: constraint is primarily Snare (internalized coercion acting through identity lock). If mixed: suppression persists after barriers removed, indicating both mechanisms at work and potentially higher effective suppression than structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_mobility, empirical, 'Structural vs internalized suppression in linguistic assimilation').

omega_variable(
    functional_necessity_of_linguistic_unity,
    'Does modern state administration actually require linguistic unity, or do multilingual governance systems (Switzerland, Singapore, Canada) demonstrate that coordination can be achieved with multiple official/recognized languages?',
    'Comparative analysis of administrative efficiency, governance capacity, and service delivery quality across monolingual and multilingual nation-states. Cost accounting for translation infrastructure, legal pluralism, and institutional complexity.',
    'If linguistic unity is functionally necessary: rope classification is correct — assimilation solves a genuine coordination problem. If multilingualism is functionally equivalent or superior: rope mischaracterizes — assimilation is primarily extractive (snare/tangled_rope), not coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_necessity_of_linguistic_unity, empirical, 'Whether linguistic unity is functionally necessary for state administration').

omega_variable(
    intergenerational_transmission_irreversibility,
    'Is the loss of heritage language in assimilated children irreversible at the biological/psychological level, or can heritage language acquisition occur later in life or across generations?',
    'Neurolinguistic research on critical periods for language acquisition; case studies of heritage language revival (Hebrew, Welsh, Māori); documentation of late-life language learning capacity in assimilated speakers.',
    'If irreversible: identity lock is stronger — the parent''s generational transmission window creates temporal urgency that cannot be recovered. If reversible: mobility option is stronger — assimilated speakers retain capacity to re-acquire or revive heritage language, reducing the sense of finality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_irreversibility, empirical, 'Whether intergenerational language transmission is biologically irreversible').

omega_variable(
    dominant_language_beneficiary_circularity,
    'Do dominant language speakers benefit from assimilation policy because it provides genuine coordination advantages, or because it naturalizes their cultural dominance and exempts them from the friction costs imposed on minorities (code-switching, self-monitoring, educational burden)?',
    'Cost accounting: measure time/cognitive/economic burden imposed on minority speakers (code-switching costs, remedial language education, employment discrimination). Compare to burden on dominant speakers in multilingual environments. Test whether dominant speakers would voluntarily incur equivalent burden if roles reversed.',
    'If genuine coordination benefit: beneficiary status is justified — dominant speakers solve a real collective action problem. If primarily distributional dominance: extraction is disguised as coordination — the constraint is snare/tangled_rope, not rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_language_beneficiary_circularity, empirical, 'Whether dominant language beneficiaries gain from coordination or dominance asymmetry').

omega_variable(
    cyclical_assimilation_vs_steady_state,
    'Does assimilation policy operate as a steady-state institutional arrangement, or as a cyclical dynamic where periods of enforcement alternate with periods of backlash and minority language revival?',
    'Historical documentation of policy enforcement intensity, educational language-of-instruction statistics, minority language media presence, and legal status across decade intervals. Identification of boom-bust cycles or monotonic assimilation trajectory.',
    'If steady-state: measurements show monotonic increase in assimilation rate, supporting snare classification. If cyclical: measurements show oscillation (enforcement → resistance → cultural revival → renewed enforcement), suggesting intermittent reinforcement mechanism and higher effective extraction than steady-state measures indicate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cyclical_assimilation_vs_steady_state, empirical, 'Cyclical vs steady-state dynamics in linguistic assimilation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(linguistic_assimilation_policy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ling_assim_tr_t0, linguistic_assimilation_policy, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ling_assim_tr_t25, linguistic_assimilation_policy, theater_ratio, 25, 0.48).
narrative_ontology:measurement(ling_assim_tr_t50, linguistic_assimilation_policy, theater_ratio, 50, 0.55).
narrative_ontology:measurement(ling_assim_tr_t75, linguistic_assimilation_policy, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(ling_assim_be_t0, linguistic_assimilation_policy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ling_assim_be_t25, linguistic_assimilation_policy, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(ling_assim_be_t50, linguistic_assimilation_policy, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(ling_assim_be_t75, linguistic_assimilation_policy, base_extractiveness, 75, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(linguistic_assimilation_policy, identity_coordination).
narrative_ontology:boltzmann_floor_override(linguistic_assimilation_policy, 0.12).
narrative_ontology:affects_constraint(linguistic_assimilation_policy, educational_access_disparity).
narrative_ontology:affects_constraint(linguistic_assimilation_policy, labor_market_discrimination_via_language).
narrative_ontology:affects_constraint(linguistic_assimilation_policy, cultural_heritage_degradation).

% DUAL FORMULATION NOTE:
% Linguistic assimilation policy comprises multiple structurally distinct constraints with different extractiveness values. The coordination mechanism (unified language infrastructure) can be modeled separately from the identity enforcement mechanism (pressure to abandon heritage language). This decomposition reveals that the genuine coordination benefit (ε ≈ 0.15, rope) is distinct from the extractive identity mechanism (ε ≈ 0.72, snare). The tangled rope classification at ε = 0.58 represents the hybrid constraint where both mechanisms operate together. Downstream constraints (labor market discrimination, educational access disparity) have higher extractiveness because they inherit the suppression from this upstream policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(linguistic_assimilation_policy, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
