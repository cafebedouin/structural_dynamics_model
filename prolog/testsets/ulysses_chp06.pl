% ============================================================================
% CONSTRAINT STORY: ulysses_chp06
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_hades_1904, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp06
 *   human_readable: The Funerary Mountain (Prospect Cemetery)
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom attends the funeral of Paddy Dignam at Prospect Cemetery in
 *   Dublin (June 10, 1904). The scene confronts two structurally distinct
 *   constraints: the absolute finality of death (a Mountain from the
 *   civilizational/analytical perspective) and the social technology of
 *   mourning ritual (a Tangled Rope coordinating grief, performing community
 *   cohesion, and extracting emotional and material resources from the
 *   bereaved). The constraint exhibits maximal perspectival divergence —
 *   death appears unchangeable to the analytical observer, yet the mourning
 *   ritual appears contingent and extractive to the powerless bereaved
 *   individual. Bloom's navigation of the funeral reveals the tension between
 *   authentic grief (which he suppresses throughout) and mandatory ritual
 *   performance (which he dutifully executes). The cemetery embodies both
 *   phenomena: it is physically a repository of the irreversible dead, and
 *   institutionally a mechanism for extracting fees and enforcing
 *   participation. The constraint's theater ratio (0.64) reflects that
 *   funeral ritual in 1904 Dublin is substantially performative — prescribed
 *   emotional expressions, standardized condolences, formal body positioning
 *   — rather than expressions of authentic grief. The rising theater_ratio
 *   over the interval (0.45 → 0.64) indicates that burial ritual is becoming
 *   increasingly formalized and less functionally necessary as urbanization
 *   provides alternatives (cremation, secular memorials, telegraph
 *   announcements replacing oral transmission).
 *
 * KEY AGENTS:
 *   - Paddy Dignam (deceased): No agency within the constraint; complete absence — the constraint's ultimate referent
 *   - Bereaved Family (Dignam's dependents): Powerless/trapped — must navigate funeral ritual, incur costs, perform grief; cannot refuse participation
 *   - Leopold Bloom: Moderate/constrained — experiences ritual as both meaningful (social coordination) and extractive (emotional labor, awkward participation); has constrained exit options (refusal damages reputation)
 *   - Funeral Attendees (community members): Moderate/constrained — similar structure to Bloom; collective obligation to attend
 *   - Religious Institution (Catholic Church): Institutional/arbitrage — benefits from funeral Mass fees, clerical income, ritual authority; has full exit option (can refuse service, modify ritual)
 *   - Cemetery Authority (Prospect Cemetery): Institutional/arbitrage — benefits from burial fees, plot maintenance charges; arbitrage options include fee modification, alternative grave types
 *   - Irish Social Norms: Organized/constrained — enforces funeral attendance expectations; suppresses alternative commemoration forms; has agency to modify norms but is constrained by cultural inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp06, 0.12).
domain_priors:suppression_score(ulysses_chp06, 0.38).
domain_priors:theater_ratio(ulysses_chp06, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp06, extractiveness, 0.12).
narrative_ontology:constraint_metric(ulysses_chp06, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ulysses_chp06, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp06, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(ulysses_chp06, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp06, tangled_rope).
narrative_ontology:human_readable(ulysses_chp06, "The Funerary Mountain (Prospect Cemetery)").
narrative_ontology:topic_domain(ulysses_chp06, "social/religious/technological").

domain_priors:requires_active_enforcement(ulysses_chp06).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp06, religious_institution).
narrative_ontology:constraint_beneficiary(ulysses_chp06, community_social_cohesion).
narrative_ontology:constraint_victim(ulysses_chp06, individual_grief_authenticity).
narrative_ontology:constraint_victim(ulysses_chp06, bereaved_family_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FINALITY OF DEATH (MOUNTAIN) — From the civilizational/universal analytical view, death is an irreducible constraint: no human agency, technology, or social arrangement can reverse it. The cemetery embodies this natural law. Base extraction ≤ 0.25, suppression ≤ 0.05 in the natural law reading — death is not imposed; it is inherent to biological existence. The constraint's immutability emerges from physics and temporality, not from social enforcement.
constraint_indexing:constraint_classification(ulysses_chp06, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE BEREAVED INDIVIDUAL (SNARE) — The family member cannot exit the mourning ritual, cannot refuse participation without social stigma, and bears the full cost of grief labor while the institution (church, cemetery, community) extracts social compliance and emotional performance. Maximum suppression: absence from the funeral damages reputation and social standing irreversibly. Theater ratio (0.64) reflects that much of the ritual is performative — the bereaved are expected to perform grief in prescribed forms rather than authentic expression. High experienced extraction through the powerless agent's structural position.
constraint_indexing:constraint_classification(ulysses_chp06, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE RELIGIOUS INSTITUTION (ROPE) — The church/cemetery system experiences the constraint as pure coordination: orchestrating collective mourning, managing social cohesion, providing ritual structure. Base extractiveness ≤ 0.45, effective extraction χ ≤ 0.35 from the institution's perspective because it is a net beneficiary with arbitrage options. The institution can adapt rituals, modify fees, or leverage alternative revenue streams. The constraint solves a genuine coordination problem — how to process death collectively without social fragmentation.
constraint_indexing:constraint_classification(ulysses_chp06, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE GRIEVING COMMUNITY MEMBER (TANGLED ROPE) — A community member (e.g., Bloom) experiences the constraint as both coordination and extraction. The funeral ritual provides genuine social support and meaning-making (coordination function), but also requires emotional performance, financial expenditure (burial fees), and time cost. Exit is constrained — refusing to attend damages reputation, but attending extracts emotional and material resources. Neither pure coordination nor pure extraction, but a hybrid where the coordination benefit and extraction cost are asymmetrically distributed.
constraint_indexing:constraint_classification(ulysses_chp06, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE MODERNIZING CEMETERY (SCAFFOLD) — By 1904, Irish cemetery practices are transitioning: cremation alternatives emerging, secular memorial practices developing, fee structures being rationalized. From the perspective of cemetery reform movements (organized agents with generational timescales), the constraint is temporary coordination scaffolding with a sunset. As secular alternatives mature and transportation improves (enabling distant cemetery use), the extraction mechanism of requiring local burial weakens. Theater ratio suppression is declining as ritual becomes optional rather than mandatory. The scaffold decays as alternatives provide exit routes.
constraint_indexing:constraint_classification(ulysses_chp06, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE BURIAL FEE STRUCTURE (PITON) — The mechanism by which cemeteries charge for burial (parcel allocation, maintenance fees, priest honorariums) is largely performative in 1904 Dublin: the fees are justified by 'maintenance' and 'administration,' but much of the extracted value flows to institutional ritual rather than functional cemetery upkeep. Theater ratio (0.64) reflects that the fee structure is maintained through institutional inertia — it persists because alternatives haven't fully replaced it and because challenging the fees is socially transgressive. The original Rope coordination function (organizing burial space) has atrophied into ritualized extraction.
constraint_indexing:constraint_classification(ulysses_chp06, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp06_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp06, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp06, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp06, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp06_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.12): Low to moderate. The constraint is not primarily extractive in raw structural terms — the funeral ritual does provide genuine coordination function (processing collective grief, maintaining social bonds, publicly acknowledging death). However, the extraction component emerges from two sources: (1) the bereaved bear mandatory emotional labor and financial costs (burial fees, required attendance), while the institutional beneficiary (church, cemetery) captures value with lower cost; (2) the ritual form suppresses authentic grief expression in favor of prescribed performance. The extractiveness value reflects that the asymmetry is real but not severe — most attendees perceive genuine value in the ritual alongside the cost. Suppression (0.38): Moderate. Social stigma for refusing funeral attendance is severe in 1904 Dublin (damages reputation, marks family as impious or uncaring), but not totalizing — individuals can negotiate partial participation, private alternatives, or relocation to avoid funeral obligation. The constraint is socially enforced, not legally mandated. Theater ratio (0.64): High. Funeral ritual consists substantially of prescribed performances: formal weeping, standardized condolences ('my sincere condolences'), body positioning, order of processional, prescribed responses to the priest. Bloom's internal monologue reveals the gap between these performances and his actual emotional state — he attends because of social obligation, not because the ritual serves his authentic grief. The theater_ratio has risen from 0.45 to 0.64 over the 25-year interval, indicating increasing formalization and declining functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. From the analytical/civilizational perspective, death is an unchangeable natural law — no perspective avoids it, no agent can negotiate with finality. This reads as Mountain: ε ≤ 0.25, suppression ≤ 0.05 (not socially imposed, inherent to existence). But from the powerless bereaved individual's perspective, death is experienced through the enacted constraint of funeral ritual — mandatory attendance, prescribed grief performance, financial extraction — which reads as Snare: high experienced extraction due to trapped exit and institutional enforcement. The beneficiary (church/cemetery) perceives Rope: genuine coordination (processing collective death, maintaining social bonds) with low experienced extraction (arbitrage options, net benefit). Bloom perceives Tangled Rope: the ritual provides meaningful community affiliation but also demands emotional performance against authentic feeling. The community perceives Scaffold: as secular alternatives (cremation, memorial associations) emerge, the ritual's mandatory character weakens and sunset logic activates. The cemetery fee structure perceives Piton: the extraction mechanism (burial fees) is maintained through institutional inertia even as functional necessity declines. The perspectival gap reveals that death-as-mountain and death-as-ritual-extraction are two different constraints conflated under a single label. Decomposition is warranted: one constraint for biological finality (mountain_death_finality), one for institutional mourning extraction (tangled_rope_funeral_ritual). However, the unified analysis preserves the phenomenological truth that in 1904 Dublin, the two cannot be separated — the finality of death cannot be confronted except through the institutional ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from structural relationship to THIS constraint: who benefits, who bears costs, what exit options exist. Bereaved individual: victim (bears costs) + trapped exit (refusal = permanent reputation damage) → d ≈ 0.95, high f(d), high experienced extraction. Religious institution: beneficiary (clerical income, ritual authority) + arbitrage exit (can refuse service, modify ritual, adjust fees) → d ≈ 0.05, negative f(d), negative experienced extraction (net benefit). Bloom: mixed (some benefits from social coordination, some costs from emotional labor) + constrained exit (refusal is possible but damaging) → d ≈ 0.60, moderate f(d), moderate experienced extraction. The directionality pipeline does not require explicit overrides — the structural data (beneficiary/victim declarations, exit options) determines d automatically. The apparent mountain classification from the analytical perspective is not an override but a genuine reading of death-as-finality when exit_options='analytical' (observe, do not participate).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE DIAGNOSIS: The constraint requires verification that it is NOT pure coordination (Rope) and NOT pure extraction (Snare), but genuinely hybrid. The tangled_rope gates require: (1) beneficiaries declared — yes, religious_institution and community_social_cohesion benefit from ritual continuity; (2) victims declared — yes, individual_grief_authenticity and bereaved_family_economy bear costs; (3) requires_active_enforcement — yes, social stigma and clerical authority enforce attendance and proper performance. The hybrid structure is confirmed: the funeral ritual provides real coordination function (collective grieving, social cohesion, meaning-making), but its coordination value is asymmetrically distributed — the institution and community benefit more than the bereaved individual, who bears extraction costs (fees, emotional labor, performance demands) in addition to grief processing. The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid: it SOLVES a coordination problem (how to process collective death and maintain social bonds) AND EXTRACTS from the bereaved (mandatory performance, financial cost, emotional suppression). Neither classification alone captures the structure. MOUNTAIN FALSE SUMMIT: The analytical perspective reads death-as-finality as Mountain, but this is a false summit because the classification naturalizes what is actually a contingent institutional arrangement. Death itself is immutable (true mountain), but the funeral ritual is socially constructed and modifiable. The perspectival divergence (powerless sees snare, institution sees rope, analytical sees mountain) reveals that the label 'death' conflates two constraints: (a) biological finality (inherent, unchangeable, structural mountain), and (b) mourning ritual (contingent, institutionally enforced, tangled rope). The unification is phenomenologically accurate for 1904 Dublin — the two cannot be separated in experience — but analytically distinct. A decomposed corpus would include two stories linked by network.affects_constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    death_as_mountain_vs_social_construction,
    'Is death an irreducible natural law (Mountain) or is the mourning constraint a social technology that conflates two separate phenomena: biological finality and ritual performance?',
    'Comparison of mourning practices across cultures and time periods; identification of which elements are universal (death finality) vs. contingent (ritual form, fee structure, participation enforcement). Analysis of whether funeral practices change in response to social pressures or economic changes, indicating contingency rather than natural law.',
    'If death finality and ritual performance are conflated: the constraint decomposes into two stories (mountain_death_finality + tangled_rope_mourning_ritual). If mourning is entirely natural-law-driven: single mountain classification for all perspectives. If mourning is contingent: tangled_rope classification remains primary, with the mountain as a false summit naturalizing institutional arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(death_as_mountain_vs_social_construction, conceptual, 'Whether death finality and mourning ritual constitute one constraint or two').

omega_variable(
    grief_authenticity_vs_performance,
    'Can bereaved individuals distinguish their authentic grief from the performative grief demanded by ritual, or does the ritual structure colonize emotional expression entirely?',
    'Phenomenological analysis of first-person accounts (journals, letters, oral histories) from bereaved individuals; comparison of private grief expressions vs. public funeral behavior; identification of whether ritual adherence correlates with reported grief resolution or emotional damage.',
    'If ritual fully colonizes grief: suppression rises toward 1.0, snare classification strengthens. If individuals can separate authentic grief from performance: suppression ≤ 0.50, tangled_rope classification remains. If ritual enables grief articulation: rope classification becomes viable from bereaved perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grief_authenticity_vs_performance, empirical, 'Whether funeral ritual enables or suppresses authentic grief expression').

omega_variable(
    cemetery_economic_extraction,
    'What proportion of burial fees actually fund cemetery maintenance vs. flow to institutional administrative overhead and clerical income?',
    'Historical ledger analysis of Prospect Cemetery (Dublin) and comparable institutions; accounting for labor costs, land maintenance, clerical salaries; comparison of fees charged vs. documented expenditures during 1890-1910 period.',
    'If maintenance costs are high (>70% of fees): fee structure is functionally justified, extraction component is low, tangled_rope classification weakened. If overhead/clerical income is substantial (>30% of fees): extraction component is high, snare classification strengthened from bereaved perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cemetery_economic_extraction, empirical, 'Proportion of burial fees that fund actual cemetery maintenance').

omega_variable(
    ritual_necessity_for_social_cohesion,
    'Is the funeral ritual actually necessary for processing collective grief and maintaining social cohesion, or would alternative commemoration structures (secular memorials, private grieving, family-centered rituals) achieve equivalent outcomes?',
    'Comparative analysis of social outcomes in communities with enforced vs. optional funeral rituals; measurement of reported social cohesion, grief resolution, community trust; historical analysis of communities that rejected institutional funeral structures.',
    'If ritual is necessary: coordination function is genuine, rope and tangled_rope classifications justified. If alternatives are equally effective: the ritual is primarily extractive theater, snare classification strengthened, piton (degraded ritual) classification becomes accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_necessity_for_social_cohesion, preference, 'Whether funeral ritual is necessary for social cohesion or ceremonial theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp06, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp06, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp06, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ulys_tr_t25, ulysses_chp06, theater_ratio, 25, 0.64).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp06, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp06, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(ulys_be_t25, ulysses_chp06, base_extractiveness, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp06, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The Funerary Mountain unifies two structurally distinct constraints: (1) the biological/existential finality of death (mountain_death_finality, ε ≈ 0.05), and (2) the institutional mourning ritual and extraction mechanism (tangled_rope_funeral_ritual, ε ≈ 0.12). In a decomposed corpus, these would be separate stories linked by affects_constraints, with the mountain as upstream (empirically universal, high confidence) and the tangled rope as downstream (socially contingent, culturally variable). The unified presentation reflects Joyce's artistic integration of biological fact and social technology in Chapter 6 (Hades).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
