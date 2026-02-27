% ============================================================================
% CONSTRAINT STORY: wikipedia_crowdsourcing_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_crowdsourcing_2026, []).

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
 *   constraint_id: wikipedia_crowdsourcing_2026
 *   human_readable: Wikipedia Crowdsourced Knowledge Governance
 *   domain: technological/social/political
 *
 * SUMMARY:
 *   Wikipedia represents a paradox of radical collaboration that conceals
 *   asymmetric extraction. From 2001 to 2026, Wikipedia evolved from a
 *   revolutionary knowledge commons into a system where volunteer labor
 *   creates value captured by downstream actors — including the Wikimedia
 *   Foundation, AI companies training on Wikipedia text, academic researchers
 *   citing Wikipedia-derived data, and the global public benefiting from free
 *   information. The constraint operates across multiple structural
 *   dimensions: (1) Asymmetric compensation for intellectual labor (experts
 *   contribute without pay while institutions capture value); (2) Global
 *   knowledge bias (English-language dominance and Western editorial control
 *   exclude non-English and non-Western knowledge traditions); (3) Governance
 *   overhead (deletion debates, edit wars, administrative processes) that
 *   consumes volunteer time while maintaining institutional control; (4)
 *   Increasing performativity (edit-a-thons and diversity initiatives that
 *   signal commitment without redistributing power). The theater ratio has
 *   risen from 0.28 in 2001 (functional coordination focused on content) to
 *   0.52 in 2026 (nearly balanced between functional governance and
 *   performative initiatives), indicating Goodhart drift as quality metrics
 *   (edit counts, article reach) replace actual knowledge equity. This
 *   constraint exhibits all six Deferential Realism types depending on
 *   observer position, making it diagnostic of how crowdsourcing conceals
 *   mixed coordination-extraction dynamics.
 *
 * KEY AGENTS:
 *   - Expert Knowledge Contributors (academics, journalists, specialists): Primary victims (powerless/trapped) — provide expert labor for zero compensation; have no exit without losing credibility and reach
 *   - Global Non-English Communities: Primary victims (powerless/trapped) — excluded from knowledge production by English dominance and Western editorial bias; permanently absent from narrative
 *   - Volunteer Regular Editors: Secondary victim/beneficiary (moderate/constrained) — constrained by governance overhead and edit-war dynamics; benefit from peer recognition and knowledge-sharing community
 *   - Wikimedia Foundation and Core Admin Editors: Primary beneficiary (institutional/arbitrage) — controls institutional processes; captures coordination benefits; can exit specific articles
 *   - Information Access Public (Readers): Secondary beneficiary (powerful/arbitrage) — benefits from free, comprehensive information; minimal participation cost
 *   - Knowledge Equity Advocates (Diversity initiatives, Wikiproject organizers): Organized reformers (organized/constrained) — see both coordination function and extraction; constrained by structural barriers
 *   - Downstream Value Extractors (AI vendors, academic researchers, tech companies): Institutional beneficiaries (institutional/arbitrage) — extract Wikipedia content for training and research; contribute no labor
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing voluntary model as inevitable law rather than contingent arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_crowdsourcing_2026, 0.38).
domain_priors:suppression_score(wikipedia_crowdsourcing_2026, 0.48).
domain_priors:theater_ratio(wikipedia_crowdsourcing_2026, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(wikipedia_crowdsourcing_2026, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_crowdsourcing_2026, tangled_rope).
narrative_ontology:human_readable(wikipedia_crowdsourcing_2026, "Wikipedia Crowdsourced Knowledge Governance").
narrative_ontology:topic_domain(wikipedia_crowdsourcing_2026, "technological/social/political").

domain_priors:requires_active_enforcement(wikipedia_crowdsourcing_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_crowdsourcing_2026, information_access_public).
narrative_ontology:constraint_beneficiary(wikipedia_crowdsourcing_2026, volunteer_editors).
narrative_ontology:constraint_beneficiary(wikipedia_crowdsourcing_2026, wikimedia_foundation).
narrative_ontology:constraint_victim(wikipedia_crowdsourcing_2026, excluded_communities).
narrative_ontology:constraint_victim(wikipedia_crowdsourcing_2026, underrepresented_narratives).
narrative_ontology:constraint_victim(wikipedia_crowdsourcing_2026, paid_knowledge_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED KNOWLEDGE WORKER (SNARE) — Academic subject matter experts, journalists, and professional researchers cannot exit Wikipedia's free-labor model without forfeiting credibility and reach. Contributors provide expert labor for zero compensation while Wikipedia captures advertising value and institutional prestige. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDERREPRESENTED NARRATIVES (SNARE) — Communities whose knowledge traditions, histories, and perspectives are absent from Wikipedia (non-English speakers, Global South, indigenous knowledge systems) cannot exit the constraint. English-language dominance and Western editorial bias create permanent structural erasure. These communities bear extraction cost (delegitimization of their knowledge) with no exit path. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULAR VOLUNTEER EDITOR (TANGLED ROPE) — Constrained by time/energy requirements and Wikipedia's complex governance rules (edit wars, deletion debates, admin decisions), but also benefits from peer recognition, knowledge-sharing community, and the coordination function of maintaining a shared reference. Exit is possible but costly (loss of identity as contributor). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WIKIMEDIA FOUNDATION & CORE EDITORS (ROPE) — Benefits from volunteer labor, operational efficiency, and institutional control. Experiences constraint primarily as coordination: managing consensus, maintaining quality standards, preventing vandalism. Can exit individual articles/projects without systemic cost. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFORMATION ACCESS PUBLIC (ROPE) — Readers and researchers benefit from free, collaborative knowledge resource with no participation cost. Experiences constraint as pure coordination: distributed expertise producing better information than proprietary models. Can exit by using other sources, but Wikipedia offers superior cost-benefit. d≈0.12, f(d)≈-0.05, σ=1.2 → χ≈-0.02. Net beneficiary.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: KNOWLEDGE EQUITY ADVOCATES (TANGLED ROPE) — Organized agents (Wikiproject diversity initiatives, edit-a-thon programs, non-English Wikipedia chapters) see both coordination function (crowdsourced knowledge) and asymmetric extraction (English dominance, Western bias). Constrained by volunteer model's limitations and entrenched editing hierarchies. See potential for transformation but face structural barriers. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.31.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ENCYCLOPEDIA INSTITUTIONAL LEGACY (PITON) — Older institutional actors (academic publishing, encyclopedic canon, university library systems) experience Wikipedia as a degraded constraint. Wikipedia's model has atrophied from its original vision of 'free knowledge' into a performative ritual: edit-a-thons, diversity initiatives, and good-faith policies persist despite structural inability to redistribute power. Theater ratio 0.52 reflects significant performative activity (deletion debates, governance theater) relative to functional governance change. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Piton classification arises from theater ≥0.70 threshold not met, but institutional inertia present.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational/universal perspective, crowdsourced knowledge coordination at Wikipedia's scale is presented as an immutable natural law: 'the crowd is wiser than individuals,' 'peer review is the best verification mechanism,' 'volunteer labor is efficient coordination.' However, structural data (ε=0.38, suppression=0.48) contradicts mountain classification. This perspective naturalizes contingent institutional arrangements. Engine false summit detector applies.
constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_crowdsourcing_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_crowdsourcing_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_crowdsourcing_2026, TR),
    TR >= 0.70.

:- end_tests(wikipedia_crowdsourcing_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. Wikipedia captures significant value from volunteer labor — measured through opportunity cost (unpaid expert time), institutional prestige (Wikipedia's global reach), and downstream commercial extraction (AI training data). However, extraction is not maximal (0.70+) because: (a) volunteer contributors choose participation partly for non-monetary rewards (reputation, mission alignment, intellectual engagement), (b) Wikimedia Foundation operates as nonprofit with limited profit extraction relative to commercial platforms, (c) exit is theoretically possible for many contributors (though costly). The 0.38 value reflects that extraction exists but is partially mitigated by ideological alignment and non-profit structure. Suppression (0.48): Moderate. Barriers to challenging the system include: administrative gatekeeping (edit wars, deletion debates), technical complexity of governance processes, accumulated reputational investment for editors, and asymmetric power in consensus-building (established editors have more voice). However, suppression is not extreme (0.60+) because Wikipedia remains formally open, edit history is transparent, and some structural barriers are documented rather than hidden. Theater ratio (0.52): Moderate-high. Goodhart drift is visible in the rise of edit-a-thons and diversity initiatives — these are performative signals of commitment to inclusion that do not redistribute structural power. Deletion debates consume significant volunteer time with unclear functional benefit. Administrative processes maintain the appearance of grassroots governance while concentrating power among core editors.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Expert knowledge workers see a snare (zero compensation, trapped by credibility loss if they refuse). Non-English speakers see a snare (permanent exclusion by network effects and Western editorial dominance). Regular volunteers see tangled rope (constrained by governance complexity, but also benefiting from community). Wikimedia Foundation and core editors see rope (coordination function with minimal extraction cost). Readers see rope (pure benefit with no participation cost). Knowledge equity advocates see tangled rope (both coordination opportunity and structural barriers). The institutional legacy view (encyclopedic canon) sees piton (Wikipedia's volunteer model is a degraded vestige of expert authority). The analytical observer risks seeing mountain (volunteering is a natural law of human cooperation) — but structural data reveals this as false summit: volunteering is contingent on ideological alignment and opportunity cost distribution. The perspectival gap reveals that 'Wikipedia's success' conceals whose success — readers and institutions benefit; contributors and excluded communities bear costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Expert knowledge workers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction because exit is unavailable without reputation loss. Non-English communities: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction because they are structurally excluded from production. Regular volunteer editors: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction but not maximum because exit is theoretically possible though costly. Wikimedia Foundation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with low effective extraction. Readers: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary, maximum benefit from coordination. Knowledge equity advocates: Organized + constrained → d≈0.55, f(d)≈0.75. Mixed because they advocate for victims while benefiting from platform existence. Downstream extractors (AI vendors): Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries extracting value with no contribution. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival naturalization; engine false summit detector applies.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies the tangled rope gate: (1) Requires active enforcement (true) — deletion debates, admin gatekeeping, vandalism suppression, governance rules all represent active enforcement of institutional control. (2) Beneficiaries declared (true) — information access public, Wikimedia Foundation, core editors, downstream extractors all benefit. (3) Victims declared (true) — expert knowledge workers, non-English communities, excluded narratives all bear costs. The constraint prevents misclassification as pure rope (which would understate extraction) or pure snare (which would understate coordination function). Wikipedia genuinely solves a coordination problem (enabling distributed knowledge production), but this coordination mechanism is weaponized to extract unpaid labor from contributors and to suppress alternative knowledge systems. The mandatrophy is resolved by distinguishing the functional coordination gain (real, empirically beneficial) from the asymmetric extraction cost (also real, concentrated on powerless agents). Neither rope nor snare captures the hybrid; tangled rope does. The rising theater ratio (0.28→0.52) indicates gradual degradation toward piton: diversity initiatives and edit-a-thons are increasingly performative, suggesting the original coordination function is atrophying while extraction mechanisms persist through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volunteer_compensation_threshold,
    'At what compensation level would Wikipedia transition from voluntary crowdsourcing to paid labor model, and would that fundamentally change the knowledge governance structure?',
    'Comparative analysis of paid vs volunteer knowledge platforms; economic modeling of compensation costs; historical transition studies of similar systems',
    'If low threshold (< $5/hour global average): Wikipedia remains voluntary by choice, not necessity — extraction is lower. If high threshold: compensation becomes prohibitive, confirming volunteer model as extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_compensation_threshold, empirical, 'Compensation level at which Wikipedia transitions to paid labor').

omega_variable(
    english_dominance_irreversibility,
    'Is English-language dominance on Wikipedia a reversible structural bias or an immutable property of network effects and global English literacy distribution?',
    'Analysis of non-English Wikipedia growth rates, editing patterns in multilingual contexts, investment in localization infrastructure; comparison with other multilingual platforms',
    'If reversible: bias is extraction mechanism (Snare for non-English speakers). If immutable: network effect is a natural law (Mountain-like constraint on knowledge pluralism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(english_dominance_irreversibility, conceptual, 'Whether English dominance on Wikipedia is reversible').

omega_variable(
    edit_war_suppression_intensity,
    'How much of Wikipedia''s governance overhead (mediation, deletion debates, admin actions) is genuine quality control versus performative conflict theater that masks unresolved power asymmetries?',
    'Comparison of deletion debate outcomes with article quality metrics; analysis of revert frequency and editor attrition patterns; measurement of time editors spend on governance vs content',
    'If high theater: suppression is active enforcement, not consensus failure. This raises tangled_rope classification confidence. If low theater: governance is functional, raising rope classification for core editors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(edit_war_suppression_intensity, empirical, 'Whether Wikipedia governance is quality control or performative theater').

omega_variable(
    platform_profit_extraction_channels,
    'How much value does the Wikimedia Foundation and downstream actors (tech companies, AI vendors, academic researchers) extract from Wikipedia content, and is this extracted value reflected in compensation to contributors?',
    'Auditing of Wikimedia Foundation finances and linked corporate relationships; analysis of AI training data sourcing; licensing compliance tracking; value-of-service estimates',
    'If high extraction channels: Wikipedia is Snare for contributors. If low channels: extraction is limited, raising Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_profit_extraction_channels, empirical, 'Whether downstream actors extract value from Wikipedia contributions').

omega_variable(
    vandalism_suppression_cost_allocation,
    'Who bears the cost of vandalism suppression and quality maintenance — is this cost distributed fairly across beneficiaries, or concentrated on volunteer editors?',
    'Time-tracking analysis of moderation labor; comparison of moderation burden across language editions; measurement of volunteer burnout and admin decision review rates',
    'If concentrated on volunteers: cost allocation is extractive (supports Snare/Tangled Rope). If distributed: governance overhead is shared coordination cost (supports Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vandalism_suppression_cost_allocation, empirical, 'Distribution of vandalism suppression costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_crowdsourcing_2026, 2001, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wiki_tr_t0, wikipedia_crowdsourcing_2026, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wiki_tr_t12, wikipedia_crowdsourcing_2026, theater_ratio, 12, 0.42).
narrative_ontology:measurement(wiki_tr_t25, wikipedia_crowdsourcing_2026, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(wiki_be_t0, wikipedia_crowdsourcing_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(wiki_be_t12, wikipedia_crowdsourcing_2026, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(wiki_be_t25, wikipedia_crowdsourcing_2026, base_extractiveness, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_crowdsourcing_2026, information_standard).
narrative_ontology:affects_constraint(wikipedia_crowdsourcing_2026, open_knowledge_commons_equity).
narrative_ontology:affects_constraint(wikipedia_crowdsourcing_2026, volunteer_labor_extraction).
narrative_ontology:affects_constraint(wikipedia_crowdsourcing_2026, global_english_dominance).

% DUAL FORMULATION NOTE:
% Wikipedia crowdsourcing decomposes into three structurally distinct constraints: (1) open_knowledge_commons_equity (ε≈0.25, Rope/Mountain view) — whether crowdsourced knowledge is genuinely equitable; (2) volunteer_labor_extraction (ε≈0.55, Snare view) — whether unpaid contribution is extractive; (3) global_english_dominance (ε≈0.60, Snare view) — whether English dominance is irreversible suppression. The present story (ε=0.38) represents the hybrid system that balances coordination and extraction. Upstream constraints (open_knowledge_commons_equity) establish that crowdsourcing can work; downstream constraints (volunteer_labor_extraction, global_english_dominance) reveal specific extraction mechanisms. All three are linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_crowdsourcing_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
