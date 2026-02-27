% ============================================================================
% CONSTRAINT STORY: wikipedia_noncommercial_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_noncommercial_model, []).

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
 *   constraint_id: wikipedia_noncommercial_model
 *   human_readable: Wikipedia's Non-Commercial, Volunteer-Driven Model
 *   domain: technological/information/governance
 *
 * SUMMARY:
 *   Wikipedia's non-commercial, volunteer-driven model represents a critical
 *   point in the history of knowledge distribution infrastructure. Launched
 *   in 2001, the platform succeeded in creating the largest free encyclopedia
 *   ever assembled through a global community of unpaid editors, sustained by
 *   a non-profit organizational structure funded through small-dollar
 *   donations. This constraint embeds a fundamental tension: the model
 *   depends on the voluntary extraction of labor from editors, justified by
 *   the ideology of 'free knowledge,' while simultaneously providing genuine
 *   coordination benefits to users (free access) and academic communities
 *   (comprehensive reference material). The tension is not incidental but
 *   structural — the non-commercial model's competitive advantage relative to
 *   paid alternatives derives partly from avoiding the salary costs of
 *   professional writers, meaning some portion of its success is built on
 *   unpaid labor extraction. Over its 20+ year lifecycle, the model has
 *   evolved from pure coordination (early Wikipedians as a technical
 *   community with shared mission) through mixed tangled-rope (current state
 *   with moderate extraction and coordination functions) toward potential
 *   piton (institutional contributions becoming performative). The constraint
 *   exhibits all six types from different perspectives, making it a
 *   diagnostic test for how 'free labor' is classified in a post-industrial
 *   economy.
 *
 * KEY AGENTS:
 *   - Volunteer Editors: Primary victims (powerless/trapped) — donate hundreds of hours for zero compensation; constrained by sunk-cost psychology and normative pressure to contribute to public knowledge
 *   - Information Access Users: Primary beneficiaries (institutional/arbitrage) — experience zero or negative extraction through free access; high exit optionality
 *   - Academic Community: Secondary beneficiary (moderate/constrained) — benefits from free reference material but constrained by citation norms and institutional evaluation pressures
 *   - Wikimedia Foundation: Organizational beneficiary (institutional/arbitrage) — controls institutional power and funding flows; coordinates volunteer labor allocation
 *   - Commercial Encyclopedia Publishers: Competitors in snare (powerless/trapped) — unable to undercut free model; trapped by legacy business model incompatibility
 *   - Moderators and Administrators: Hidden victims (powerless/trapped) — undertake essential infrastructure labor (vandalism revert, spam removal, dispute resolution) with zero compensation
 *   - AI and Commercial Knowledge Platforms: Emerging competitors (organized/arbitrage) — developing alternative knowledge distribution models that may eventually displace Wikipedia's unique value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_noncommercial_model, 0.38).
domain_priors:suppression_score(wikipedia_noncommercial_model, 0.42).
domain_priors:theater_ratio(wikipedia_noncommercial_model, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, extractiveness, 0.38).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_noncommercial_model, tangled_rope).
narrative_ontology:human_readable(wikipedia_noncommercial_model, "Wikipedia's Non-Commercial, Volunteer-Driven Model").
narrative_ontology:topic_domain(wikipedia_noncommercial_model, "technological/information/governance").

domain_priors:requires_active_enforcement(wikipedia_noncommercial_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, information_access_users).
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, wikipedia_foundation_institutional_power).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, volunteer_editor_labor).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, commercial_encyclopedia_publishers).
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, for_profit_knowledge_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOLUNTEER EDITOR (SNARE) — Trapped in unpaid labor extraction justified by 'free knowledge' ideology. Editors donate hundreds of hours for zero compensation, competing with professional writers. Suppression is high: psychological sunk costs (identity as 'Wikipedian'), social norms of voluntarism, and lack of alternative platforms for free knowledge contribution. No viable exit without abandoning the shared goal. Maximum experienced extraction.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC COMMUNITY (TANGLED ROPE) — Constrained by citation norms (Wikipedia not citeable) and research evaluation pressures, yet benefits enormously from free access to comprehensive summaries for teaching and literature review. Academics experience both extraction (cannot monetize their knowledge contribution to Wikipedia articles) and coordination (access to freely indexed knowledge). Suppression is moderate: institutional pressure to publish in commercial journals limits alternative knowledge-sharing venues.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INFORMATION ACCESS USER BASE (ROPE) — Primary beneficiaries with high exit optionality (can access commercial alternatives, paywalled encyclopedias, AI summaries). Yet the non-commercial model directly benefits these users through free access. Experienced extraction is negative or near-zero: they pay nothing and receive knowledge. The constraint functions purely as coordination: Wikipedia's non-profit structure enables free distribution that commercial competitors cannot undercut.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL ENCYCLOPEDIA PUBLISHERS (SNARE) — Trapped by the non-commercial model's competitive advantage. Unable to charge subscription fees without appearing exploitative relative to free Wikipedia. Exit options are severely limited: pivoting to free ad-supported models still lose content-creation differentiation. High suppression: brand reputation damage, sunk costs in proprietary content, locked-in reader habits. Experienced extraction flows away from them toward Wikipedia, making this a snare from the opposite direction.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: WIKIMEDIA FOUNDATION GOVERNANCE COALITION (SCAFFOLD) — Organized institutional actors (Wikipedia Foundation, chapter organizations, editor communities) see the non-commercial model as a temporary coordination scaffold with a sunset implicit in its structure. The model's sustainability depends on sustained donor engagement and volunteer morale. As AI and commercial knowledge platforms mature, the 'free encyclopedia' mission may transition to 'free knowledge verification infrastructure' or 'community editorial oversight layer.' The coalition has partial agency and sees potential exit paths through mission evolution.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL CONTENT CONTRIBUTION (PITON) — Many institutional actors (universities, government agencies, NGOs, corporations) nominally contribute to Wikipedia through official channels, but the theater ratio is high: institutional contributions are often low-effort, poorly maintained, or quickly reverted by volunteer editors. The performative aspect — 'we participate in Wikipedia' — exceeds the functional contribution. Institutional arbitrage (reputation via Wikipedia presence) with low actual influence over content.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC LAW VIEW (MOUNTAIN) — From a civilizational perspective, the non-commercial model appears to embody an immutable economic principle: knowledge goods exhibit infinite marginal reproduction cost approaching zero; therefore free distribution is the inevitable endpoint for information. Suppression of commercial alternatives is 'natural' — paid encyclopedias cannot compete with free. However, the structural data reveals this as a false summit: the non-commercial model is contingent on sustained volunteer motivation, donor funding, and organizational governance, not on economic laws.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_noncommercial_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_noncommercial_model, TR),
    TR >= 0.70.

:- end_tests(wikipedia_noncommercial_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The non-commercial model exhibits clear asymmetry: volunteer editors create all content and sustain operations through unpaid labor, while the Wikimedia Foundation captures institutional control, donor access, and operational authority. However, the extraction is not maximal (ε > 0.46) because (a) editors gain genuine benefits (reputation, learning, community belonging, impact on public knowledge) that partially offset labor costs, and (b) the model produces real coordination benefits for users and society. The intermediate value reflects that the model combines genuine coordination (free knowledge access) with asymmetric extraction (labor capture). Suppression (0.42): Moderate. Multiple suppression mechanisms operate: (1) ideological framing ('free knowledge as public good') that discourages treating editorial labor as valuable; (2) sunk-cost psychology where editors have already invested identity in Wikipedia; (3) lack of alternative platforms for free knowledge contribution at scale; (4) normative pressure within editor communities; (5) structural barriers to collective bargaining (globally distributed, anonymous volunteers). However, suppression is not extreme: some editors do leave, alternative platforms exist (specialized wikis, Reddit communities), and some institutional criticism of the model is visible. Theater ratio (0.58): Moderate-high. Significant performative elements: institutional 'contributions' from corporations and governments are often low-effort; ceremonial 'Edit-a-thons' that produce minimal net content improvement; the facade of 'community consensus' governance while actual power concentrates in administrator and foundation roles; donor recognition rituals that outpace actual knowledge contribution. The theater ratio has increased over time as the platform matured — early Wikipedia was mostly functional (editors creating content), but recent platforms include substantial performative governance (RfC ceremonies, process theater, bureaucratic review cycles).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. Volunteer editors see a snare: they are trapped in unpaid labor extraction justified by ideology. Information users see rope: they experience pure coordination (free access, no extraction). Academics see tangled rope: genuine benefit from free access, but constrained by institutional pressures and unable to reciprocate knowledge contribution. The Wikimedia Foundation sees rope or weak tangled rope: successful coordination of global knowledge, with moderate institutional extraction from donors and volunteers. Commercial competitors see snare: trapped by a model they cannot compete with. Moderators see hidden snare: essential infrastructure labor performed for zero compensation. The analytical observer sees mountain: free knowledge distribution is economically inevitable given information cost structure. The perspectival gap reveals that the model's stability depends on suppressing awareness of the snare from volunteer editors and moderators — if editors recognized that their labor is being extracted, model collapse would accelerate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction and coordination flows. Volunteer editors are trapped victims (d ≈ 0.90): they receive normative and social pressure to contribute without exit options, their labor is captured by the institution, and they bear the cost (time investment with no compensation). Information users are beneficiaries with arbitrage exits (d ≈ 0.05): they can access commercial alternatives but choose free Wikipedia; their experienced extraction is negative. The Wikimedia Foundation is an institutional beneficiary (d ≈ 0.10): it controls labor allocation, donor funding, and institutional power, with full exit optionality (could pivot the model). Commercial publishers are trapped by the model's competitive advantage (d ≈ 0.85 in reverse): they cannot compete with free without abandoning their revenue model, and they bear the cost of reduced market share. The academic community is moderately constrained (d ≈ 0.55): they benefit from access but are constrained by citation norms and institutional pressure that prevent them from reciprocating or monetizing their knowledge contribution. Moderators are trapped victims (d ≈ 0.92): they undertake essential infrastructure labor with zero compensation and high burnout risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy arises from the classification ambiguity: is Wikipedia primarily a Rope (pure coordination for free knowledge) or a Tangled Rope (coordination plus asymmetric labor extraction)? The snare perspective from volunteers and the rope perspective from users seem to contradict. Resolution: these are not contradictions but perspectival readings of the same structure. From the user's position (d ≈ 0.05), experienced extraction χ is near zero — they see rope. From the editor's position (d ≈ 0.90), experienced extraction χ is high — they see snare. The base properties (ε = 0.38, moderate extractiveness, moderate suppression) support tangled rope as the structural classification: the model contains BOTH genuine coordination (free access) AND asymmetric extraction (unpaid labor). The mandatrophy is resolved by recognizing that the base properties do not determine a single perspective but rather a family of perspectives. The model IS a snare for editors, IS a rope for users, IS a tangled rope as a structural whole. No single type is 'correct' — the indexical classification system maps each agent to their experienced type. The false summit risk is that treating Wikipedia as a 'natural law' of information distribution (mountain) obscures the contingent social arrangements (normative suppression, labor invisibility, institutional power concentration) that hold the model together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volunteer_sustainability_threshold,
    'What rate of volunteer burnout and editor attrition becomes incompatible with Wikipedia''s coverage and quality maintenance?',
    'Longitudinal tracking of editor retention rates, article quality metrics, and coverage expansion rates; correlation with compensation introduction pilots or alternative incentive structures',
    'If sustainability threshold exceeded: volunteer snare becomes untenable, forcing model transition to hybrid paid/unpaid or full professionalization. If threshold remains distant: current extractive labor model persists as structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_sustainability_threshold, empirical, 'Critical volunteer attrition threshold for model stability').

omega_variable(
    commercial_platform_capability_convergence,
    'Will AI-generated knowledge platforms (ChatGPT, Claude, Gemini) or commercial alternatives (Brittanica Online, specialized wikis) converge to matching Wikipedia''s breadth and verifiability?',
    'Comparative analysis of coverage breadth, citation accuracy, expert review depth, and user trust across platforms; longitudinal tracking of market share and usage metrics',
    'If convergence occurs: rope perspective for users weakens — free access is no longer a unique advantage. If convergence fails: rope coordination stabilizes indefinitely. Either way, tangled rope classification may shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_platform_capability_convergence, empirical, 'Whether commercial alternatives can match Wikipedia''s coverage and trust').

omega_variable(
    donation_dependency_risk,
    'Is the non-commercial model''s reliance on donor funding (particularly foundation grants) compatible with editorial independence, or does it introduce hidden extraction through donor influence?',
    'Analysis of funding source concentration, donor influence on editorial policy, comparison of editorial outcomes in donor-dependent vs volunteer-driven sections',
    'If hidden extraction detected: the model shifts from snare-on-volunteers to tangled-rope-with-donors, revealing a second layer of asymmetric extraction. If independence confirmed: volunteer snare remains primary structural feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(donation_dependency_risk, empirical, 'Whether donation funding creates hidden donor influence on Wikipedia content').

omega_variable(
    content_moderation_labor_invisibility,
    'Is content moderation (vandalism revert, spam removal, dispute resolution) being suppressed from consciousness as ''volunteer labor'' rather than recognized as essential infrastructure requiring compensation?',
    'Tracking of moderation labor hours, comparison with equivalent professional content moderation salaries, analysis of editor burnout in high-moderation areas (politics, current events)',
    'If recognized as essential infrastructure: model pressures increase to compensate moderators, reducing pure snare classification. If invisibility persists: extraction deepens as moderation needs grow with platform scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(content_moderation_labor_invisibility, empirical, 'Whether content moderation labor is being invisibilized in the volunteer model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_noncommercial_model, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wiki_tr_t0, wikipedia_noncommercial_model, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wiki_tr_t7, wikipedia_noncommercial_model, theater_ratio, 7, 0.47).
narrative_ontology:measurement(wiki_tr_t14, wikipedia_noncommercial_model, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(wiki_be_t0, wikipedia_noncommercial_model, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(wiki_be_t7, wikipedia_noncommercial_model, base_extractiveness, 7, 0.28).
narrative_ontology:measurement(wiki_be_t14, wikipedia_noncommercial_model, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_noncommercial_model, information_standard).
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, open_source_software_volunteer_model).
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, knowledge_commons_sustainability).

% DUAL FORMULATION NOTE:
% Wikipedia's non-commercial model should be decomposed into (1) the knowledge coordination constraint (free encyclopedia as information standard) with ε ≈ 0.08 (Mountain) and (2) the volunteer labor extraction constraint (unpaid editor labor as institutional dependency) with ε ≈ 0.52 (Snare). These are structurally distinct: the first is about what Wikipedia produces; the second is about how it produces it. Current analysis combines both under tangled rope at ε = 0.38. Future corpus expansion should separate these stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_noncommercial_model, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
