% ============================================================================
% CONSTRAINT STORY: governance__customary_rule
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance__customary_rule, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: governance__customary_rule
 *   human_readable: Customary Rule: Immemorial Authority and Inertial Suppression
 *   domain: political/legal
 *
 * SUMMARY:
 *   Customary rule grounds governing authority in the accumulated weight of
 *   immemorial tradition — practices so old that no living person made them
 *   and no living person may claim the authority to remake them. This reading
 *   of the governance kernel is one instantiation among five competing claims
 *   to legitimate authority. The customary rule framing appears in tribal
 *   governance structures, feudal systems, colonial indigenous
 *   administration, and persistent claims in contemporary legal systems
 *   (common law, constitutional tradition, communal norms). The structural
 *   signature is distinctive: suppression operates primarily through inertia
 *   rather than active enforcement (the new is presumptively illegitimate
 *   simply because it is new), beneficiaries are whoever custom already
 *   favors (hereditary elites, established castes, prior winners), and
 *   victims are those whom custom subordinates (lower castes, serfs,
 *   reform-seekers, conquered populations). Extractiveness is typically low
 *   to moderate because custom often coordinated something real at some point
 *   in history — it is not pure extraction. But the extractiveness is frozen
 *   in place: custom cannot adapt easily to changing material conditions, and
 *   the presumptive illegitimacy of innovation locks the distribution of
 *   power into historical patterns. The constraint exhibits all six DR types
 *   depending on the observer's structural position, making it a diagnostic
 *   exemplar of how governance frames produce different experienced
 *   classification.
 *
 * KEY AGENTS:
 *   - Established Status Groups (e.g., hereditary nobles, priestly castes, historical merchant guilds): Primary beneficiaries (institutional/arbitrage) — custom encodes their advantages; they experience it as legitimate coordination
 *   - Subordinated Castes or Classes (e.g., serfs, untouchables, conquered populations): Primary victims (powerless/trapped) — custom freezes them in subordinate status; exit is impossible
 *   - Reform Seekers (merchants, intellectuals, entrepreneurs blocked by custom): Secondary victims (moderate/constrained) — benefit from some coordination features of customary order but constrained by rules blocking their advancement; exit costly but possible
 *   - Reformist Coalition (emergent professional classes, colonial administrators, popular movements): Organized agents (organized/mobile) — building alternative legitimacy sources (written law, elections, administration) to supersede custom; see sunset path
 *   - Customary Apparatus (councils of elders, judges citing precedent, administrative officers claiming tradition): Institutional maintainer (institutional/arbitrage) — claims authority from immemorial custom while functionally enforcing modern policy; increasingly theatrical
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as inevitable feature of human governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance__customary_rule, 0.32).
domain_priors:suppression_score(governance__customary_rule, 0.48).
domain_priors:theater_ratio(governance__customary_rule, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance__customary_rule, extractiveness, 0.32).
narrative_ontology:constraint_metric(governance__customary_rule, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(governance__customary_rule, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance__customary_rule, rope).
narrative_ontology:human_readable(governance__customary_rule, "Customary Rule: Immemorial Authority and Inertial Suppression").
narrative_ontology:topic_domain(governance__customary_rule, "political/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(governance__customary_rule, '6a7b41a3-fa59-4876-adee-16fff7686cb3').
narrative_ontology:cs_kernel_codification('6a7b41a3-fa59-4876-adee-16fff7686cb3', formalized).
narrative_ontology:cs_authority_grounding('6a7b41a3-fa59-4876-adee-16fff7686cb3', practice).
narrative_ontology:cs_interpretation_layer_present('6a7b41a3-fa59-4876-adee-16fff7686cb3').
narrative_ontology:cs_reading_relation('6a7b41a3-fa59-4876-adee-16fff7686cb3', governance__autocratic_rule, coexists_with).
narrative_ontology:cs_reading_relation('6a7b41a3-fa59-4876-adee-16fff7686cb3', governance__constitutional_government, forecloses).
narrative_ontology:cs_reading_relation('6a7b41a3-fa59-4876-adee-16fff7686cb3', governance__direct_democracy, coexists_with).
narrative_ontology:cs_reading_relation('6a7b41a3-fa59-4876-adee-16fff7686cb3', governance__theocratic_rule, coexists_with).
narrative_ontology:cs_axiom('6a7b41a3-fa59-4876-adee-16fff7686cb3', foundational, immemorial_legitimacy).
narrative_ontology:cs_axiom_status(immemorial_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6a7b41a3-fa59-4876-adee-16fff7686cb3', immemorial_legitimacy, conventional).
narrative_ontology:cs_axiom('6a7b41a3-fa59-4876-adee-16fff7686cb3', foundational, ruler_is_bound_by_custom).
narrative_ontology:cs_axiom_status(ruler_is_bound_by_custom, holdable).
narrative_ontology:cs_axiom_grounding('6a7b41a3-fa59-4876-adee-16fff7686cb3', ruler_is_bound_by_custom, deontological).
narrative_ontology:cs_reference_frame('6a7b41a3-fa59-4876-adee-16fff7686cb3', established_traditional_order).
narrative_ontology:cs_drift_state('6a7b41a3-fa59-4876-adee-16fff7686cb3', contemporary_alternative_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a7b41a3-fa59-4876-adee-16fff7686cb3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(governance__customary_rule, governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance__customary_rule, established_status_groups).
narrative_ontology:constraint_beneficiary(governance__customary_rule, beneficiaries_of_prior_custom).
narrative_ontology:constraint_victim(governance__customary_rule, reform_seekers).
narrative_ontology:constraint_victim(governance__customary_rule, subordinated_castes_or_classes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED CASTE (SNARE) — Those whom custom has permanently fixed in a lower status (serfs, untouchables, conquered populations) experience the constraint as immovable extraction. The immemorial claim naturalizes their subordination; no living person made this rule, so no living person can unmake it. Exit is impossible — their status is not a policy choice but an ancient fact. The suppression mechanism is not active enforcement (no guards watching) but ideological: custom IS the natural order.
constraint_indexing:constraint_classification(governance__customary_rule, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM SEEKER (TANGLED ROPE) — A merchant, intellectual, or regional actor who sees opportunities for wealth, power, or justice that custom blocks. They benefit from some coordination features of the customary order (predictability, legitimacy grounding, social stability) but bear extraction from rules that subordinate their interests. Exit is costly but not impossible: migrate to a different jurisdiction, or build a coalition to shift custom (very slow, very high risk). The tension between coordination function and asymmetric extraction classifies as tangled rope.
constraint_indexing:constraint_classification(governance__customary_rule, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED ELITE (ROPE) — Those whom custom has favored (nobles, priestly classes, hereditary merchants) experience the constraint as pure coordination. Custom binds others to respect their prerogatives; custom binds them to fulfill ceremonial duties and honor precedent. This is coordination: each party knows what to expect, authority rests on shared understanding, and the system is stable because it is customary. The elite benefits because custom encodes their advantages, but they experience the constraint as a coordination mechanism, not extraction. They can arbitrage (interpret custom favorably, accumulate power via custom channels) without exiting.
constraint_indexing:constraint_classification(governance__customary_rule, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMIST COALITION (SCAFFOLD) — Organized agents (urban merchants, emergent professions, literate classes, colonial administrators imposing new governance) who see customary rule as a temporary obstacle. They organize alternative legitimacy sources (written law codes, popular elections, rational administration) that they expect will supersede custom. Low effective extraction because the coalition perceives an exit path and is actively building it. Theater is low because the reformists are not just critiquing custom but constructing alternatives. The sunset is not built into custom itself but into the coalition's structural position.
constraint_indexing:constraint_classification(governance__customary_rule, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: DEGRADED CUSTOMARY APPARATUS (PITON) — At civilizational timescale, customary rule becomes vestigial. The mechanism that once provided real coordination (predictability via tradition) persists as theater. Elders claim authority from immemorial custom while actually enforcing modern administrative preferences. Judges cite precedent while implementing new policy. The apparatus is maintained through inertia — the language of custom persists, the rituals continue, the claim to immemorial authority remains — but the functional core has atrophied. Theater ratio is high because performance (invoking custom) is doing more work than the actual coordination (which is now handled by administration, writing, codification).
constraint_indexing:constraint_classification(governance__customary_rule, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a deep historical or civilizational perspective, some form of customary authority may appear to be a universal feature of human governance: humans always ground legitimacy in tradition, precedent, or 'how things have always been done.' This perspective sees customary rule as an inevitable structural property of governance itself — not a contingent institutional choice, but an immutable law of political organization. However, the structural data contradicts this. The constraint has identifiable beneficiaries, active suppression mechanisms, and variable extractiveness across contexts. The engine will classify this as a false summit: the observation that customary authority is *common* has been naturalized into the claim that it is *inevitable*.
constraint_indexing:constraint_classification(governance__customary_rule, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance__customary_rule_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance__customary_rule, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance__customary_rule, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance__customary_rule, TR),
    TR >= 0.70.

:- end_tests(governance__customary_rule_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32, measured at t=100): Moderate-low. Customary rule performs real coordination work (it provides predictability, legitimacy grounding, stability) but encodes historical power asymmetries. The extractiveness is not as severe as a pure snare (which would score 0.66+) because genuine coordination exists — custom is not solely a mechanism for extraction. However, extractiveness is not low (≤0.05) like a pure rope because the distribution of benefits is frozen in place and innovation is presumptively illegitimate. The measurement shows rising extractiveness over the interval (0.18 → 0.28 → 0.32) reflecting increasing tension: as material conditions change and alternative possibilities emerge, the constraint's function shifts from coordination (custom was adaptive when it was recent) toward extraction (custom becomes a drag on adaptation). Suppression (0.48): Moderate. The immemorial claim creates presumptive illegitimacy for the new and thus suppresses alternatives. Barriers to reform include: (1) ideological — custom is naturalized as inevitable, legitimate, ancient; (2) material — beneficiaries have power and resources to enforce it; (3) cognitive — low literacy, weak documentation of alternatives, limited exposure to different governance models. Suppression is not at the 0.60+ level of a snare because suppression operates largely through internalization and inertia rather than active coercion. Theater ratio (0.38, measured at t=100): Low-moderate. At early time points (t=0), theater is very low (0.25) because custom is genuinely believed and its coordination function is real. As alternatives emerge and material conditions diverge from the custom's assumptions, theater rises (reaching 0.38 at t=100) because the apparatus must work harder to maintain the immemorial claim in the face of pressure for change. By the piton perspective (civilizational timescale), theater is much higher because the apparatus is performing tradition while actually implementing modern policy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. The established elite see customary rule as legitimate, stable, coordinating — a rope. The subordinated caste see it as immovable extraction — a snare. The reform seeker sees it as mixed coordination and extraction with constrained but possible exit — tangled rope. The reformist coalition see it as temporary obstacle being superseded — scaffold. The apparatus itself sees its own tradition as increasingly theatrical as material conditions diverge from custom's assumptions — piton. The analytical observer risks seeing immutability (mountain) where the other perspectives reveal contingency. The gap reveals that customary rule's classification depends entirely on one's structural position: whether you are beneficiary or victim, whether you have exit options, whether you can organize alternatives, whether you have time horizons measured in lifetimes or civilizations.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural relationship to the extraction flow. The established elite are beneficiaries with arbitrage options (custom encodes their advantages and they can interpret it favorably): low d → low χ → experience as rope. The subordinated caste are victims with no exit (custom freezes their status): high d → high χ → experience as snare. The reform seeker is a victim with constrained exit options (can migrate, can organize slowly, but faces high costs): moderate-high d → moderate χ → experience as tangled rope. The reformist coalition is organized with mobile exit (building alternatives, perceiving a sunset path): moderate d → moderate χ → experience as scaffold. The customary apparatus at civilizational timescale arbitrages custom (interprets favorably, extracts legitimacy from the immemorial claim) despite the functional atrophy: low-moderate d → low χ experienced, but high theater makes classification piton rather than rope. The analytical observer at civilizational scope sees the constraint as a potential natural law of governance itself, risking false summit naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in customary rule is resolved by recognizing that the constraint performs BOTH coordination AND extraction, and the balance shifts over time. At t=0 (custom is recent, material conditions align with custom's assumptions), the constraint is closer to rope — the coordination function dominates. At t=100 (material conditions diverge, alternatives emerge, custom constrains adaptation), the constraint is closer to snare for victims — the extraction function dominates. The analytical observer's mountain classification is a false summit: customary rule is not a law of nature but a contingent institutional choice that appears inevitable only when viewed from outside the system. The reform seeker's exit options determine the classification for the moderate agent: if truly constrained, tangled rope; if mobile (alternatives are building successfully), scaffold. The key structural question that resolves the mandatrophy: Does the customary order expand to incorporate new actors and adapt to new conditions (rope-like), or does it freeze benefits in place and resist adaptation (snare-like)? History suggests customary systems do the latter — they were adaptive when they encoded recent successful practice but become extractive when they resist change. The fixture of the immemorial claim (no living person made this, no living person may remake it) locks the system into historical patterns and prevents the adaptation that would keep it coordinate-functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immemorial_authenticity,
    'Is the claimed immemorial custom actually ancient, or is it a retrospective invention projected onto an invented past?',
    'Comparative history and genealogy: examine written records (when they appear), archaeological evidence, linguistic layers, and accounts by neighboring societies to establish when the custom actually originated.',
    'If custom is genuinely ancient (500+ years): legitimacy claim has real structural power and the constraint is harder to challenge. If custom is recent invention (50-200 years): the immemorial framing is theater, and the constraint''s classification shifts toward snare (the suppression is active deception, not just inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immemorial_authenticity, empirical, 'Whether claimed immemorial custom is genuinely ancient or retrospectively invented').

omega_variable(
    suppression_mechanism_locus,
    'Does suppression operate via ideological internalization (the ruled believe the custom is natural/legitimate) or via material coercion (enforcement by guards, economic dependency, geographic isolation)?',
    'Ethnographic observation: examine patterns of rule-breaking, punishment, collective action, and exit attempts to determine whether suppression derives from internalized legitimacy or external barriers.',
    'If primarily ideological: constraint is a rope (coordination via shared belief) rather than snare (coercion). If primarily material: constraint is a snare (suppression ≥ 0.60). If mixed: tangled rope. The locus of suppression determines whether the constraint can be challenged by shifting beliefs (ideology) or requires material change (barriers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Locus of suppression: ideological internalization vs material coercion').

omega_variable(
    beneficiary_expansion_over_time,
    'Does customary rule expand the set of beneficiaries over time (incorporating new groups into the ruling coalition) or freeze benefits in place (excluding newcomers)?',
    'Historical tracking of who custom favors at different time points; analysis of coalitions and exclusion mechanisms.',
    'If expanding: custom can function as a mechanism for integrating new actors (more coordination-like). If freezing: custom perpetuates historical extraction (more snare-like). Measurement of this dynamic determines whether the reform seeker''s exit option is truly constrained or mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_expansion_over_time, empirical, 'Whether customary rule expands or freezes the beneficiary set over time').

omega_variable(
    alternative_legitimacy_feasibility,
    'In this context, is written law, democratic election, or expert administration a feasible alternative to customary authority, or does the material/cognitive infrastructure required make alternatives impossible?',
    'Structural analysis: literacy rates, administrative capacity, resource base, colonial/imperial pressure, international norms, and documented attempts to institute alternatives.',
    'If alternatives are feasible: scaffold perspective is real and extractiveness should be lower (agents see an exit path). If alternatives are impossible (absent literacy, absent state apparatus, absent external pressure): the constraint is more entrenched and extractiveness higher. This determination affects whether the reform seeker is truly constrained or trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_feasibility, empirical, 'Feasibility of alternatives to customary authority in this context').

omega_variable(
    kernel_reading_contest,
    'Which reading of the governance kernel (customary_rule, autocratic_rule, constitutional_government, direct_democracy, theocratic_rule) is actually instantiated in this political system?',
    'Documentary analysis and structural observation: examine claims to authority, enforcement mechanisms, distribution of power, and how the system legitimates its own operations. A system may claim one reading while actually instantiating another (e.g., claim customary authority while functionally operating as autocracy).',
    'If this reading (customary_rule) is the actual governing logic: the constraint story is accurate. If another reading is actually operative: this story captures aspirational legitimacy rather than structural reality, and the engine should consider whether to reclassify toward a different reading or flag a divergence between claimed and actual authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the governance kernel is actually instantiated (committer ambiguity)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance__customary_rule, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(customary_tr_t0, governance__customary_rule, theater_ratio, 0, 0.25).
narrative_ontology:measurement(customary_tr_t50, governance__customary_rule, theater_ratio, 50, 0.32).
narrative_ontology:measurement(customary_tr_t100, governance__customary_rule, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(customary_be_t0, governance__customary_rule, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(customary_be_t50, governance__customary_rule, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(customary_be_t100, governance__customary_rule, base_extractiveness, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance__customary_rule, identity_coordination).
narrative_ontology:boltzmann_floor_override(governance__customary_rule, 0.12).
narrative_ontology:affects_constraint(governance__customary_rule, governance__autocratic_rule).
narrative_ontology:affects_constraint(governance__customary_rule, governance__constitutional_government).
narrative_ontology:affects_constraint(governance__customary_rule, governance__direct_democracy).
narrative_ontology:affects_constraint(governance__customary_rule, governance__theocratic_rule).

% DUAL FORMULATION NOTE:
% The governance kernel decomposes into five competing constraint stories, one per reading. Each reading instantiates a distinct constraint with its own extractiveness, suppression, and perspectival structure. The customary_rule reading is structurally distinct from autocratic_rule (which lacks the immemorial/untouchable claim and substitutes active will), from constitutional_government (which introduces a higher law that binds governors), from direct_democracy (which roots authority in the assembled citizens rather than tradition), and from theocratic_rule (which roots authority in divine order). Network links indicate that these readings operate in the same conceptual space and that a shift from one reading to another (e.g., from customary to constitutional) is a structural transformation of governance itself, not merely a policy change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
