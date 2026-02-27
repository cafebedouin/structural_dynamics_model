% ============================================================================
% CONSTRAINT STORY: bushman_money_magic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bushman_money_magic, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bushman_money_magic
 *   human_readable: The Trickster's Asymmetric Scam
 *   domain: economic/social
 *
 * SUMMARY:
 *   The trickster's asymmetric scam is a structural extraction mechanism
 *   operating through narrative control and informational asymmetry. The
 *   Jackal (or similar trickster figure in oral traditions) exploits cattle
 *   ranchers' vulnerability to persuasion by promising 'money magic' — a
 *   ritual that will multiply wealth without labor. The scam succeeds because
 *   it operates in a context where (1) informational verification is costly
 *   and slow (ranchers cannot immediately test the magic), (2) social
 *   authority is difficult to challenge (the trickster presents as an
 *   expert/magician), (3) collective belief creates psychological lock-in
 *   (admitting the scam requires admitting public shame), and (4) the promise
 *   maps onto culturally embedded templates (belief in magical transformation
 *   of wealth). The constraint exhibits high extractiveness (0.68) because
 *   the mechanism successfully transfers capital from victims to beneficiary
 *   with minimal resistance, high suppression (0.72) because victims have no
 *   legitimate exit (admission of shame, loss of capital, social stigma) and
 *   cannot organize counter-narrative without collective agreement, and high
 *   theater ratio (0.85) because the scam is almost entirely performative —
 *   the 'magic money' ritual has zero material function, only psychological
 *   manipulation. Over the constraint's lifetime (6 time units),
 *   extractiveness accumulates as more victims participate and the
 *   trickster's confidence grows, while theater ratio increases as the
 *   performance becomes more elaborate to justify delayed results.
 *
 * KEY AGENTS:
 *   - Jackal/Trickster: Primary beneficiary (organized/arbitrage) — designs and executes the deception; captures extracted wealth; has full exit capacity and faces no suppression
 *   - Cattle Ranchers: Primary victims (powerless/trapped) — commit capital and trust; bear full cost of lost wealth plus reputational damage; cannot exit without shame
 *   - Community Trust Commons: Secondary victim (powerless/trapped) — abstract epistemic infrastructure contaminated by scam; future members inherit damaged baseline trust
 *   - Community Skeptics: Mixed experience (moderate/constrained) — suspect the scam but constrained by social pressure; experience both coordination pressure and extraction suppression
 *   - Anthropological Observer: Institutional perspective (analytical/analytical) — sees scam as degraded shamanic ritual; notes atrophied function (Piton classification)
 *   - Game Theorist: Analytical perspective (analytical/analytical) — risks naturalizing asymmetric information as immutable law; false-summing risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bushman_money_magic, 0.68).
domain_priors:suppression_score(bushman_money_magic, 0.72).
domain_priors:theater_ratio(bushman_money_magic, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bushman_money_magic, extractiveness, 0.68).
narrative_ontology:constraint_metric(bushman_money_magic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bushman_money_magic, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bushman_money_magic, snare).
narrative_ontology:human_readable(bushman_money_magic, "The Trickster's Asymmetric Scam").
narrative_ontology:topic_domain(bushman_money_magic, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bushman_money_magic, jackal_trickster).
narrative_ontology:constraint_victim(bushman_money_magic, cattle_ranchers).
narrative_ontology:constraint_victim(bushman_money_magic, community_trust_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFRAUDED RANCHER (SNARE) — The victim has committed capital and trust to the scam narrative. Exit options are severely constrained: admitting deception requires public shame, loss of reputation, and capital loss simultaneously. Social/emotional attachment to the promised outcome (magical wealth) creates psychological lock-in. Maximum extraction experienced — the rancher bears full cost of lost capital plus reputational damage, with no organizational capacity to resist or recover.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY TRUST COMMONS (SNARE) — The broader community's epistemic infrastructure is contaminated by the scam. Future legitimate social coordination (collective herding, trade, mutual aid) becomes harder because baseline trust is damaged. This abstract collective has no exit option and cannot organize self-defense. Extraction runs against the generational interest — the constraint depletes social capital that future members depend on.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: TRICKSTER/JACKAL (ROPE) — The extractor experiences this as a pure coordination problem solved through narrative control. The scam works because the trickster is skilled at synchronizing the victims' beliefs and suspending their skepticism. From the trickster's perspective, the constraint is a coordination mechanism: 'How do I get these ranchers to all believe in magic money simultaneously?' The trickster has full exit capacity (departs after extraction) and experiences no suppression — they are the designer of the deception, not its prisoner. Effective extraction (chi) is very high because the trickster faces minimal resistance, but the classification is Rope because the trickster sees the mechanism as pure coordination.
constraint_indexing:constraint_classification(bushman_money_magic, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: COMMUNITY SKEPTICS (TANGLED ROPE) — Some community members suspect the scam but lack sufficient evidence or social position to openly challenge it. They face a mixed constraint: the scam mechanism coordinates collective belief (Rope function) but also extracts from doubters who are suppressed via social pressure ('Don't spoil the magic for those who believe'). These agents experience both coordination and extraction — constrained exit because rejecting the narrative invites social sanctions, but also some benefit from community participation. Moderate power because skeptics can organize quietly but cannot override the trickster's narrative control.
constraint_indexing:constraint_classification(bushman_money_magic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANTHROPOLOGICAL OBSERVER (PITON) — From a civilizational view, the scam appears to ritualize a degraded form of shamanic authority. Historical shamanism solved genuine coordination problems (ritual, healing uncertainty, group cohesion). The trickster's scam mimics shamanic theater (mystery, authority, narrative control) but has lost the shamanic function — it is pure theatrical performance with no real benefit to the community. The mechanism persists through inertia: the ranchers' cultural template for 'magical authority figures' remains intact even as the function (actual coordinated action) has atrophied. High theater ratio (0.85) reflects that the scam is almost entirely performative — the 'magic money' ritual has no material function, only narrative function.
constraint_indexing:constraint_classification(bushman_money_magic, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: GAME THEORIST / NATURAL LAW VIEW (MOUNTAIN) — From a universal game-theoretic perspective, some degree of informational asymmetry and deception potential is inherent to any economic exchange. The trickster merely exploits an intrinsic structural feature: the victim cannot simultaneously invest capital AND verify the claim before the trade completes. This temporal gap (commitment before verification) is unavoidable in real economies. From this view, the scam is not contingent on the trickster's skill but on the immutable structure of trust-requiring transactions. However, this perspective risks false-summing: the structural vulnerability is real, but whether it is exploited depends entirely on agent choices and community enforcement mechanisms.
constraint_indexing:constraint_classification(bushman_money_magic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bushman_money_magic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bushman_money_magic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bushman_money_magic, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bushman_money_magic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bushman_money_magic, TR),
    TR >= 0.70.

:- end_tests(bushman_money_magic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The trickster successfully transfers material wealth (cattle, goods, capital) from multiple victims with no compensating benefit flow. The mechanism is efficient: one performer extracts from many victims simultaneously through narrative control. The value reflects strong extraction but not maximum (0.85+) because the mechanism eventually collapses when belief fails — it is not self-sustaining indefinitely. Suppression (0.72): High. Victims face multiple suppression vectors: (1) informational (cannot verify the magic before committing capital), (2) social (admitting deception invites shame), (3) psychological (emotional investment in the promised outcome creates cognitive resistance to disconfirming evidence), (4) economic (admitting loss is admitting resource depletion). Exit options are minimal except through social shame. Theater ratio (0.85): Very high. The entire mechanism is performative. The 'magic money' ritual produces zero material goods; it is pure psychological manipulation. No legitimate function is being coordinated — unlike shamanic rituals that might coordinate group action or healing, the scam only coordinates false belief. The performance must become increasingly elaborate over time to maintain credibility as initial promised results fail to materialize.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence between extractor and victims. The Jackal (Rope perspective) sees pure coordination: 'How do I synchronize these ranchers' belief in magic money?' From the rancher's view (Snare), the 'coordination' is experienced as coercion — they are locked into a false belief system with no legitimate exit. The community skeptics occupy a middle position (Tangled Rope) — they feel both the coordination pressure (everyone is supposed to believe) and the extraction pressure (skepticism is socially punished). The anthropological observer notes that the scam mimics legitimate shamanic authority but has lost its function (Piton) — the theatrical performance persists even after the coordinating mechanism (actual shared ritual benefit) has atrophied. The game theorist risks false-summing by treating the asymmetric information vulnerability as an immutable natural law rather than a contingent institutional arrangement vulnerable to counter-narrative and verification mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jackal's directionality (d ≈ 0.15) reflects organized power + arbitrage exit + beneficiary status: low extraction experienced because the trickster is not trapped in the system they designed; they can depart at will. The ranchers' directionality (d ≈ 0.92) reflects powerless agents + trapped exit + victim status: very high extraction experienced because they have committed capital, face reputational barriers to admitting deception, and cannot organize escape. The community trust commons has no exit at all (d ≈ 0.98): it is a pure victim of the constraint, bearing generational cost without agency or recovery options. Skeptics have moderate directionality (d ≈ 0.55): constrained exit + mixed victim/observer status. The anthropological observer has analytical directionality (d ≈ 0.72): external position but full awareness of the constraint's damage to institutional function. The game theorist's directionality (d ≈ 0.50) reflects that they occupy a neutral analytical position from which they risk naturalizing what is actually a contingent institutional vulnerability.
 *
 * MANDATROPHY ANALYSIS:
 *   SCAM AS PURE SNARE: The high extractiveness (0.68) combined with high suppression (0.72) and the absence of genuine coordination function (zero material benefit to victims) resolves this as definitively Snare rather than Tangled Rope. While the constraint appears to 'coordinate' belief, coordination requires that all participants benefit from synchronized action. Here, the ranchers gain nothing from synchronized belief in false magic — their participation costs them capital and dignity. The coordination is one-directional (trickster coordinates victims' belief) not mutual (trickster and victims coordinating shared benefit). Theater ratio (0.85) confirms Snare classification: the mechanism is almost entirely performative extraction with no functional coordination substrate. The piton classification at the anthropological perspective correctly identifies that the scam degraded shamanic ritual (which did have coordinating function) into pure theatrical extraction. The mountain classification at the game-theorist level is FALSE SUMMIT: informational asymmetry is a real structural feature, but whether it is exploited depends on agent design choices (reputation mechanisms, verification protocols, community enforcement) — it is not an immutable law. The mandatrophy resolves by confirming Snare as the structural reality across all empirical perspectives; the mountain and piton are perspectival artifacts from higher abstraction levels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    belief_collapse_timeline,
    'What triggers the ranchers'' collective recognition that the scam is false? Is it a single disconfirming event, gradual accumulation of failed promises, or external reputation signal?',
    'Historical analysis of scam collapse patterns; interviews with victims about moment of belief abandonment; comparison with anthropological case studies of ritual failure',
    'If triggered by external signal: constraint is fragile (Snare with low persistence). If gradual: constraint becomes entrenched (Snare with institutional depth). If single event: constraint is brittle and dependent on rare conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_collapse_timeline, empirical, 'Timeline and mechanism for collapse of collective belief in the scam').

omega_variable(
    trickster_skill_versus_victim_credulity,
    'Is the scam''s success primarily the result of the trickster''s exceptional deceptive skill, or primarily the result of the victims'' cultural predisposition to believe in magic and authority?',
    'Comparative study: does the same trickster succeed equally with different cultural groups? Does the same cultural group fall for different tricksters at similar rates? Historical variation in scam success across communities.',
    'If skill-dependent: constraint is contingent on specific agent (Snare with low generality). If culture-dependent: constraint is structural to belief systems (Snare with institutional depth). If both: constraint requires both enabling conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trickster_skill_versus_victim_credulity, empirical, 'Relative contributions of trickster skill and victim cultural predisposition').

omega_variable(
    recovery_and_restitution_feasibility,
    'Can the community recover the extracted wealth (cattle, capital) or rebuild trust after the scam is exposed? Are there institutional mechanisms for restitution?',
    'Post-exposure analysis: did victims recover losses? Did community develop enforcement mechanisms against future tricksters? Did social trust recover within one generation?',
    'If recovery impossible: constraint generates permanent damage (Snare with cyclical victims). If recovery feasible: constraint is reversible (Snare with recovery pathway). If community develops detection mechanisms: constraint evolves into Scaffold with sunset clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recovery_and_restitution_feasibility, empirical, 'Whether victims can recover losses and community can rebuild institutional trust').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bushman_money_magic, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmm_tr_t0, bushman_money_magic, theater_ratio, 0, 0.55).
narrative_ontology:measurement(bmm_tr_t3, bushman_money_magic, theater_ratio, 3, 0.72).
narrative_ontology:measurement(bmm_tr_t6, bushman_money_magic, theater_ratio, 6, 0.85).

% Extraction over time
narrative_ontology:measurement(bmm_be_t0, bushman_money_magic, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bmm_be_t3, bushman_money_magic, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(bmm_be_t6, bushman_money_magic, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bushman_money_magic, enforcement_mechanism).
narrative_ontology:affects_constraint(bushman_money_magic, reputation_cascade_degradation).
narrative_ontology:affects_constraint(bushman_money_magic, credulity_learning_lag).

% DUAL FORMULATION NOTE:
% The trickster scam is a downstream manifestation of informational asymmetry and community trust vulnerability. Upstream constraints (reputation systems, verification infrastructure) determine whether this scam even becomes possible. If those upstream constraints are degraded (reputation_cascade_degradation), the scam extractiveness increases. The constraint family includes the scam mechanism itself, the community trust commons it damages, and the recovery/restitution systems that could rebuild institutional resilience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
