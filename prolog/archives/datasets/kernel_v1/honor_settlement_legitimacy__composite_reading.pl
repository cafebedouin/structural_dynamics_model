% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor Settlement Legitimacy (Composite Reading): Multiple Reinforcing Mechanisms with Contraction Edge
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   Honor settlement (ritualized dueling) declined across Europe and North
 *   America between the late 18th and early 20th centuries through a
 *   convergence of multiple reinforcing mechanisms that together
 *   overdetermined the constraint's degradation. This reading instantiates
 *   the hypothesis that dueling's decline cannot be explained by any single
 *   causal pathway — neither legal suppression alone, nor cultural
 *   unthinkability alone, nor the emergence of alternative status mechanisms
 *   alone — but rather by their mutual reinforcement. The contraction edge
 *   refers to the shift in cultural legitimacy (the perception that honor
 *   settlement became cognitively unthinkable to successive generations)
 *   which acts as the integrating mechanism: once a cultural framework shifts
 *   to treat honor settlement as barbaric or irrational, the other mechanisms
 *   (legal penalties, institutional suppression, alternative status systems)
 *   acquire moral and epistemic force. Yet these material and institutional
 *   changes would independently suppress the practice even without cultural
 *   unthinkability — the constraint exhibits genuine overdetermination. By
 *   the early 20th century, honor settlement persisted only in fringe
 *   practitioners and residual clubs maintained through identity lock and
 *   institutional inertia (the piton perspective), while the functional role
 *   of status allocation migrated to law, credentials, professional
 *   licensing, and reputation systems. The composite reading's contribution
 *   is to explain why dueling's suppression was so complete and stable:
 *   multiple independent suppression mechanisms created path-dependent
 *   equilibrium where removing any single mechanism would not restore the
 *   practice.
 *
 * KEY AGENTS:
 *   - Honor-Bound Aristocracy: Primary victim (powerless/trapped) — structurally dependent on honor settlement for status maintenance; faces extraction through both identity lock ('cowardice' if refusing duel) and institutional suppression (legal penalties)
 *   - State Enforcement Apparatus: Primary beneficiary (institutional/arbitrage) — monopolizes legitimate violence and dispute resolution; benefits from suppression of honor settlement which reinforces state authority
 *   - Honor Culture Coalition: Secondary victim and temporary beneficiary (organized/constrained) — coordinates on honor settlement as genuine status mechanism but faces extraction as institutional conditions change and legal penalties accumulate
 *   - Rising Merchant/Professional Class: Transitional beneficiary (moderate/constrained) — adopts alternative honor-based status mechanisms (credentials, contracts, professional licensing) that perform honor's function without dueling
 *   - Residual Honor Clubs: Identity-locked practitioners (institutional/arbitrage, but functionally powerless through identity fusion) — maintain fringe practice through institutional inertia and identity lock despite complete loss of functional value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.38).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.52).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor Settlement Legitimacy (Composite Reading): Multiple Reinforcing Mechanisms with Contraction Edge").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '9d7f5080-a263-4e5d-abbc-1ea56c184af0').
narrative_ontology:cs_kernel_codification('9d7f5080-a263-4e5d-abbc-1ea56c184af0', distributed).
narrative_ontology:cs_authority_grounding('9d7f5080-a263-4e5d-abbc-1ea56c184af0', lineage).
narrative_ontology:cs_interpretation_layer_present('9d7f5080-a263-4e5d-abbc-1ea56c184af0').
narrative_ontology:cs_reading_relation('9d7f5080-a263-4e5d-abbc-1ea56c184af0', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('9d7f5080-a263-4e5d-abbc-1ea56c184af0', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('9d7f5080-a263-4e5d-abbc-1ea56c184af0', foundational, multiple_mechanisms_reinforce_each_other).
narrative_ontology:cs_axiom_status(multiple_mechanisms_reinforce_each_other, holdable).
narrative_ontology:cs_axiom_grounding('9d7f5080-a263-4e5d-abbc-1ea56c184af0', multiple_mechanisms_reinforce_each_other, empirically_contingent).
narrative_ontology:cs_axiom('9d7f5080-a263-4e5d-abbc-1ea56c184af0', foundational, contraction_edge_integrates_mechanisms).
narrative_ontology:cs_axiom_status(contraction_edge_integrates_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('9d7f5080-a263-4e5d-abbc-1ea56c184af0', contraction_edge_integrates_mechanisms, empirically_contingent).
narrative_ontology:cs_reference_frame('9d7f5080-a263-4e5d-abbc-1ea56c184af0', honor_settlement_as_legitimate_status_mechanism).
narrative_ontology:cs_drift_state('9d7f5080-a263-4e5d-abbc-1ea56c184af0', post_enlightenment_professionalization, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('9d7f5080-a263-4e5d-abbc-1ea56c184af0', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_monopoly_enforcement).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, bourgeois_institutional_authority).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, feudal_aristocracy).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, masculine_status_alternative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONOR-BOUND ARISTOCRAT (SNARE) — Structurally trapped. The practice persists through identity fusion: refusal to duel becomes perceived cowardice, social ostracism, loss of marriage prospects, economic marginalization through broken alliances. Material suppression (legal penalties, state violence) is reinforced by cultural suppression (unthinkability once honor culture loses intellectual legitimacy). The trapped agent cannot exit without losing social existence; the constraint extracts full compliance through both coercive and internalized mechanisms.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HONOR CULTURE COALITION (TANGLED ROPE) — Organized agents (dueling clubs, honor codes, aristocratic networks) coordinate on honor settlement as a genuine mechanism for status allocation and dispute resolution. But they also face genuine extraction: the state monopolizes legitimate violence, legal penalties accumulate, labor-market competition from rising merchant classes erodes honor-based hierarchy. The constraint coordinates honor settlement AND extracts from its own constituency as institutional conditions change. Sunset logic does not apply — the coalition has no exit strategy because honor culture lacks an alternative institutional ground.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ENFORCEMENT APPARATUS (ROPE) — Institutional beneficiary with maximum arbitrage. The state's monopoly on legitimate violence is reinforced by suppressing honor settlement; legal penalties against dueling strengthen state authority. Pure coordination benefit: the state is solving the legitimate collective action problem (monopoly enforcement). No experienced extraction — the constraint works for this agent's explicit goals. State power increases as dueling becomes illegal and institutionally delegitimized.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RESIDUAL HONOR RITUAL (PITON) — The constraint persists through institutional inertia long after its primary function (status allocation in honor culture) has atrophied. 18th-century dueling clubs in Europe continued performative honor rituals well into the 19th century despite cultural condemnation and legal suppression. Theater ratio (0.85+): the ritual is maintained for identity reasons, not functional dispute resolution. Dueling persists as fringe practice precisely because the identity lock keeps it alive even after the institutional ground has collapsed.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSITIONAL HONOR MERCHANT (SCAFFOLD) — Rising merchant class and professional groups that claim honor-based status without dueling. Legal professions, banking dynasties, manufacturing magnates adopt modified honor codes (reputation, contractual integrity, professional credentials) that perform honor's status function without the duel mechanism. Low extraction (χ ≤ 0.30): the constraint has a functional sunset — alternative status mechanisms replace honor settlement. Beneficiary status is temporary: as institutional alternatives mature, honor settlement loses even performative force. Theater ratio remains moderate (0.40–0.50) because these groups are genuinely solving status allocation problems through alternative mechanisms (credentials, contract law, professional licensing).
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FUNCTIONAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, honor settlement appears as an inherent mechanism for resolving disputes in pre-state honor cultures — a natural law of how status operates before institutional alternatives exist. Status allocation always requires some mechanism; honor settlement was THE mechanism for centuries. This perspective treats dueling as immutable within its ecological niche. However, this naturalizes a contingent institutional arrangement: once alternative status mechanisms (law, credentials, reputation systems) emerge, honor settlement becomes structurally unnecessary. Engine will classify as false summit.
constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_settlement_legitimacy__composite_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, TR),
    TR >= 0.70.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine mixed coordination and extraction. Early in the interval (t=0, ε=0.22), honor settlement functions primarily as a coordination mechanism for status allocation within honor culture — legitimate functional purpose with modest extractive overhead. By mid-interval (t=25, ε=0.35), legal suppression and cultural delegitimization increase the extractive force: agents continue the practice under increasing coercion and cultural pressure. By late interval (t=50, ε=0.38), the constraint has become primarily extractive for remaining practitioners: they maintain honor settlement despite zero functional value, driven by identity lock and institutional inertia. Suppression (0.52): Moderate-high and increasing. Initial suppression is modest (0.30) — honor settlement operates within a framework that legitimates it. As alternatives emerge and cultural delegitimization spreads, suppression mechanisms intensify: legal penalties harden, social stigma increases, institutional opportunities require renunciation of honor codes. Terminal suppression (0.52) reflects the complete institutional hostility to the practice. Theater ratio (0.48): Moderate and increasing. Early practice exhibits genuine functional purpose (theater ≈ 0.35) — honor settlement solves real status disputes. As the practice degrades to fringe persistence, theater increases (0.48 at terminal): the ritual persists for identity reasons, not functional dispute resolution. The rising theater ratio signals degradation from rope (pure coordination) toward piton (performative inertia).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. The honor-bound aristocrat sees a snare (extraction through identity lock and legal suppression). The honor culture coalition sees tangled rope (mixed coordination and extraction as institutional conditions shift). The state apparatus sees pure rope (coordination benefit with zero extraction). The residual practitioner sees piton (degraded ritual maintained through institutional inertia). The transitional merchant sees scaffold (temporary coordination through alternative status mechanism with sunset). The analytical observer risks seeing mountain (honor settlement as a natural immutable feature of status systems), but this naturalizes a contingent institutional arrangement that disappeared within a century. The composite reading's analytical work is to show that all six perspectives are structurally legitimate — the constraint's classification depends entirely on the observer's position and time horizon, and no single perspective reveals the full causal structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions relative to the constraint. The state apparatus (institutional/arbitrage beneficiary) has low d — it gains authority from suppressing honor settlement. The honor-bound aristocrat (powerless/trapped victim) has high d — maximum extraction flows toward them through identity lock and legal suppression. The honor culture coalition (organized/constrained) has moderate-high d — they coordinate genuinely but face increasing extraction as conditions change. The transitional merchant (moderate/constrained beneficiary) has low-moderate d — they benefit from alternative status mechanisms but face some extraction through credential systems. The residual practitioner (institutional/arbitrage but identity-locked) has mixed d — structurally positioned as beneficiary but functionally trapped by identity lock, producing d ≈ 0.40–0.50. The analytical observer has high d (1.0) if misled by the natural-law perspective, but the composite reading's structure reveals this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION IN COMPOSITE READING: The constraint resolves mandatrophy by showing that honor settlement operates simultaneously as genuine coordination mechanism (status allocation in honor culture) AND as extractive mechanism (suppression of alternatives, identity lock). The tangled rope classification is the correct synthesis: the constraint has BOTH a real coordination function (solving status disputes within honor culture) AND asymmetric extraction (legal penalties, cultural suppression, institutional barriers). The mandatrophy dissolves when we recognize that the same institutional form performs both functions at different time points: early (t=0) predominantly rope (coordination), late (t=50) predominantly snare (extraction for residual practitioners). The composite reading's contribution is to show that this transition is overdetermined — multiple independent mechanisms reinforce each other such that removal of any single mechanism would not restore the practice. The contraction edge (cultural unthinkability) is the integrating mechanism: once a cultural framework shifts to treat honor settlement as barbaric, all other mechanisms (legal suppression, institutional alternatives, economic restructuring) acquire legitimacy and force. Without the contraction edge, legal suppression alone might not suppress the practice completely; without legal suppression, cultural unthinkability alone might not eliminate fringe persistence through identity lock. The conjunction of mechanisms creates stability that exceeds what any single mechanism could achieve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_unthinkability_vs_legal_suppression_primacy,
    'Did honor settlement decline primarily because the cultural framework delegitimized it (making dueling cognitively unthinkable to new generations) or because legal/institutional suppression created material barriers (penalties, state violence, loss of employment)?',
    'Historical comparison of decline trajectories: regions with strong legal suppression but persistent cultural honor codes (e.g., Southern US post-Civil War, Mediterranean cultures) vs regions with cultural framework shift but weaker legal enforcement (e.g., Scandinavia). Mechanisms: letters/diaries showing internalized unthinkability vs survival of clandestine clubs; correlation between legal penalty severity and decline rate; comparative prosecution patterns.',
    'If cultural unthinkability dominates: contraction_reading confirmed — the constraint''s decline is overdetermined by ideological capture making dueling unthinkable. If legal suppression dominates: drop_reading mechanism (institutional barriers drive persistence of fringe practice among residuals). If mechanisms are truly independent and reinforcing: composite_reading confirmed — multiple pathways converge such that removal of any single mechanism would not fully suppress the practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_unthinkability_vs_legal_suppression_primacy, empirical, 'Whether dueling''s decline is primarily cultural unthinkability or legal suppression').

omega_variable(
    alternative_status_mechanism_sufficiency,
    'Did honor settlement decline because genuine alternative status mechanisms (law, credentials, professional licensing, reputation systems) emerged to perform its functional role, or did alternative mechanisms emerge BECAUSE honor settlement was already being suppressed?',
    'Timeline analysis: (a) Do alternative mechanisms predate or postdate legal suppression of dueling? (b) Historical documents showing merchants/professionals adopting credentials-based status BEFORE or AFTER honor culture loses legitimacy? (c) Do regions with weak legal suppression of dueling show faster adoption of alternative status mechanisms (suggesting functional replacement) or slower adoption (suggesting institutional lock-in)?',
    'If alternatives predate suppression: scaffold logic confirmed — functional replacement is the causal driver. If alternatives postdate suppression: composite mechanism confirmed — legal suppression creates demand for alternatives, which then reinforce the original suppression through path dependency. If alternatives emerge simultaneously from independent social changes: true overdetermination — multiple reinforcing mechanisms with no single primary cause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_status_mechanism_sufficiency, empirical, 'Whether alternative status mechanisms caused honor settlement decline or emerged in response to it').

omega_variable(
    identity_lock_persistence_after_institutional_collapse,
    'For the agents in the Piton perspective (residual honor clubs, fringe practitioners), is honor settlement identity lock sufficient to sustain the practice even after all institutional functions have disappeared?',
    'Ethnographic/historical analysis of 19th-century European dueling clubs and 20th-century honor code survivals: Do members persist in ritual practice despite loss of status function? Do they rationalize practice through identity fusion (''this is who we are'') rather than instrumental claims? Survival rate of honor clubs after legal penalties increase; membership composition (insiders maintaining identity vs outsiders seeking status).',
    'If identity lock sustains the practice: piton mechanism confirmed — the constraint can persist through pure performative identity even with zero functional value. If identity lock collapses when institutional function disappears: piton classification is incorrect (the practice decays to noise, not inertial persistence). This determines whether the composite reading''s ''contraction edge'' (cultural unthinkability) is sufficient to suppress the practice or whether identity-locked agents maintain fringe persistence requiring stronger institutional suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_after_institutional_collapse, empirical, 'Whether identity lock sustains honor settlement practice after institutional function collapses').

omega_variable(
    reading_contest_empirical_signature,
    'Which reading of the contested honor_settlement_legitimacy kernel does the historical record support: composite_reading (multiple reinforcing mechanisms with contraction edge), contraction_reading (cultural unthinkability as primary), or drop_reading (institutional barriers drive fringe persistence)?',
    'Comparative historical analysis across European regions with different suppression intensities and cultural framework shifts. Three empirical signatures: (A) Composite reading predicts decline happens fastest where BOTH cultural unthinkability AND legal suppression coincide; other mechanisms alone would show slower decline. (B) Contraction reading predicts decline driven by cultural legitimacy shift; regions with strong honor culture but weak legal suppression should show rapid decline. (C) Drop reading predicts fringe persistence in proportion to cultural honor-code survival; legal suppression alone cannot eliminate identity-locked practitioners. Test signatures against regional decline trajectories, club survival rates, and legal documentation.',
    'Reading contest resolution determines which sibling reading this constraint (composite_reading) forecloses or coexists with. If composite mechanism is supported: composite_reading confirms overdetermination and contraction edge as the integrating framework. If single mechanism dominates: composite_reading mischaracterizes the causal structure and coexists_with relation should be reconsidered as forecloses or weaker empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_signature, empirical, 'Empirical support for composite vs contraction vs drop reading of honor settlement kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_comp_theater_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(honor_comp_theater_t25, honor_settlement_legitimacy__composite_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(honor_comp_theater_t50, honor_settlement_legitimacy__composite_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(honor_comp_extract_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(honor_comp_extract_t25, honor_settlement_legitimacy__composite_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(honor_comp_extract_t50, honor_settlement_legitimacy__composite_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(honor_comp_supp_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(honor_comp_supp_t25, honor_settlement_legitimacy__composite_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(honor_comp_supp_t50, honor_settlement_legitimacy__composite_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, resource_allocation).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% The three readings (composite, contraction, drop) are distinct constraint stories analyzing the same historical phenomenon — dueling's decline — from different causal framings. Composite reading asserts overdetermination (multiple reinforcing mechanisms with contraction edge as integrating factor). Contraction reading emphasizes cultural unthinkability as primary driver. Drop reading emphasizes institutional suppression creating identity-locked fringe persistence. Each reading has distinct ε values, distinct perspectival distributions, and distinct omega variables focused on empirically resolving which causal mechanism dominated. The network links them through affects_constraints to enable contamination analysis: if the composite mechanism is correct, both sibling readings are partially correct but incomplete. If contraction dominates, the composite reading overestimates material suppression's role. If drop dominates, the composite reading underestimates identity-lock persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
