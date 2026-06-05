% ============================================================================
% CONSTRAINT STORY: solidarity_mirror_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solidarity_mirror_trap, []).

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
 *   constraint_id: solidarity_mirror_trap
 *   human_readable: Solidarity Mirror Trap in Epistemic Communities
 *   domain: epistemology/social_psychology/discourse
 *
 * SUMMARY:
 *   The solidarity mirror trap emerges within epistemic communities when the
 *   boundary between legitimate coordination (defending shared commitments)
 *   and extractive suppression (protecting leadership and dominant factions
 *   from dissent) becomes invisible to participants. The constraint begins as
 *   a rational response to external threat: a field under criticism from
 *   neighboring disciplines or funding bodies faces coordination pressure to
 *   present unified positions and suppress internal fragmentation that
 *   critics can exploit. Over time, the coordination mechanism becomes
 *   self-perpetuating. Leadership benefits from dissent suppression
 *   (maintains authority, protects theoretical investment). Early-career
 *   researchers perform loyalty signaling as a career necessity. Dissenters
 *   face reputation damage and publication gatekeeping. The community's
 *   epistemic integrity decays silently—contaminated literature accumulates,
 *   alternative explanations go unexplored, empirical anomalies are reframed
 *   as methodological errors rather than theoretical problems. The constraint
 *   exhibits the full range of DR types depending on observation point:
 *   genuine coordination (rope) from leadership's perspective, structural
 *   extraction (snare) from the dissenter's perspective, degraded theater
 *   (piton) from the successor generation's perspective, and a temporary
 *   coordination failure with emerging alternatives (scaffold) from
 *   cross-disciplinary movements. The constraint's measurements show a
 *   characteristic trajectory: theater increases as dissent suppression
 *   becomes more ritualized, extractiveness increases as more dissenters
 *   learn self-censorship, and suppression intensifies as the constraint's
 *   authority grows. The analytical observer risks naturalizing this as
 *   'inevitable group dynamics,' obscuring that identifiable beneficiaries
 *   and active enforcement mechanisms are sustaining it.
 *
 * KEY AGENTS:
 *   - Community Leadership: Primary beneficiary (institutional/arbitrage) — maintains authority and theoretical framework; faces no significant cost to dissent suppression
 *   - Dominant Theoretical Faction: Primary beneficiary (powerful/arbitrage) — protected from empirical challenge and theoretical competition by solidarity norms
 *   - Internal Dissenters: Primary victim (powerless/trapped or moderate/constrained) — face reputation damage, publication gatekeeping, and career risk; lack exit options without abandoning professional identity
 *   - Field Epistemic Integrity: Secondary victim (powerless/identity_locked) — abstract collective good that cannot organize or defend itself; contaminated by suppressed anomalies
 *   - Careful Moderates: Secondary victims/partial beneficiaries (moderate/constrained) — benefit from community infrastructure but suppress honest dissent; experience mixed extraction
 *   - Successor Generation: Organized performers (organized/constrained) — socialized into loyalty signaling; maintain constraint through theater rather than conviction
 *   - Cross-Disciplinary Challenge Movements: Organized challengers (organized/mobile) — building alternative epistemic pathways; creating structural exits that undermine the community's gatekeeping monopoly
 *   - Prestigious Outsider Critics: Powerful challengers (powerful/mobile) — can criticize publicly without career destruction; benefit from and reinforce the dissent suppression they critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solidarity_mirror_trap, 0.54).
domain_priors:suppression_score(solidarity_mirror_trap, 0.68).
domain_priors:theater_ratio(solidarity_mirror_trap, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solidarity_mirror_trap, extractiveness, 0.54).
narrative_ontology:constraint_metric(solidarity_mirror_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(solidarity_mirror_trap, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solidarity_mirror_trap, tangled_rope).
narrative_ontology:human_readable(solidarity_mirror_trap, "Solidarity Mirror Trap in Epistemic Communities").
narrative_ontology:topic_domain(solidarity_mirror_trap, "epistemology/social_psychology/discourse").

domain_priors:requires_active_enforcement(solidarity_mirror_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solidarity_mirror_trap, community_leadership).
narrative_ontology:constraint_beneficiary(solidarity_mirror_trap, dominant_theoretical_faction).
narrative_ontology:constraint_victim(solidarity_mirror_trap, internal_dissenters).
narrative_ontology:constraint_victim(solidarity_mirror_trap, field_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNAL DISSENTER (SNARE) — Structurally trapped within the community. Exit from the epistemic community means loss of professional identity, peer relationships, and collaboration networks. Suppression operates through reputation damage, exclusion from publication pipelines, and social ostracism. The dissenter bears maximum extraction cost with no structural exit option.
constraint_indexing:constraint_classification(solidarity_mirror_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FIELD EPISTEMIC INTEGRITY (SNARE) — The abstract collective good of truth-seeking within the field cannot organize, exit, or defend itself. Bears the structural cost of suppressed dissent, premature consensus, and contaminated literature. Identity-locked at civilizational scale: the field's commitment to its own epistemic mission is overridden by solidarity norms.
constraint_indexing:constraint_classification(solidarity_mirror_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CAREFUL MODERATE (TANGLED ROPE) — Mid-career scholar who benefits from community membership and collaborative access but faces constraints on public dissent. Experiences both genuine coordination (shared research infrastructure, peer support during external criticism) and asymmetric extraction (pressure to self-censor, career risk of speaking up). High cost but not insurmountable — has some agency and some benefit.
constraint_indexing:constraint_classification(solidarity_mirror_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY LEADERSHIP (ROPE) — Experiences the solidarity norm as a coordination mechanism. Leadership perceives the constraint as essential to maintaining community cohesion against external critique and internal fragmentation. Benefits from dissent suppression (maintains authority, protects dominant theoretical framework). Arbitrage exit option: can exit the community entirely if pressure becomes intolerable, maintain institutional position elsewhere. Low experienced extraction.
constraint_indexing:constraint_classification(solidarity_mirror_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUCCESSOR GENERATION (PITON) — Early-career researchers socialized into the solidarity norm without having chosen it. Perform loyalty signaling as a career necessity (theater ratio high: public declarations of commitment, ritual criticism of 'bad faith' external critics). The constraint persists through institutional inertia and career path dependence rather than ongoing active enforcement. Theater serves as the primary function: loyalty is performatively demonstrated, not instrumentally defended.
constraint_indexing:constraint_classification(solidarity_mirror_trap, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CROSS-DISCIPLINARY CHALLENGE MOVEMENTS (SCAFFOLD) — Organized groups (methodological pluralists, replication advocates, external skeptics) are building parallel epistemic pathways that bypass solidarity-enforcing gatekeepers. Preprint servers, open-science norms, and cross-field collaboration reduce the community's monopoly on legitimacy. The constraint has a sunset: as alternatives mature, internal dissent becomes less risky. Beneficiaries from coordination (access to networks, publication pipelines) have declining ability to enforce solidarity against competing standards.
constraint_indexing:constraint_classification(solidarity_mirror_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, group cohesion always requires suppression of dissent; belonging and truth-seeking are inherently in tension; communities under threat inevitably prioritize survival over inquiry. This perspective treats the solidarity trap as a natural law of group dynamics. However, the structural data contradicts this: identifiable beneficiaries exist (leadership, dominant factions), enforcement is active, and alternatives are emerging. The engine will classify this as a false summit, revealing that 'natural group dynamics' naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(solidarity_mirror_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: PRESTIGIOUS OUTSIDER CRITIC (TANGLED ROPE) — Senior figure with enough institutional power and external standing to criticize the community publicly without career destruction. Benefits from the constraint's existence (their criticism gains attention precisely because dissent is suppressed) while also being constrained by it (the community can attempt exclusion). Mobile exit option: can publish in alternative venues, secure funding from outside the community. Moderate extraction because power asymmetry is real but not total.
constraint_indexing:constraint_classification(solidarity_mirror_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solidarity_mirror_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(solidarity_mirror_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solidarity_mirror_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(solidarity_mirror_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(solidarity_mirror_trap, TR),
    TR >= 0.70.

:- end_tests(solidarity_mirror_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): High-moderate. The constraint extracts significant benefit for leadership and dominant factions—they maintain authority, protect theoretical investments, and avoid public challenges to their core claims. However, the extraction is not maximal (would require ε ≥ 0.66) because the constraint preserves genuine coordination value: the field does benefit from reduced fragmentation during external threat, publication pipelines do enable collaborative work, and some theoretical protection is legitimate. The measured value reflects that extractive suppression is layered over genuine coordination, not pure extraction. Suppression (0.68): High. Significant barriers to dissent include: career risk of speaking up (early-career researchers depend on community for funding and positions), reputation damage (dissenters are labeled as 'bad faith' or 'externally influenced'), publication gatekeeping (dissenters face difficulty publishing in high-status community venues), and social ostracism (informal exclusion from collaboration networks and conferences). These barriers are substantial but not total—some high-status figures do dissent, and cross-disciplinary pathways provide alternatives. Theater ratio (0.61): Moderate-high. Loyalty signaling consumes significant effort: public declarations of commitment to community positions, ritualized criticism of external skeptics, performative displays of group solidarity. However, some actual epistemic work happens—the community does produce genuine insights and novel empirical findings. Theater has increased over the measurement interval as dissent suppression became more formalized; younger researchers spend more effort on loyalty performance than on novel inquiry.
 *
 * PERSPECTIVAL GAP:
 *   The solidarity mirror trap exhibits maximal perspectival divergence. Community leadership sees a coordination mechanism (Rope)—their genuine experience is that suppressing dissent enables collective action and external defense. Internal dissenters see pure extraction (Snare)—they bear all costs with no benefit. Careful moderates see mixed extraction and coordination (Tangled Rope)—they gain infrastructure access but lose intellectual autonomy. The successor generation sees institutional inertia (Piton)—they perform loyalty as career requirement, not conviction. Cross-disciplinary movements see a problem with a sunset (Scaffold)—alternatives are emerging that bypass the community's gatekeeping. Prestigious outsider critics see a Tangled Rope—they benefit from the suppression they critique (their criticism gains attention precisely because dissent is dangerous inside). The civilizational analytical observer risks seeing natural law (Mountain)—that groups inevitably suppress dissent when threatened. The engine's false summit detector will flag this last perspective, revealing that 'natural group dynamics' naturalizes what is actually a contingent institutional choice with identifiable beneficiaries and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality follows from the agent's structural position relative to the constraint. Beneficiaries (leadership, dominant faction) have low d values (0.10–0.25)—they experience the constraint as beneficial, f(d) is near zero or negative, χ approaches zero. Trapped dissenters have high d values (0.90–0.95)—they are full targets of extraction, f(d) is maximized, χ is high. Constrained moderates have moderate d values (0.55–0.65)—they are partial targets experiencing both benefits and costs. The analytical observer derives d from their position outside the extraction relationship: d ≈ 0.70–0.75, producing f(d) in the moderate range. The dissenters at identity_locked exit options are particularly diagnostic: they have high d (are victims) but cannot exercise their structural mobility because their identity is constituted through the community. This produces the characteristic signature of identity lock: high experienced extraction despite theoretical exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing solidarity into two structurally distinct mechanisms: (1) genuine coordination—reducing fragmentation, enabling collaborative infrastructure, protecting vulnerable theories from premature dismissal; (2) extractive suppression—protecting leadership authority, deterring empirical challenges to dominant factions, concentrating epistemic advantage. Both mechanisms are real and operate simultaneously in tangled_rope territory. The mandatrophy question is not 'is this coordination or extraction?' but 'what is the balance?' The measured value (ε=0.54, suppression=0.68) indicates extraction has become dominant: initial coordination value (0.20–0.30 at t0) has been buried under suppression accumulation. The theater increase (0.38→0.61) indicates the transition point: as explicit enforcement became ritualized into loyalty performance, the constraint shifted from 'we suppress dissent to coordinate' (honest framing) to 'we suppress dissent because that's what community members do' (theatrical framing). The Tangled Rope classification captures both aspects: genuine coordination exists (beneficiaries + enforcement), asymmetric extraction is measured (victims + high suppression), but neither dominates. The Snare and Piton perspectives capture asymptotic outcomes: as theater rises to 0.7+, the constraint degrades to pure performance (Piton). As extraction rises to 0.66+, suppression becomes the primary mechanism (Snare). Current state is mid-range Tangled Rope with warning signals of both asymptotic paths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_threshold,
    'At what level of external threat does a genuine coordination problem (protecting vulnerable theory from premature dismissal) transition into an extractive suppression mechanism?',
    'Longitudinal analysis of threat perception vs suppression intensity; comparison with communities facing similar threats but adopting different dissent norms; interviews with leadership about explicit intent',
    'If threshold is low: solidarity norms appear rational even at high suppression levels (Rope classification dominates). If threshold is high: suppression mechanisms are identified as extraction machinery (Snare classification dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_threshold, conceptual, 'Distinction between legitimate group defense and extractive suppression').

omega_variable(
    dissenter_capability_distribution,
    'Are suppressed dissenters primarily low-status early-career researchers (career risk is legitimate constraint) or do high-status figures also self-censor (indicating identity lock rather than material barriers)?',
    'Survey of dissenter demographics: position level, publication record, institutional affiliation; comparison of self-censorship across career stages; analysis of why high-status figures remain silent',
    'If primarily early-career: exit_options classification as trapped is justified; dissent suppression is a coordination cost. If high-status also silent: identity_locked exit option is justified; the constraint is binding through identity fusion rather than material barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dissenter_capability_distribution, empirical, 'Distribution of suppressed dissenters by career status').

omega_variable(
    alternative_epistemic_pathway_viability,
    'Can cross-disciplinary preprint and open-science pathways genuinely replace the community''s gatekeeping function, or do they require community legitimacy to gain traction?',
    'Historical analysis of field-external critiques: which ones gained traction without community acceptance? Comparison of citation trajectories for papers published via alternative pathways vs community-endorsed venues',
    'If viable independently: scaffold sunset is real; the constraint''s power is declining. If dependent on eventual community acceptance: alternatives are pressure valves rather than structural exits; the constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemic_pathway_viability, empirical, 'Whether alternative epistemic pathways provide structural exits from community gatekeeping').

omega_variable(
    false_summit_natural_group_dynamics,
    'Does the ''group dynamics inevitably suppress dissent'' framing naturalize a contingent institutional arrangement, or does it accurately identify an immutable constraint on human group behavior?',
    'Cross-cultural and historical comparison: do communities with strong solidarity norms against dissent have systematically worse epistemic outcomes? Can groups maintain both high cohesion and high dissent tolerance? What mechanisms enable this?',
    'If contingent: the mountain perspective is a false summit; the constraint should be reclassified to tangled_rope or snare. If immutable: the mountain perspective is justified; solidarity and truth-seeking are fundamentally in tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_group_dynamics, conceptual, 'Whether solidarity-dissent tension is natural law or constructed constraint').

omega_variable(
    identity_lock_mechanism_in_field,
    'For internal dissenters classified as identity_locked at global scope, what specific identity frame prevents them from leaving the field? Is it professional identity (career built within the community), relational identity (friendship networks constitute self-concept), or ideological identity (commitment to the field''s core mission overrides commitment to truth)?',
    'Qualitative interviews with self-censoring scholars; analysis of identity statements; longitudinal follow-up on scholars who did exit the field and their stated reasons',
    'If professional/relational lock: exit would require career reconstruction; structural exit barriers are real. If ideological lock: the field''s identity is constituted through loyalty; exiting would mean becoming a different person epistemically. Different mechanisms imply different intervention points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_field, conceptual, 'Specific identity-fusion mechanism binding dissenters to community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solidarity_mirror_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smt_tr_t0, solidarity_mirror_trap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(smt_tr_t5, solidarity_mirror_trap, theater_ratio, 5, 0.51).
narrative_ontology:measurement(smt_tr_t10, solidarity_mirror_trap, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(smt_be_t0, solidarity_mirror_trap, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(smt_be_t5, solidarity_mirror_trap, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(smt_be_t10, solidarity_mirror_trap, base_extractiveness, 10, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(smt_su_t0, solidarity_mirror_trap, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(smt_su_t5, solidarity_mirror_trap, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(smt_su_t10, solidarity_mirror_trap, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solidarity_mirror_trap, identity_coordination).
narrative_ontology:affects_constraint(solidarity_mirror_trap, theoretical_monoculture_drift).
narrative_ontology:affects_constraint(solidarity_mirror_trap, replication_crisis_epistemic_pathology).
narrative_ontology:affects_constraint(solidarity_mirror_trap, publication_bias_gatekeeping).

% DUAL FORMULATION NOTE:
% The solidarity mirror trap is upstream of multiple field-degradation constraints. Theoretical monoculture drift describes the outcome when alternative frameworks are suppressed; replication crisis describes the empirical consequences when dissenters cannot voice methodological concerns; publication bias gatekeeping describes the institutional mechanism through which suppression is enforced. All three are downstream of the solidarity trap's core extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(solidarity_mirror_trap, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
