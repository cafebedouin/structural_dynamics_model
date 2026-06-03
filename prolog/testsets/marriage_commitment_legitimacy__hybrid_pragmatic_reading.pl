% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The Manifesto represents a distinctive institutional strategy: prophetic
 *   authority is deployed as a legitimation mechanism to manage exogenous
 *   federal crisis while preserving core theological commitments through
 *   strategic scope ambiguity. The constraint operates at the intersection of
 *   three structural forces: (1) federal legal/political pressure requiring
 *   institutional compliance with new marriage practice norms; (2) internal
 *   doctrinal commitment to the immutability of revealed doctrine; (3)
 *   institutional leadership's need to maintain authority over the tradition
 *   while adapting its practice. The hybrid pragmatic reading frames the
 *   Manifesto as neither pure exogenous coercion (which would undermine
 *   prophetic authority) nor pure endogenous revelation (which would require
 *   doctrine to have genuinely evolved). Instead, it positions the Manifesto
 *   as strategic deployment of the prophetic authority mechanism — the
 *   leadership invokes a transcendent legitimation source to reframe the
 *   federal pressure as divine guidance, thereby converting political
 *   necessity into theological development. This reframing creates a
 *   structural gap: the doctrine can be read as 'unchanged but locally
 *   suspended' (satisfying internal traditionalists) AND as 'genuinely
 *   evolved via revelation' (satisfying those who embrace the new practice).
 *   The constraint's extractiveness derives from the institutional
 *   leadership's ability to use this ambiguity to benefit from both federal
 *   compliance AND internal theological flexibility, while rank-and-file
 *   adherents bear the cost of doctrinal incoherence and identity
 *   uncertainty. The theater ratio rises over time (0.35 → 0.65) as the
 *   distance between the narrative frame (divine revelation) and the
 *   structural reality (political accommodation) becomes harder to sustain,
 *   requiring increasingly elaborate interpretive work to maintain the
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — preserves institutional authority, enables doctrinal flexibility, manages federal pressure without explicit surrender
 *   - Rank-and-File Adherents: Primary victim (powerless/identity_locked) — bears identity deformation, doctrinal uncertainty, cognitive dissonance; identity constituted through tradition makes exit identity death
 *   - Federal State Apparatus: Secondary beneficiary (institutional/arbitrage) — achieves policy compliance without requiring institutional collapse or external enforcement escalation
 *   - Intermediate Clergy: Mixed beneficiary/victim (moderate/constrained) — benefits from institutional preservation; bears enforcement burden and congregational friction; has costly but possible exit
 *   - Doctrinal Coherence: Victim (institutional/trapped) — the tradition's internal logic is fragmented; future authoritative claim-making is blocked by the precedent of ambiguous reversals
 *   - Prophetic Authority Frame: Institutional inertia (institutional/arbitrage) — sustained through theological commitment to the authority mechanism itself; increasingly theatrical as credibility gap widens
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.48).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '79ec25f7-7bc9-45b3-b4b8-fe1d02d37153').
narrative_ontology:cs_kernel_codification('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', fixed_text).
narrative_ontology:cs_authority_grounding('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', extraction).
narrative_ontology:cs_interpretation_layer_present('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153').
narrative_ontology:cs_reading_relation('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', foundational, prophetic_authority_institutional_strategy).
narrative_ontology:cs_axiom_status(prophetic_authority_institutional_strategy, holdable).
narrative_ontology:cs_axiom_grounding('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', prophetic_authority_institutional_strategy, deontological).
narrative_ontology:cs_axiom('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', foundational, scope_ambiguity_preservation_legitimacy).
narrative_ontology:cs_axiom_status(scope_ambiguity_preservation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', scope_ambiguity_preservation_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', prophetic_institutional_authority_intact).
narrative_ontology:cs_drift_state('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', contemporary_doctrinal_fragmentation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('79ec25f7-7bc9-45b3-b4b8-fe1d02d37153', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_state_apparatus).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RANK-AND-FILE ADHERENT (SNARE) — Identity fused with doctrinal tradition that forbade the practice now mandated by institutional authority. Faces maximum extraction: must either internalize the reversal (identity deformation) or exit the community (identity death). No structural exit available without abandoning both the faith and the social/kinship bonds constituted through it. Experiences the Manifesto as coercive erasure dressed in prophetic language.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE CLERGY (TANGLED ROPE) — Must enforce the new policy to rank-and-file while navigating their own doctrinal uncertainty. Benefits from preservation of institutional structure (employment, authority, tradition continuity); bears costs of interpretive incoherence and congregational friction. Exit is costly but possible (relocation, transition out of ministry). Mixed extraction and coordination function — the Church's survival as an institution requires their compliance.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Primary beneficiary. Experiences the constraint as coordination: the Manifesto enables the institution to navigate federal pressure while preserving internal authority structures and theological flexibility. Can reframe the reversal as a higher prophetic truth without losing doctrinal face. Maintains institutional cohesion through managed ambiguity — the constraint serves the institution's adaptive capacity.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL STATE APPARATUS (ROPE) — Secondary beneficiary. Experiences the constraint as coordination: institutional compliance without requiring the Church's doctrinal surrender. The state achieves its policy objective (marriage practice alignment) while preserving the Church's internal legitimacy and avoiding the costlier alternative of direct suppression or schism. The ambiguity serves state interests: it contains the conflict within the institution rather than externalizing it to the polity.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DOCTRINAL COHERENCE (TANGLED ROPE) — The tradition itself is a victim: the constraint embeds a logical gap (the reversal is presented as faithful development rather than strategic reversal) that blocks future authoritative claim-making. The tradition still coordinates belief and practice for many agents; but the coordinate space is now explicitly fragmented — different readings of the Manifesto coexist without logical reconciliation. The constraint preserves institutional function at the cost of doctrinal purity.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PROPHETIC AUTHORITY FRAME (PITON) — At civilizational scale, the prophetic authority mechanism is substantially theatrical: the Manifesto invokes a transcendent source (divine revelation) to legitimize what is structurally a political accommodation. The frame persists through institutional inertia and theological commitment to the authority itself — adherents need to believe that prophetic channels remain open and authoritative, so the frame cannot be acknowledged as performative without destroying its legitimacy. High theater ratio indicates the prophetic frame is increasingly maintained through narrative and tradition rather than explanatory power.
constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_commitment_legitimacy__hybrid_pragmatic_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate, rising. The constraint's extractiveness is not as severe as a pure snare (0.72+) because genuine coordination functions are embedded — the institution does solve the problem of maintaining internal cohesion while adapting to external pressure. But it is higher than a pure rope (0.35) because the solution extracts disproportionate costs from identity-locked adherents while concentrating benefits on institutional leadership. The rising trajectory (0.32 → 0.48) reflects the increasing cognitive load required to maintain the ambiguity as generational distance from the Manifesto grows. Suppression (0.58): Moderate-high and rising. Barriers to exit include kinship networks, material dependency, identity fusion, and the cost of cognitive rupture. Suppression is not total (some adherents do exit) but sufficient to make the constraint binding for most. The rise reflects that as the doctrinal incoherence becomes more visible, institutional leadership must actively enforce the narrative frame to prevent cognitive dissonance from triggering defection. Theater ratio (0.65): High and rising. The prophetic authority frame is substantially theatrical — it invokes a transcendent source to legitimize a political accommodation. The rising trajectory indicates that the frame is increasingly maintained through institutional authority and narrative work rather than explanatory power. Theater at 0.65 (not yet piton-level 0.70+) reflects that enough adherents still experience the Manifesto as genuinely connected to the tradition that the theatrical element is not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence across power levels and exit options. The institutional leadership sees rope — the Manifesto solves the coordination problem of institutional survival in the face of external pressure while preserving internal authority. The federal apparatus sees rope — compliance without institutional collapse. Intermediate clergy see tangled rope — genuine coordination functions (institutional preservation) embedded with extraction (enforcement burden, doctrinal dissonance). Rank-and-file adherents see snare — pure extraction, dressed in prophetic language, with no exit available because identity is constituted through the tradition. The doctrinal coherence itself is trapped — the constraint fragments the coordinate space of belief without resolving the contradiction. The prophetic authority frame at civilizational scale appears increasingly theatrical (piton) as the credibility gap widens. This perspectival spread from rope (leadership) to snare (adherents) is diagnostic of a constraint where institutional leadership extracts from rank-and-file members through the mechanism of ambiguity management.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. The institutional leadership as beneficiary with arbitrage exit options experiences low effective d (≈0.15) — they have exit paths and benefit from the constraint, so their experienced extractiveness is dampened. Rank-and-file adherents as victims with identity_locked exit options experience high d (≈0.88) — they bear costs and cannot exit without identity death, so their experienced extractiveness is amplified. Intermediate clergy with constrained exit (high cost but possible) experience moderate d (≈0.55). The federal apparatus as beneficiary with arbitrage options experience low d. This directionality gradient is the source of the perspectival gap: the same institutional arrangement classifies as rope for leadership, tangled rope for intermediate clergy, and snare for identity-locked adherents.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER-SYSTEM READING: This constraint is ONE reading of a contested kernel (marriage_commitment_legitimacy). The mandatrophy is resolved by acknowledging that three structurally distinct readings produce three different extraction profiles: (1) endogenous_reinterpretation_reading positions the reversal as genuine prophetic evolution, collapsing extractiveness toward 0.25 (rope/mountain from most perspectives); (2) exogenous_override_reading positions the reversal as federal coercion with doctrine unchanged, raising extractiveness toward 0.72 (snare from adherent perspective); (3) hybrid_pragmatic_reading (this one) positions the Manifesto as strategic institutional deployment of prophetic authority, maintaining moderate extractiveness (0.48) by sustaining ambiguity. The three readings are NOT competing descriptions of a single fact. They are competing framings that constitute different constraints with different extraction profiles. The hybrid reading's value lies in its capacity to sustain institutional function despite doctrinal incoherence — but this value accrues only to institutional leadership, while adherents bear the cognitive costs. The mandatrophy is resolved by specifying which reading is instantiated: this story commits to the hybrid pragmatic framing and documents its structural consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_strategy_ambiguity,
    'Is the Manifesto an instance of genuine prophetic revelation or strategic institutional deployment of prophetic authority to manage political crisis?',
    'This omega is structurally irreducible within the reading. The hybrid pragmatic reading DEPENDS on treating this as an open ambiguity — the institutional value of the Manifesto lies precisely in its capacity to be read BOTH ways simultaneously. Resolution would require either (a) metaphysical proof of divine communication (impossible), or (b) explicit institutional acknowledgment that the prophetic frame is strategic cover (impossible — would destroy institutional legitimacy). The reading sustains itself by preventing this resolution.',
    'If revelation: reclassify to endogenous_reinterpretation_reading; the constraint becomes rope/mountain across all perspectives. If strategy: reclassify to exogenous_override_reading; the constraint becomes snare from adherent perspective. The hybrid reading''s moderate extractiveness (0.48) depends on this ambiguity being preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_strategy_ambiguity, conceptual, 'Whether the Manifesto expresses genuine revelation or strategic institutional adaptation').

omega_variable(
    scope_ambiguity_sustainability,
    'How long can the Manifesto maintain doctrinal legitimacy through scope ambiguity (local practice vs. universal doctrine) before the gap between the two readings becomes too large to sustain?',
    'Historical trajectory analysis: track divergence in interpretation across generational cohorts; measure rate of doctrinal contestation in institutional texts; identify the inflection point where ''creative reinterpretation'' becomes ''acknowledged contradiction''. Proxy metrics: (a) rate of internal doctrinal publications disputing the Manifesto vs. defending it; (b) generational cohort data on adherent belief in the doctrinal coherence of the tradition; (c) defection rates correlated with doctrinal literacy.',
    'If sustainable (>2 generations): the constraint stabilizes as tangled_rope or even rope-like hybrid. If unsustainable (<1 generation): the constraint drifts toward snare as the ambiguity collapses and institutional leadership must choose between explicit reversal (admitting strategy) or explicit defense (forcing doctrinal renovation). Extractiveness will rise as the ambiguity erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_sustainability, empirical, 'Sustainability of scope-ambiguity legitimation strategy across generational time').

omega_variable(
    competing_reading_institutional_power,
    'Which of the three readings (hybrid_pragmatic, endogenous_reinterpretation, exogenous_override) has institutional power to define what ''the Manifesto really means'' for rank-and-file adherents?',
    'Institutional authority mapping: identify which institutional actors have the authority to adjudicate readings within the tradition; measure their alignment with each reading; track which reading is taught/enforced in official channels (sermons, catechesis, institutional communications). The reading with institutional enforcement power is the one that functions as canonical for adherent identity-formation.',
    'If leadership fully commits to endogenous_reinterpretation: the constraint becomes rope-like (doctrine evolved via revelation). If they fully commit to exogenous_override: the constraint becomes snare (doctrine unchanged, practice under duress). The hybrid reading persists only if institutional leadership maintains strategic ambiguity and prevents any reading from achieving full canonical status. Loss of institutional neutrality (choosing one reading) collapses the hybrid into one of its sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_institutional_power, empirical, 'Institutional power distribution across the three competing readings').

omega_variable(
    identity_locked_mechanism_specificity,
    'Is the identity-lock on rank-and-file adherents primarily cognitive (internalized the doctrine as core identity) or structural (embedded in kinship/community bonds that are materially costly to exit)?',
    'Ethnographic/survey data: measure exit barriers separately — (a) cognitive: can adherents articulate coherent alternative identities outside the tradition? Do they experience exit as identity death or as costly but livable relocation? (b) Structural: what material costs does exit carry (kinship loss, economic dependency, housing, social capital)? Dissect the two components to identify which is the binding constraint.',
    'If primarily cognitive: the constraint''s suppression can be reduced through identity-reframing (alternative narrative frames that preserve community while recontextualizing doctrine). If primarily structural: suppression persists regardless of cognitive work — exit remains materially impossible. The hybrid reading''s moderate suppression (0.58) assumes mixed mechanisms; pure cognitive lock would lower it, pure structural would raise it toward 0.85+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_specificity, empirical, 'Identity-lock mechanism: cognitive vs. structural binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_hyb_theater_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mcl_hyb_theater_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(mcl_hyb_theater_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(mcl_hyb_extract_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mcl_hyb_extract_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(mcl_hyb_extract_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mcl_hyb_suppress_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mcl_hyb_suppress_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(mcl_hyb_suppress_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% The marriage commitment legitimacy kernel decomposes into three constraints corresponding to three structurally distinct readings. Each reading has its own ε value, beneficiary/victim structure, and classification profile. The hybrid pragmatic reading is the institutional leadership's preferred frame; it preserves organizational authority while adapting doctrine through ambiguity. The three stories are linked via network.affects_constraints because each reading's institutional viability depends on the others' non-dominance — if institutional leadership explicitly committed to any single reading (endogenous or exogenous), the hybrid's strategic value would collapse. The constraint family instantiates the commitment-system's core mechanism: maintaining institutional legitimacy by sustaining irreducible interpretive ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional, 0.12).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
