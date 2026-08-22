% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence via Mutual Dual Legitimacy Recognition (1967 Boundaries)
 *   domain: political/territorial
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   territorial_legitimacy_dual. The reading accepts post-1948 legitimacy for
 *   Israeli statehood AND post-1948 Palestinian claims to statehood, resolves
 *   the partition via 1967 boundaries, limits return rights to the
 *   Palestinian state, and proposes security cooperation as the mechanism to
 *   replace zero-sum territorial competition. This is a coexistence reading
 *   that mutually affirms both peoples' legitimate presence. The constraint
 *   operates as a compromise framework: extractive for those whose
 *   territorial or return claims it forecloses (settlers, diaspora
 *   advocates), coordinating for those who accept bounded statehood and
 *   mutual recognition. The reading is neither the only defensible framing
 *   nor the only constraint this kernel instantiates; sibling readings
 *   include palestinian_autochthony_reading (grounding legitimacy in
 *   displacement and continuous habitation, without the 1967 boundary
 *   concession) and zionist_refuge_reading (grounding legitimacy in
 *   historical persecution and UN partition acceptance, without mutual
 *   recognition of equal Palestinian statehood). The engine computes per-seat
 *   classification from the structural data; this reading's metrics should
 *   diverge from the metrics of sibling readings because they have different
 *   ε values, beneficiary structures, and persistence conditions.
 *
 * KEY AGENTS:
 *   - Israeli government coexistence faction: Agenda-setter, institutional power, accepts 1967 boundaries and mutual recognition, constrained by domestic opposition
 *   - Palestinian Authority coexistence faction: Agenda-setter, organized power, accepts Israeli legitimacy and boundary compromise, constrained by return advocates
 *   - Settler movement actors: Payers, moderate power, identity-locked (territorial claim fused with ideological identity), frozen by boundary constraint
 *   - Palestinian return advocates: Payers, powerless, trapped by statelessness and diaspora status, diaspora return right foreclosed by this reading
 *   - International legal order: Non-agent beneficiary, vindicates self-determination and territorial integrity norms
 *   - Peace process mediators: Beneficiary institutions, constrain legitimacy to two-state framing, institutional power tied to reading's success
 *   - Security establishments: Dual payers/beneficiaries, must transition from adversarial to cooperative posture, constrained by operational difficulty
 *   - Diaspora Palestinian communities: Excluded (no voice in return-right framework), powerless, trapped globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence via Mutual Dual Legitimacy Recognition (1967 Boundaries)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '572e504c-0613-46ed-8bb7-a03e37fc93a9').
narrative_ontology:cs_kernel_codification('572e504c-0613-46ed-8bb7-a03e37fc93a9', fixed_text).
narrative_ontology:cs_authority_grounding('572e504c-0613-46ed-8bb7-a03e37fc93a9', distributed).
narrative_ontology:cs_reading_relation('572e504c-0613-46ed-8bb7-a03e37fc93a9', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('572e504c-0613-46ed-8bb7-a03e37fc93a9', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_axiom('572e504c-0613-46ed-8bb7-a03e37fc93a9', foundational, dual_legitimacy_post_1948).
narrative_ontology:cs_axiom_status(dual_legitimacy_post_1948, holdable).
narrative_ontology:cs_axiom_grounding('572e504c-0613-46ed-8bb7-a03e37fc93a9', dual_legitimacy_post_1948, deontological).
narrative_ontology:cs_axiom('572e504c-0613-46ed-8bb7-a03e37fc93a9', foundational, partition_by_1967_boundary).
narrative_ontology:cs_axiom_status(partition_by_1967_boundary, holdable).
narrative_ontology:cs_axiom_grounding('572e504c-0613-46ed-8bb7-a03e37fc93a9', partition_by_1967_boundary, conventional).
narrative_ontology:cs_reference_frame('572e504c-0613-46ed-8bb7-a03e37fc93a9', mutual_post_1948_statehood_framework).
narrative_ontology:cs_drift_state('572e504c-0613-46ed-8bb7-a03e37fc93a9', contemporary_post_2020_accords_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('572e504c-0613-46ed-8bb7-a03e37fc93a9', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_legal_order).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, peace_process_mediators).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_return_advocates).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, settlement_expansion_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_government_coexistence_faction).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_coexistence_faction).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, security_establishment_both_sides).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, settler_movement_actors).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, security_establishment_both_sides).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_integrity_norm).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__two_state_coexistence_reading, two_state_solution_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Endorses mutual recognition of Israeli statehood (post-1948) and Palestinian statehood (post-1967) as legitimate within their respective territorial bounds. Administers the security framework and negotiates terms. Collects the benefit of normalized regional relations and reduced military expenditure. Constrained by domestic pressure from settlers and security hawks who reject the concession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_government_coexistence_faction, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_government_coexistence_faction, beneficiary).

% Accepts Israeli legitimacy post-1948 and mutual recognition within 1967 borders. Administers Palestinian governance and security protocols. Collects the benefit of sovereign statehood, territorial administration, and reduced asymmetric conflict. Constrained by diaspora and return-advocate factions who view the compromise as betrayal.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_coexistence_faction, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_coexistence_faction, beneficiary).

% Bear the cost of territorial freezing at 1967 lines: settlements in occupied territories would remain frozen under the constraint, legal status ambiguous, expansion halted. Identity is fused with territorial presence (ideological claim of biblical/historical right to settlement). Exit would require abandoning the settlement enterprise and the identity framework built around it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, settler_movement_actors, payer,
    moderate, biographical, identity_locked, regional).

% Bear the cost of limiting right of return to the Palestinian state only: diaspora Palestinians (refugee camps, diaspora communities) cannot claim individual right to return to homes in what is now Israeli territory. Trapped by statelessness and lack of alternative legal standing; return advocates have no political power to enforce their claim within this reading's framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_return_advocates, payer,
    powerless, generational, trapped, regional).

% The reading vindicates self-determination and territorial integrity norms by resolving a high-salience exception case. If this reading becomes operative, the global precedent for two-state resolution strengthens the framework; if it fails, the exception erodes the norms. Not an actor, but a structural beneficiary of the constraint's legitimacy narrative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(territorial_legitimacy_dual__two_state_coexistence_reading, international_legal_order).

% UN, regional powers (Egypt, Jordan, Saudi Arabia), and external mediators (US, EU) collect institutional legitimacy and soft power from brokering the reading into operative status. Their careers and institutional mandates are tied to the two-state framing. Constrained by the fact that neither party can be forced to accept without cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, peace_process_mediators, beneficiary,
    institutional, biographical, constrained, global).

% Must transition from zero-sum adversarial posture to cooperative security architecture: intelligence sharing, joint border control, mutual early-warning systems. Pay the cost of dismantling adversarial command structures; benefit from reduced military expenditure and frontline risk. Constrained by the difficulty of operational transition and domestic suspicion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, security_establishment_both_sides, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, security_establishment_both_sides, beneficiary).

% Are structurally excluded from the negotiation framework: the reading fixes return rights at the Palestinian state boundary, not individual diaspora choice. They cannot voice claims within this reading's legitimacy structure and have no formal seat at negotiations, though their demographic weight shapes resistance from outside.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, diaspora_palestinian_communities, excluded,
    powerless, generational, trapped, global).

% Intellectual and policy communities that analyze whether the two-state reading is structurally sustainable, whether the dual legitimacy claim is operationally coherent, and whether security cooperation can replace zero-sum competition. Provide expertise but hold no enforcement power; assessments feed back to mediators and parties.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, liberal_internationalist_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, peace_process_mediators).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the allocation of territorial sovereignty and legitimate statehood: both Israeli and Palestinian peoples are recognized as legitimate inhabitants entitled to self-determined states within defined borders (1948 boundaries for Israel, 1967 for Palestine). Replaces zero-sum territorial competition with a bounded, negotiated partition.
% TRANSFER_FUNCTION: Moves territorial concessions, legal recognition, and security guarantees between the parties. Israel accepts Palestinian statehood and 1967 territorial boundaries (abandoning claims to occupied territories); Palestine accepts Israeli statehood post-1948 and limits return rights to Palestinian territory only. Both transfer security cooperation obligations and mutual recognition commitments.
% ABSENT_VOICES: Palestinian diaspora and return advocates are structurally excluded from this reading's legitimacy framework — they would argue for unconditional right of return and territorial restoration but are frozen out by the boundary-fixing logic of the constraint. Israeli settlement advocates are similarly excluded — they oppose the territorial freeze. The reading cannot be unanimous because its core mechanism is boundary-drawing that forecloses other claims.
% DISAPPEARANCE_RATIONALE: If the two-state coexistence framework vanished, territorial claims would revert to zero-sum competition: the question of legitimate ownership of pre-1967 territory would re-open, return rights would be re-contested, and security architecture would collapse to adversarial posture. Regional geopolitics, refugee law, and international legitimacy would reorganize around competing nationalist and autochthony claims.
% FOUNDING_PROBLEM: Post-1948, two peoples claim legitimate statehood and territorial sovereignty over overlapping geography. The problem is not the existence of either claim, but the absence of a mechanism to resolve competing legitimacy assertions without zero-sum conflict.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the two-state reading attest the problem remains live: territorial disputes and legitimacy contests persist despite decades of attempted resolution, requiring a framework that mutually recognizes both peoples. Critics (return advocates, settlement advocates, and some analysts) attest that the founding problem is unsolvable within this reading because it forecloses claims they regard as foundational; no corroboration from outside the reading's beneficiaries exists for the claim that this particular boundary-fixing is the solution.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction coefficient (0.58 baseline, rising to terminal 0.58) reflects the constraint's hybrid nature: genuine coordination function (partition resolution, mutual recognition, norm-vindicating) coupled with asymmetric costs imposed on return advocates and settlement advocates. Suppression (0.72 terminal) is high because the constraint's persistence depends on active exclusion of boundary-contest claims and return-right arguments — those voices must be silenced or sidelined for the framework to hold. Theater ratio (0.41 terminal) reflects moderate performative maintenance: the 'security cooperation' and 'mutual recognition' rhetoric masks continuing adversarial position-taking and the structural foreclosure of return claims. The measurement series track the trajectory from negotiation (t=0, lower extraction/suppression) toward operationalization (t=40, higher extraction/suppression as boundary-freezing hardens). At t=15 and beyond, measurements shift to projected basis because empirical observation stops at the last documented status; projections model what values would stabilize if the reading became operative. The constraint's terminal state is not a rope (genuine mutual benefit) but a tangled rope: coordination benefit for the boundary-accepting factions, extraction from those foreclosed by the boundary-fixing, and persistent enforcement required to maintain the boundary against return/expansion pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli coexistence faction seat, the constraint is genuine coordination and security framework. From the Palestinian Authority seat, it is a pragmatic compromise accepting concessions. From the settler seat, it is territorial confiscation and identity erasure. From the return advocate seat, it is foundational injustice. The engine computes these per-seat types from the structural relationship each stakeholder bears to the constraint — the authored claim (tangled_rope, coordination + extraction) sits between the coexistence factions' reading and the payer factions' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli coexistence faction and Palestinian Authority coexistence faction sit near symmetric directionality (d ≈ 0.5): each gives up territorial claims, each gains recognized statehood, each benefits from reduced conflict cost. However, they are NOT fully symmetric: the Israeli institutional seat has more power and better exit options (arbitrage into regional security agreements, technological leverage, diaspora capital), pushing d toward 0.4 (net beneficiary). The Palestinian Authority seat has constrained exit (dependent on Israeli recognition, limited economic leverage), pushing d toward 0.55 (near-symmetric but slightly target-facing). Settlers sit at high d (0.8+): identity-locked, trapped territorial exposure, no meaningful exit. Return advocates sit at highest d (0.9+): powerless, diaspora-trapped, structural foreclosure of their central claim. Mediators sit at low d (0.2+): they benefit from the reading's operationalization (institutional legitimacy) without bearing enforcement cost. The engine derives these from the stakeholder power/exit/beneficiary data; the commentary explains why the payer seats compute as substantially more extractive than the coexistence factions.
 *
 * MANDATROPHY ANALYSIS:
 *   The two-state reading is not subject to mandatrophy in the classical sense (founding problem dead but arrangement persists). Instead, it faces a different version: the founding problem (partition without zero-sum conflict) remains contested — return advocates argue it is unsolved, settlement advocates argue it is solved by Israeli expansion, coexistence factions argue their reading solves it. The persistence condition depends on active enforcement of boundary-freezing and return-right limitation, not on natural persistence or universal legitimacy. The constraint would decay (piton-ward) only if enforcement capacity eroded AND alternative framings (autochthony reading, refuge reading) filled the legitimacy vacuum. As long as the international legal order and mediators maintain the two-state framework as the normative default, the constraint persists. The measurement series show extraction rising (early negotiation is lower-extraction; operationalized boundary-freezing is higher-extraction), theater stable (security cooperation language is consistent), and suppression rising (boundary-holding requires more active foreclosure as time passes and alternative claims intensify). This pattern is consistent with a tangled_rope hardening under pressure, not decaying into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_freezing_legitimacy,
    'Is the 1967 boundary freeze itself a legitimate constraint on territorial claims, or is it merely a pragmatic checkpoint in an ongoing boundary-contest?',
    'Operational test: does intergenerational transmission of the boundary as ''natural'' or ''unjust'' occur? Do young Palestinians and Israelis raised in a two-state framework treat the 1967 line as foundational or temporary? Does international law treat it as terminal?',
    'If the boundary becomes naturalized across generations, the reading stabilizes and extraction moderates (payer accommodation). If it remains contested, extraction remains high and the constraint risks piton-ward decay or violent challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_freezing_legitimacy, conceptual, 'Whether the 1967 boundary is legitimated by time/consent or remains structurally contested.').

omega_variable(
    security_cooperation_feasibility,
    'Can Israeli and Palestinian security establishments actually transition from zero-sum adversarial posture to cooperative intelligence/border/early-warning architecture, given the history of betrayal and the structural incentive to cheat?',
    'Empirical: do pilot security arrangements (e.g., joint border patrols, intelligence sharing) persist for 5+ years without major incidents? Do both sides invest institutional capacity in the mechanism, or do they simulate cooperation while maintaining adversarial preparation?',
    'If security cooperation is structurally feasible, the constraint''s extraction cost moderates (security benefit is real), extraction becomes closer to coordination-dominated. If it collapses, the constraint becomes pure theater (high theater_ratio) and the arrangement reverts to coercive zero-sum (snare-ward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cooperation_feasibility, empirical, 'Whether security cooperation can replace zero-sum competition or remains theatre masking adversarial preparation.').

omega_variable(
    return_right_internalization,
    'Is suppression of diaspora return claims structural (external barriers: international law, state control) or partially internalized (diaspora communities accept the return limit as legitimate)?',
    'Post-framework empirical test: if the two-state reading became operational and diaspora return routes were formally closed, would diaspora communities remain mobilized for return, or would a generation of diaspora Palestinians raised in stateless stability come to accept the limit?',
    'If suppression is purely structural, the constraint''s effective suppression remains high (external force required). If internalization occurs, suppression moderates but the framework becomes culturally embedded and stable. If internalization fails, suppression must increase (higher enforcement cost) or the constraint yields to pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_right_internalization, empirical, 'Whether suppression of return claims is structural or becomes internalized across diaspora generations.').

omega_variable(
    sibling_reading_coexistence,
    'The three readings of this kernel (two-state coexistence, Palestinian autochthony, Zionist refuge) are held as incompatible by their respective factions. Does the two-state reading logically foreclose the other two within a single framework, or do they coexist as contestable interpretations of the same kernel?',
    'Conceptual analysis: the autochthony reading denies that mutual 1948 legitimacy is valid (Palestinian claims predate 1948, Israeli claims are post-hoc colonial); the refuge reading denies that Palestinian statehood is equivalent to Israeli legitimacy (refuge is foundational, statehood is derivative). The two-state reading asserts both legitimacies are equal post-1948. Do these premises logically contradict, or are they held by different parties without logical resolution?',
    'If foreclosure is real (logical contradiction), the engine reclassifies the two-state reading as a competitive snare (one reading imposed against the others'' core premises). If coexistence is real (different parties hold different readings without logical resolution), the classification remains tangled_rope (coordination + extraction, both present). The answer determines whether the reading can legitimately claim to ''solve'' the founding problem or only to impose a particular settlement on a genuinely open question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the two-state reading''s core axioms logically foreclose the sibling readings or coexist as contestable interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t5, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(terr_tr_t5, observed).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(terr_tr_t10, observed).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(terr_tr_t15, observed).
narrative_ontology:measurement(terr_tr_t25, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(terr_tr_t25, projected).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(terr_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t5, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(terr_be_t5, observed).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(terr_be_t10, observed).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(terr_be_t15, observed).
narrative_ontology:measurement(terr_be_t25, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(terr_be_t25, projected).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(terr_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t5, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(terr_su_t5, observed).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(terr_su_t10, observed).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(terr_su_t15, observed).
narrative_ontology:measurement(terr_su_t25, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(terr_su_t25, projected).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(terr_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.14).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the territorial_legitimacy_dual constraint family. The family comprises three readings of a single contested kernel: (1) palestinian_autochthony_reading — legitimacy grounded in continuous habitation, displacement trauma, unconditional return rights; (2) two_state_coexistence_reading — this reading — mutual recognition of dual legitimacy, 1967 boundary partition, limited return; (3) zionist_refuge_reading — legitimacy grounded in historical persecution, UN partition, potential expansion. Each reading has a distinct ε (extracted from the standing arrangement assessed by the reading's own lights), distinct beneficiary/victim structure, and distinct persistence conditions. The readings do not collapse into a single constraint with measurement ambiguity — they are three separate constraints with different types (autochthony and refuge are higher-extraction snare candidates; coexistence is tangled_rope). The family is linked via network.affects_constraints: shifts in the viability of one reading (e.g., international law erosion of the two-state framework) create structural pressure on the siblings' operative conditions but do not logically foreclose them. Decomposition per ε-invariance principle (OQ-258): the standing arrangement (territorial control, statehood claims, security posture circa 1967-present) is the same for all three readings; the readings' different ε values reflect different interpretations of extraction-vs-coordination within that same arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
