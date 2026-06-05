% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Dispute
 *   domain: international_law/treaty_interpretation/dispute_resolution
 *
 * SUMMARY:
 *   United Nations Security Council Resolution 242 (1967) mandates
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict,' but the phrase 'from territories' (not 'the
 *   territories') creates a deliberate interpretive ambiguity regarding
 *   whether Israel must withdraw from all or some occupied territory. This
 *   constraint—the dispute over WHO HAS AUTHORITY TO RESOLVE the textual
 *   ambiguity—is distinct from the substantive dispute over what withdrawal
 *   actually means. The kernel is the text itself; the reading instantiated
 *   here focuses on the meta-institutional structure that prevents any single
 *   authority from settling the interpretation. The International Court of
 *   Justice claims judicial interpretation authority; the drafting states
 *   (particularly the US) claim superior access to authorial intent; the
 *   occupying state claims customary practice and operational control
 *   override textual interpretation. This reading
 *   (interpretive_authority_structure) models the constraint as a snare in
 *   which the meta-dispute prevents resolution of the substantive dispute,
 *   allowing all parties to maintain their preferred readings while remaining
 *   technically within 'legitimate interpretation.' The theater ratio
 *   increases over time as the interpretive debate becomes more purely
 *   performative and less focused on producing actual legal closure.
 *
 * KEY AGENTS:
 *   - International Court of Justice: Claims judicial interpretation authority (institutional/constrained) — issues advisory opinions that are not binding; constrained by need to maintain legitimacy while lacking enforcement power
 *   - Drafting States (US, USSR, UK, France): Claim authorial intent authority (institutional/arbitrage) — can invoke intent when convenient but cannot unilaterally impose interpretation; arbitrage between intent claims and practice realities
 *   - Occupying State (Israel): Claims customary practice and operational control (powerful/arbitrage) — can unilaterally assert its preferred reading through state behavior; high exit capacity and veto power
 *   - Non-occupying Treaty Signatories: Seek definitive legal closure (moderate/constrained) — trapped in the meta-dispute; bear cost of incoherence without power to resolve it
 *   - International Legal Community: Seeks coherent doctrine (organized/constrained) — cannot exit the ambiguity; consensus blocked at the root by meta-dispute
 *   - Analytical Observer: Views as inherent legal indeterminacy (analytical/analytical) — risks naturalizing contingent institutional arrangement as universal legal property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.68).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Dispute").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/treaty_interpretation/dispute_resolution").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '7c53c782-271a-4ecc-96d3-de41735ef7d2').
narrative_ontology:cs_kernel_codification('7c53c782-271a-4ecc-96d3-de41735ef7d2', fixed_text).
narrative_ontology:cs_authority_grounding('7c53c782-271a-4ecc-96d3-de41735ef7d2', extraction).
narrative_ontology:cs_interpretation_layer_present('7c53c782-271a-4ecc-96d3-de41735ef7d2').
narrative_ontology:cs_reading_relation('7c53c782-271a-4ecc-96d3-de41735ef7d2', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c53c782-271a-4ecc-96d3-de41735ef7d2', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('7c53c782-271a-4ecc-96d3-de41735ef7d2', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('7c53c782-271a-4ecc-96d3-de41735ef7d2', interpretive_authority_is_contested, empirically_contingent).
narrative_ontology:cs_axiom('7c53c782-271a-4ecc-96d3-de41735ef7d2', foundational, multiple_overlapping_authorities_block_closure).
narrative_ontology:cs_axiom_status(multiple_overlapping_authorities_block_closure, holdable).
narrative_ontology:cs_axiom_grounding('7c53c782-271a-4ecc-96d3-de41735ef7d2', multiple_overlapping_authorities_block_closure, conventional).
narrative_ontology:cs_reference_frame('7c53c782-271a-4ecc-96d3-de41735ef7d2', unified_interpretive_authority).
narrative_ontology:cs_drift_state('7c53c782-271a-4ecc-96d3-de41735ef7d2', contemporary_post_1990s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c53c782-271a-4ecc-96d3-de41735ef7d2', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_with_veto_power).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_community_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTIES SEEKING CLOSURE (SNARE) — States and non-state actors seeking definitive resolution of withdrawal obligations face a trap: no authority has the power to resolve the meta-dispute over who interprets the treaty. The ICJ can issue advisory opinions that are not binding; drafting states' claims to authorial intent lack enforcement; occupying states can assert customary practice unilaterally. No exit from the ambiguity without accepting one party's authority claim — but all such claims are contested. Maximum extraction: the trapped party's legal certainty is perpetually hostage to the meta-dispute.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL LEGAL CONSENSUS (SNARE) — The abstract collective of legal scholars, jurists, and treaty adherents seeking coherent international law doctrine cannot exit the ambiguity. Consensus formation is blocked by the meta-dispute: no neutral arbiter can adjudicate competing authority claims. The legal community bears the cost of incoherence (contradictory precedents, forum shopping, custom erosion) with no mechanism to resolve it at the root. Generational time horizon: institutional legal doctrine is captive across decades.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: OCCUPYING STATE (ROPE) — The occupying state experiences the interpretive ambiguity as coordination: it can assert its preferred reading (customary practice / de facto control) while remaining technically within 'competing legitimate interpretations.' The ambiguity is not a constraint but a feature—it preserves maximum discretion. The state can arbitrage between the multiple authority claims, invoking whichever serves its interests at any moment. Net beneficiary with high exit capacity (can withdraw, can claim compliance, can reframe the debate).
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ICJ (TANGLED ROPE) — The ICJ experiences both genuine coordination and extraction from the meta-dispute. Coordination: it provides the institutional service of legal interpretation, enabling international dispute settlement. Extraction: its authority is perpetually questioned (states can ignore advisory opinions, drafting states claim superior authorial access, customary practice claims bypass judicial reasoning entirely). The ICJ has constrained exit—it cannot refuse to exist as an interpretive authority but its pronouncements remain advisory and contestable. The court both enables and suffers extraction from competing authority claims.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TREATY TEXT AS INSTITUTIONAL ARTIFACT (PITON) — The text of UNSC 242 persists as a formal object of reference while its functional interpretive role has atrophied. Multiple institutions (ICJ, state practice, diplomatic negotiation, UN organs) all claim to interpret it, yet none has decisive authority. The text is maintained through institutional inertia and performative reference (quoted in legal documents, cited in opinions) despite the loss of primary function (determining withdrawal obligations). Theater ratio is high: the text is invoked but its meaning is not determinative. Piton classification derives from degraded function maintained by institutional ceremony.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CUSTOMARY PRACTICE COALITION (SCAFFOLD) — International relations practitioners and diplomatic actors view the interpretive ambiguity as a temporary coordination failure being solved through accumulated state practice. Custom is being built through repeated patterns of withdrawal behavior, treaty amendments, and diplomatic precedent. This coalition has agency and sees a sunset: as customary practice crystallizes, the need for authoritative interpretation of the written text diminishes. Effective extraction is low because the coalition perceives an exit path (custom replacing written text) and has the power to shape its emergence.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational analytical perspective, the interpretive ambiguity is an inevitable property of language and treaty law: no written text can fully specify its own interpretation, and no single authority structure can adjudicate all competing framings without itself claiming a position within the dispute. The ambiguity is not contingent but structural—inherent to how international law works. However, this perspective risks naturalizing what is actually a specific institutional arrangement (multiple overlapping authority claims with no hierarchy). The engine will identify this as a false summit.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__interpretive_authority_structure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, TR),
    TR >= 0.70.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The ambiguity over who interprets the clause perpetuates substantive ambiguity, allowing the occupying state and veto-power holders to maintain their preferred readings indefinitely without legal determination. This is extractive because it denies legal closure to parties seeking it. The extraction increases over the interval (0.48→0.68) as the meta-dispute becomes more entrenched and purely performative. Suppression (0.72): High. Multiple mechanisms prevent resolution: (1) veto power of P5 states blocks UN institutional clarification, (2) inability to convene a treaty conference to amend or clarify without consensus, (3) ICJ authority limitations (advisory only, non-binding), (4) state sovereignty norm prevents external imposition of interpretation. Suppression rises over the interval as institutional mechanisms for closure are exhausted. Theater ratio (0.81): Very high and rising. The interpretive debate increasingly becomes performative: legal arguments are deployed to justify predetermined positions rather than to discover the text's actual meaning. The theater rises over time because the dispute is no longer genuinely about interpretation—it is about institutional authority and veto power, but all parties maintain the fiction that they are arguing about what UNSC 242 'really means.' The measurement trajectory (0.55→0.81) reflects the shift from substantive interpretive debate (1960s-1970s) to purely strategic deployment of competing authority claims (2000s-present).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a radical perspectival gap. Trapped parties and the international legal community see a snare with no exit (no authority can impose closure). The occupying state and veto holders see coordination with maximum discretion (rope or even scaffold). The ICJ sees a tangled rope: it provides the institutional coordination of dispute settlement but suffers perpetual extraction from contested authority. The treaty text itself appears as a piton: maintained through performative reference despite loss of functional interpretive role. The analytical observer risks seeing an immutable natural law (legal indeterminacy is inherent to language and treaty law) when in fact the constraint is a contingent institutional arrangement—multiple overlapping authority claims with no hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and veto-power holders benefit from the meta-dispute: ambiguity preserves their discretion (d ≈ 0.15, derived from beneficiary status + arbitrage exit). Parties seeking closure are trapped: they have no exit and cannot impose a resolution (d ≈ 0.92, derived from victim status + trapped exit). The ICJ occupies an intermediate position: it benefits from existing as an interpretive authority but suffers extraction from having its authority perpetually contested (d ≈ 0.55, derived from institutional power + constrained exit). The directionality derivation produces a snare classification for the powerless perspective (trapped parties) because their d value yields high f(d) ≈ 1.28, making χ = 0.68 × 1.28 × 1.0 ≈ 0.87, well above the snare threshold (χ ≥ 0.66). For the occupying state perspective (d ≈ 0.15), f(d) ≈ -0.01, making χ ≈ -0.01, which produces the rope classification—the constraint is beneficial, not extractive, from that vantage.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_hierarchy_possibility,
    'Is the absence of a unified interpretive authority inherent to international law, or is it a contingent feature of how UNSC 242 was drafted and has been litigated?',
    'Comparative analysis of other multiparty treaties (UN Convention on Law of the Sea, WTO agreements, regional human rights treaties) and their interpretive authority structures. Do they exhibit similar multi-source authority ambiguity, or do they establish clearer hierarchies?',
    'If inherent: mountain classification is correct (legal indeterminacy is a law of treaty interpretation). If contingent: the mountain is a false summit, and the snare is the accurate classification—the ambiguity serves the interests of powerful parties and is perpetuated instrumentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_hierarchy_possibility, conceptual, 'Whether interpretive authority ambiguity is structural or contingent to UNSC 242').

omega_variable(
    customary_practice_crystallization_timeline,
    'How many instances of state withdrawal behavior constitute ''crystallized custom'' sufficient to settle the interpretation question?',
    'Track documented state withdrawals from similar occupations over 50+ year period; analyze whether pattern shows convergence toward one reading (maximal vs. partial withdrawal). Assess whether legal literature converges on custom-based interpretation.',
    'If custom crystallizes within 30 years: scaffold perspective is correct and sunset is real. If no crystallization emerges after 50 years: custom remains contestable and the constraint persists as snare indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_practice_crystallization_timeline, empirical, 'Pace and definitiveness of customary practice crystallization on withdrawal interpretation').

omega_variable(
    veto_player_interest_alignment,
    'Do the parties with veto power over interpretive authority (P5 states, occupying state, major treaty signatories) have aligned or divergent interests in perpetuating the ambiguity?',
    'Archive analysis of diplomatic negotiation over UNSC 242 clarifications, amendments, and reinterpretations. Assess whether veto players have consistently blocked definitional closure or whether blocking has been incidental to other disputes.',
    'If interests are aligned in perpetuating ambiguity: the snare is intentional and stable (beneficiaries actively maintain it). If interests are divergent but veto structure prevents resolution: the snare is structural but not intentionally sustained—different classification implications for mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_player_interest_alignment, empirical, 'Whether veto players actively maintain or merely incidentally preserve interpretive ambiguity').

omega_variable(
    kernel_reading_relationship,
    'Which kernel reading (maximal, partial, or interpretive authority structure) is primary, and which are derivative?',
    'Historical-genealogical analysis: when was the withdrawal clause drafted, what ambiguity was it intended to address, and when did interpretive authority disputes become salient? Does the authority dispute derive from unresolved substantive readings, or vice versa?',
    'If authority dispute is primary (meta-dispute prevents substantive resolution): this reading (interpretive_authority_structure) correctly identifies the structural constraint. If authority dispute is derivative (arose from prior substantive disagreement): focus should shift to maximal_withdrawal_reading and partial_withdrawal_reading, with this reading as secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Logical and historical priority of interpretive authority dispute vs. substantive withdrawal readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is the legal indeterminacy from the analytical mountain perspective a genuine feature of treaty law universally, or a false summit naturalizing the specific institutional failure of UNSC 242?',
    'Analysis of successful multiparty treaties with definitive interpretive authority (WHO constitution, WTO Dispute Settlement, International Criminal Court statute). If those treaties manage determinate interpretation despite similar language indeterminacy, the mountain is false.',
    'If mountain is false: the constraint is a snare perpetuated by specific institutional arrangements (multiple overlapping authority sources with no hierarchy). If mountain is real: legal indeterminacy is inherent and inescapable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether treaty interpretation indeterminacy is universal natural law or contingent institutional failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242auth_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.55).
narrative_ontology:measurement(unsc242auth_tr_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 10, 0.68).
narrative_ontology:measurement(unsc242auth_tr_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(unsc242auth_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(unsc242auth_be_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(unsc242auth_be_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(unsc242auth_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(unsc242auth_su_t10, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(unsc242auth_su_t20, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause decomposes into three structurally distinct constraints, each with its own ε value. (1) interpretive_authority_structure (this story, ε=0.68, snare) — the meta-dispute over who interprets prevents resolution of the substantive dispute. (2) maximal_withdrawal_reading (ε≈0.10-0.25, mountain or rope) — if the text obligates complete withdrawal, that is a legal fact with determinate content. (3) partial_withdrawal_reading (ε≈0.30-0.50, tangled_rope or snare depending on perspective) — if the text permits selective withdrawal, that creates a coordination problem with extraction. The three readings are linked by the kernel: they all address the same text. The upstream constraint (interpretive_authority_structure) influences both downstream constraints: as long as no authority can settle which reading is correct, both substantive readings remain institutionally live, perpetuating the meta-dispute. If the interpretive authority dispute were resolved (e.g., ICJ authority accepted as binding, or custom crystallizes), the ε values for both downstream readings would drop sharply because their meaning would become determinate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
