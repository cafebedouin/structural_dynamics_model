% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor-Violence Legitimacy (Drop Reading)
 *   domain: social/legal/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the 'drop reading' of honor-violence
 *   legitimacy: dueling remained a conceptually available, structurally
 *   legitimate mechanism for defending honor (authorized by honor codes,
 *   recognized by practitioners as a valid recourse) even as its actual
 *   frequency declined due to external costs. State criminalization, military
 *   losses to dueling practice, economic disruption, and growing social taboo
 *   raised the cost of participation without delegitimizing the mechanism
 *   itself. A disputant in this period could still be held to the code and
 *   face shame for refusing a challenge, even though accepting carried legal
 *   and life-threatening consequences. The reading emphasizes the gap between
 *   what the honor code treats as legitimate and what people actually do — a
 *   snare in which the mechanism remains thinkable even as participation
 *   becomes increasingly costly.
 *
 * KEY AGENTS:
 *   - honor_code_practitioners: maintain and defend dueling as legitimate honor mechanism (regional organized power, identity_locked exit)
 *   - dispute_parties: trapped between accepting insult or accepting violence (powerful to powerful, trapped exit)
 *   - collateral_injury_bearers: family and companions of disputants, bearing injury costs with no voice (powerless, trapped by kinship)
 *   - state_authorities: criminalize and enforce the ban, raising external costs without changing the code's legitimacy (institutional, constrained exit)
 *   - contraction_reading_advocates: excluded, would redefine honor to exclude violence (organized, constrained but excluded from authority)
 *   - analytical observer: measures the gap between conceptual legitimacy and practice frequency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.42).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, snare).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor-Violence Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "social/legal/commitment_system").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '7ee39aba-0b13-4253-ace3-ad57c2f3ea0e').
narrative_ontology:cs_kernel_codification('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', distributed).
narrative_ontology:cs_authority_grounding('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', practice).
narrative_ontology:cs_interpretation_layer_present('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e').
narrative_ontology:cs_reading_relation('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', foundational, honor_code_authorization_persists).
narrative_ontology:cs_axiom_status(honor_code_authorization_persists, holdable).
narrative_ontology:cs_axiom_grounding('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', honor_code_authorization_persists, conventional).
narrative_ontology:cs_axiom('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', foundational, external_costs_suppress_practice).
narrative_ontology:cs_axiom_status(external_costs_suppress_practice, holdable).
narrative_ontology:cs_axiom_grounding('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', external_costs_suppress_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', classical_honor_code_authority).
narrative_ontology:cs_drift_state('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', criminalization_and_taboo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ee39aba-0b13-4253-ace3-ad57c2f3ea0e', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_code_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, dispute_parties).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, collateral_injury_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, state_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and defend the duel as the legitimate mechanism for resolving insult and defending social standing. Their identity is fused with honor codes; accepting non-violent remedies is experienced as acceptance of permanent shame. They administer duel protocols and control whether a slight constitutes a challenge. As the constraint operates, they collect no material rents but retain control over a status-allocation mechanism.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_code_practitioners, agenda_setter,
    organized, biographical, identity_locked, regional).

% Insulted parties face the choice: accept reputational damage (permanent social diminishment) or accept the duel challenge and face death or serious injury. Once the challenge is issued and accepted, exit is structurally impossible — refusing is equivalent to accepting the insult. High-status dispute parties are trapped by their own standing; low-status parties are trapped by vulnerability to exploitation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dispute_parties, payer,
    powerful, biographical, trapped, regional).

% Family members, companions, and bystanders who bear the costs of injuries and deaths from duels. They have no voice in the challenge decision and no exit — they are trapped by kinship or proximity to a disputant. They receive no status benefit from the duel.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, collateral_injury_bearers, payer,
    powerless, immediate, trapped, local).

% Criminalize dueling and enforce that criminalization, raising the external cost (legal penalty, exile, execution risk). They do not participate in duels but bear the cost of administering the ban and live with the non-compliance as a fact of regional governance. Their enforcement capacity is constrained by the honor code's deep cultural roots.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, state_authorities, payer).

% Intellectual and religious authorities who redefine honor to exclude violence — arguing dueling is incompatible with emerging definitions of honor as moral virtue, rational dignity, or Christian principle. They are excluded from the arena where honor codes are authoritatively defined; if present, they would demand a different reading of what honor is and whether violence serves it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, contraction_reading_advocates, excluded,
    organized, generational, constrained, regional).

% Analysts and historians who document the constraint as operating under this reading: dueling remains structurally legitimate (unconditionally authorized by honor codes) even as its frequency drops due to external costs (legal penalties, military losses, social taboo growth, economic disruption). They measure the gap between conceptual availability and practice frequency.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, drop_reading_observers, observer,
    analytical, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, honor_code_practitioners).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — dueling is a pure status-allocation mechanism, not a coordination problem solver.
% TRANSFER_FUNCTION: Transfers status (reputation, social standing, lineage honor) from the insulted party to the duelist who accepts and wins the challenge. Also transfers injury risk, death risk, and family disruption from challenge-issuers to disputants and their dependents.
% ABSENT_VOICES: Contraction-reading advocates (intellectuals, clergy, foreign observers) who reject the legitimacy of violence-as-honor would argue honor can be defended without dueling. They are excluded from the authoritative redefinition of honor within honor-code communities.
% DISAPPEARANCE_RATIONALE: If the duel-as-honor-remedy vanished, dispute parties would shift to non-violent remedies (legal proceedings, public apology, reputation repair through alternative mechanisms). The status-allocation system would be rerouted. The trap binding disputants to violence would dissolve.
% FOUNDING_PROBLEM: Honor codes emerged to resolve disputes and allocate status in societies lacking centralized legal authority. Dueling formalized the mechanism: ritualized combat determined whether an insult stood, who bore shame, and who retained standing.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and contraction-reading advocates both attest that centralized legal systems now handle dispute resolution. Historical evidence from jurisdictions that criminalized dueling shows legal remedies and alternative status-restoration pathways filled the functional gap. This reading explicitly acknowledges the founding problem is dead while the constraint persists.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint forces disputants into a harm-trap: refusing results in permanent status loss; accepting results in death or injury risk. The constraint collects this extraction by fusing identity with honor codes — exit requires not just leaving the practice but abandoning one's social self. Suppression is moderate (0.42) because the constraint does not rely primarily on coercion from above; it relies on the disputants' internalized identity fusion with the code. State enforcement (criminalization) adds suppression over time, but the core suppression mechanism is the disputant's own inability to imagine stepping outside the honor frame. Theater rises from 0.3 to 0.55 as the actual practice frequency drops: the code continues to authorize duels, but fewer duels occur, so more of the remaining activity is maintenance of the code's authority (ritual, rhetoric, public discourse about honor) relative to actual harm-producing practice. The measurement series track one shared time grid across all three metrics.
 *
 * PERSPECTIVAL GAP:
 *   The honor-code practitioners and the state authorities occupy opposite structural positions. From the practitioners' seat, dueling remains legitimate and necessary — refusal to accept a challenge is a real status loss in their framework; the constraint operates as it should. From the state authorities' seat, the same constraint is a persistent problem they must suppress, whose legitimacy they actively contest. From the disputants' seat, the constraint is a trap: the code's legitimacy (which they internalize) and the state's enforcement (which threatens them) operate in tension, leaving no exit. The engine computes these divergent classifications from the authored structural data; the commentary does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are honor-code practitioners: they control the mechanism, maintain its authority, and use it to allocate status. Their directionality is near the beneficiary end (d low). Victims are dispute parties (trapped between shame and violence) and collateral bearers (injury without choice). Their directionality is at the target end (d high). State authorities are partially captured into the constraint: they must administer the ban, invest enforcement resources, and live with non-compliance; they are not pure beneficiaries, though they also do not pay the core extraction cost. Their d sits mid-range, reflecting constrained participation in enforcement without direct harm from the mechanism. The identity_locked exit for practitioners and trapped exit for disputants are critical to the high extractiveness: the constraint's force rests on the inability to leave the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly addresses mandatrophy: the founding problem (dispute resolution in stateless societies) is dead — centralized legal systems now handle disputes. The constraint persists not because the problem exists but because the identity fusion with honor codes keeps the mechanism thinkable as legitimate. The high theater ratio rising over time marks the constraint's shift from functional (solving a real coordination problem) to theatrical (maintaining an authority structure whose function has been supplanted). The reading distinguishes this from the contraction reading by NOT claiming the concept of honor itself has been redefined — the drop reading holds that honor codes remain as they were (dueling remains legitimate within them) while external costs and competing legitimacy systems reduce practice frequency. This is a snare because the mechanism persists as authoritative even as participation becomes increasingly costly, trapping those bound by the code.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_coercion,
    'Is the measured suppression (0.42) primarily structural (external barriers, legal penalties, death risk) or primarily internalized (identity fusion with honor codes, inability to imagine stepping outside the frame)?',
    'Post-criminalization behavior in jurisdictions that eventually decriminalized dueling (or where enforcement lapsed): if dueling remains rare despite legal decriminalization, suppression was largely internalized; if it rebounds, suppression was primarily external coercion.',
    'If internalized, the constraint''s true suppressive force is higher than the raw measure suggests — the target carries the suppression with them even after legal prohibition ends. If external, the constraint becomes less extractive once the state enforcement mechanism weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_coercion, empirical, 'Structural vs. internalized suppression in honor codes.').

omega_variable(
    reading_boundary_ambiguity,
    'Is the distinction between the drop_reading (practice drops, code stays the same) and the contraction_reading (code is redefined) a real empirical difference, or does any successful conceptual redefinition look, in practice, like practice decline?',
    'Examine historical sources for explicit statements by authorities about whether honor codes were being redefined or merely becoming harder to follow: contemporaneous debate over the meaning of honor vs. debate over the costs of dueling.',
    'If the distinction collapses, the two readings are the same constraint read through different narratives, not two structurally distinct constraints. If the distinction holds, the drop and contraction readings have different ε values because they assign extraction to different mechanisms (code authority vs. conceptual shift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether practice decline and code redefinition are empirically distinguishable or narrative framings of the same process.').

omega_variable(
    collective_identity_lock,
    'Can honor-code practitioners genuinely exit the frame by individual choice, or is the identity lock collective — leaving requires the community to accept an alternative frame?',
    'Cases of individual practitioners who rejected dueling and were accepted as honorable through alternative status mechanisms (legal career, religious conversion, patronage networks) vs. those who were permanently diminished in standing for refusing the code.',
    'If individual exit is possible, directionality for practitioners should reflect choice (d lower, less pure extraction). If exit requires collective frame-shift, the identity lock is genuine and directionality remains at the beneficiary end (practitioners control the code, anyone who leaves is out).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_identity_lock, empirical, 'Whether identity lock to honor codes is individual or collective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hono_tr_t3, honor_violence_legitimacy__drop_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(hono_tr_t6, honor_violence_legitimacy__drop_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(hono_tr_t12, honor_violence_legitimacy__drop_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(hono_tr_t18, honor_violence_legitimacy__drop_reading, theater_ratio, 18, 0.53).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__drop_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hono_be_t3, honor_violence_legitimacy__drop_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(hono_be_t6, honor_violence_legitimacy__drop_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(hono_be_t12, honor_violence_legitimacy__drop_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(hono_be_t18, honor_violence_legitimacy__drop_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__drop_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hono_su_t3, honor_violence_legitimacy__drop_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(hono_su_t6, honor_violence_legitimacy__drop_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(hono_su_t12, honor_violence_legitimacy__drop_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(hono_su_t18, honor_violence_legitimacy__drop_reading, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__drop_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'honor_violence_legitimacy'. The kernel is the binding commitment to honor codes as normative systems. The drop_reading interprets the kernel as holding that dueling remains structurally legitimate even as external costs make practice rare (state criminalization, social taboo growth, economic disruption). The contraction_reading interprets the kernel as having been redefined — honor itself was reconceptualized to exclude violence. The composite_reading claims both mechanisms operated simultaneously. These are three different constraints (different ε values, different beneficiary structures, different readings of what changed) because they disagree on the core question: did the honor code itself change, or did external costs suppress a still-legitimate practice? All three readings grant that dueling declined in frequency; they differ on whether the code's authorization of dueling changed. The network links them as members of the same kernel family, but each story is analyzed independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
