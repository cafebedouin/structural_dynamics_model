% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense — Narrow Armed-Attack Reading (State Attribution Required)
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The narrow armed-attack reading of Article 51 confines lawful unilateral
 *   self-defense to responses to actual or imminent armed attacks by a state
 *   attributable under international law. It is the ICJ's position (Nicaragua
 *   1986, Oil Platforms 2003) and the mainstream doctrinal position of most
 *   states. This story instantiates ONE reading of the
 *   article_51_self_defense kernel; the expansive_preventive_reading and
 *   unable_unwilling_doctrine_reading siblings are separate constraint files
 *   with their own epsilon values, beneficiary structures, and
 *   classifications, linked through network.affects_constraints. The epsilon
 *   authored here is for the standing arrangement — the narrow reading as
 *   governing law — assessed by the reading's own lights: the reading does
 *   not deny that the constraint bites on strategic freedom; it regards that
 *   bite as a constitutional limit rather than predation. The claim/metric
 *   relationship is deliberate and independent: the claimed type
 *   (tangled_rope) is my structural read — genuine coordination function plus
 *   asymmetric extraction plus active enforcement — while the metrics
 *   describe the arrangement's actual operation without being tuned to any
 *   predicted engine output.
 *
 * KEY AGENTS:
 *   - great_powers: Primary target (powerful/arbitrage) — strategic freedom forgone; can defect at reputational cost and fund doctrinal challenge
 *   - states_attacked_by_nonstate_actors: Secondary target (moderate/constrained) — host-state remedy denied absent attribution; partially shielded in turn by the same rule
 *   - weaker_states: Primary beneficiary (organized/trapped) — legal shield against pretext intervention; bloc power in the General Assembly
 *   - host_states_of_nonstate_actors: Secondary beneficiary (powerless/trapped) — territorial shield they could never defend militarily
 *   - nonstate_armed_groups: Incidental beneficiary (moderate/mobile) — sanctuary preserved by the attribution threshold
 *   - un_security_council: Agenda-setter and receipt seat (institutional/constrained) — collects decision authority; capacity bounded by P5 vetoes
 *   - icj: Doctrinal agenda-setter (institutional/analytical) — maintains the reading through adjudication without coercive capacity
 *   - permanent_five_members: Dual-positioned payers (powerful/arbitrage) — constrained outside the Council, veto-protected inside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.48).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense — Narrow Armed-Attack Reading (State Attribution Required)").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'f7ab48d2-43c1-4cc2-a0db-24addb545701').
narrative_ontology:cs_kernel_codification('f7ab48d2-43c1-4cc2-a0db-24addb545701', fixed_text).
narrative_ontology:cs_authority_grounding('f7ab48d2-43c1-4cc2-a0db-24addb545701', lineage).
narrative_ontology:cs_interpretation_layer_present('f7ab48d2-43c1-4cc2-a0db-24addb545701').
narrative_ontology:cs_reading_relation('f7ab48d2-43c1-4cc2-a0db-24addb545701', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('f7ab48d2-43c1-4cc2-a0db-24addb545701', article_51_self_defense__unable_unwilling_doctrine_reading, forecloses).
narrative_ontology:cs_axiom('f7ab48d2-43c1-4cc2-a0db-24addb545701', foundational, state_attribution_required).
narrative_ontology:cs_axiom_status(state_attribution_required, holdable).
narrative_ontology:cs_axiom_grounding('f7ab48d2-43c1-4cc2-a0db-24addb545701', state_attribution_required, conventional).
narrative_ontology:cs_axiom('f7ab48d2-43c1-4cc2-a0db-24addb545701', foundational, armed_attack_imminence_threshold).
narrative_ontology:cs_axiom_status(armed_attack_imminence_threshold, holdable).
narrative_ontology:cs_axiom_grounding('f7ab48d2-43c1-4cc2-a0db-24addb545701', armed_attack_imminence_threshold, conventional).
narrative_ontology:cs_axiom('f7ab48d2-43c1-4cc2-a0db-24addb545701', secondary, collective_security_primacy).
narrative_ontology:cs_axiom_status(collective_security_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f7ab48d2-43c1-4cc2-a0db-24addb545701', collective_security_primacy, conventional).
narrative_ontology:cs_reference_frame('f7ab48d2-43c1-4cc2-a0db-24addb545701', charter_collective_security_settlement).
narrative_ontology:cs_drift_state('f7ab48d2-43c1-4cc2-a0db-24addb545701', post_911_state_practice_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f7ab48d2-43c1-4cc2-a0db-24addb545701', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, host_states_of_nonstate_actors).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, nonstate_armed_groups).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, great_powers).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_attacked_by_nonstate_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, states_attacked_by_nonstate_actors).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, permanent_five_members).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, effective_control_attribution_standard).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, charter_collective_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the decision monopoly this reading preserves: force responses beyond immediate armed-attack self-defense must route through its authorization, and every use of force it does not authorize is presumptively unlawful. It collects the decision authority the rule channels to it. Its capacity to exercise that authority is bounded by the permanent five's vetoes, so its collection is sometimes nominal rather than operative.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Maintains the reading through adjudication: Nicaragua v. United States (1986) rejected the unable-or-unwilling argument and fixed the effective-control attribution standard; Oil Platforms (2003) tightened the armed-attack analysis. Its judgments are the rule's doctrinal enforcement, though it cannot compel compliance — the United States terminated its acceptance of ICJ jurisdiction after losing Nicaragua.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, icj, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, icj, observer).

% Small and medium states that cannot deter intervention by their own power. The narrow reading is their principal legal shield: it denies powerful states a self-defense justification that does not rest on an actual armed attack, and it keeps force decisions in a body where their votes and bloc organization (Non-Aligned Movement, G77 majorities in the General Assembly) count. Leaving the Charter regime would cost them the shield itself, so they defend the reading in the General Assembly and before the ICJ.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    organized, generational, trapped, global).

% States on whose territory organized armed groups operate — typically because they cannot suppress them (Lebanon with Hezbollah, Pakistan with cross-border militant networks, Syria during the ISIS years). The reading shields their territory from cross-border attack unless the groups' conduct is attributable to them under the effective-control standard, which is rarely satisfied. They receive protection they could never defend militarily; their inability to control the groups is precisely what the shield covers.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, host_states_of_nonstate_actors, beneficiary,
    powerless, biographical, trapped, regional).

% Organized armed groups that launch attacks from host-state territory. The reading protects their sanctuary: an attacked state may lawfully strike the group's forces directly, but cannot strike the host state's territory or institutions unless attribution is established. Their cross-border mobility lets them hold the sanctuary while keeping the host state's control below the attribution threshold.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, nonstate_armed_groups, beneficiary,
    moderate, immediate, mobile, regional).

% States with global force-projection capacity whose strategic freedom the reading constrains: they may not act against emerging threats, against non-state actors on host territory, or preventively, however strong their necessity claims. They bear the heaviest costs when facing threats that originate in states they could overpower militarily. Their exit is arbitrage-grade: they can act outside the law and absorb the reputational cost (the 2003 Iraq invasion proceeded without Council authorization and without legal consequence), and they can fund doctrinal challenges through state practice and legal scholarship.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, great_powers, payer,
    powerful, generational, arbitrage, global).

% States that suffer armed attacks launched by non-state groups from neighboring or host territory — Israel from Lebanon, Turkey from northern Iraq and Syria, India from Pakistan-based networks, the United States after September 2001. The reading gives them a right of self-defense against the group itself but denies the host-state remedy unless attribution is established; they absorb repeated attacks from territory the rule places off-limits. The same rule also shields them from pretextual intervention by their own more powerful neighbors, which is the benefit side of their position.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_attacked_by_nonstate_actors, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, states_attacked_by_nonstate_actors, beneficiary).

% The veto-holding architects of the 1945 settlement. They are constrained in unilateral action like other powerful states, but they hold a veto inside the body the rule empowers: they can block authorization against themselves and their clients, so the collective channel is partly their own instrument. Their position is the arrangement's sharpest internal tension — the strategic freedom they forgo outside the Council returns to them as blocking power inside it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, permanent_five_members, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, permanent_five_members, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of unilateral war: by confining lawful self-defense to responses to actual or imminent armed attacks by states, it prevents the self-defense exception from swallowing the prohibition on force (Article 2(4)), denies powerful states pretext cover for intervention, and channels force decisions to a collective body in which weaker states hold votes.
% TRANSFER_FUNCTION: Moves decision authority over lawful force from powerful states acting unilaterally to the Security Council acting collectively; moves legal protection from powerful states' strategic discretion to weaker states' territorial security.
% ABSENT_VOICES: Non-state armed groups and the civilian populations of host states have no seat in the doctrine's formation. States attacked by non-state actors are formally present in state practice, but their remedy claims are subordinated by the ICJ's attribution standard. The populations the rule protects — those of states that would be invaded under expansive readings — are represented only indirectly through their governments.
% DISAPPEARANCE_RATIONALE: If the narrow reading dissolved overnight — if self-defense reverted to a free-standing right judged by each state's own necessity — weaker states would lose their principal shield, the Council's force monopoly would collapse, and powerful states would reclaim unilateral discretion against emerging threats and host-state sanctuaries. The post-1945 prohibition architecture would unravel with it, because a self-defense exception without a defined trigger swallows the rule it qualifies.
% FOUNDING_PROBLEM: The interwar failure: the Kellogg-Briand Pact prohibited war but left self-defense undefined, and aggressor states dressed wars of conquest as self-defense — the Nuremberg tribunal had to reject Germany's expansive claims article by article. The Charter's drafters therefore needed a self-defense exception precise enough that it could not serve as a loophole: hence 'armed attack' as the trigger, and a state attributable under international law as its subject.
% FOUNDING_PROBLEM_CORROBORATION: Nuremberg and Tokyo jurisprudence — which predates the current beneficiary bloc — attests the founding problem: the IMT rejected expansive self-defense claims as cover for aggression. The rule's targets corroborate it from the other side: great powers repeatedly argue within the armed-attack framework (the United States framed its 2001 invocation as a response to an armed attack rather than openly abandoning the trigger) instead of ignoring it — compliance-seeking behavior by the paying seats is corroboration from outside the beneficiary set. Historians of the San Francisco drafting conference document the trigger's deliberate narrowness.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.55: the constraint's cost is real and asymmetric — concentrated on great powers (forgone strategic freedom) and on states attacked by non-state actors (denied host-state remedy) — but it is reciprocal (all states are bound) and was willingly founded in 1945. Suppression 0.48: the rule forecloses alternatives legally, but its coercive teeth are weak; enforcement runs through ICJ adjudication, Council censure, and reputational cost, and states demonstrably CAN defect (Iraq 2003) — the alternative is priced, not physically closed, which is why accessibility_collapse is a modest 0.38. Theater 0.44: a large share of the enforcement machinery is performative — Council debates that end in veto, resolutions without consequence — while the doctrinal constraint still binds state justification: even violators argue within the armed-attack framework. The theater series shows a regime-driven dip-and-rise (Cold War veto paralysis, Kuwait-era Council activism, post-2003 paralysis return), not an oscillating extraction mechanism. The suppression_requirement series rises monotonically: as powerful states pushed unable-or-unwilling and preventive claims, the doctrinal enforcement effort (ICJ pushback, General Assembly majorities, collective censure) had to intensify to hold the reading — this is an enforcement-ratchet story, which is why suppression_requirement is tracked alongside the other metrics on one shared time grid. Suppression here is authored as the raw structural property it is — unscaled; only extractiveness gets scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the weaker-state seat the arrangement is close to pure coordination: protection without cost, defended by their Assembly majorities. From the great-power seat it is a binding constraint on strategic freedom — real, asymmetric, and resented, but defection is affordable (arbitrage-grade exit), which damps its experienced force. From the attacked-state seat the arrangement is harshest: they pay (absorbed attacks, denied remedy) while receiving only the diffuse shield against pretext — the seat where a snare-like reading of the same structure is most plausible. The permanent five sit at the same nominal power level as other great powers but experience the constraint differently because the veto converts the collective channel into a partly owned instrument — the same-level differentiation the engine should surface from identical power atoms and different structural relationships. The ICJ seat sees constitutional maintenance; the Council seat sees collected authority bounded by its own veto politics.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: weaker_states (organized, trapped — no exit from the regime that shields them), host_states_of_nonstate_actors (powerless, trapped — the shield subsidizes precisely their incapacity), nonstate_armed_groups (moderate, mobile — the attribution threshold preserves their sanctuary), and un_security_council (institutional — the rule channels decision authority to it). Victim declarations drive high directionality: great_powers (powerful — but arbitrage-grade exit places them short of full-target, since they can defect and survive) and states_attacked_by_nonstate_actors (moderate, constrained — trapped by the attack itself; their secondary beneficiary position is real but secondary, so no override is authored: the derivation from the primary victim declaration plus constrained exit captures their net position). No directionality overrides are used: every seat's derived directionality follows from its declared role, power, and exit options, and the one genuinely mixed seat (permanent_five_members, payer with beneficiary secondary) is left to the derivation rather than hand-tuned. Global spatial scope applies to the state seats and modestly amplifies effective extraction for targets, as the engine owns.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing the self-defense exception from swallowing the prohibition on force — is live: pretext-war risk has not gone away, and the constraint's function is operative, so this is not a mandate outliving its function and mandatrophy is not resolved. The tangled_rope claim is what prevents mislabeling in both directions: a pure-rope reading would miss the real, concentrated cost borne by great powers and attacked states; a snare reading would miss the genuine coordination function that weaker states would lose first and that the paying seats themselves still argue within. The trajectory risk the measurements track: if Council paralysis becomes total and the effective-control attribution standard stays as demanding as Nicaragua set it, the coordination function decays while the constraint's costs persist — the theater_ratio series rising past 0.4 is exactly that symptom. At that limit the arrangement drifts toward theatrical maintenance of a rule whose collective channel no longer functions (piton-shaped from the system seat) or toward pure extraction from the attacked-state seat, which is why the remedy-gap omega below is the story's most consequential open question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This story instantiates the narrow_armed_attack_reading of the article_51_self_defense kernel: how would the classification and epsilon change under the expansive_preventive_reading or unable_unwilling_doctrine_reading siblings, and is the narrow reading''s profile stable across the contest?',
    'Author and classify the sibling readings as separate constraint stories and compare per-seat classifications and epsilon across the family. If the siblings produce materially different victim sets and epsilon, the colloquial label is confirmed as covering multiple constraints and the family decomposition stands.',
    'Under the expansive reading, host states and their civilian populations enter the victim set (pretext risk) while great powers move toward the beneficiary side — the extraction structure inverts. Under the unable-unwilling reading, host states flip from shielded beneficiaries to exposed targets. The narrow reading''s classification does not transfer to either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    remedy_gap_under_council_paralysis,
    'When the Security Council is veto-paralyzed, does the narrow reading leave states attacked by non-state actors with no lawful remedy — converting the coordination benefit into a pure cost for that seat?',
    'Code the rate at which actual or imminent armed attacks receive Council authorization versus go unremedied across the interval, cross-referenced against the theater_ratio series; a chronic gap with rising theater indicates the collective channel is not delivering the protection side of the bargain to attacked-state seats.',
    'If the remedy gap is chronic, the arrangement''s classification from attacked-state seats approaches pure extraction, the coordination-function gate weakens toward theatrical maintenance, and the drift trajectory bends toward the piton/snare boundary the mandatrophy analysis flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_gap_under_council_paralysis, empirical, 'Whether the collective channel actually delivers the remedy that justifies the constraint''s costs to attacked states.').

omega_variable(
    attribution_standard_determinacy,
    'Is the effective-control attribution standard (Nicaragua) determinate enough to operate, or so demanding that the host-state shield approaches absoluteness in practice?',
    'Code attribution determinations across ICJ jurisprudence, commission-of-inquiry findings, and state practice: how often has attribution been established against a host state for a non-state actor''s armed attack, and under what evidentiary conditions?',
    'If attribution is effectively unattainable, extraction from attacked-state seats is higher than authored and the host-state subsidy is larger; if attribution is workable in defined conditions, the constraint''s balance between shield and remedy holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_standard_determinacy, empirical, 'Whether the attribution threshold functions as a workable test or a near-absolute shield.').

omega_variable(
    compliance_mechanism_ambiguity,
    'Is state compliance with the narrow reading structural (fear of censure, reciprocal exposure, Council procedure) or internalized (norm legitimacy accepted by legal advisers and militaries regardless of enforcement)?',
    'Post-enforcement-decay trajectory: if compliance behavior holds as Council capacity and ICJ acceptance erode further, internalization dominates; if compliance tracks enforcement capacity one-for-one, the constraint is purely structural and decays with its machinery.',
    'Internalized compliance means the constraint persists even as enforcement theater rises — supporting rope-like persistence beneath the tangled surface. Purely structural compliance means the arrangement decays with its enforcement capacity and the theater_ratio series is a leading indicator of dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Whether the constraint''s hold on state behavior is structural enforcement or internalized legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51_narrow_reading_tr_t0, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t0, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t10, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t10, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t20, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t20, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t30, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t30, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t40, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t40, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t50, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t50, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t60, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t60, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t70, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 70, 0.41).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t70, observed).
narrative_ontology:measurement(a51_narrow_reading_tr_t80, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement_basis(a51_narrow_reading_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(a51_narrow_reading_be_t0, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t0, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t10, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t10, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t20, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t20, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t30, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t30, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t40, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t40, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t50, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t50, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t60, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t60, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t70, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 70, 0.54).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t70, observed).
narrative_ontology:measurement(a51_narrow_reading_be_t80, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement_basis(a51_narrow_reading_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(a51_narrow_reading_su_t0, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t0, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t10, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t10, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t20, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t20, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t30, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t30, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t40, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t40, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t50, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t50, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t60, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t60, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t70, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 70, 0.46).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t70, observed).
narrative_ontology:measurement(a51_narrow_reading_su_t80, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement_basis(a51_narrow_reading_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_2_4_force_prohibition).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 51 self-defense' covers three structurally distinct constraints — readings of one fixed-text kernel — and is decomposed per the epsilon-invariance principle into three stories: this narrow armed-attack reading (attribution required, trigger confined to actual or imminent attack), the expansive preventive reading (trigger extended to emerging threats), and the unable-unwilling doctrine reading (attribution displaced by host incapacity). Each carries its own epsilon, beneficiaries, and victims; measuring the kernel through different readings yields different epsilon values, which is the signal that these are different constraints, not one constraint under different observables. This reading is the upstream member: the ICJ's Nicaragua-line enforcement of the narrow reading is the doctrinal authority the sibling readings attack, so classification drift here propagates to both siblings' legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
