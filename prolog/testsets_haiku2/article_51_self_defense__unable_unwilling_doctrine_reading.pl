% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable/Unwilling Host State Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   The unable/unwilling doctrine reading of Article 51 establishes a legal
 *   framework permitting unilateral military action across borders when a
 *   non-state actor attack originates from a host state that lacks either the
 *   capacity or the political will to suppress the threat. This reading
 *   emerged prominently in post-9/11 security discourse as powerful states
 *   sought to conduct counterterrorism operations without Security Council
 *   authorization. The doctrine is claimed as a Tangled Rope: it coordinates
 *   legitimate use of force (the coordination function—states need a rule for
 *   when cross-border self-defense is permitted) while extracting sovereignty
 *   from host states (the asymmetric cost—host states lose territorial
 *   authority and control over foreign military operations). The referent
 *   under this reading is the standing arrangement of unilateral self-defense
 *   claims justified by host-state inability/unwillingness; this is assessed
 *   from the intervening states' own security logic (what they regard as
 *   necessary for counterterrorism mandate fulfillment). The reading does NOT
 *   describe what a rights-respecting or narrow-attack-only reading would
 *   endorse; those are different constraints, different ε values, authored
 *   separately.
 *
 * KEY AGENTS:
 *   - Intervening states with counterterrorism mandates (institutional power, arbitrage exit) — set the operational rules and determine when the doctrine applies
 *   - Host states with weak capacity (moderate power, constrained exit) — lose territorial sovereignty and bear foreign military operations they cannot prevent
 *   - Host states with contested sovereignty (powerful but politically constrained) — lose control over whether they suppress non-state actors, lose ability to make that choice deliberately
 *   - Non-state actor groups (organized, trapped) — their presence in host territory triggers the doctrine regardless of host-state complicity or capacity
 *   - International Court of Justice (institutional, analytical) — interprets Article 51 but has limited enforcement power against intervening states
 *   - UN Security Council (institutional, trapped) — bypassed by unilateral self-defense claims, especially when permanent members are the intervening parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable/Unwilling Host State Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '32599bd5-9d24-4679-95e3-824cf6e6bee0').
narrative_ontology:cs_kernel_codification('32599bd5-9d24-4679-95e3-824cf6e6bee0', fixed_text).
narrative_ontology:cs_authority_grounding('32599bd5-9d24-4679-95e3-824cf6e6bee0', lineage).
narrative_ontology:cs_interpretation_layer_present('32599bd5-9d24-4679-95e3-824cf6e6bee0').
narrative_ontology:cs_reading_relation('32599bd5-9d24-4679-95e3-824cf6e6bee0', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('32599bd5-9d24-4679-95e3-824cf6e6bee0', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('32599bd5-9d24-4679-95e3-824cf6e6bee0', foundational, non_state_actor_attack_suffices_for_self_defense).
narrative_ontology:cs_axiom_status(non_state_actor_attack_suffices_for_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('32599bd5-9d24-4679-95e3-824cf6e6bee0', non_state_actor_attack_suffices_for_self_defense, instrumental).
narrative_ontology:cs_axiom('32599bd5-9d24-4679-95e3-824cf6e6bee0', foundational, host_state_incapacity_or_unwillingness_permits_unilateral_response).
narrative_ontology:cs_axiom_status(host_state_incapacity_or_unwillingness_permits_unilateral_response, holdable).
narrative_ontology:cs_axiom_grounding('32599bd5-9d24-4679-95e3-824cf6e6bee0', host_state_incapacity_or_unwillingness_permits_unilateral_response, empirically_contingent).
narrative_ontology:cs_reference_frame('32599bd5-9d24-4679-95e3-824cf6e6bee0', article_51_state_self_defense_classical_framework).
narrative_ontology:cs_drift_state('32599bd5-9d24-4679-95e3-824cf6e6bee0', contemporary_counterterrorism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32599bd5-9d24-4679-95e3-824cf6e6bee0', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandate).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_weak_capacity).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, non_intervening_middle_powers).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, right_to_self_defense_survives_state_failure).
narrative_ontology:constraint_vindicates(article_51_self_defense__unable_unwilling_doctrine_reading, sovereign_immunity_conditional_on_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the operational rules for when the unable/unwilling doctrine applies. Determine what constitutes 'unable' (a host state's security capacity) and 'unwilling' (a host state's political choice not to act). Control the interpretation of necessity and can invoke the doctrine unilaterally without Security Council approval. Collect the operational benefit: ability to conduct cross-border counterterrorism operations and establish precedent for future interventions.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandate, agenda_setter,
    institutional, generational, arbitrage, global).

% Possess limited security infrastructure and state capacity. When non-state actors operate from their territory, they become targets for foreign military intervention. They cannot prevent the intervention because the doctrine defines their weakness as justification for it. They lose operational sovereignty: foreign military forces conduct strikes without their consent or control. They cannot strengthen their position by refusing to cooperate because refusal is then labeled 'unwillingness' and triggers intervention anyway.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_weak_capacity, payer,
    moderate, biographical, constrained, global).

% Possess contested political legitimacy: they may deliberately choose not to suppress non-state actors for factional reasons (the groups may align with sections of the regime, or suppression would destabilize the ruling coalition). The doctrine collapses this deliberate political choice into incapacity: 'unwillingness' is treated as equivalent to 'inability' and triggers foreign intervention regardless. They retain global-level diplomatic and military options but lose local-level choice over whether to suppress or tolerate specific non-state groups. They benefit secondarily if they can invoke the doctrine to justify actions they want to take, but this requires alignment with intervening-state norms.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_contested_sovereignty, beneficiary).

% Mount attacks that trigger the doctrine's application. Their presence in host-state territory—regardless of the host state's capacity or willingness—justifies foreign military strikes by intervening states. They have no formal standing in the doctrine's interpretation and no ability to contest determinations of 'unable' or 'unwilling'. Their own territorial presence becomes the mechanism of their targeting.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_groups, excluded,
    organized, biographical, trapped, global).

% Tasked with interpreting Article 51 and adjudicating state claims of self-defense. Has issued opinions (Nicaragua case) suggesting the doctrine's scope is narrower than intervening states claim but has limited enforcement power. Intervening states can proceed without deference to ICJ interpretation when political stakes are high. The Court remains the formal authority but faces a credibility gap when its interpretations are ignored.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, observer,
    institutional, generational, analytical, universal).

% Mandated by the Charter to authorize use of force and determine threats to peace. The unable/unwilling doctrine has created a parallel authorization channel that bypasses the Council: intervening states can claim self-defense without seeking Council approval. When the intervening state is a permanent member, it can additionally block any Council action to constrain the doctrine. The Council's relevance in security matters is thus diminished by the very doctrine that claims to operate within the Charter system.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, excluded,
    institutional, generational, trapped, universal).

% Can invoke the unable/unwilling doctrine when their own territories host non-state actors. They benefit from the ability to conduct unilateral counterterrorism without Security Council authorization. However, their ability to invoke the doctrine effectively depends on institutional credibility and alignment with the practice established by major intervening powers. They are constrained by the fact that the doctrine's application is not symmetric: major powers get deference; middle powers face greater scrutiny and pushback.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_intervening_middle_powers, beneficiary,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandate).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decision rule for when unilateral military action is legally justified: when a non-state actor attack has occurred and the host state is either incapable of suppressing it or unwilling to do so. This rule coordinates legitimate use of force by specifying conditions that justify cross-border military action without requiring Security Council authorization or direct state-on-state armed attack.
% TRANSFER_FUNCTION: Transfers territorial sovereignty from host states to intervening states: host states lose control over military operations in their territory; intervening states gain authority to determine what constitutes 'unable' or 'unwilling' and thus when to strike. Also transfers legitimacy: intervening states convert military operations into legal self-defense claims rather than unlawful intervention.
% ABSENT_VOICES: Weak host states are often unable to mount effective diplomatic resistance or bring claims to the ICJ; they lack the institutional capacity and international standing to contest doctrine interpretations. Non-state actors have no formal standing. Host states that are politically unwilling to suppress non-state actors have incentives to stay silent (admitting unwillingness undermines their own domestic authority) rather than contest the doctrine publicly. The doctrine is primarily articulated by security scholars and officials from intervening states; critical legal scholarship from host-state or non-aligned perspectives is marginalized in mainstream security discourse.
% DISAPPEARANCE_RATIONALE: If the doctrine disappeared and Article 51 reverted to strict state-on-state armed attack interpretation, intervening states would (1) seek Security Council authorization for counterterrorism operations (facing possible vetoes from permanent members aligned with host states), (2) operate covertly and acknowledge unlawful intervention, or (3) shift resources to capacity-building partnerships with host states. Host states would regain de facto territorial sovereignty; the international security landscape would reorganize around bilateral agreements, Security Council authorization processes, and capacity-building rather than unilateral doctrine.
% FOUNDING_PROBLEM: Post-9/11 transnational terrorism: non-state actors (al-Qaeda, ISIS, affiliated groups) mount attacks from territories whose host states are either unable to suppress them (failed/fragile states with weak security capacity) or unwilling to act (states harboring groups for strategic reasons). Powerful intervening states required a legal framework permitting cross-border counterterrorism operations when the host state could not or would not cooperate, without waiting for Security Council authorization (which could be vetoed).
% FOUNDING_PROBLEM_CORROBORATION: U.S. government and allied security officials attest the founding problem is live and persistent: transnational terror networks (ISIS, al-Qaeda branches) operate from weak states and ungoverned spaces; capacity-building efforts have progressed but gaps remain. Security scholars and practitioners from intervening states largely endorse this assessment. However, host-state governments, Global South legal scholars, the ICJ in dicta, and independent international-law institutes attest that while the original problem was genuine, the doctrine's application has expanded far beyond what the problem justifies: the doctrine is now invoked for regional power projection against non-state actors that do not threaten the intervening state, and operational targets are selected based on geopolitical alignment rather than actual threat. The founding problem has been substantially attenuated through counterterrorism operations and capacity-building, but the doctrine persists and has expanded.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.51 (moderate: genuine coordination function present—states do need a rule for when cross-border force is justified—plus meaningful constraint on host-state authority) and rises to 0.68 by interval end (higher extraction as the doctrine's scope expands and host-state resistance hardens but the doctrine persists). The trajectory reflects an institution whose initial legitimacy (responding to genuine security need) gradually erodes into routine application decoupled from necessity. Suppression is high and stable (0.58–0.72) because the doctrine's persistence depends on active suppression of alternative readings: intervening states must exclude narrow interpretations (the doctrine would not work if limited to state-on-state attacks) and prevent host-state pushback (the doctrine would collapse if host states successfully contested 'unable/unwilling' determinations in all cases). Theater ratio rises modestly (0.28–0.41) because intervening states increasingly invoke the doctrine for operations that stretch beyond the stated core (counterterrorism) into broader regional power projection—the machinery is preserved to justify actions increasingly decoupled from the founding problem it was built to solve. Accessibility collapse is moderate (0.62): once the doctrine is established and enshrined in security practice, host states face high barriers to exit—but the doctrine is not yet locked into physical/logical necessity as a mountain would be. Resistance is moderate (0.58) because host states, particularly powerful ones, mount diplomatic and legal resistance; however, that resistance is structurally weak because intervening states control the interpretation of 'unable' and 'unwilling' and can override host-state objections by invoking necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state seat: the doctrine is justified coordination—a necessary legal framework for counterterrorism when host capacity fails. From the weak host-state seat: the doctrine is extraction—a legal pretext for territorial violation that strips them of sovereignty without their consent. From the contested-sovereignty host-state seat: it is compulsion—they lose the ability to make deliberate political choices about whether to suppress certain groups because any deliberate choice to not suppress is relabeled 'unwillingness' and triggers intervention. The engine computes these divergent effective extractions (χ) from the structural data: intervening states have high directionality toward beneficiary (low d, low χ); weak host states have high directionality toward target (high d, high χ); contested-sovereignty states sit asymmetrically—powerful globally but locally constrained and thus higher d on this specific constraint. The narrow-attack reading (the structural alternative this reading coexists with) would compute radically differently: all parties would sit near symmetric (d≈0.5) because the narrow reading does not permit unilateral action on weak-state inability, so no targeting of host states occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states (institutional power, arbitrage exit options) are the structural beneficiaries: they gain operational authority and legal cover for actions they would otherwise have to justify differently or abandon. Host states with weak capacity (moderate power, constrained exit) are primary targets: they lose sovereignty without consent or ability to resist. Host states with contested sovereignty (powerful nominal power but politically constrained on this specific axis) are dual-positioned: they retain global arbitrage options but are locally trapped on the decision to suppress vs. not suppress non-state actors. The doctrine collapses that local political choice into a legal mandate. Non-intervening middle powers (powerful, constrained exit) benefit secondarily: they can invoke the doctrine themselves when needed, but their invocation depends on institutional credibility and alignment with intervening-state practice. The directionality computation flows from: (1) explicit beneficiary/victim declaration, (2) power atoms and exit options, (3) asymmetry: intervening states can change the rules; host states cannot. No overrides are needed; the structural data produces the right d directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-9/11 transnational terror from ungoverned spaces) was real and pressing. The doctrine was justified as a temporary bridge: when host states cannot suppress non-state actors, intervening states may act unilaterally in self-defense. This is Tangled Rope structure—genuine coordination (states need a rule) plus asymmetric extraction (host states pay). The mandatrophy question is whether the founding problem persists or whether the doctrine has outlived its justifying function. The measurement series shows extractiveness plateauing (0.68 by t25) while theater ratio rises (0.41 by t25), suggesting the doctrine is being applied to cases increasingly distant from the original problem. Host-state resistance hardens but does not dislodge the doctrine because intervening states control the interpretation of 'unable' and 'unwilling'. The classification as Tangled Rope is stable: the coordination function (the rule itself) is real; the extraction (host-state sovereignty cost) is real; active enforcement (suppression of alternative readings, diplomatic pressure against host-state contestation) is real. Mandatrophy would be triggered only if the founding problem disappeared entirely AND host states successfully foreclosed the doctrine—neither has happened. Instead, the constraint has shifted: it was born as a necessity-driven exception; it has become routine practice. This is not mandatrophy (the founding problem did not die) but drift (the constraint has expanded beyond its founding problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_vs_unwilling_conflation,
    'Is the distinction between ''unable'' and ''unwilling'' host states stable and enforceable, or does the doctrine collapse them into a single category that permits intervention in both cases?',
    'Systematic review of state practice: How consistently do intervening states actually distinguish between weak-capacity host states and politically-motivated unwilling hosts? When intervening states invoke the doctrine, do they explicitly argue inability vs. unwillingness separately, or do they fold them together? Do host states ever successfully contest a determination of ''unwillingness'' by demonstrating actual will to suppress?',
    'If the categories remain distinct and enforceable, the doctrine is more narrowly constraining (intervening states cannot simply declare any host state ''unwilling'' at will). If they collapse into a single permissive category, the doctrine expands to permit intervention in nearly any host-state territory where non-state actors operate, effectively delegitimizing host-state sovereignty as a barrier to intervention. Classification could shift from Tangled Rope (genuine coordination + extraction) to closer to Snare (extraction with coordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unable_vs_unwilling_conflation, empirical, 'Whether ''unable'' and ''unwilling'' remain structurally distinct or collapse into an undifferentiated permission to intervene').

omega_variable(
    founding_problem_persistence,
    'Is the post-9/11 security problem (transnational terror from ungoverned spaces) still the primary driver of doctrine invocation, or has the doctrine decoupled from its founding problem and become a general framework for unilateral intervention?',
    'Analysis of state invocations over time: Compare the ratio of invocations for (a) genuine counterterrorism (non-state actors that mount attacks on the intervening state''s territory) vs. (b) regional power projection (operations against non-state actors that do not directly threaten the intervening state). Compare stated justifications in official documents to actual operational targets. Monitor whether the doctrine is invoked for operations nominally against terror groups that are actually targeting regional competitors.',
    'If the doctrine has decoupled from counterterrorism and become a general unilateral-intervention framework, mandatrophy is present: the founding problem is solved or irrelevant, but the constraint persists. Classification pressure shifts from Tangled Rope (coordination + extraction driven by genuine security need) toward Piton (an atrophied coordination function maintained theatrically while extraction continues). Extractiveness might rise as the constraint becomes purely extractive cover rather than responding to real coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the doctrine remains anchored to its founding security problem or has decoupled and become a tool for broader intervention').

omega_variable(
    reading_foreclosure_scope,
    'Does the unable/unwilling reading logically foreclose the narrow_armed_attack reading within a single state''s legal framework, or do they coexist as alternative positions that different states can hold simultaneously?',
    'Examine whether any state that invokes the unable/unwilling doctrine also commits to rejecting the narrow reading entirely, or whether states strategically deploy whichever reading suits current operational interests. Check whether the ICJ could theoretically accept both readings as legitimate under different circumstances, or whether accepting one requires rejecting the other as a matter of logical necessity.',
    'If unable/unwilling forecloses narrow_armed_attack, the reading_relations should be ''forecloses'' (rare). If they coexist, the relation is ''coexists_with'' (the likely case—states hold different readings based on geopolitical interest). If unable/unwilling structurally enables the expansive_preventive reading by loosening the requirement for state action, then unable/unwilling ''influences'' expansive_preventive toward broader application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_scope, conceptual, 'Logical relationship between this reading and sibling readings of the Article 51 kernel').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high measured suppression (0.72) structural (enforced by diplomatic pressure and threat of countervailing intervention from major powers) or internalized (host states have adopted the doctrine''s premises and enforce it against themselves)?',
    'Post-contestation behavior: If a host state successfully resisted the doctrine and operators removed intervening military operations, would the suppression persist or decay? Do host states actively invoke the doctrine against their own non-state actors (internalized acceptance) or only accept it under duress (structural coercion)? Compare weak host states that have hosted successful resistance to the doctrine (e.g., diplomatic campaigns at the UN) against those that have not.',
    'If suppression is structural, the constraint''s effective suppression is tied to the power imbalance; if the imbalance shifts, suppression could decay. If suppression is partially internalized, host states carry it forward even post-intervention, making the constraint''s persistence more robust. The distinction affects whether the constraint would collapse if intervening states faced serious counterbalancing pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative host-state positions is externally enforced or has been internalized by host-state actors').

omega_variable(
    kernel_reading_identity,
    'Is the unable/unwilling doctrine a reading of the Article 51 kernel (interpreting existing text) or a new constraint that has been retrofitted onto Article 51 for legitimacy?',
    'Textual analysis and historical genealogy: Does Article 51''s language (right to self-defense if armed attack occurs) plausibly support the unable/unwilling reading as an interpretation, or does the doctrine require reading into the text something not stated? When and how did the doctrine emerge in legal writing—as an interpretation of existing Article 51 or as a new justification for a practice that existed before the legal rationale was developed?',
    'If unable/unwilling is a genuine reading of Article 51, the committer frame is valid (one kernel, multiple readings). If it is a new constraint retrofitted onto the kernel for legitimacy, it should be decomposed into a separate constraint with its own justification. This affects the network structure: if it is a reading, it connects to siblings via reading_relations; if it is a separate constraint, it connects via affects_constraints network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the unable/unwilling doctrine is a legitimate reading of Article 51 or a separate constraint that appropriated Article 51 language').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t17, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 17, 0.4).
narrative_ontology:measurement_basis(arti_tr_t17, observed).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t17, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 17, 0.67).
narrative_ontology:measurement_basis(arti_be_t17, observed).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t17, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 17, 0.71).
narrative_ontology:measurement_basis(arti_su_t17, observed).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_sovereignty_and_territorial_integrity_constraint).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, unilateral_vs_multilateral_security_authorization).

% DUAL FORMULATION NOTE:
% The Article 51 kernel has three constituent readings: narrow_armed_attack (mountain-like, strict text-based), unable_unwilling (this story, tangled_rope, moderate extraction), and expansive_preventive (snare-like, maximum extraction). Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and type. They are linked via network.affects_constraints and cs_structure.reading_relations because they compete to define what Article 51 permits. The unable_unwilling reading coexists_with narrow_armed_attack (different states hold both positions) and influences expansive_preventive (the unable/unwilling loosening of requirements structurally enables preventive doctrine to extend further).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
