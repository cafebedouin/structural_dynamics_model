% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Unable/Unwilling Doctrine: Non-State Actor Self-Defense Trigger
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   Article 51 of the UN Charter grants states the inherent right of
 *   self-defense 'if an armed attack occurs.' The unable/unwilling doctrine
 *   reads Article 51 to permit unilateral self-defensive action against
 *   non-state actors when the host state that harbors them is unwilling or
 *   unable to suppress the threat. This reading bridges traditional
 *   state-to-state self-defense with counterterrorism by treating host-state
 *   capacity/willingness as a trigger independent of explicit state
 *   attribution. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination problem + asymmetric extraction) while MEASURED as
 *   substantially extractive and highly suppressive (extraction rising over
 *   time, theater rising as doctrine invocations increase without comparable
 *   host-state capacity improvements). The gap between claim and metrics is
 *   the central analytical question: does the doctrine coordinate genuine
 *   security cooperation or legitimate unilateral power politics under a
 *   doctrine umbrella?
 *
 * KEY AGENTS:
 *   - intervening_state_with_counterterrorism_mandate: Institutional power; benefits from legal authority to act unilaterally; arbitrage exit (can withdraw invocation if politically costly)
 *   - host_state_whose_sovereignty_bypassed: Moderate power; bears operational costs and sovereignty erosion; constrained exit (cannot stop doctrine invocation without eliminating threat or building capacity to intervening state's standard)
 *   - civilian_populations_in_host_state: Powerless; experience direct kinetic harm; trapped exit
 *   - security_council: Excluded from the doctrine's invocation; bypassed by design; institutional authority weakened
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Unable/Unwilling Doctrine: Non-State Actor Self-Defense Trigger").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'af71dcdc-f0bd-482c-9323-6763af18f8a6').
narrative_ontology:cs_kernel_codification('af71dcdc-f0bd-482c-9323-6763af18f8a6', fixed_text).
narrative_ontology:cs_authority_grounding('af71dcdc-f0bd-482c-9323-6763af18f8a6', lineage).
narrative_ontology:cs_interpretation_layer_present('af71dcdc-f0bd-482c-9323-6763af18f8a6').
narrative_ontology:cs_reading_relation('af71dcdc-f0bd-482c-9323-6763af18f8a6', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('af71dcdc-f0bd-482c-9323-6763af18f8a6', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('af71dcdc-f0bd-482c-9323-6763af18f8a6', foundational, capacity_failure_attribution_model).
narrative_ontology:cs_axiom_status(capacity_failure_attribution_model, holdable).
narrative_ontology:cs_axiom_grounding('af71dcdc-f0bd-482c-9323-6763af18f8a6', capacity_failure_attribution_model, empirically_contingent).
narrative_ontology:cs_axiom('af71dcdc-f0bd-482c-9323-6763af18f8a6', foundational, self_defense_right_preservation).
narrative_ontology:cs_axiom_status(self_defense_right_preservation, holdable).
narrative_ontology:cs_axiom_grounding('af71dcdc-f0bd-482c-9323-6763af18f8a6', self_defense_right_preservation, deontological).
narrative_ontology:cs_reference_frame('af71dcdc-f0bd-482c-9323-6763af18f8a6', article_51_collective_security_framework).
narrative_ontology:cs_drift_state('af71dcdc-f0bd-482c-9323-6763af18f8a6', post_cold_war_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af71dcdc-f0bd-482c-9323-6763af18f8a6', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_state_with_counterterrorism_mandate).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_whose_sovereignty_bypassed).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_originating_threat).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_security_apparatus).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_originating_threat).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_security_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the unable/unwilling doctrine as a legal authority to justify cross-border counterterrorism operations. Claims the doctrine is a narrow, necessity-driven reading of Article 51 that permits intervention only when host-state capacity genuinely fails. Controls the evidentiary standard for 'unable' and 'unwilling,' determines attribution of non-state actor attacks, and frames doctrine invocations in public international law terms. Can withdraw from the doctrine if political costs rise (arbitrage exit: can invoke alternative legal justifications or seek Security Council authorization instead). Collects operational authority, intelligence access, and geopolitical positioning from the doctrine's availability.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_state_with_counterterrorism_mandate, agenda_setter,
    institutional, generational, arbitrage, global).

% Bears the primary structural cost: territorial incursions authorized unilaterally by the intervening state; loss of control over security operations on its own soil; reputational damage from being labeled 'unwilling or unable'; subordination of security apparatus to intervening-state authority; weakened legitimacy domestically if unable to protect civilians. Cannot exit the doctrine short of eliminating the non-state threat (impossible without intervening state assistance, creating dependency) or building state capacity to a standard the intervening state controls. Constrained exit: can object diplomatically but cannot prevent unilateral invocation. Sovereignty is bypassed not by force of circumstance but by the doctrine's legal structure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_whose_sovereignty_bypassed, payer,
    moderate, generational, constrained, national).

% Experience direct material harm from cross-border operations: kinetic strikes with collateral casualties, infrastructure damage, displacement, economic disruption from conflict, and institutional collapse if the host state's authority erodes. Have no formal standing in doctrine invocations and no mechanism to contest the intervening state's capacity/willingness assessment. Trapped exit: can only flee across borders (physically displaced) or await resolution of the conflict. Bear the immediate costs of the constraint while having no voice in its application.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_state, payer,
    powerless, immediate, trapped, local).

% Bears targeting and kinetic consequences of doctrine invocation: direct military strikes, intelligence operations, and priority targeting. Simultaneously may benefit from ambiguity about host-state unwillingness/capacity: if the doctrine is invoked, it implicitly legitimizes their operational environment as beyond host-state control, which paradoxically enables their operations to continue without host-state interference until the doctrine is activated. Trapped exit: can only relocate to another host state (which brings the doctrine's domain with it) or be destroyed.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_originating_threat, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_originating_threat, beneficiary).

% Benefits from intelligence sharing, military equipment provision, training partnerships, and capacity-building assistance offered alongside doctrine invocation. Simultaneously is subordinated to intervening-state authority in joint operations; loses independent decision-making power; becomes identity-fused with intervening-state counterterrorism framework. Faces institutional pressure: officials who resist doctrine invocations face removal or marginalization; institutional culture shifts toward dependency and compliance. Identity_locked exit: the apparatus's institutional identity has become fused with the intervening-state relationship, making exit architecturally difficult even if political will existed. Over decades, capacity-building is channeled through the intervening state, making autonomous action implausible.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_security_apparatus, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_security_apparatus, payer).

% Is structurally bypassed by the doctrine: unilateral self-defense invocations occur outside the Council's authorization gate. The Council has no ex-ante power to prevent doctrine invocations; ex-post condemnation is possible but carries no enforcement mechanism if the intervening state has sufficient power. Trapped exit: the Council cannot exit its institutional role, but the doctrine's existence has trapped it in diminished authority. Could only reassert authority by establishing a competing interpretation that requires explicit Council approval, but no such interpretation has gained traction.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, security_council, excluded,
    institutional, generational, trapped, global).

% Interprets the doctrine's legitimacy and scope through scholarship, court opinions, and commentary. Provides the epistemic authority that states cite for doctrine invocations or constraints. Divided: some scholars defend the doctrine as narrow necessity-driven reading; others critique it as power politics cover. The community's interpretive output feeds state practice and future doctrine applications. Analytical position: can observe and critique but cannot directly enforce constraints on state behavior.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Oppose the doctrine as a precedent that legitimizes unilateral power politics and threatens their own sovereignty. Cannot prevent intervening states from invoking it but can raise costs through diplomatic objection, Security Council statements, and general assembly votes. Excluded from the doctrine's core invocation frame because their objections do not affect the intervening state's calculation: the doctrine permits action regardless of regional consensus. Their inclusion would require a reading that made regional consent binding, which this reading explicitly does not.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, regional_and_non_aligned_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_state_with_counterterrorism_mandate).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared responsibility for counterterrorism: allocates primary burden to host states to suppress non-state threats but permits intervening states to act when host-state capacity genuinely fails, creating a two-tier system that avoids requiring explicit consent while maintaining a logic of delegation and capacity substitution.
% TRANSFER_FUNCTION: Transfers sovereignty over territorial control and security operations from host state to intervening state when 'unwilling or unable' is invoked; transfers legal authority to justify military force from the Security Council to the intervening state's unilateral judgment; transfers the operational risk and civilian casualty burden to the host-state population; transfers geopolitical positioning and intelligence access benefits to the intervening state.
% ABSENT_VOICES: Host states subjected to doctrine invocation are structurally subordinated: their own assessment of capacity/willingness is overridden by the intervening state's assessment, and they have no formal mechanism to contest that determination. Regional states that would oppose the doctrine as a precedent are excluded because the doctrine requires no regional consensus. The Security Council's authority is bypassed by design and has no voice in ex-ante authorization. Civil society and humanitarian organizations document harms but have no standing in doctrine invocations.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine disappeared, intervening states would lose unilateral legal authority for cross-border counterterrorism. State practice would require either Security Council authorization (restoring the Council's gate), explicit host-state consent (restoring host sovereignty), or a return to narrower self-defense triggering (constraining to state-attributed attacks). The global counterterrorism operational model would contract or require institutional redesign. Host states would regain territorial sovereignty and the ability to refuse external operations. The constraint's disappearance is precisely what constraining doctrines exist to prevent; its absence would materially reshape security relations.
% FOUNDING_PROBLEM: Non-state actors conduct attacks from territories controlled by failed or deliberately unwilling host states; traditional Article 51 self-defense, tied to state-attributed armed attacks, creates a legal gap that prevents intervening states from responding to non-state threats that host states cannot or will not suppress; this gap creates a mismatch between the security problem (non-state actor sanctuary) and the legal solution (state-to-state self-defense).
% FOUNDING_PROBLEM_CORROBORATION: The United States, Israel, Turkey, and other states with active counterterrorism mandates attest the founding problem is live: non-state actor sanctuaries in weak or hostile states continue to pose threats, and the doctrine is necessary to defend citizens. Host states subjected to doctrine invocation (Pakistan, Yemen, Iraq, Syria) attest the founding problem is overstated: their capacity has improved substantially, and the doctrine is invoked beyond necessity as a cover for power politics and territorial incursion. International legal scholars are divided: defenders cite contemporary non-state actor threats and the necessity of response; critics cite the doctrine's use to justify political targeting, territorial expansion, and civilian harm. UN Office on Counterterrorism reports document continued non-state actor activity but do not adjudicate the founding problem's legitimacy. No party outside the intervening state/beneficiary set independently corroborates the founding problem as currently invoked; corroboration comes only from those authorized by the doctrine or those seeking to constrain it.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.48 to 0.68 over the 20-year interval, reflecting the doctrine's evolution from narrow legal theory to operational practice: early invocations were framed as extraordinary necessity (low extraction); later invocations became routine counterterrorism operations (high extraction). Theater ratio rises modestly (0.28 to 0.42), indicating the doctrine's coordinative framing (security cooperation, burden-sharing) is increasingly decoupled from actual host-state capacity improvements. Suppression rises from 0.58 to 0.72 as the intervening state enforces the doctrine against host-state resistance and international objections. The measurement grid is shared across all three metrics to avoid misalignment-induced type reclassification artifacts. Accessibility collapse (0.61) reflects that host states cannot practically exit the doctrine without either eliminating the threat themselves or accepting external intervention. Resistance (0.71) is substantial because host states consistently object but cannot effectively block unilateral invocations.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state's seat, the doctrine is legitimate coordination: non-state actors create a gap in security responsibility, host states are unable to fill it, unilateral intervention restores equilibrium and is necessary. From the host state's seat, the doctrine is sovereignty bypass and extraction: the intervening state unilaterally determines capacity/willingness standards, controls attribution, subordinates host-state preferences, and collects geopolitical benefits. From the Security Council's excluded seat, the doctrine is institutional authority erosion: the Council's monopoly on authorizing force is circumvented by doctrine invocation. These divergent readings are not resolvable by additional data; they reflect asymmetric structural relationships to the same constraint. The engine computes per-seat types; divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The intervening state sits at d ≈ 0.1–0.2 (strong beneficiary: collects legal authority, operational control, indirect security benefits, diplomatic positioning). The host state sits at d ≈ 0.85–0.95 (full target: bears sovereignty costs, operational disruption, civilian casualties, institutional subordination, no reciprocal benefits). Host-state security apparatus sits at d ≈ 0.65–0.75 (mixed: benefits from capability building but subordinated to intervening state, identity_locked into dependency structure). Civilian populations sit at d ≈ 1.0 (full target: experience kinetic harm with no governance voice). The Security Council sits analytically off the directionality axis (excluded, bypassed). Effective extraction χ for the host state is amplified by spatial_scope (global doctrine precedent affects all host states simultaneously) and suppression (resistance is actively contained through diplomatic and operational pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('non-state actors originating from unable/unwilling host states create a defense gap') is CONTESTED as status. Intervening states claim it is live; host states claim it has been substantially solved by improved border control, intelligence sharing, and capacity-building partnerships. The doctrine persists regardless of founding-problem status, suggesting potential mandatrophy: the legal authority to act unilaterally under the doctrine is maintained even where the original problem (non-state actor safe haven) has been mitigated. However, classification as tangled_rope depends on documenting genuine coordination: the doctrine DOES coordinate burden-sharing in counterterrorism (live coordination function). The extraction piece (sovereignty bypass, unilateral authority) is asymmetric but rides on the coordination. If the founding problem is truly dead but the doctrine persists, the constraint moves toward piton (atrophied coordination, maintained theatrically). Evidence: doctrine invocations continue even where host-state capacity has improved (Pakistan, Iraq post-2014); theater ratio rises, suggesting performative maintenance. A mandatrophy verdict would require founding-problem-status = dead AND disappearance-verdict = world_rearranges-but-actually-stays-similar. Current data suggests contested founding status and live institutional contestation, so classification holds as tangled_rope with mandatrophy ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_vs_unable_boundary,
    'What evidentiary standard distinguishes a host state that is genuinely ''unable'' to suppress a non-state threat from one that is merely ''unwilling''? Who sets and verifies this standard?',
    'Case-law accumulation: examine invocations of the doctrine and the empirical criteria states cite for ''inability'' (military capacity, institutional reach, intelligence access) versus ''unwillingness'' (deliberate sanctuary provision, political tolerance, revenue capture). Cross-reference with host-state counterarguments and third-party assessments.',
    'If ''unwilling'' becomes the dominant criterion, the doctrine tilts toward unilateral authority to override host-state preferences (higher extraction). If ''unable'' requires hard evidence of capacity failure, the doctrine is more constrained. If no stable evidentiary standard exists, the doctrine becomes a cover for discretionary power politics, shifting classification from tangled_rope (coordinated + extractive) to snare (pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unwilling_vs_unable_boundary, conceptual, 'Whether the unable/unwilling standard is epistemic or political.').

omega_variable(
    kernel_reading_contest_structure,
    'Does this reading (unable/unwilling doctrine) genuinely coexist with the narrow_armed_attack_reading, or does it foreclose it within a single legal framework?',
    'Examine whether courts, treaty bodies, and states have adopted BOTH readings as live positions on the same facts, or whether acceptance of the unable/unwilling doctrine has led to abandonment of the narrow reading. Track ICJ rulings, Security Council resolutions, and state practice on self-defense triggers post-2001.',
    'If coexists_with is correct, the two readings remain in live dispute and both constraint stories remain valid. If unable/unwilling forecloses narrow_armed_attack, the narrow_armed_attack_reading should be reclassified or marked as superseded. This determines whether the constraint family holds two distinct-but-live constraints or whether one has become historically obsolete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Structural relationship between this reading and the narrow armed-attack reading.').

omega_variable(
    sovereignty_bypass_legitimacy_under_contest,
    'Is the sovereignty bypass (territorial incursion without consent) a legitimate coordination cost of the doctrine, or is it itself the extraction mechanism the doctrine is designed to effect?',
    'Examine whether intervening states offer compensation, institutional reform partnerships, or other benefits to host states subjected to doctrine-triggered operations. Compare outcomes in scenarios where intervening states had Security Council authorization (explicit collective action) versus unable/unwilling doctrine invocation (unilateral). If host states are systematically worse off and receive no reciprocal benefits, the sovereignty bypass is extraction; if host-state security capacity improves as a side effect, it is a coordination cost.',
    'If the sovereignty bypass is legitimate coordination cost, the constraint remains tangled_rope (genuine coordination + asymmetric extraction). If it is pure extraction, the constraint reclassifies toward snare. This affects whether the doctrine is defensible within a multipolar balance-of-power framework or is structurally incompatible with sovereign equality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_bypass_legitimacy_under_contest, empirical, 'Whether the constraint''s extraction is coupled to the coordination function or is separable from it.').

omega_variable(
    non_state_actor_attribution_chain,
    'How does attribution of non-state actor attacks to ''origin from a host state'' work in practice, and who controls that determination?',
    'Trace case histories: examine statements by intervening states claiming attribution (intelligence briefs, public declarations, military court filings) and compare with independent assessments from neutral parties, regional governments, and post-conflict accountability mechanisms. Establish whether attribution is transparent, contested, or unilateral.',
    'If attribution is transparent and independently verifiable, the doctrine''s triggering is constrained by empirical facts. If attribution is controlled by the intervening state and not independently verified, the doctrine becomes a discretionary authority to designate targets and host states, shifting toward snare. This feeds back to the unwilling/unable boundary: attribution ambiguity expands what counts as ''origin from'' the host state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_attribution_chain, empirical, 'Whether the doctrine''s triggering condition is empirically verifiable or discretionary.').

omega_variable(
    identity_fusion_host_state_capacity_assessment,
    'To what extent has the host-state security apparatus fused its institutional identity with the intervening state''s counterterrorism framework, making exit from the doctrine architecturally infeasible?',
    'Examine institutional dependency: intelligence sharing agreements, joint operations command structures, equipment provision, and personnel training pipelines. Track whether host-state officials who object to unilateral doctrine invocation face pressure to acquiesce or resign. Assess whether host-state capacity is built toward independence or continued dependency.',
    'If identity fusion is substantial and directionality is asymmetric (intervening state sets the terms), the host-state security apparatus faces identity_locked exit and cannot credibly assert capacity or willingness independent of the intervening state''s assessment. This amplifies extraction and makes the constraint more extractive than the base metrics alone suggest; effective extraction χ rises sharply for the host-state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_host_state_capacity_assessment, empirical, 'Whether host-state security capacity is autonomous or fused with intervening-state authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_responsibility_doctrine_non_state_actors).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_capacity_failure_attribution).

% DUAL FORMULATION NOTE:
% This story is one reading of the Article 51 kernel constraint. The narrow_armed_attack_reading constrains self-defense to state-attributed attacks; the expansive_preventive_reading permits preemptive strikes. The unable/unwilling reading sits between: requires non-state attack response (unlike expansive) but unilateral host-state bypass (unlike narrow). The three readings form a constraint family: all ground in Article 51 but disagree on the triggering conditions and permissibility of unilateral action. Each reading has its own ε, beneficiary/victim structure, and type. Sibling readings are NOT alternatives within this story — they are separate constraint files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__unable_unwilling_doctrine_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
