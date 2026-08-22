% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Coordination Under Composite Exogenous/Endogenous Pressure
 *   domain: social/cultural/legal
 *
 * SUMMARY:
 *   The honor satisfaction substrate — the cultural and social system that
 *   made dueling intelligible as a mechanism for resolving affronts to
 *   masculine reputation — experienced composite pressure from 1600–1900:
 *   legal prohibition (exogenous suppression) AND cultural delegitimation of
 *   honor-through-violence (endogenous substrate transformation) operated
 *   simultaneously with non-independent causal pathways. This reading claims
 *   that dueling's disappearance cannot be explained by either mechanism
 *   alone. Legal enforcement succeeded partly because cultural reformers had
 *   already eroded the cognitive frame that made dueling a duty; cultural
 *   reformers succeeded partly because state prohibition provided
 *   institutional backing for their reframing. The two agendas (legal
 *   elimination of private violence, cultural transformation of masculine
 *   identity) reinforced each other in ways that neither would have achieved
 *   unilaterally. The constraint is CLAIMED as tangled rope: coordination
 *   function (honor-satisfaction community coordinating status claims) plus
 *   asymmetric extraction (practitioners identity-locked into legal jeopardy
 *   and cultural delegitimation simultaneously) plus active enforcement
 *   (legal apparatus + cultural pressure). The claim/metric divergence is
 *   intentional: the reading's metrics show substantial extraction (0.58) and
 *   high suppression (0.72) because the measurement captures the composite
 *   pressure phase. The OUTCOME (dueling's termination) is less controversial
 *   than the MECHANISM — the reading's point is to model the causal
 *   entanglement, not to claim dueling was secretly a snare.
 *
 * KEY AGENTS:
 *   - honor_code_community: institutional actor maintaining the cultural substrate and benefiting from reputation coordination (d near 0.2)
 *   - dueling_practitioners: identity-locked payers subjected to dual enforcement (d near 0.85)
 *   - reformist_legal_apparatus: institutional agenda-setter applying criminal suppression (d near 0.3)
 *   - cultural_reformers: organized agenda-setter delegitimating violence-based honor (d near 0.35)
 *   - families_of_participants: moderate-power payers absorbing spillover damage (d near 0.72)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Coordination Under Composite Exogenous/Endogenous Pressure").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "social/cultural/legal").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '533da1fb-2111-40ac-9c7d-a6fae1887361').
narrative_ontology:cs_kernel_codification('533da1fb-2111-40ac-9c7d-a6fae1887361', distributed).
narrative_ontology:cs_authority_grounding('533da1fb-2111-40ac-9c7d-a6fae1887361', extraction).
narrative_ontology:cs_reading_relation('533da1fb-2111-40ac-9c7d-a6fae1887361', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('533da1fb-2111-40ac-9c7d-a6fae1887361', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('533da1fb-2111-40ac-9c7d-a6fae1887361', foundational, dual_pressure_mechanisms_causally_entangled).
narrative_ontology:cs_axiom_status(dual_pressure_mechanisms_causally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('533da1fb-2111-40ac-9c7d-a6fae1887361', dual_pressure_mechanisms_causally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('533da1fb-2111-40ac-9c7d-a6fae1887361', foundational, honor_substrate_transformation_simultaneous_with_legal_suppression).
narrative_ontology:cs_axiom_status(honor_substrate_transformation_simultaneous_with_legal_suppression, holdable).
narrative_ontology:cs_axiom_grounding('533da1fb-2111-40ac-9c7d-a6fae1887361', honor_substrate_transformation_simultaneous_with_legal_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('533da1fb-2111-40ac-9c7d-a6fae1887361', honor_code_as_coordination_mechanism).
narrative_ontology:cs_drift_state('533da1fb-2111-40ac-9c7d-a6fae1887361', peak_dual_enforcement_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('533da1fb-2111-40ac-9c7d-a6fae1887361', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_community).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_practitioners).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, families_of_participants).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, honor_as_social_coordination_mechanism).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, non_additive_causal_pathways_in_institutional_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the cultural substrate that makes honor-satisfaction meaningful: the system of reputation, shame, masculine identity, and social standing that dueling coordinated. This group includes nobility, military officers, professionals claiming status through honor codes. They benefit from the coordination function (shared understanding of what restores status) even as legal enforcement simultaneously pressures the practice away from violent resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_community, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_community, agenda_setter).

% Nobility and military officers whose identity is constituted through the honor code and its validation via dueling. They face simultaneous legal prohibition (criminalization of dueling, enforcement via prosecution) and cultural delegitimation (the honor code itself transforms — the meaning of satisfying honor shifts away from violent redress). Their exit from dueling means reconstructing their identity category, which is identity-locked. They bear the cost of both legal enforcement machinery and the erosion of the cognitive frame that made dueling intelligible as honor-satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_practitioners, payer,
    powerful, biographical, identity_locked, national).

% Spouses, children, and relatives of dueling practitioners carry the material and emotional costs: death of breadwinner, social stigma of association with criminalized practice, pressure to disavow the honor code that motivated the duel. They cannot exit the family relationship; they can only buffer or absorb the consequences of dual pressure (legal penalty + cultural delegitimation).
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, families_of_participants, payer,
    moderate, biographical, constrained, national).

% State authorities, legislators, prosecutors who enact and enforce legal prohibition of dueling. They view dueling as a criminal breach of state monopoly on legitimate violence; they apply legal suppression (arrest, prosecution, imprisonment). This agenda-setter does not depend on the honor code's persistence and actively works to dismantle it through law.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, reformist_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Intellectuals, clergy, middle-class advocates, journalists who delegitimate the honor code's reliance on violence. They promote alternative frames for masculine identity, status, and reputation (professional achievement, moral virtue, intellectual accomplishment rather than readiness to kill for insult). They do not enforce law but shift the cultural substrate that makes dueling intelligible.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_reformers, agenda_setter,
    organized, generational, analytical, national).

% Peasants, merchants, working poor who never had standing in the honor code system and have no voice in its transformation. They are excluded from the dueling practice itself (no claim to honorable status) and from the cultural negotiation over what honor means. They would argue for elimination of a system that privileges violent male status-assertion, but that argument is not seated at the table.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, excluded_lower_classes, excluded,
    powerless, biographical, trapped, national).

% Historical and sociological analysis of the constraint's causal structure: how legal suppression and cultural delegitimation interact, whether they are independent or entangled, which is primary, and what the termination of dueling actually reveals about social coordination mechanisms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code + dueling system coordinated masculine identity, reputation, and social standing within a status-competitive elite. Dueling resolved affronts to honor by demonstrating willingness to risk death for status; this made the honor claim credible and restored social standing. The system coordinated without a centralized authority — participants understood the shared frame and stakes.
% TRANSFER_FUNCTION: Moves social standing, masculine identity, and access to status-dependent roles (military commission, professional standing, marriage eligibility) to those who successfully navigate the honor code. Simultaneously, moves physical risk (death, injury), legal risk (prosecution), and psychological/familial damage to dueling practitioners and their dependents. The transfer is asymmetric: beneficiaries (honor-code community) capture the legitimacy-via-participation; payers (practitioners facing dual enforcement, families) bear the material costs.
% ABSENT_VOICES: Lower classes excluded entirely — they have no stake-holding in the honor system and no say in its transformation. Women are absent from the dueling practice itself, though their role as guarantors of male reputation (through marriage, family standing, sexual honor) makes them stakeholders in the system's logic. Colonial subjects and conquered peoples are absent — the honor code operated within European aristocratic/military circles and was not negotiated with those subjected to state violence.
% DISAPPEARANCE_RATIONALE: The reading contests the verdict itself. Under practice-decline reading (exogenous suppression only), dueling disappears because enforcement made it too costly; the honor code substrate persists and merely finds new outlets (professional duels in writing, social ostracism as substitute violence). Under cultural-contraction reading (endogenous delegitimation only), dueling disappears because honor itself transforms from a violence-dependent concept to a violence-free one; the coordination mechanism survives but dueling was never essential to it. Under THIS reading (composite), both mechanisms operated with non-independent pathways: legal enforcement delegitimated practitioners (making honor-defense look like criminality rather than duty) AND cultural reformers exploited legal suppression to erode the frame that made dueling intelligible (legal criminalization provided cover for cultural reframing). If dueling disappeared due to enforcement alone, practitioners would maintain the code privately and seek new outlets; if only cultural transformation, enforcement would be unnecessary. The fact that BOTH were necessary and mutually reinforcing is exactly the claim.
% FOUNDING_PROBLEM: Elite masculine identity required proof through willingness to risk death for reputation; affronts to honor could only be resolved through combat because no other mechanism was credible. Dueling coordinated the response: both parties understood the stakes and would participate because exit meant social death (loss of status, masculine identity, access to power). This was the founding problem the honor code + dueling system solved.
% FOUNDING_PROBLEM_CORROBORATION: Historians of honor codes (Kiernan, Redding, Schwerhoff) attest that the founding problem was real: elite identity truly depended on honor-satisfaction mechanisms and dueling was the culturally-endorsed method. Reformist theorists of the period (Enlightenment critics of dueling) attest that by the 18th century, dueling had become a social pathology disconnected from the founding problem — the honor code had been delegitimated by intellectual critique before enforcement machinery became effective (cited by Spierenburg, Esposito). The dispute is whether the founding problem remained live (practice-decline reading) or had already dissolved (cultural-contraction reading). THIS reading asserts that both were live simultaneously: the legal apparatus enforced against a practice whose cultural substrate was simultaneously being eroded by reformist messaging.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs 0.38 (early 1600s, honor code operating without state opposition) → 0.66 (1880s, peak pressure from dual enforcement) → back down to 0.58 (1900, dueling nearly extinct, enforcement no longer needed at same intensity). The rise reflects mounting extraction as legal penalties + cultural delegitimation combine; the plateau-then-slight-decline reflects the constraint's function shifting from active suppression to maintenance of the new equilibrium (honor-satisfaction divorced from violence). Suppression requirement rises monotonically (0.25 → 0.78) as the legal and cultural machinery strengthens. Theater_ratio rises (0.12 → 0.48) because enforcement energy increasingly goes to maintaining the new cultural frame (praising non-violent honor, celebrating military service divorced from personal dueling) rather than directly preventing duel-participation. Accessibility collapse rises (practitioners facing both legal barriers AND cultural barriers) while resistance falls (practitioners increasingly isolated as the frame erodes). The grid shows the structure clearly: at individual level, accessibility climbs fastest (a duel is simultaneously illegal AND socially unintelligible); at structural level, suppression climbs but accessibility climbs slower (the system-level alternatives exist — professional careers, intellectual standing — but are not accessible to identity-locked practitioners). By 1900, an individual nobleman interested in defending his honor via combat faces both criminal prosecution AND social mockery; the alternatives (professional achievement, published writing, intellectual reputation) are available but require identity reconstruction, not just behavior change.
 *
 * PERSPECTIVAL GAP:
 *   From the honor-code community's seat (beneficiary), the constraint is a coordination mechanism for reputation that happens to face external pressure; from the practitioner's seat (payer), it is a trap combining legal jeopardy + identity erosion; from the reformist seat (agenda-setter), it is a dysfunctional tradition that needed dismantling and was finally dismantled when law and culture aligned. The engine computes per-seat classification: beneficiaries see rope (coordination with external costs), payers see snare (no exit, compounding costs), agenda-setters see scaffold (transitional pressure toward a new equilibrium). The composite reading asserts this multiplicity is structural, not an artifact of measurement — the same constraint operates as different types from different seats because the causal mechanisms are entangled. A purely legal reading (practice-decline) would compute more uniformly as rope-with-enforcement from all seats; a purely cultural reading (cultural-contraction) would compute more uniformly as mountain-with-erosion. The divergence is the diagnostic signal of composite causation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (honor-code community): d ≈ 0.2–0.3. They benefit from the coordination function (shared understanding of honor-satisfaction), bear little direct cost from enforcement (it targets practitioners, not the abstract community), and maintain arbitrage through redefining honor to exclude violence. Practitioners: d ≈ 0.75–0.85. They are trapped (identity-locked into the old frame), face dual enforcement (legal + cultural), and have no exit that doesn't require identity reconstruction. Families: d ≈ 0.65–0.75. They bear material spillover (death, imprisonment, social stigma) without participating in the honor system. Legal apparatus and cultural reformers: d ≈ 0.25–0.35 (agenda-setters, analytical seats). They set the enforcement agenda and benefit from its success (state monopoly on violence consolidates, cultural pluralism advances) but don't absorb extraction — they apply it. The directionality divergence between beneficiary and payer seats is precisely what triggers the tangled-rope classification: one group coordinates a function, another group pays for the transition, and active enforcement maintains the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elite identity requires honor-satisfaction; dueling was the credible method) was genuinely live in 1600 and genuinely DEAD by 1880, not merely contested. The composite reading tracks this via the non-monotonic metrics: extractiveness peaks when dual pressure is highest (around 1850–1880), then begins to decline as the new equilibrium (non-violent honor-satisfaction) stabilizes. This is the mandatrophy signature: a coordination function whose founding rationale has been undermined. A pure rope would maintain steady metrics; a pure snare would show monotonically rising extraction as the trap tightens. The composite reading shows a rise-and-plateau pattern because it models the transition: the constraint was built to solve a real problem (coordinate honor-satisfaction); exogenous + endogenous pressure delegitimated the founding problem simultaneously; the constraint persisted briefly as pure enforcement against an eroded substrate (peak theater_ratio, peak suppression_requirement); then the new frame solidified and the constraint could relax (honor-satisfaction now means professional achievement, not dueling risk). This is NOT mandatrophy-resolved in base_properties — the founding problem is contested (practice-decline reading says it remains live; cultural-contraction reading says it was already dead; this reading says both happened at once). The measurements show when the problem became dead (extractiveness plateau around 1900).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_vs_additive,
    'Were legal enforcement and cultural delegitimation genuinely causally entangled (non-independent pathways where each enabled the other), or merely additive (two independent mechanisms that both pushed in the same direction)?',
    'Comparative historical analysis: examine dueling''s trajectory in jurisdictions where legal prohibition was weak but cultural delegitimation was strong (vs. the reverse). If dueling declined equally in both, the mechanisms are additive; if decline correlates with the combination (weak in low-suppression + low-delegitimation, strong in high-suppression + high-delegitimation), the mechanisms are entangled.',
    'If additive, the constraint decomposes into two independent stories (one legal-suppression rope, one cultural-delegitimation mountain); if entangled, it remains one tangled-rope story with non-independent causal structure. The classification changes from tangled_rope to rope + mountain (different family) if decomposition is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_entanglement_vs_additive, empirical, 'Whether dual-pressure mechanisms were non-independent or additive.').

omega_variable(
    substrate_transformation_depth,
    'Did the honor code''s cognitive substrate undergo fundamental transformation (honor became incompatible with violence, not just incompatible with dueling), or did practitioners find non-violent outlets for honor-satisfaction (military service, professional duels in writing, social dominance through intellectual or economic means)?',
    'Ethnographic + historical analysis of post-dueling honor practices: if honor-seeking individuals rapidly transitioned to alternative status-achieving mechanisms (all available within the old frame), substrate transformation was shallow; if honor-seeking itself declined and people reorganized identity around achievement (professional, intellectual, economic), substrate transformation was deep.',
    'If shallow, dueling was a coordination mechanism that persisted on a stable substrate; if deep, the substrate eroded and dueling was never essential. Shallow → tangled_rope may persist in new form; deep → mountain erosion (the founding problem itself vanished). This omega addresses whether the constraint is the honor-satisfaction system (deep transformation = mountain erosion) or just dueling''s role within it (shallow = tangled rope with new outlets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_transformation_depth, empirical, 'Depth of honor-code cognitive transformation versus outlet substitution.').

omega_variable(
    practitioners_choice_vs_coercion,
    'Did practitioners choose to exit dueling because they genuinely accepted cultural reframing (honor no longer requires combat), or because legal and social costs made continued participation irrational, regardless of their internalized frame?',
    'Post-suppression persistence analysis: track practitioners'' private beliefs and actions in contexts where enforcement relaxed or was evaded. If practitioners immediately resumed dueling (legally, informally, or by emigrating to low-enforcement jurisdictions), they had not internalized the delegitimation; if they accepted the new frame even when enforcement relaxed, internalization occurred.',
    'If coercion-driven exit without internalization, the constraint is pure snare (extraction + exit prevention); if chosen exit with internalization, the constraint is tangled rope (coordination + extraction with eventual acceptance). Affects whether the payer seats experience the constraint as trap or as internalized norm shift. Also affects stability: coercion-based exits are fragile (practitioners revert if costs decline); internalization-based exits are stable (new frame persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioners_choice_vs_coercion, empirical, 'Whether practitioners'' exit from dueling was coerced or internalized.').

omega_variable(
    identity_lock_vs_identity_reconstruction,
    'Were dueling practitioners genuinely identity-locked (unable to reconstruct their status claims), or were alternative honor-satisfaction mechanisms available to them at acceptable cost?',
    'Biographical analysis of practitioners'' post-dueling trajectories: those who successfully transitioned to alternative status-seeking (military advancement via bureaucracy rather than combat reputation, professional standing via credentials rather than personal dueling record) were not fully locked; those who declined socially or emigrated were locked. Aggregate transition success rates reveal the degree of lock.',
    'Full lock (exit_options: identity_locked) drives tangled_rope via compounding extraction; partial lock (exit_options: constrained) softens the classification. Affects directionality: if truly locked, d approaches 1.0; if constrained but navigable, d ≈ 0.65–0.75. This omega addresses whether the payer identity is structurally constituted through dueling or merely historically dependent on it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_identity_reconstruction, empirical, 'Degree of identity-lock versus availability of alternative status-seeking pathways for practitioners.').

omega_variable(
    kernel_reading_alternative_frames,
    'Could a single coherent commitment framework (a single party''s epistemic and normative commitments) hold BOTH the practice-decline reading and the cultural-contraction reading, or are they genuinely incompatible framings of the same historical record?',
    'Conceptual analysis: attempt to construct a reading that endorses both ''honor code persists, dueling declined exogenously'' (practice-decline) AND ''honor code transformed fundamentally'' (cultural-contraction) without logical contradiction. If construction succeeds (e.g., ''the code persisted in form but substance eroded, and legal enforcement accelerated form-collapse''), the readings coexist within one framework; if it fails, they foreclose each other.',
    'If they coexist in one framework, reading_relations should show ''coexists_with'' (both readings held simultaneously by different parties); if they foreclose, ''forecloses'' (one reading''s core premise contradicts the other). Affects the network topology of the kernel: some kernels have coexisting readings (competing parties hold both live), some have foreclosing pairs (one reading rules out the other). This omega documents the alternative framing choice and why the composite reading was selected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_frames, conceptual, 'Logical compatibility of sibling readings versus foreclosure relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1720, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1720, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1720, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.31).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1880, 0.48).
narrative_ontology:measurement_basis(hono_tr_t1880, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.41).
narrative_ontology:measurement_basis(hono_tr_t1900, projected).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1720, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1720, 0.42).
narrative_ontology:measurement_basis(hono_be_t1720, observed).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.54).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.61).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1880, 0.66).
narrative_ontology:measurement_basis(hono_be_t1880, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement_basis(hono_be_t1900, projected).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1600, 0.25).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1720, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1720, 0.38).
narrative_ontology:measurement_basis(hono_su_t1720, observed).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.61).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.71).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1880, 0.78).
narrative_ontology:measurement_basis(hono_su_t1880, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(hono_su_t1900, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1600, 0.48).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1900, 0.81).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1600, 0.35).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1900, 0.89).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1600, 0.42).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1900, 0.85).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1600, 0.22).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1900, 0.72).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1600, 0.64).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1900, 0.38).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1600, 0.72).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1900, 0.28).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1600, 0.68).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1900, 0.35).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1600, 0.58).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1900, 0.42).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1600, 0.38).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1900, 0.61).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1600, 0.55).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1900, 0.78).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1600, 0.42).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1600, 0.28).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1900, 0.51).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1600, 0.15).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1900, 0.62).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1600, 0.18).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1900, 0.81).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1600, 0.22).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1900, 0.76).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1600, 0.12).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1900, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: honor_satisfaction_substrate kernel with three sibling readings. This reading (composite_overdetermined) models the causal entanglement of exogenous legal suppression and endogenous cultural delegitimation. Practice_decline_reading isolates legal suppression as primary; cultural_contraction_reading isolates cultural transformation as primary. The three readings partition the explanatory space for dueling's decline. All three share the same historical referent (dueling's disappearance 1600–1900) but assign different ε-values and beneficiary/victim structures because they identify different causal mechanisms as primary. Decomposition per ε-invariance principle: ε is not observable-relative here; rather, each reading's ε (extractiveness of the composite system / legal enforcement alone / cultural transformation alone) differs because the causal referent differs. Network edges allow contamination analysis: if practice_decline reading's empirical support strengthens, composite reading's causal entanglement claim weakens (the mechanisms may be more separable than claimed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, powerful, 0.81).
constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
