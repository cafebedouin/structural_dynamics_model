% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Reachability of Total War — Inertial Contraction Reading
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Since 1945, total war between great powers has been out of the feasible
 *   set, and since 1991 the arrangements that once actively maintained that
 *   boundary — dense arms-control architecture, verification regimes, crisis
 *   channels — have progressively collapsed. This story instantiates the
 *   contingent_reachability_reading of the total_war_reachability_boundary
 *   kernel: the current contraction is not a natural limit and not a healthy
 *   coordination equilibrium but an atrophied capability persisting by
 *   inertia, which technological change (missile defense maturation,
 *   hypersonic delivery, space-based sensing, AI-enabled targeting) could
 *   reverse. KEY AGENTS (by structural relationship): nuclear-weapons-states
 *   leaderships administer the boundary and sit near-symmetrically;
 *   destabilizing-technology investor states and defense contractors collect
 *   from the arrangement without maintaining it; civilian populations inside
 *   and outside the nuclear states bear its unmanaged tail-risk; the
 *   dissolved arms-control profession is the absent maintainer; deterrence
 *   theorists observe. EPSILON REFERENT: epsilon is authored for the standing
 *   arrangement — the current contraction as it actually operates — assessed
 *   by this reading's lights; the sibling readings assess the same referent
 *   under different lights and author their own epsilon in their own files.
 *   FAMILY NOTE: the colloquial label 'the nuclear peace' decomposes into
 *   three structurally distinct claims (permanent contraction / contingent
 *   contraction / probabilistic drop under stable coordination), linked via
 *   network.affects_constraints; the sibling stories should reciprocate the
 *   linkage and document the decomposition in their own commentary.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states_leaderships: Administrator seat (institutional/identity_locked) — sets doctrine, posture, and treaty participation; collects security from war-absence, pays in foregone escalation options
 *   - destabilizing_technology_investor_states: Primary beneficiary (powerful/mobile) — builds reversal-capable capabilities unconstrained by the standing arrangement
 *   - strategic_defense_contractors: Secondary beneficiary (organized/arbitrage) — collects revenue from posture maintenance and modernization on both sides of the ledger
 *   - civilian_populations_nuclear_states: Primary target (powerless/trapped) — bears unmanaged catastrophic tail-risk and the tax burden of posture maintenance
 *   - nonaligned_third_party_states: Secondary target (moderate/constrained) — bears escalation spillover risk without any seat in the postures that generate it
 *   - arms_control_verification_professionals: Excluded maintainer (moderate/identity_locked) — the dissolved management layer; would object to the drift, no longer in the conversation
 *   - deterrence_theorists: Analytical observer (analytical/analytical) — sees the full structure; holds no enforcement power and collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.51).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.3).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.51).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Reachability of Total War — Inertial Contraction Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '9c51320f-6557-4270-82c1-103986b4f89d').
narrative_ontology:cs_kernel_codification('9c51320f-6557-4270-82c1-103986b4f89d', distributed).
narrative_ontology:cs_authority_grounding('9c51320f-6557-4270-82c1-103986b4f89d', distributed).
narrative_ontology:cs_reading_relation('9c51320f-6557-4270-82c1-103986b4f89d', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('9c51320f-6557-4270-82c1-103986b4f89d', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('9c51320f-6557-4270-82c1-103986b4f89d', foundational, reachability_is_technology_contingent).
narrative_ontology:cs_axiom_status(reachability_is_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('9c51320f-6557-4270-82c1-103986b4f89d', reachability_is_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('9c51320f-6557-4270-82c1-103986b4f89d', foundational, unadministered_contraction_decays_to_inertia).
narrative_ontology:cs_axiom_status(unadministered_contraction_decays_to_inertia, holdable).
narrative_ontology:cs_axiom_grounding('9c51320f-6557-4270-82c1-103986b4f89d', unadministered_contraction_decays_to_inertia, empirically_contingent).
narrative_ontology:cs_reference_frame('9c51320f-6557-4270-82c1-103986b4f89d', technology_indexed_reachability_equilibrium).
narrative_ontology:cs_drift_state('9c51320f-6557-4270-82c1-103986b4f89d', contemporary_post_arms_control_collapse, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('9c51320f-6557-4270-82c1-103986b4f89d', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investor_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, strategic_defense_contractors).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, nonaligned_third_party_states).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, technology_dependent_reachability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set nuclear doctrine, force posture, and participation in or withdrawal from arms-control frameworks; their choices determine whether the boundary is actively managed or left to drift. They collect security from the war-absence the boundary provides while paying in foregone escalation options and in the political cost of any attempt to rebuild or restore the arrangements that once maintained it. Standing outside the predicament is effectively unavailable to them: arsenals are fused with great-power status and domestic politics, so no leadership can exit the arrangement it administers.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapons_states_leaderships, agenda_setter,
    institutional, generational, identity_locked, global).

% Invest heavily in missile defense, hypersonic delivery, space-based sensing, and AI-enabled targeting — capabilities that matter most if the current mutual-vulnerability equilibrium gives way. The standing arrangement asks nothing of them while they build: no operative treaty constrains these programs, and the boundary's day-to-day persistence costs them little, so their investments accumulate quietly as options on a different strategic future. Redirecting investment is easy; the programs are fungible across missions.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investor_states, beneficiary,
    powerful, generational, mobile, global).

% Build and maintain the deterrent forces, sensors, and delivery systems on both the status-quo and the destabilizing side of the ledger, collecting revenue from modernization programs, sustainment contracts, and next-generation strategic systems whichever direction the strategic environment moves. Portfolios span customers and mission areas, so shifts in posture policy change what they sell more than whether they sell.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_defense_contractors, beneficiary,
    organized, immediate, arbitrage, continental).

% Live under the residual risk that the war-absence fails, with no seat in the doctrinal or budgetary decisions that set that risk and no realistic escape from it: intercontinental reach makes relocation irrelevant, and the relevant choices are made by governments they influence only indirectly. They also carry the tax burden of posture maintenance.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_nuclear_states, payer,
    powerless, generational, trapped, global).

% Chose no part in the strategic competition but sit downwind of it: escalation between the great powers would devastate them through direct strikes, fallout, economic collapse, and alliance entanglement, while their diplomatic weight is too small to shape the postures that generate the risk. Their main lever is coalition diplomacy in multilateral fora, which has historically moved declaratory policy but not force posture.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nonaligned_third_party_states, payer,
    moderate, generational, constrained, regional).

% Inspected deployments, monitored treaties, and ran the crisis-communication channels that once gave the boundary its active management layer; successive treaty collapses dissolved their institutions, and the survivors work in shrinking consultancies and university centers with no formal role in current posture decisions. Their professional identity is bound up in a mission they can no longer perform.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_verification_professionals, excluded,
    moderate, biographical, identity_locked, global).

% Track the gap between declaratory policy, force structure, and technological trend lines, and publish the net assessments on which the contending accounts of the boundary's durability rest. They hold no enforcement power and collect nothing; their influence runs through the adoption of their frameworks by planning staffs.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bounds great-power conduct around a shared expectation that total war is off the menu, letting each side plan, spend, and signal without preparing for industrial-scale total war; historically carried by treaty architecture and crisis channels, now carried by inertia and residual retaliatory capability.
% TRANSFER_FUNCTION: Transfers war-absence diffusely to all parties; transfers foregone escalation options from great-power leaderships; transfers unmanaged catastrophic tail-risk onto civilian populations inside and outside the nuclear states; transfers fiscal resources from taxpayers into posture maintenance and destabilizing-technology programs.
% ABSENT_VOICES: Arms-control verification professionals — the seats that once administered the boundary — are out of the conversation: inspection regimes are dissolved and no institutional voice currently represents managed maintenance of the boundary. Civilian populations of the nuclear states likewise hold no seat in the posture decisions that set their exposure. Both absences are recorded as excluded stakeholders and remain commentary-grade.
% DISAPPEARANCE_RATIONALE: If the contraction vanished overnight — if total war between great powers became reachable tomorrow — great-power military planning, alliance commitments, and defense-industrial priorities would rearrange immediately: force structures would reopen mobilization questions, alliance guarantees would be repriced against new war-fighting scenarios, markets would reprice defense and insurance exposures, and crisis bargaining would lose the shared assumption that escalation has a floor, producing a period of acute instability while new equilibria are sought.
% FOUNDING_PROBLEM: After 1945, the problem was how to prevent a repeat of industrial-scale total war between great powers now armed with nuclear weapons — how to make total war unreachable, or so dangerous as to be irrational, and how to manage that boundary actively rather than leave it to chance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: declassified war-plan and crisis records (including the 1962 crisis archives), the SIPRI and Pugwash literatures, and official histories of the arms-control negotiations document the founding problem; contemporary statements by non-nuclear NATO members and Global South diplomatic blocs attest that it remains live. No corroborating source outside the arrangement's beneficiaries attests that the problem is solved.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.51, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE (piton): the arrangement's active management function has died — INF, Open Skies, CFE, and New START-era verification are gone — and what remains persists by inertia and performance, with no administered transition and no sunset mechanism (which is why the manifest's scaffold hypothesis was verified and rejected; see the scaffold_vs_piton_framing omega). METRICS: extractiveness 0.51 — the arrangement's service (war-absence) is real but decaying in credibility while its costs (posture spending, foregone options, unmanaged tail-risk) persist, and the net burden is worsening; suppression 0.30 — compliance is inertial and taboo-based rather than actively enforced, and suppression is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope); theater_ratio 0.61 — parades, declaratory policy, modernization rhetoric, and photo-op 'strategic stability dialogues' now outweigh the functional residue (real SSBN patrol tempo, warhead stewardship, actual R&D); accessibility_collapse 0.65 — the alternative paths (treaty restoration, deliberate capability rebuild, managed transition) are all politically closed or prohibitively costly, though none is physically impossible, so collapse is substantial but short of mountain-grade; resistance 0.35 — resistance is quiet (capability-building by investor states, revisionist nuclear signaling) rather than frontal. The measurement series run on one shared time grid (all three metrics at all seven points). The trajectories are monotonic decay with a visibly shallower slope across 2003–2015, corresponding to the 2009–2011 reset interlude when New START briefly slowed the enforcement collapse; the decay resumed steeply after 2014.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership seat the arrangement is an inherited predicament they administer: security collected roughly offsets options foregone, so their experienced extraction sits near the middle. From the population seats the same arrangement is unchosen exposure — a war-absence they consume but do not control, with the catastrophic downside loaded entirely onto them. From the investor-state seat the arrangement is a free option-writing desk: it asks nothing while they build capabilities that pay off only if it fails. From the excluded verification-professional seat it is a lost vocation — the management function they embodied was dissolved beneath them. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive destabilizing_technology_investor_states (mobile exit amplifies the subsidy side) and strategic_defense_contractors (arbitrage exit lets them collect under any posture outcome) toward the beneficiary end. Victim declarations drive civilian_populations_nuclear_states (trapped: relocation is irrelevant against intercontinental reach) and nonaligned_third_party_states (constrained: coalition diplomacy moves declaratory policy, never posture) toward the target end. The leaderships are deliberately LEFT UNDECLARED in the beneficiaries/victims arrays because their position is genuinely two-sided — they collect the arrangement's security and pay its foregone-option costs in roughly comparable measure — and declaring them on either side would falsify the structure; without a declaration the derivation falls back to the institutional power-atom default, which cannot express that symmetry, hence the explicit override pinning d at 0.5 for the institutional atom (no other stakeholder carries that atom, so the override is unambiguous). The excluded verification-professional seat is left on its fallback: excluded voices are commentary-grade and must not drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton claim guards against two symmetrical misclassifications. Against the mountain-reading (the contraction_reading's hazard): treating a technologically contingent, administratively abandoned arrangement as natural law launders political abandonment as physics and erases the reversal risk that is the arrangement's defining feature. Against the rope-reading (the dropping_reading's hazard): treating inertia as healthy coordination masks the atrophy, the theater, and the fact that nobody is minding the store. On mandatrophy proper: the founding mandate — actively prevent great-power total war — is still live, but the administrative machinery built to execute it has died; the arrangement now delivers its function accidentally, through residue rather than management. Because the function is still delivered, mandatrophy is not fully resolved — this is not yet a pure zombie — but the classification as inertial residue rather than scaffold reflects the absence of any administered transition toward an end-state. The manifest's scaffold hypothesis was tested against the structural gates (no sunset clause, no transition-management intent, no administered beneficiaries-of-transition) and failed them; the finding is preserved as an omega rather than forced into the type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency_dispute,
    'This story is one reading of the kernel total_war_reachability_boundary — the contingent_reachability_reading. The sibling readings (contraction_reading: permanent contraction, mountain-claimed; dropping_reading: retained reachability under stable deterrence coordination, rope-claimed) instantiate different constraints with different epsilon over the same referent arrangement. Where exactly do the readings diverge?',
    'Comparative classification across the three sibling stories: convergence on mechanism-attribution (natural limit vs coordination equilibrium vs inertial contingency) from independent technical net assessments would resolve the dispute; persistent divergence across evidence regimes marks it irreducibly conceptual.',
    'If the contraction_reading''s permanence claim is sustained, this story''s reversal-risk structure (investor-state beneficiaries, population tail-risk) is misdirected and the arrangement reclassifies toward mountain; if the dropping_reading''s coordination claim is sustained, the atrophy and theater are noise and the arrangement reclassifies toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency_dispute, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in the boundary''s persistence mechanism (law vs coordination vs inertia).').

omega_variable(
    reversal_threshold_location,
    'How far is the strategic-technology frontier from the threshold at which one or more great powers could plausibly damage-limit a retaliatory strike (midcourse-defense maturity, counterforce accuracy against mobile and deep targets, AI-enabled sensor-strike loops)?',
    'Independent technical net assessments of penetration-versus-interception exchange ratios, counterforce kill-chain reliability against hardened and mobile targets, and doctrinal statements that leak wargame findings into public posture documents.',
    'Near-term threshold arrival converts the contraction''s latent reversal into live instability and dates the arrangement''s terminal transition; a distant threshold extends the inertial phase indefinitely and strengthens the piton authoring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversal_threshold_location, empirical, 'Location of the technological threshold at which the current contraction of total-war reachability reverses.').

omega_variable(
    mobilization_atrophy_depth,
    'How deep is the atrophy of total-war-fighting capability — industrial mobilization capacity, munitions depth, manpower pipelines, civil defense — and could it be rebuilt on politically relevant timescales, making the contraction shallower than this reading assumes?',
    'Industrial-base audits, comparison of munitions consumption rates against production rates in ongoing high-intensity conflicts, and recruitment, retention, and reserve-mobilization exercise data.',
    'Shallow atrophy supports the dropping_reading''s retained-reachability claim and raises effective reachability now; deep atrophy confirms the atrophied-capability structure and lengthens the reversal lag.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_atrophy_depth, empirical, 'Depth and rebuild-timescale of the capability atrophy that constitutes the current contraction.').

omega_variable(
    investor_state_directionality_ambiguity,
    'Are destabilizing-technology investor states genuinely beneficiaries of the standing arrangement — free-riding on its inertia while writing cheap options on reversal — or payers bearing large modernization costs against uncertain payoff?',
    'Budget-share analysis of strategic-modernization programs against measurable posture benefits delivered under the current equilibrium, plus revealed preference in arms-control negotiating positions.',
    'If payers, their derived directionality rises and the arrangement looks more symmetrically burdensome, weakening the asymmetric-position signal; if beneficiaries, the current declaration stands and the free-ride structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_state_directionality_ambiguity, conceptual, 'Ambiguity in the investor-state seat''s structural position relative to the standing arrangement.').

omega_variable(
    scaffold_vs_piton_framing,
    'The decomposition manifest hypothesized a scaffold — a temporary constraint awaiting a technological transition; the authored analysis found no sunset mechanism, no administered transition, and no transition-management intent, only inertia. Is the contraction an unadministered scaffold or an inertial residue?',
    'Search for intentional transition-management signals: declared modernization roadmaps aimed at restoring war-termination options, planned treaty successors with built-in review-and-exit design, or doctrine documents framing the current boundary as explicitly transitional.',
    'A discovered transition-management layer would re-author the story as scaffold (has_sunset_clause: true, beneficiaries re-scoped) and change the engine''s certification path; its absence confirms the piton authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_piton_framing, conceptual, 'Framing under-determination between the manifest''s scaffold hypothesis and the authored inertial-residue analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_contingent_reachability_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t1991, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t1997, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1997, 0.33).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t1997, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t2003, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2003, 0.4).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t2003, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t2009, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2009, 0.45).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t2009, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t2015, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t2021, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2021, 0.57).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t2021, observed).
narrative_ontology:measurement(twrb_contingent_reachability_tr_t2026, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2026, 0.61).
narrative_ontology:measurement_basis(twrb_contingent_reachability_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(twrb_contingent_reachability_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.32).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t1991, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t1997, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t1997, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t2003, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t2003, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t2009, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2009, 0.4).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t2009, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t2015, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t2021, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t2021, observed).
narrative_ontology:measurement(twrb_contingent_reachability_be_t2026, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2026, 0.51).
narrative_ontology:measurement_basis(twrb_contingent_reachability_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(twrb_contingent_reachability_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.78).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t1991, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t1997, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1997, 0.71).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t1997, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t2003, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2003, 0.61).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t2003, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t2009, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t2009, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t2015, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t2021, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2021, 0.38).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t2021, observed).
narrative_ontology:measurement(twrb_contingent_reachability_su_t2026, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2026, 0.3).
narrative_ontology:measurement_basis(twrb_contingent_reachability_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the nuclear peace / total-war infeasibility' decomposes into three structurally distinct claims per the epsilon-invariance principle. contraction_reading authors epsilon for a permanent-barrier claim (negligible extraction, mountain-claimed, emerges_naturally asserted); dropping_reading authors epsilon for a coordination-equilibrium claim (low extraction, rope-claimed); this story authors epsilon for the SAME standing arrangement as the contingent reading sees it — an inertially maintained, technology-contingent contraction whose service is decaying (moderate extraction, piton-claimed). One referent, three readings, three reading-indexed epsilon values. Upstream/downstream: the contraction_reading's permanence claim is the historically established position and structurally influences both siblings; this reading's reversal thesis exerts downstream pressure on the dropping_reading's stability assumption. Each sibling file should link back to this one and carry its own half of this decomposition note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
