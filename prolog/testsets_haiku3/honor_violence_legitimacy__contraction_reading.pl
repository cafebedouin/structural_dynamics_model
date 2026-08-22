% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor-Violence Legitimacy Redefinition (Contraction Reading)
 *   domain: legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Between roughly 1550–1750, dueling transformed from a legitimate
 *   (sometimes mandatory) honor response to an increasingly unthinkable act.
 *   This constraint describes that transformation through the lens of
 *   conceptual redefinition: honor itself was redefined from a system that
 *   legitimated and required violent response to insult to a system that
 *   delegitimated violence as a valid honor expression. The reading assumes
 *   that the cognitive shift was primary—once the conceptual space of
 *   legitimate honor contracted to exclude violence, dueling became not
 *   merely forbidden but unthinkable. This is one reading of the contested
 *   kernel 'honor_violence_legitimacy'. Other readings (drop_reading,
 *   composite_reading) propose that the change was primarily due to rising
 *   external costs, or to simultaneous external and conceptual pressure. This
 *   constraint focuses on the redefinition itself as the extraction
 *   mechanism—the state and clergy seized the authority to define honor and
 *   reshaped it in ways that benefited state consolidation at the cost of the
 *   aristocratic warrior class's identity and standing.
 *
 * KEY AGENTS:
 *   - Centralized state authority — agenda-setter, redefines honor legitimacy, enforces through law, benefits from monopoly on force
 *   - Aristocratic warrior class — structural victim, identity-locked, loses capacity for autonomous honor defense
 *   - Honor-bound individuals — dual-positioned victim/beneficiary, trapped between old and new honor standards
 *   - Commercial mercantile class — beneficiary, gains status mobility and reduction of arbitrary violence
 *   - Clergy and moral authorities — co-agenda-setters, provide theological legitimation
 *   - Legal system and courts — enforcement agents, institutionalize new standard through prosecution and trial
 *   - Excluded traditionalists — would defend dueling legitimacy but are structurally silenced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.68).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.72).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor-Violence Legitimacy Redefinition (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '8d22ad46-7e07-4611-9ac3-4dac1b99f532').
narrative_ontology:cs_kernel_codification('8d22ad46-7e07-4611-9ac3-4dac1b99f532', distributed).
narrative_ontology:cs_authority_grounding('8d22ad46-7e07-4611-9ac3-4dac1b99f532', extraction).
narrative_ontology:cs_interpretation_layer_present('8d22ad46-7e07-4611-9ac3-4dac1b99f532').
narrative_ontology:cs_reading_relation('8d22ad46-7e07-4611-9ac3-4dac1b99f532', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d22ad46-7e07-4611-9ac3-4dac1b99f532', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('8d22ad46-7e07-4611-9ac3-4dac1b99f532', foundational, honor_concept_excludes_violence).
narrative_ontology:cs_axiom_status(honor_concept_excludes_violence, holdable).
narrative_ontology:cs_axiom_grounding('8d22ad46-7e07-4611-9ac3-4dac1b99f532', honor_concept_excludes_violence, deontological).
narrative_ontology:cs_axiom('8d22ad46-7e07-4611-9ac3-4dac1b99f532', secondary, state_monopoly_requires_delegitimation).
narrative_ontology:cs_axiom_status(state_monopoly_requires_delegitimation, holdable).
narrative_ontology:cs_axiom_grounding('8d22ad46-7e07-4611-9ac3-4dac1b99f532', state_monopoly_requires_delegitimation, instrumental).
narrative_ontology:cs_reference_frame('8d22ad46-7e07-4611-9ac3-4dac1b99f532', honor_violence_legitimacy).
narrative_ontology:cs_drift_state('8d22ad46-7e07-4611-9ac3-4dac1b99f532', post_redefinition_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8d22ad46-7e07-4611-9ac3-4dac1b99f532', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, centralized_state_authority).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, commercial_mercantile_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, honor_bound_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, honor_bound_individuals).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, clergy_and_moral_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the redefinition of honor through law, proclamation, theology, and institutional practice. Benefits from monopoly on legitimate violence and reduction of aristocratic internal conflict. The redefinition is the enforcement mechanism itself—by making dueling conceptually incoherent within the new honor framework, the state does not merely forbid behavior but transforms what counts as thinkable. Administers the constraint through courts, clergy coordination, and public rituals that continually reinforce the new standard.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, centralized_state_authority, agenda_setter,
    institutional, generational, analytical, national).

% Loses the legitimacy of their primary identity-constituting practice. Dueling was not optional or incidental—it was the performed proof that they possessed honor, that they were willing to die for their standing. The redefinition strips this away. They cannot exit the honor system (they are still bound by honor's demands) without ceasing to be what they understand themselves to be. They must either accept the new definition (which requires submitting to external authority) or accept loss of honored status. Identity-locked means they carry the constraint with them; exit is not available.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class, payer,
    powerful, biographical, identity_locked, national).

% Individuals whose self-conception is fused with honor-responsiveness experience the constraint as a double bind. To maintain honor-as-redefined (obedience to law, civic virtue) requires accepting submission to external authority—contradicting the autonomy and prideful self-direction that honor traditionally meant. To reject the redefinition is to be read as dishonorable by the new standard. They also benefit materially from the reduction in violent deaths (their own and kin), but that benefit is inseparable from the loss of a meaningful self-expression category. Identity-locked: they cannot simply exit honor; it is constitutive to their self-understanding.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_bound_individuals, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, honor_bound_individuals, beneficiary).

% Benefits from the redefinition in two ways: (1) reduction of arbitrary violence that disrupts commerce, credit networks, and economic stability; (2) opening of status-advancement pathways that do not require aristocratic birth or warrior prowess—wealth and professional achievement become convertible to social honor without the existential identity risk that dueling carries. Can adopt the new honor standard without identity loss because their status was never constituted through dueling readiness. Mobile exit: they could participate in either honor system, but have no reason to; the new one serves them better.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, commercial_mercantile_class, beneficiary,
    organized, biographical, mobile, national).

% Provide theological and moral legitimacy for the redefinition: sermons arguing that true honor lies in obedience to divine and human law, that dueling is prideful rebellion against God's order, that honor-through-violence contradicts Christian virtue. Benefit from increased alignment between church teaching and state enforcement; their voice becomes the authoritative articulation of what honor now means. Co-agenda-setters with the state: they do not enforce the redefinition raw, but they provide the moral framework that makes it thinkable and acceptable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, clergy_and_moral_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, clergy_and_moral_authorities, beneficiary).

% Codifies and enforces the ban through law: makes dueling a capital crime, prosecutes cases, publicizes trials and executions. Courts serve as the stage on which the new honor standard is publicly established and performed. The law is not merely forbidding a behavior—it is institutionalizing the claim that dueling is illegitimate even as a matter of personal honor. Legal system enforcers of the constraint, agenda-setters in the sense that they articulate and perform the new standard in high-visibility contexts.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_system_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and factions who would defend the legitimacy of dueling as a valid honor response—rival aristocratic factions, military traditionalists, honor-code philosophers—are structurally excluded from the definition-setting process. Their voice is present only as dissent, resistance, or private adherence to the old code; it is never admitted as a legitimate articulation of what honor means. The constraint's operation depends partly on keeping their legitimating speech out of the authoritative conversation. Constrained exit: they cannot exit honor itself (they are still bound by honor's demands), but they are prevented from legitimating their understanding of it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, excluded_honor_traditionalists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, centralized_state_authority).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, state-centered definition of what counts as honorable, replacing fragmented, kinship-based, violence-responsive honor codes with a law-based, obedience-centered standard. Solves the collective-action problem of preventing aristocratic warrior factions from legitimating internal violence that destabilizes centralized state authority. Coordinates expectations about honor across the entire polity on a single standard enforced by state institutions and moral authorities.
% TRANSFER_FUNCTION: Moves the authority to define honor from distributed, kinship-based actors to centralized state and clerical institutions. Transfers the capacity to claim honor from the aristocratic warrior class (who once demonstrated it through readiness for violence) to state authorities, clergy, merchants, and professionals (who demonstrate it through obedience to law, service to institutions, commercial success, and civic virtue). Reallocates status pathways from birth-based (aristocracy) to achievement-based (state service, commerce, professional credentials).
% ABSENT_VOICES: Aristocratic traditionalists, proponents of rival honor codes, warrior-class philosophers who would defend violence as an integral part of honor's meaning—these voices are excluded from the authoritative definition-setting process. They appear only as resistance or dissent, never as legitimate articulations of what honor requires. Military societies and honor-code traditions that would maintain dueling's legitimacy are structurally silenced.
% DISAPPEARANCE_RATIONALE: If the redefinition and its enforcement disappeared, the aristocratic warrior class would face an identity opening: dueling could re-emerge as a live option for honor defense, potentially reinvigorating internal violence and destabilizing state authority. The status hierarchy would reorganize with birth-based, warrior-proven honor reasserting value. Commercial and professional status advancement would become less convertible into social respect. State monopoly on legitimate violence would become contestable. The entire legitimacy structure would shift.
% FOUNDING_PROBLEM: Centralized state authority requires monopoly on the legitimate use of force. Aristocratic warrior classes, historically legitimated in defending their own honor through violence, constitute competing sources of legitimate force. Dueling as a valid honor response means that individuals and families can claim legitimacy for violence outside state authorization. Early modern state consolidation requires suppressing this alternative legitimacy source and establishing that only state-authorized violence is legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Early modern historians document that state consolidation (Tilly, Spruyt, Philpott) required disarming the aristocracy and establishing state monopoly on legitimate violence. The founding problem's persistence is corroborated by comparative analysis: states that failed to delegitimize rival violence sources (including honor-justified dueling) experienced recurring aristocratic conflict and failed to consolidate. This corroboration comes from outside the benefiting parties—from structural historians analyzing the pattern across multiple societies and centuries.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series (interval 0–200, roughly 150 years) shows extractiveness rising from 0.42 to 0.68 then plateauing. This trajectory reflects the redefinition's consolidation phase. Early extractiveness (0.42 at t=0) captures the initial imposition—the state and clergy announce the redefinition, but it is still contested and resisted. Extractiveness rises sharply through t=60 (0.59) as enforcement intensifies and the redefinition begins to shape behavior and self-conception across generations. By t=130–200, extractiveness plateaus at 0.68—the redefinition is hegemonic, but some extraction persists because the aristocracy never fully reconciles to the loss of honor-through-violence (this unresolved tension is captured by an omega). Suppression_requirement shows a similar pattern: it rises from 0.55 to 0.73 as enforcement machinery is built, then stabilizes at 0.72. The plateau suggests that suppression, while stable, is increasingly internalized (identity-fusion makes external force less necessary). Theater_ratio rises consistently from 0.22 to 0.41, indicating that as the redefinition consolidates, more of the constraint's operation is theatrical—public ceremonies reaffirming the new honor standard, trials that perform state justice, rituals that reinforce obedience-as-honor. By t=170–200, the theater ratio has risen substantially, suggesting that the constraint increasingly operates through performance and meaning rather than raw enforcement. This is a piton-warning sign: the founding problem (state monopoly on force) is now solved by structural state strength, not by the redefinition itself, yet the redefinition persists due to institutional inertia. Accessibility_collapse is high (0.78) because once the redefinition takes hold, dueling becomes genuinely unthinkable—not merely forbidden but conceptually incoherent. Resistance is moderate-high early (0.52 at t=0–30) when the constraint is being imposed against active aristocratic resistance, then declines as younger generations are socialized into the new framework from birth.
 *
 * PERSPECTIVAL GAP:
 *   The state authority experiences the constraint as pure coordination: establishing a unified legitimacy standard that enables state consolidation and reduces the internal violence that destabilizes central authority. From this seat, dueling was eliminated because it had to be—it was an alternative legitimacy source that competed with state authority. The aristocratic warrior class, by contrast, experiences the same constraint as pure extraction: loss of the capacity to determine and defend honor through their own means. From their seat, the state did not merely ban dueling—it stole honor itself, replacing an autonomous, self-directed moral practice with one subject to external definition and control. For honor-bound individuals, the constraint is a cognitive trap—a double bind in which accepting the new honor standard requires renouncing autonomy (the very thing honor traditionally meant), while rejecting the standard means being read as dishonorable by the new measure. The commercial class experiences it as beneficial coordination (clearer rules, predictable status pathways, reduced violence-disruption) with no identity loss, because their status was never constituted through warrior prowess. The engine should compute these seat divergences from the structural data: the state's institutional power and analytical exit_options produce a beneficiary d; the aristocracy's identity_locked exit and payer role produce a target d; the dual-positioned honor-bound individuals split between beneficiary and victim directionalities. The perspectival gap is not a defect in the data—it is the core signal the constraint story is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the aristocratic warrior class (powerful, identity_locked, payer role) is high—nearly full target (d ~0.78–0.82). The override value of 0.78 reflects the structural trap: they are powerful actors in a static sense (they have martial resources, social standing, organized networks), but identity_locked makes their power inaccessible to resistance (they cannot simply decide to stop being honor-bound; honor is constitutive to their selfhood). The constraint forces a choice between accepting submission (adopting the new honor standard) or accepting dishonorable status. For honor-bound individuals (moderate power, identity_locked, dual-positioned), directionality is similarly high on the victim side (d ~0.75) but with a secondary beneficiary element (they do benefit from reduction in violent death, so effective extraction is damped somewhat by that offsetting benefit). For the state authority (institutional, analytical exit_options, beneficiary role), directionality is low (d ~0.15–0.20): they gain substantial authority without bearing the identity cost. For the commercial class (organized, mobile exit_options, beneficiary role), directionality is moderate-low (d ~0.25–0.35): they benefit from the constraint but do not actively enforce it, so their extractive position is limited. The directionality_overrides entry for powerful actors at 0.78 reflects the trap: raw power alone cannot escape an identity-locked constraint; the directionality must be high despite institutional strength because the constraint operates through meaning (honor redefinition) rather than raw coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing state monopoly on legitimate violence, unified legitimacy standard to prevent aristocratic internal conflict) is documented as live at the end of the interval. However, the theater_ratio rising from 0.22 to 0.41 suggests a risk of mandate atrophy: if dueling is already unthinkable and the state's monopoly on force is maintained by structural strength (not by the redefinition itself), is the constraint still solving the founding problem, or is it increasingly maintained as institutional theater? The founding_problem_status is authored as 'live', but an omega variable (mandatrophy_consolidation) flags this as a live question: has the constraint achieved genuine internalization, or is it increasingly a piton—atrophied in functional necessity but persisting due to institutional inertia? This is not a contradiction in the data; it is a structural ambiguity the constraint story highlights. The commentary explains why: the redefinition successfully consolidated the founding standard (honor-without-violence), but over time the constraint's function may have shifted from actively suppressing an alternative legitimacy source to ceremonially maintaining the boundary between state and aristocratic authority. This shift would move the constraint toward piton territory, though measurements at t=200 alone cannot confirm it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_vs_conceptual_capture,
    'Did dueling become unthinkable because the conceptual space of honor contracted (internalized redefinition), or because identity-fusion with the warrior role meant that losing access to dueling felt like losing selfhood (making cognitive adoption of the new standard impossible)?',
    'Historical analysis of diary, correspondence, and literary evidence from the transition period: do accounts document a genuine shift in how people understood honor itself, or a sense of loss and alienation from a redefined category they never accepted? Comparison of age cohorts: did younger generations (socialized into the new standard from childhood) genuinely think dueling was unthinkable, or did they perform acceptance while retaining the old framework privately?',
    'If contraction (true redefinition), the constraint operates through changed meaning—a genuine shift in what counts as honorable. If identity-fusion (cognitive lock), the constraint operates through coerced identity-loss—dueling remains thinkable but unsayable, and the suppression is primarily internalized. Different impact on classification: pure redefinition approaches rope (coordination of meaning); coerced identity-loss approaches snare (asymmetric extraction with cognitive suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_conceptual_capture, empirical, 'Whether the constraint''s primary mechanism is conceptual redefinition or identity-fusion suppression').

omega_variable(
    redefinition_authorship,
    'Who genuinely authored the redefinition of honor? State authority and clergy claim to have redefined it; did they impose a redefinition from above, or did merchant and professional classes (with less identity-investment in warrior honor) organically drift toward a different honor standard, which state and clergy then formally codified?',
    'Genealogical analysis of honor discourse in merchants'' records, professional guilds, and non-aristocratic sources predating state proclamations: did alternative honor standards exist before legal bans, or only after? Social network analysis of who first articulated the new standard.',
    'If state/clergy imposed (top-down redefinition), the constraint is primarily extractive—they seized the power to define honor. If merchant/professional classes originated it (bottom-up drift), the constraint involves genuine coordination—a new legitimacy standard emerged from multiple actors and was then institutionalized. Different type signatures: top-down snare; bottom-up tangled_rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redefinition_authorship, empirical, 'Whether redefinition was imposed from above or emerged from multiple actors').

omega_variable(
    mandatrophy_consolidation,
    'Has the constraint achieved stable hegemony where dueling is genuinely unthinkable, or is it increasingly maintained through theatrical ritual while the underlying identity tension remains unresolved?',
    'Long-term historical follow-up (t=200–400): does the constraint persist by continued enforcement and theater, or does it become self-maintaining through internalization? Do later generations experience dueling as unthinkable or as suppressed? What happens under crisis (war, state collapse) when enforcement capacity decreases—does dueling re-emerge?',
    'If stable hegemony, the constraint has solved the founding problem and transitioned from active extraction to coordination (a completed transformation). If theatrical maintenance, the constraint is increasingly a piton—atrophied function (the founding problem is solved by structural state strength, not by the redefinition itself), but persists due to institutional inertia. Different trajectory implications: hegemony suggests rope; theater suggests piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_consolidation, empirical, 'Whether the redefinition has achieved genuine internalization or relies on continued performative maintenance').

omega_variable(
    reading_frame_ambiguity,
    'Is this constraint best described as a contraction of the honor concept itself, or as the replacement of one honor system (violence-responsive) with another equally elaborate honor system (law-obedient)? The boundary between ''contraction'' and ''replacement'' depends on whether violence is constitutive to honor or merely one expression among many.',
    'Theological and philosophical analysis of the honor tradition: in classical and early modern thought, is honor inherently linked to autonomous, potentially violent response to insult (making exclusion of violence a true contraction), or is honor a broader category that can accommodate multiple legitimacy standards? Comparison with honor systems that exclude violence without crisis (e.g., some mercantile or clerical honor codes).',
    'If contraction (violence constitutive): the constraint''s operation involves genuine cognitive closure—the space of what counts as honorable has been narrowed. If replacement (violence optional): the constraint''s operation involves reallocation—violence-based honor is displaced by alternative honor markers, but both remain available. Different omega_c framing: contraction suggests foreclosure-risk; replacement suggests coexistence-with-reallocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_ambiguity, conceptual, 'Whether this reading claims honor''s conceptual contraction or system replacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__contraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(hono_tr_t60, observed).
narrative_ontology:measurement(hono_tr_t90, honor_violence_legitimacy__contraction_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement_basis(hono_tr_t90, observed).
narrative_ontology:measurement(hono_tr_t130, honor_violence_legitimacy__contraction_reading, theater_ratio, 130, 0.4).
narrative_ontology:measurement_basis(hono_tr_t130, observed).
narrative_ontology:measurement(hono_tr_t170, honor_violence_legitimacy__contraction_reading, theater_ratio, 170, 0.41).
narrative_ontology:measurement_basis(hono_tr_t170, observed).
narrative_ontology:measurement(hono_tr_t200, honor_violence_legitimacy__contraction_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement_basis(hono_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__contraction_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(hono_be_t60, observed).
narrative_ontology:measurement(hono_be_t90, honor_violence_legitimacy__contraction_reading, base_extractiveness, 90, 0.64).
narrative_ontology:measurement_basis(hono_be_t90, observed).
narrative_ontology:measurement(hono_be_t130, honor_violence_legitimacy__contraction_reading, base_extractiveness, 130, 0.67).
narrative_ontology:measurement_basis(hono_be_t130, observed).
narrative_ontology:measurement(hono_be_t170, honor_violence_legitimacy__contraction_reading, base_extractiveness, 170, 0.68).
narrative_ontology:measurement_basis(hono_be_t170, observed).
narrative_ontology:measurement(hono_be_t200, honor_violence_legitimacy__contraction_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement_basis(hono_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__contraction_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__contraction_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(hono_su_t60, observed).
narrative_ontology:measurement(hono_su_t90, honor_violence_legitimacy__contraction_reading, suppression_requirement, 90, 0.72).
narrative_ontology:measurement_basis(hono_su_t90, observed).
narrative_ontology:measurement(hono_su_t130, honor_violence_legitimacy__contraction_reading, suppression_requirement, 130, 0.73).
narrative_ontology:measurement_basis(hono_su_t130, observed).
narrative_ontology:measurement(hono_su_t170, honor_violence_legitimacy__contraction_reading, suppression_requirement, 170, 0.72).
narrative_ontology:measurement_basis(hono_su_t170, observed).
narrative_ontology:measurement(hono_su_t200, honor_violence_legitimacy__contraction_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement_basis(hono_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (contraction_reading) of the contested kernel honor_violence_legitimacy. The sibling readings (drop_reading, composite_reading) are separate constraints with different structural properties. The contraction reading emphasizes conceptual redefinition as the primary mechanism; the drop reading emphasizes external cost structure; the composite reading treats both mechanisms as simultaneous. The three stories are linked by the shared kernel but maintain independent ε values, beneficiary/victim structures, and type classifications. See commentary.kernel_context for the reading distinctions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
