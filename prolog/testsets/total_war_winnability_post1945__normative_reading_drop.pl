% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Illegitimacy of Total War
 *   domain: international_relations/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the normative-reading interpretation of the
 *   post-1945 total-war winnability kernel. The reading asserts that total
 *   war was NOT physically eliminated by nuclear weapons
 *   (structural_contraction_reading) nor erased by ideational drift in
 *   strategic culture alone (strategic_culture_drift), but rather
 *   delegitimized through a coordinated normative commitment: Article 2(4) of
 *   the UN Charter, ratified humanitarian law, war crimes prosecutions, and
 *   institutional reinforcement of the ban on wars of conquest and civilian
 *   destruction. Under this reading, total war remains technically feasible
 *   but normatively illegitimate — the constraint operates through legitimacy
 *   denial, not physical impossibility. A state that waged total war would
 *   face international pariah status, intervention, ICC prosecution, and
 *   institutional isolation, making the option strategically irrational even
 *   though the physical capacity persists. This is a Rope-class coordination
 *   mechanism: it solves a genuine collective-action problem (preventing
 *   escalation to mutual annihilation) through shared commitment to a
 *   boundary rule. The claim and metrics are aligned: both describe a genuine
 *   coordination function with modest extraction (the constraint locks in
 *   post-war territorial distribution, preventing revisionist challenges via
 *   total mobilization) and active enforcement (prosecution of violations,
 *   institutional pressure, legitimacy denial). The kernel contest frames
 *   this reading against two structural alternatives: if the constraint's
 *   force derives from nuclear physics rather than normative commitment, or
 *   from cultural drift rather than formal treaty, the classification would
 *   differ. This reading treats the normative framework as primary and
 *   causal.
 *
 * KEY AGENTS:
 *   - Global civilian populations (beneficiaries, trapped exit, organized power) — gain protection from deliberate targeting; cannot individually access the benefit
 *   - Revisionist/challenger powers (payers, institutional power, constrained exit) — bear the cost of strategic immobility; retain total-war capacity but cannot employ it without delegitimation
 *   - Hegemonic powers (agenda-setters, institutional power, arbitrage exit) — set and maintain the norm; benefit from locked status quo; can reinterpret scope without exiting
 *   - Non-hegemonic states (beneficiaries, moderate power, identity-locked exit) — locked into the institutional order; benefit from protection; cannot credibly repudiate without isolation
 *   - Humanitarian law implementers (agenda-setters + payers, organized power, constrained exit) — enforce the norm through prosecution and monitoring; constrained by sovereignty limits
 *   - Strategic culture carriers (excluded, moderate power, identity-locked) — carry pre-1945 logic within military traditions; excluded from formal norm-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.38).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Illegitimacy of Total War").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/commitment_systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '3ffd0350-83c7-460c-8371-68d7237fba0f').
narrative_ontology:cs_kernel_codification('3ffd0350-83c7-460c-8371-68d7237fba0f', formalized).
narrative_ontology:cs_authority_grounding('3ffd0350-83c7-460c-8371-68d7237fba0f', extraction).
narrative_ontology:cs_interpretation_layer_present('3ffd0350-83c7-460c-8371-68d7237fba0f').
narrative_ontology:cs_reading_relation('3ffd0350-83c7-460c-8371-68d7237fba0f', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ffd0350-83c7-460c-8371-68d7237fba0f', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('3ffd0350-83c7-460c-8371-68d7237fba0f', foundational, normative_illegitimacy_is_binding).
narrative_ontology:cs_axiom_status(normative_illegitimacy_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('3ffd0350-83c7-460c-8371-68d7237fba0f', normative_illegitimacy_is_binding, conventional).
narrative_ontology:cs_axiom('3ffd0350-83c7-460c-8371-68d7237fba0f', foundational, institutional_enforcement_sustains_constraint).
narrative_ontology:cs_axiom_status(institutional_enforcement_sustains_constraint, holdable).
narrative_ontology:cs_axiom_grounding('3ffd0350-83c7-460c-8371-68d7237fba0f', institutional_enforcement_sustains_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('3ffd0350-83c7-460c-8371-68d7237fba0f', post_1945_treaty_framework).
narrative_ontology:cs_drift_state('3ffd0350-83c7-460c-8371-68d7237fba0f', contemporary_hybrid_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ffd0350-83c7-460c-8371-68d7237fba0f', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, non_hegemonic_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_seeking_total_victory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_implementers).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_principle).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, sovereignty_equality_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, collective_security_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from deliberate targeting and systematic destruction via Article 2(4) and humanitarian law. Do not choose to be protected — the norm operates on them as a structural shield whose legitimacy depends on universal acceptance. Cannot exit the constraint without exiting state jurisdiction itself. Benefit is not earned and cannot be individually captured.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    organized, generational, trapped, global).

% Constrained from pursuing total war strategy (mobilization of entire population, destruction of enemy economy and civilian morale as war aims) by normative prohibition in Article 2(4) and laws of armed conflict. Retain the physical capacity to wage total war but face legitimacy cost, third-party intervention, and war crimes accountability if they attempt it. Exit would require rejecting the international legal order entirely.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_seeking_total_victory, payer,
    institutional, biographical, constrained, global).

% Set and maintain the normative framework through the UN Charter, Security Council, and enforcement of humanitarian law doctrine. Benefit from the norm's operation: it locks in territorial status quo and prevents challenger states from mobilizing populations for existential wars. Can redefine the norm's scope through interpretation (e.g., counterinsurgency framing, humanitarian intervention doctrine) without formally exiting.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, established_hegemonic_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% International courts, ICRC, human rights monitors, war crimes prosecutors. Enforce the norm through documentation, investigation, and prosecution. Bear the cost of standing against powerful actors who violate the norm; operationally constrained by state sovereignty and enforcement capability limits. Dependent on hegemonic powers' political support for major prosecutions.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_implementers, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_implementers, payer).

% Locked into the post-1945 rules by institutional identity: membership in the UN, participation in international institutions, diplomatic legitimacy all depend on accepting the prohibition on total war and Article 2(4). Exit would mean international pariah status and isolation. Benefit from the constraint's protection against existential threats from larger neighbors; cannot credibly repudiate it without losing standing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, non_hegemonic_states, beneficiary,
    moderate, biographical, identity_locked, global).

% Military intellectual traditions, officer corps cultures, strategic schools of thought that once treated total war as a legitimate strategy option (Clausewitz lineage, conquest-based security models, civilizational conflict frames). Excluded from formal treaty negotiation and norm-setting but carry the pre-1945 strategic logic within professional communities. Their voices are present in actual strategic planning but illegitimate in formal discourse.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, strategic_culture_carriers, excluded,
    moderate, generational, identity_locked, global).

% Measure the constraint's operation through case studies, doctrinal development, and violations. Analyze whether the norm is eroding (terrorism, drone strikes, information warfare, asymmetric conflict framing challenge the boundary). Can document the gap between the norm's stated scope and actual practice.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_scholars, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, established_hegemonic_powers).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing escalation to mutual annihilation: absent a shared commitment to restrict means (bombardment of civilians, destruction of economic substrate, mobilization for existential war), any rational actor facing an existential threat would employ total war, creating a security dilemma where all parties escalate toward mutual destruction. The norm coordinates on a boundary: warfare permitted, total war (as a distinct category of intensity and intention) prohibited.
% TRANSFER_FUNCTION: Moves strategic constraint from revisionist/challenger powers toward status-quo powers. Challenger states give up the option of all-out mobilization for existential victory; in exchange, they receive protection of civilian populations and non-combatant infrastructure, which makes limited war economically and politically recoverable. Status-quo powers gain a lock on the post-war distribution without facing total-war threats from within their sphere.
% ABSENT_VOICES: Strategic cultures that treat total war as a rational option (current revisionist powers, historical great-power militaries, any actor for whom limited war appears to guarantee defeat). They are excluded from formal norm-setting; their objections appear in classified strategic planning, military academies, and revisionist manifestos, not in treaty negotiation. Military planners in rising powers harbor doubts about the norm's binding force under existential threat.
% DISAPPEARANCE_RATIONALE: If the post-1945 normative prohibition on total war vanished overnight, major powers would immediately face renewed existential-war threats from rivals; civilian populations would become explicit targeting categories; industrial mobilization and scorched-earth strategies would be optionally employed by states facing conventional defeat. The international system would revert toward pre-1945 dynamics: arms racing, preventive wars, and escalation dynamics as states prepared for wars of annihilation. The UN system itself would collapse as the foundational norm permitting it was rejected.
% FOUNDING_PROBLEM: World War II and prior total wars (Napoleonic Wars, American Civil War, Franco-Prussian War, WWI) demonstrated that mobilizing entire populations for wars of national survival produced catastrophic casualties, civilizational exhaustion, and mutual devastation with no decisive victor. The founding problem was: how to prevent rational actors facing existential threat from employing total-war strategy, given that total war offers the highest probability of defeating an existential threat if the opponent does not also employ it?
% FOUNDING_PROBLEM_CORROBORATION: The problem's liveness is attested by: (1) ongoing arms control negotiations and strategic stability doctrines (mutual assured destruction, extended deterrence) that assume total war remains the lurking threat if deterrence fails; (2) military planning in all major powers that maintains contingency plans for total mobilization; (3) scholarly consensus in strategic studies that the post-1945 order is built on suppression of this threat, not elimination of it; (4) periodic challenges to the norm (drone strikes, cyber warfare, information warfare) that test whether the norm holds or is being reinterpreted. Parties outside the benefiting consensus (revisionist states, historical military traditions) explicitly dispute the norm's permanence, attesting its contestation.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured low-to-moderate (0.38 at interval end) because the constraint's primary function is genuine coordination (prevent mutual annihilation), not rent extraction. However, extractiveness is not zero because the constraint locks in the post-1945 territorial distribution, preventing revisionist great-power wars that might restructure the system. A rising power wishing to overturn regional hegemony through total mobilization finds itself constrained by this norm — this is a real cost imposed on a minority (revisionist states) for the benefit of the majority (status-quo powers and civilian populations). Suppression is moderate (0.42) because the constraint's enforcement depends on active institutional pressure (war crimes prosecutions, institutional isolation, third-party intervention), not merely on rational calculation. The suppression trajectory is relatively flat because the enforcement mechanism has stabilized since 1945 — it is not intensifying dramatically but remains stable at a level sufficient to deter most overt attempts at total war. Theater ratio is low (0.18) because the constraint's functional core (preventing escalation to mutual annihilation) is genuine and continuously valuable; the theatrical component grows slightly over time as discourse becomes more refined and legal interpretations proliferate, but the ratio remains modest. Accessibility of alternatives collapses substantially (0.72) once the constraint is understood: a state that attempts total war immediately faces multinational intervention, prosecution, institutional exclusion, and reputational destruction — the alternatives (compliance or exit from the international system) are the only accessible paths. Resistance is moderate (0.55) because revisionist powers and military cultures do resist the norm through strategic ambiguity (cyber warfare, drone strikes, information warfare that test the boundary), even though they do not openly reject it. The measurement series tracks the constraint's operation over 80 years (1945-2025): modest growth in extractiveness as the benefits of locking in the territorial distribution become more apparent, slight growth in theater as legal doctrine proliferates and reinterpretation becomes more visible, and stable suppression as enforcement machinery has matured and stabilized.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (global civilian populations, non-hegemonic states) and the agenda-setter seats (hegemonic powers, humanitarian law implementers) should compute as Rope: genuine coordination solving an existential collective-action problem, with legitimate benefits accruing to those protected. The payer seats (revisionist powers, strategic culture carriers) compute the same constraint as asymmetrically extractive: they are constrained from a strategic option (total war) that their rivals theoretically retain (if the constraint dissolved tomorrow), and the constraint serves the security interests of the status-quo powers more than theirs. From the revisionist perspective, the norm operates as a coordination mechanism that locked in the distribution favorable to the winners of WWII, preventing their challenge through the most decisive means available. From the beneficiary perspective, the constraint is the foundation of civilizational survival. Both are structurally true — the divergence is not a measurement error but the core seat-dependent computation the engine performs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation runs as follows: Global civilian populations and non-hegemonic states are beneficiaries (low d, benefiting from protection); their power and exit options (organized/moderate power, trapped/identity_locked exit) yield d in the 0.15-0.35 range — they benefit from the constraint and would suffer its dissolution, so they sit at the beneficiary end. Hegemonic powers are agenda-setters who benefit (low d by beneficiary status) but also capture the enforcement capacity and territorial lock-in; their institutional power and arbitrage exit yield d near 0.35-0.45 — they set the rule that benefits them. Revisionist powers are payers (high d, constrained by the rule, bear the cost of immobility); their institutional power and constrained exit yield d near 0.65-0.80 — they are the true targets of the extraction mechanism. Humanitarian law implementers are dual-positioned: they benefit from the norm (it validates their institutions and missions) but also bear the cost of enforcement (standing against powerful violators, operational constraints from sovereignty limits) — their d falls near 0.50-0.55, symmetric or slightly toward payer. No directionality overrides are required; the structural derivation from beneficiary/victim + exit + power captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits no mandatrophy: the founding problem (prevention of total-war escalation) remains live, the norm has not outlived its function, and the institutional machinery (UN, ICC, humanitarian law doctrine) continues to serve its original purpose. The slight rise in theater ratio (0.08 → 0.18) reflects doctrinal refinement and the emergence of boundary cases (drone strikes, cyber warfare), not functional decay. The constraint remains actively enforced (prosecution of war crimes, institutional pressure against violators), though the enforcement machinery sometimes struggles against state power (ICC arrest warrants ignored, Security Council vetoes preventing prosecution of great powers). The constraint does NOT yet exhibit the piton pattern (atrophied function maintained theatrically while costs diffuse); it remains operationally functional, with real coordination benefits and real (if asymmetric) costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_force_vs_structural_impossibility,
    'Does the post-1945 norm against total war operate primarily through normative delegitimation (this reading), or through structural elimination by nuclear weapons (the structural_contraction reading)? That is: would a nuclear-weapons-free world inhabited by states that formally committed to Article 2(4) maintain the ban on total war indefinitely, or would the absence of nuclear deterrence allow revisionist powers to return to total-war strategy?',
    'Historical counterfactual analysis (unlikely to be resolved empirically); theoretical analysis of whether the norm is self-sustaining without nuclear backup. Examine cases where nuclear deterrence failed or was absent (conventional wars post-1945) to assess whether the normative constraint held independent of nuclear fear.',
    'If normative force is primary, the constraint is classified as Rope (coordination mechanism) and persists even absent nuclear weapons. If structural force is primary, the constraint is Mountain (physical law) and the normative apparatus is epiphenomenal. This reading''s classification as Rope depends on the answer being ''normative force is primary''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_force_vs_structural_impossibility, empirical, 'Whether the post-1945 norm''s binding force derives from normative commitment or from structural (nuclear) impossibility.').

omega_variable(
    legitimacy_denial_vs_physical_capacity,
    'How much of the measured suppression (0.42) reflects the constraint''s normative enforcement (prosecution, institutional isolation, legitimacy denial) versus rational fear of nuclear escalation or conventional defeat? If a state were assured of nuclear immunity and conventional victory in a total war, would normative illegitimacy alone deter it?',
    'Analyze revisionist state rhetoric and strategic planning documents to assess whether they cite legal risk or normative delegitimation as a constraint on total-war strategy, or whether they focus on military defeat risk. Compare suppression requirements in nuclear and non-nuclear conflict contexts.',
    'If normative enforcement dominates suppression, the constraint''s durability depends on continued institutional commitment and prosecution capacity. If military defeat risk dominates, the norm is a veneer over rational calculation. High reliance on normative enforcement increases vulnerability to institutional collapse (UN Security Council dysfunction, ICC irrelevance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_denial_vs_physical_capacity, empirical, 'The relative weight of normative versus military-rational components in the suppression mechanism.').

omega_variable(
    asymmetric_extraction_or_status_quo_lock,
    'Is the measured extractiveness (0.38) extractive rent-seeking by status-quo powers using the norm as a lock-in device, or is it a legitimate cost borne by revisionist powers as the price of civilization-wide stability? That is: are revisionist powers being exploited through the constraint, or are they accepting a fair bargain (protection of their own civilians in exchange for accepting the existing territorial distribution)?',
    'Normative analysis of whether the constraint is justified as applied: do non-aligned and developing states whose interests were not represented in the post-1945 settlement accept the constraint as legitimate, or do they treat it as imposed? Compare the interests of rising powers under the constraint to their interests if the constraint dissolved.',
    'If the constraint is perceived as legitimate by all major parties, it remains Rope (coordination). If major parties treat it as illegitimate imposition, it trends toward Snare (extraction with coerced consent). The engine computes this from the beneficiary/victim declarations and resistance levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_extraction_or_status_quo_lock, preference, 'Whether the constraint redistributes security benefits fairly across parties or extracts rents from revisionist powers.').

omega_variable(
    boundary_erosion_via_reinterpretation,
    'The boundary between ''total war'' and ''limited war'' is maintained through doctrinal interpretation (humanitarian law, law of armed conflict). As military technology evolves (autonomous weapons, cyber warfare, information warfare, drone strikes), is the boundary being reinterpreted or maintained? Are newer forms of conflict being absorbed into the ''limited war'' category, or is the constraint boundary eroding?',
    'Track legal doctrine and state practice over time: assess whether novel conflict modes trigger the same institutional response (prosecution, delegitimation, intervention) as traditional total-war indicators, or whether they fall into regulatory gaps.',
    'If the boundary erodes faster than doctrine can accommodate, the theater_ratio rises (more performance, less function) and extractiveness rises (constraint becomes maintenance theater). If the boundary is actively reinforced through new legal interpretation, the constraint remains functionally coherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_erosion_via_reinterpretation, empirical, 'Whether the post-1945 norm against total war is being actively reinforced or passively eroded through reinterpretation.').

omega_variable(
    strategic_culture_coexistence_with_normativity,
    'Pre-1945 strategic cultures (treating total war as a rational option under existential threat) persist within military academies and strategic planning communities even though they are excluded from formal norm-setting. Are these cultures genuinely suppressed by the post-1945 normative order, or do they coexist with it, waiting for shifts in power distribution to resurface? Can normative constraints permanently override strategic cultures that treat them as optional?',
    'Examine military professional education, strategic doctrine in rising powers, and classified planning documents to assess what strategic logics are being transmitted and whether they treat the total-war prohibition as binding or as a temporary imposition.',
    'If strategic cultures treat the norm as optional and are waiting for power shifts, the constraint is vulnerable to sudden rejection if a major power gains confidence it can survive total-war retaliation. If the norm has genuinely replaced the prior strategic culture, the constraint is more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_culture_coexistence_with_normativity, conceptual, 'Whether the post-1945 norm against total war has permanently displaced prior strategic cultures or coexists with them in potential conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(tota_tr_t60, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 60, 0.37).
narrative_ontology:measurement_basis(tota_be_t60, observed).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t40, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(tota_su_t40, observed).
narrative_ontology:measurement(tota_su_t60, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(tota_su_t60, observed).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(tota_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__normative_reading_drop, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel total_war_winnability_post1945. The kernel contest frames three structurally distinct causal theories: (1) normative_reading_drop (this story) — total war was delegitimized through treaty and institutional enforcement, operating as a Rope-class coordination mechanism; (2) structural_contraction_reading — nuclear weapons physically removed total war from the reachable space, operating as a Mountain-class constraint (irreversible physical fact); (3) strategic_culture_drift — military professional thinking evolved to treat total war as strategically irrational, operating through ideational diffusion rather than formal commitment. Each reading decomposes the same natural-language claim ('total war became illegitimate after 1945') into a distinct constraint with different causal mechanisms, beneficiary structures, and vulnerability profiles. The readings coexist: different state actors and strategic communities hold different causal theories, and the constraint's actual operation depends on which theory dominates decision-making in a given moment. Link all three stories via network.affects_constraints for cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
