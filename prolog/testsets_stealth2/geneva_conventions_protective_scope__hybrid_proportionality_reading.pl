% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Tiered Geneva Protective Scope under Classification and Proportionality Determination (Hybrid-Proportionality Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   Under this reading, the Geneva protective-scope regime assigns protection
 *   by conflict classification: full Additional Protocol I standards in
 *   international armed conflict, the Common Article 3 / Additional Protocol
 *   II minimum in non-international armed conflict, with the necessity-harm
 *   proportionality balance governing targeting decisions within whichever
 *   tier applies. The tiered architecture was built to secure universal
 *   consent — deep obligations where reciprocity is strongest, an automatic
 *   floor everywhere else. In operation, however, the classification decision
 *   and the proportionality estimate are produced by the belligerent's own
 *   legal machinery, so the parties with the greatest legal-advisory capacity
 *   effectively set the scope of their own obligations, while populations in
 *   downgraded conflicts and detainees denied Article 4 status inherit
 *   whatever minimum the classifier concedes. The claim/metric split is
 *   deliberate: the claimed type records the structure this reading believes
 *   true (real coordination substrate carrying asymmetric discretionary
 *   extraction), and the metrics record how the arrangement actually
 *   operates. KEY AGENTS (by structural relationship): -
 *   major_military_powers: Agenda-setting beneficiary
 *   (institutional/arbitrage) — controls classification and proportionality
 *   practice, collects the widest operational latitude -
 *   military_legal_advisory_corps: Professional beneficiary
 *   (moderate/identity_locked) — staffs the interpretive machinery, careers
 *   fused to framework complexity - civilians_in_niac_theaters: Primary
 *   target (powerless/trapped) — protected status varies with classification
 *   decisions they cannot contest - unprivileged_detainees: Primary target
 *   (powerless/trapped) — status determined unilaterally by the captor -
 *   weaker_state_parties: Target (moderate/constrained) — bears obligations
 *   without interpretive capacity - non_state_armed_groups: Bound party
 *   (organized/constrained) — carries obligations without combatant privilege
 *   - icrc_delegation_system: Monitoring observer (institutional/analytical)
 *   — access-based oversight without enforcement power -
 *   international_war_crimes_tribunals: Adjudicative observer
 *   (institutional/analytical) — jurisprudence feeds back into practice -
 *   human_rights_advocacy_networks: Excluded voice (organized/mobile) —
 *   argues a universal floor from outside the interpretive process
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Tiered Geneva Protective Scope under Classification and Proportionality Determination (Hybrid-Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '2e9f09d0-d3cf-4426-a238-c9cf7fab4dde').
narrative_ontology:cs_kernel_codification('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', fixed_text).
narrative_ontology:cs_authority_grounding('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', lineage).
narrative_ontology:cs_interpretation_layer_present('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde').
narrative_ontology:cs_reading_relation('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_axiom('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', foundational, protection_application_tracks_conflict_classification).
narrative_ontology:cs_axiom_status(protection_application_tracks_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', protection_application_tracks_conflict_classification, conventional).
narrative_ontology:cs_axiom('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', foundational, proportionality_balances_necessity_against_incidental_harm).
narrative_ontology:cs_axiom_status(proportionality_balances_necessity_against_incidental_harm, holdable).
narrative_ontology:cs_axiom_grounding('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', proportionality_balances_necessity_against_incidental_harm, instrumental).
narrative_ontology:cs_axiom('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', secondary, differentiated_obligations_secure_universal_consent).
narrative_ontology:cs_axiom_status(differentiated_obligations_secure_universal_consent, holdable).
narrative_ontology:cs_axiom_grounding('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', differentiated_obligations_secure_universal_consent, conventional).
narrative_ontology:cs_reference_frame('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', consent_based_tiered_protection_framework).
narrative_ontology:cs_drift_state('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2e9f09d0-d3cf-4426-a238-c9cf7fab4dde', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, major_military_powers).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisory_corps).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac_theaters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, unprivileged_detainees).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field large, technologically advanced militaries and hold the treaty system's interpretive center of gravity: their legal advisers publish the manuals, their commands produce the classification determinations and collateral-harm estimates that define how the tiered standards apply in practice. They decide whether a given conflict is governed by the full Additional Protocol I standard or the Common Article 3 minimum, ratify with reservations, and reinterpret flexibly as circumstances change. They collect the widest operational latitude the system permits; exit means nothing to them, since they can reshape the rules they operate under.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, major_military_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, major_military_powers, beneficiary).

% Uniformed and government lawyers who run the classification reviews, collateral-damage estimation methodology, and targeting-board procedures. Their professional standing, career paths, and doctrinal authority are built on the complexity of the tiered framework; they staff the machinery that converts battlefield decisions into legally defensible records. Leaving the framework would mean abandoning the professional identity the framework itself confers.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisory_corps, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisory_corps, agenda_setter).

% Maintains delegations in conflict zones, visits detainees under the Geneva framework, transmits confidential findings to detaining authorities, and promotes incorporation of the treaties into military doctrine. Its access depends on confidentiality and on the goodwill of the very parties whose conduct it monitors; it holds no enforcement power of its own.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_delegation_system, observer,
    institutional, generational, analytical, global).

% Live in conflicts their governments or adversaries classify as internal disturbances or non-international armed conflicts, which places them under the Common Article 3 minimum rather than the fuller Additional Protocol I protections covering international war. Whether they hold protected status can turn on a classification decision made in a distant capital; they cannot leave the conflict zone, cannot petition for reclassification, and learn the applicable standard only through what the parties choose to disclose.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac_theaters, payer,
    powerless, biographical, trapped, regional).

% Fighters and suspected fighters held by state parties who deny them prisoner-of-war status under the Third Convention's Article 4 criteria, leaving them with whatever minimum the detaining power acknowledges — in non-international classifications, Common Article 3 only. They have no forum in which to contest their status determination and remain detained at the captor's discretion for the duration.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, unprivileged_detainees, payer,
    powerless, biographical, trapped, national).

% Signed nothing, yet are bound by Common Article 3 and, where thresholds are met, Additional Protocol II; their members face domestic prosecution for acts that would be lawful acts of war if committed by privileged forces, while receiving none of the combatant immunity state parties reserve for themselves. Their practical option set is selective compliance, special agreements with counterparts, or rejection of applicability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).

% Ratified the Conventions and Protocols but lack the legal-advisory infrastructure to shape interpretation, produce elaborate proportionality methodologies, or sustain classification litigation. They carry the full treaty obligations and face accusation when they deviate, while richer parties' deviations are absorbed into evolving interpretive practice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_state_parties, payer,
    moderate, generational, constrained, national).

% Human rights treaty bodies, NGOs, and scholars arguing for a floor of protection independent of conflict classification. They have no seat in the state-led interpretive process that produces military manuals and classification practice; their interventions arrive as shadow reports and amicus briefs after the operative determinations have been made.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, human_rights_advocacy_networks, excluded,
    organized, generational, mobile, global).

% Ad hoc tribunals and the International Criminal Court prosecute war crimes and, in doing so, author classification tests and proportionality jurisprudence that feed back into state practice. Their dockets depend on state cooperation and Security Council referrals, so their interpretive authority waxes and wanes with great-power politics.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_war_crimes_tribunals, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_war_crimes_tribunals, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, major_military_powers).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes reciprocal, consent-based minimum standards for the treatment of the wounded, sick, shipwrecked, prisoners, and civilians, so that belligerents share predictable restraint expectations; the tiered structure matches obligation depth to conflict type, which is what secured near-universal ratification.
% TRANSFER_FUNCTION: Moves interpretive discretion — over conflict classification, detainee status determination, and the military-necessity-versus-harm balance — from affected populations, detainees, and capacity-poor parties to the militarily and legally strongest parties; moves the risk of unprotected status onto those least able to contest the determinations.
% ABSENT_VOICES: Affected civilian populations and detainees have no seat anywhere in the interpretive process; non-state armed groups had no voice in setting the Additional Protocol II thresholds that govern them; human rights advocates stand outside the state-led machinery and are heard only after operative determinations are made.
% DISAPPEARANCE_RATIONALE: If the tiered protective scope vanished overnight, detention and targeting practice would lose their shared reference points: ICRC access frameworks, prisoner-of-war accounting and exchange mechanics, protecting-power arrangements, and war-crimes adjudication would all need rebuilding, and conduct of hostilities would reorganize around bare reciprocity and force. Detention regimes in particular would lose the minimum floor that currently attaches automatically to every conflict.
% FOUNDING_PROBLEM: Protection evaporated entirely whenever a government denied that a conflict was a war: in civil wars and colonial conflicts the full treaty machinery switched off, leaving the wounded, prisoners, and civilians with nothing. The tiered structure — and Common Article 3 in particular — was built to solve this by attaching an automatic minimum floor to every armed conflict while reserving deeper obligations for conflicts states would accept them in.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 and 1974-77 diplomatic conference records and the published ICRC commentaries attest the founding problem from outside any single benefiting state, and historical scholarship on Common Article 3's drafting corroborates it. No source outside the state-and-ICRC system attests that today's tiered structure still serves that original problem: human rights treaty bodies explicitly dispute it, and no external corroboration exists for the claim that current classification practice tracks the original protective purpose rather than the classifying party's exposure.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.65 because the operative determinations (classification, status, proportionality) are self-judged by the strongest parties, and the expected structural delta — a victim set that varies with classification and an epsilon that fluctuates with the proportionality calculus — is visible in the series. Suppression is 0.58: persistence depends on controlling the classification gate and resisting reclassification pressure, not on participant preference; it is authored as a raw structural property and is deliberately NOT scaled — only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio crosses 0.5 by interval end: collateral-damage estimation methodology, targeting-board documentation, and annual law-of-armed-conflict reporting increasingly substitute paperwork for substantive restraint (Goodhart drift), though ICRC visits and POW accounting remain functional. Accessibility collapse is moderate-low (0.45): the universal-floor alternative remains live and some parties voluntarily apply higher standards, so understanding the constraint does not eliminate alternatives. Resistance is 0.62: classification disputes, tribunal pushback, advocacy pressure, and reservation politics are constant. The temporal series runs on ONE shared seven-point grid (all three metrics authored at every point, 1977-2025, all observed). The extractiveness trajectory is not a smooth cycle but an episodic oscillation driven by the interaction of enforcement pressure and classification discretion: extraction climbs as major powers learn to work the classification gate (post-9/11 status determinations, expanded proportionality interpretations), dips when tribunal jurisprudence and litigation raise the cost of gross abuse, then climbs again as enforcement decays and new classification contests open. The oscillation is partly an extraction mechanism in itself — each crisis tests the boundary, and the resulting settlements ratchet interpretive discretion upward. Enforcement (suppression_requirement) traces a distinct arc: thin at adoption, surging with the ICTY and ICC era, decaying since through non-cooperation and withdrawals. Boltzmann coordination type is enforcement_mechanism (default floor 0.10, no override): the constraint is a legal-regulatory framework whose inherent coordination cost is real but bounded. Coalition note: the excluded advocacy networks plus weaker state parties have historically formed effective coalitions (Additional Protocol I itself was such a product), which caps resistance below the level a fully closed system would show.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the arrangement is calibrated flexibility: obligations matched to conflict type, proportionality as responsible command decision-making, reservations as legitimate sovereignty. From the payer seats the identical structure is exposure: their protected status is a variable set by someone else's determination, the applicable standard can be downgraded mid-conflict, and the proportionality balance that governs whether they are struck is computed by the party doing the striking. The advisory corps experiences the same machinery as professional craft — the complexity that extracts from others constitutes their careers. The engine computes these per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   major_military_powers are declared beneficiaries and hold the agenda: their derived directionality sits near the beneficiary end — the arrangement subsidizes their operational latitude, amplified by arbitrage-grade exit (they can reclassify, reserve, and reinterpret). military_legal_advisory_corps collect professional rents from framework complexity: low directionality, with identity-lock damping any movement toward reform from inside. The three victim-declared seats derive directionality near the target end: civilians_in_niac_theaters and unprivileged_detainees are trapped (they cannot exit the conflict, the detention, or the classification), and trapped targets sit nearest the full-target pole; weaker_state_parties are constrained — obligated but unable to shape interpretation. non_state_armed_groups derive high directionality despite organizational power: obligations without privilege is a net-cost position. The excluded advocacy networks sit outside the derivation — their exclusion is itself part of the enforcement object, since a universal floor would dissolve the gate the machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protections switching off entirely wherever a government denied a conflict was a war — was substantially solved by Common Article 3's automatic minimum floor; that specific mandate is dead or dying. The arrangement persists because a second function has grown inside it: administering the discretion that classification and proportionality judgments create. The classification prevents two opposite mislabelings. Reading the whole structure as pure coordination would erase the asymmetry — who gets to classify, and who lives inside the classification. Reading it as pure extraction would erase the protection genuinely delivered daily under the same texts: ICRC visits, prisoner accounting, wounded collection, the floor that never switches off. The tangled-rope classification holds both apart: coordination substrate intact, extraction riding the discretionary machinery. The R5 mismatch signal is live here — founding_problem_status is contested while disappearance_verdict is world_rearranges — flagging the zombie component: the tiering's original justification is aging faster than the tiering itself, and what now holds the structure up is the discretion architecture plus institutional inertia rather than the founding protective purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_liability_driver,
    'Do conflict-classification determinations track the objective character of the violence, or the classifying party''s exposure to fuller treaty obligations?',
    'Cross-conflict comparison of classification positions where factual profiles are similar but the classifying parties differ in capability and exposure; declassified legal-advisor deliberations where available.',
    'If liability-driven, the burden of the classification gate concentrates on conflicts the strong party prefers to downgrade, and the victim set is systematically the population of downgraded conflicts rather than a neutral function of conflict type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_liability_driver, empirical, 'Whether classification practice is evidence-driven or liability-driven.').

omega_variable(
    proportionality_estimate_bias,
    'Is the collateral-harm versus military-advantage balance computed with honest estimates, or do anticipated advantage and self-assessed necessity systematically dominate the inputs?',
    'Comparison of pre-strike collateral-damage estimates with post-strike battle-damage assessments across campaigns; tribunal jurisprudence cataloguing proportionality failures; leaked or disclosed targeting-board records.',
    'Determines whether the fluctuation in effective extraction reflects genuine situational variation in the balance or a systematic bias that converts the proportionality rule into an instrument of the stronger party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_estimate_bias, empirical, 'Bias profile of the proportionality calculus in operational practice.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the hybrid_proportionality_reading of the kernel geneva_conventions_protective_scope: what changes structurally under the sibling readings — state_centric_reading (Article 4 status gates the protected class) and universal_rights_reading (classification-independent universal floor) — and where exactly is the disagreement located?',
    'Comparative compilation of the three readings'' victim sets and epsilon referents: the state-centric reading removes unprivileged detainees from the protected class entirely; the universal reading dissolves the classification gate and makes extraction uniform across conflict types; this reading keeps the gate and locates the live question in its discretionary operation. The disagreement sits in the determinative criterion for protective scope: conflict classification, individual status, or humanity alone.',
    'Under the state-centric sibling the victim set shrinks and measured costs concentrate on status denials; under the universal sibling the classification gate disappears and this reading''s epsilon collapses toward the residual gap beneath the universal floor; the choice of reading relocates both the victim set and the mechanism of harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and locus of disagreement.').

omega_variable(
    tiering_consent_counterfactual,
    'Was differentiated protection a consent-necessary design (deeper uniform obligations would have lost ratification) or a concession extracted by capable parties from the weak during the drafting bargains?',
    'Counterfactual analysis of the 1949 and 1974-77 diplomatic conferences: voting records, reservation patterns, and which states demanded the tiered structure; comparison with human rights treaty trajectories that accepted uniform obligations.',
    'If consent-necessary, part of the measured extraction is the irreducible price of universality and belongs below the coordination floor; if not, the tiering is better read as a term imposed by the capable parties on the ratification bargain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tiering_consent_counterfactual, preference, 'Legitimacy of the tiered design: consent-necessity versus power concession.').

omega_variable(
    enforcement_recovery_trajectory,
    'Will individual-accountability enforcement capacity recover (universalization of the ICC, new ad hoc mechanisms) or continue eroding through non-cooperation and withdrawals?',
    'Track ratifications, cooperation resolutions, and completed prosecutions per decade; monitor Security Council referral practice and immunity negotiations.',
    'Continued erosion alongside rising classification discretion predicts the payer seats'' experience drifting toward unreviewed imposition with no accountability offset; recovery would re-tether the proportionality calculus to external review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_recovery_trajectory, empirical, 'Future path of enforcement capacity under the tiered regime.').

omega_variable(
    martens_clause_gap_fill,
    'How much of the clarity deficit borne by weaker parties and civilians is already filled by the Martens Clause and customary-IHL study conclusions, which supply residual principles where treaty classification runs out?',
    'Doctrinal analysis of invocation practice: when parties and tribunals actually reach for customary principles despite a classification dispute, and with what observable effect on conduct.',
    'If the residual layer reliably fills classification gaps, the clarity harm to weaker parties is smaller than the treaty-tier structure suggests; if invocations are selective and contested, the deficit stands as modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_gap_fill, conceptual, 'Residual customary-law coverage versus the classification-generated clarity deficit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(gene_tr_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(gene_tr_t48, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 48, 0.52).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(gene_be_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(gene_be_t48, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 48, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(gene_su_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(gene_su_t48, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 48, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Geneva protections' decomposes, per the epsilon-invariance principle, into three structurally distinct scope claims — state_centric_reading (status-gated scope), this hybrid_proportionality_reading (classification-gated scope with proportionality discipline), and universal_rights_reading (classification-independent floor). Each carries its own epsilon, victim set, and beneficiaries. They are linked because the upstream texts (Common Article 3, Additional Protocol I) are cited as evidence by all three, and interpretive practice under any one reading shifts the operating environment of the others: classification practice under this reading supplies the status-denial cases the state-centric reading excludes and the gap cases the universal reading organizes against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
