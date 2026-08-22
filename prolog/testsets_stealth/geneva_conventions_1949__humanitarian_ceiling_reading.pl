% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Binding Regardless of Reciprocity)
 *   domain: legal/political/international
 *
 * SUMMARY:
 *   The four Geneva Conventions of 1949, read through the
 *   humanitarian-ceiling lens, function as an unconditional floor beneath
 *   state violence: Common Article 1's undertaking to respect and ensure the
 *   Conventions 'in all circumstances' binds each party absolutely,
 *   independent of adversary compliance, reciprocity, or military necessity.
 *   On this reading the regime imposes real, asymmetric burdens on complying
 *   state militaries — restrictive targeting, detainee-care obligations,
 *   self-prosecution duties — while conferring protections on civilians,
 *   detainees, the wounded, and irregular fighters who may extend no
 *   equivalent restraint. This file instantiates ONE reading of the contested
 *   kernel geneva_conventions_1949; the sibling readings
 *   (conditional_reciprocity_reading, security_maximization_reading) are
 *   separate constraints with their own epsilon values, victim sets, and
 *   classifications, linked through network.affects_constraints. Measuring
 *   the regime as unconditional floor versus reciprocal restraint yields
 *   different extraction profiles because they are different constraints, not
 *   one constraint viewed twice. Claim and metrics are authored
 *   independently: the claimed type (tangled_rope) states the structure
 *   believed true — a genuine coordination function carrying asymmetric,
 *   actively enforced extraction — while the metrics describe the regime's
 *   actual operation as this reading assesses it.
 *
 * KEY AGENTS:
 *   - - high_contracting_parties: agenda setter (institutional/constrained) — ratifies, administers, and diplomatically sustains the regime
 *   - - icrc_depository_institution: primary institutional beneficiary and custodian (organized/identity_locked)
 *   - - complying_state_militaries: primary target (powerful/constrained) — bears the unconditional compliance burden
 *   - - protected_civilians_in_conflict_zones: principal protected class (powerless/trapped)
 *   - - hors_de_combat_combatants_and_detainees: protected class wholly in captor power (powerless/trapped)
 *   - - irregular_armed_group_members: protected without status reciprocity (moderate/mobile)
 *   - - international_criminal_tribunals: enforcement arm accumulating interpretive authority (institutional/analytical)
 *   - - humanitarian_ngo_sector: secondary beneficiary — monitoring and advocacy standing (organized/mobile)
 *   - - civilian_populations_of_nonparty_adversaries: excluded voice — governed by the floor but unrepresented at drafting (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.6).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Binding Regardless of Reciprocity)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "legal/political/international").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a').
narrative_ontology:cs_kernel_codification('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', fixed_text).
narrative_ontology:cs_authority_grounding('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', lineage).
narrative_ontology:cs_interpretation_layer_present('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a').
narrative_ontology:cs_reading_relation('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', foundational, humanitarian_minimums_bind_in_all_circumstances).
narrative_ontology:cs_axiom_status(humanitarian_minimums_bind_in_all_circumstances, holdable).
narrative_ontology:cs_axiom_grounding('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', humanitarian_minimums_bind_in_all_circumstances, deontological).
narrative_ontology:cs_axiom('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', foundational, protections_attach_to_persons_not_status).
narrative_ontology:cs_axiom_status(protections_attach_to_persons_not_status, holdable).
narrative_ontology:cs_axiom_grounding('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', protections_attach_to_persons_not_status, deontological).
narrative_ontology:cs_reference_frame('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', absolute_humanitarian_minimum_floor).
narrative_ontology:cs_drift_state('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', contemporary_asymmetric_conflicts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b5b9e7e-5a17-4af6-b57e-ff69b0b30b9a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, hors_de_combat_combatants_and_detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_group_members).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_depository_institution).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_ngo_sector).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, complying_state_militaries).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, common_article_1_universality).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, in_all_circumstances_obligation).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, martens_clause_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The states that ratified the four Conventions and convene the diplomatic conferences that amend and extend the regime. They administer the treaty framework through depositary functions, fund and staff the enforcement ecosystem, and bear the diplomatic cost of sustaining it. Individual denunciation is legally available under the withdrawal articles but politically catastrophic, so exit is nominal rather than real.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, high_contracting_parties, agenda_setter,
    institutional, generational, constrained, global).

% Custodian and promoter of the regime: conducts detention visits, tracing, family reunification, and confidential representations to belligerents. Its mandate, guaranteed access rights, funding base, and interpretive authority are constituted by the Conventions' operation; abandoning the function would dissolve the institution's reason for existing, so it cannot exit the role it embodies.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_depository_institution, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_depository_institution, agenda_setter).

% Armed forces of states that honor the floor regardless of adversary conduct. They restrict targeting, resource detainee care, investigate and prosecute their own personnel, and accept tactical disadvantage against adversaries who exploit the asymmetry. Leaving the constraint would mean denouncing the treaties or fighting outside the law — both forfeit alliances, legitimacy, and reciprocal expectations. Professional identity ties lawful conduct to institutional self-worth.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, complying_state_militaries, payer,
    powerful, biographical, constrained, global).

% Civilians in war zones who hold the floor's protections against targeting, hostage-taking, reprisal, and starvation. They cannot leave the conflict zone and possess no enforcement lever of their own; the ceiling is the only restraint they hold on the forces operating around them.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Wounded, shipwrecked, captured, and interned fighters entitled to humane treatment, medical care, and process. They are wholly in the captor's power for the duration of captivity; the regime's oldest core exists precisely for their situation, and they have no alternative protection once in enemy hands.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, hors_de_combat_combatants_and_detainees, beneficiary,
    powerless, immediate, trapped, regional).

% Non-state fighters who under this reading retain baseline Common Article 3 protections even without lawful-combatant status. They typically lack the command structure to extend equivalent discipline, and their mobility — blending into civilian populations — lets them operate where the ceiling binds their state adversary most tightly. They carry residual Common Article 3 obligations they rarely have the organization to meet.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_group_members, beneficiary,
    moderate, immediate, mobile, regional).

% Ad hoc tribunals and the permanent court that prosecute grave breaches and command responsibility. They convert the floor's norms into individual criminal liability and accumulate interpretive authority over what the Conventions require with each judgment, shaping the regime's operative meaning without themselves bearing its compliance costs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, analytical, continental).

% Monitoring and advocacy organizations whose reporting mandate, funding streams, and public standing depend on the normative baseline the Conventions supply. They document violations and mobilize diplomatic consequence without bearing compliance costs themselves, and can redirect to adjacent causes if the regime's relevance faded.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_ngo_sector, beneficiary,
    organized, biographical, mobile, global).

% Populations under bombardment or occupation by powers whose governments did not represent them at the 1949 Diplomatic Conference. The floor governs their attackers in their favor, but they had no voice in drafting and have none in the interpretive bodies that now define its reach; their protection rides on commitments made entirely without them.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_of_nonparty_adversaries, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_depository_institution).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, predictable minimum for the conduct of hostilities and treatment of persons: bounded targeting, humane treatment of detainees, medical care for the wounded, neutral monitoring access, and workable frameworks for surrender and exchange — problems each warring party faces and cannot solve unilaterally.
% TRANSFER_FUNCTION: Moves operational freedom, tactical flexibility, and detention discretion from complying state militaries toward physical security and dignified treatment for protected persons; secondarily moves institutional mandate, access rights, and interpretive authority to the regime's custodians (ICRC, tribunals, monitoring organizations).
% ABSENT_VOICES: Civilian populations of adversary states and occupied territories had no seats at the 1949 Diplomatic Conference, and non-state armed groups — bound by Common Article 3 without having consented — were unrepresented. Both would object that the floor was drafted around interstate war between signatory armies and only later, imperfectly, extended to them.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reorganize detention, targeting, and captive treatment around raw reciprocity and commander discretion within a single campaign cycle. The ICRC's access architecture, prisoner-exchange frameworks, grave-breach prosecutions, and the professional norms built on the floor would lose their legal basis, and protected classes would hold nothing against the forces around them.
% FOUNDING_PROBLEM: The demonstrated failure of pre-1949 law: industrialized slaughter of civilians, systematic abuse and murder of prisoners and the wounded, and the absence of any binding floor that survived a belligerent's invocation of military necessity — the wartime record that produced the 1949 Diplomatic Conference.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: military historians and postwar documentary records independent of the regime attest the founding harms; the paying states' own doctrinal writings accept the floor's premise; and United Nations investigative mechanisms and neutral monitoring reports document that the underlying harm pattern — atrocity against civilians and detainees in armed conflict — persists. No party to the dispute denies the founding harms occurred; the contest is over the remedy's conditionality, not the problem's reality.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.60 at interval end): the compliance burden on state militaries is real and asymmetric — paid in full regardless of adversary conduct — but the paying state also draws long-run returns (its own personnel protected when hors de combat, bounded wars, institutional legitimacy), and much of the transferred value is the floor's intended protective output rather than captured rent. Suppression is high (0.75) and is authored as a raw structural property, unscaled by power or scope: security rationales are suppressed by an enforcement lattice (courts-martial, command-responsibility doctrine, tribunal jurisdiction, ICRC monitoring, alliance and aid conditionality) and by internalized professional norms; the reading's defining move is disallowing necessity as a defense. Theater is low-moderate (0.25): detention visits, prisoner frameworks, and prosecutions are substantively functional, with a minority of performative adherence — formal compliance narratives over divergent detention practice. Accessibility collapse is low (0.30): the sibling readings remain live, workable alternatives that states periodically adopt in practice, so understanding the ceiling does not close off alternatives. Resistance is moderate-high (0.55): states resist through reservations, interpretive maneuver, status manipulation, and periodic repudiation proposals. The temporal series run on ONE shared grid (1949, 1955, 1977, 1990, 2001, 2011, 2025) with every tracked metric authored at every point; trajectories are monotonic rather than cyclical — enforcement capacity ratchets upward (suppression_requirement 0.35 to 0.75) as the machinery matured from diplomatic protest to criminal liability, extraction creeps upward as obligations accrete through Protocols and jurisprudence, and theater spikes around 2001 with status-manipulation practice before partially receding. Endpoint values equal the base_properties scalars. Receipt: the floor's protective value diffuses across protected classes, but the concentrated institutional accrual — mandate, guaranteed access, funding, interpretive authority — demonstrably lands on the ICRC seat, which is why gain_flow names it. Fixing: denunciation is legally available but politically prohibitive relative to the compliance costs escaping it would recover, hence fixing_cost: prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from identical structural facts. From complying_state_militaries the ceiling operates as unilateral burden: costs paid in full, reciprocation contingent on an adversary's choice, tactical disadvantage deliberately exploited by opponents — a seat that computes high effective extraction. From protected_civilians_in_conflict_zones and hors_de_combat_combatants_and_detainees the same structure is the only restraint they hold — pure subsidy, near-zero experienced extraction. The ICRC seat experiences the regime as constitutive: its mandate, access, and authority exist only because the floor holds. High_contracting_parties sit near the administrative middle — they set and underwrite the burden they also impose. The engine derives these per-seat classifications from the declared structure; the divergence between seats is the measurement, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected civilians, hors-de-combat detainees, and irregular fighters derive low directionality (subsidized end): the floor transfers protection to them at the complying military's expense. Irregular fighters sit slightly above the pure-beneficiary end because they carry residual Common Article 3 obligations they rarely have structure to meet and their conduct triggers the asymmetry — hence the explicit override (moderate to 0.20), correcting a derivation that would otherwise read them as full beneficiaries. The ICRC and the humanitarian NGO sector derive low directionality as institutional beneficiaries whose mandates and standing the regime constitutes. Complying state militaries derive high directionality (target end): they bear the transfer, their exit is constrained (denunciation is ruinous; fighting outside the law forfeits legitimacy and invites prosecution), and professional identity reinforces compliance. High contracting parties and tribunals occupy administrative seats near the symmetric middle — they set and enforce the arrangement rather than absorbing its extraction. Global spatial scope modestly amplifies effective extraction for the target seat, since verification of compliance across worldwide operations is harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Against snare: the ceiling's coordination function is genuine and prior to its extraction — bounded hostilities, safe-surrender incentives, neutral monitoring access, and exchange frameworks solve real collective-action problems in war — so the burden on militaries is not cover for pure predation. Against mountain: the reading's 'absolute minimums' rhetoric carries natural-law flavor, but the floor is constructed treaty law whose persistence depends on active enforcement machinery (requires_active_enforcement: true; emerges_naturally remains false) — declaring it a mountain would immunize it from the extraction accounting its asymmetric burden demands. Mandatrophy is not resolved: the founding problem (industrialized atrocity without a floor) is live and independently corroborated, so no sunset logic applies and the arrangement is not scaffold; theater remains low enough that piton decay is not indicated. The live risk this reading faces is not atrophy but displacement — drift toward the reciprocity sibling under asymmetric-conflict cost pressure, tracked in the omegas and the drift_state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geneva_kernel_reading_locus,
    'This constraint is one reading of kernel geneva_conventions_1949. Is the regime''s core an unconditional humanitarian floor (this reading), a conditional reciprocity structure (conditional_reciprocity_reading), or a peacetime aspiration subordinate to operational necessity (security_maximization_reading)?',
    'Treaty text and authentic interpretation — Common Article 1''s ''in all circumstances'', the ICRC commentaries, International Court of Justice jurisprudence, and accumulated state practice and opinio juris on whether compliance survives adversary violation.',
    'Sibling adoption restructures the constraint entirely: the reciprocity reading converts the complying military''s unconditional burden into conditional coordination (lower epsilon at the payer seat, different victim set); the security reading collapses most protections to aspiration (near-zero enforcement, a different classification altogether).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geneva_kernel_reading_locus, conceptual, 'Which reading of the 1949 kernel is structurally operative — the locus of the committer contest.').

omega_variable(
    asymmetric_burden_political_feedback,
    'Does the measurable cost of unilateral compliance against systematically violating adversaries generate durable political pressure shifting states toward the reciprocity sibling?',
    'Cross-conflict comparison of casualty and political-cost differentials for complying forces facing compliant versus violating adversaries, plus longitudinal indicators of doctrinal and legislative drift toward conditionality.',
    'A sustained cost differential predicts revival pressure toward conditional_reciprocity_reading and progressive erosion of this reading''s hold on state practice, dating a reading-level transition before any formal renunciation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_burden_political_feedback, empirical, 'Whether the ceiling''s asymmetric burden feeds a feedback loop toward the reciprocity reading.').

omega_variable(
    customary_floor_vs_constructed_regime,
    'Is the humanitarian floor crystallized custom binding all parties (quasi-natural, self-executing) or constructed treaty law sustained by enforcement machinery?',
    'State practice and opinio juris analysis for non-party states and non-state armed groups: does the floor hold where enforcement is absent?',
    'If genuinely customary and self-enforcing, suppression and enforcement-dependence fall and the constraint trends mountainward; if constructed, the tangled_rope structure stands and enforcement decay becomes the primary failure mode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_floor_vs_constructed_regime, conceptual, 'Naturality ambiguity: custom versus construction for the humanitarian floor.').

omega_variable(
    interpretive_flexibility_trajectory,
    'Does the recurring pattern of status manipulation — unlawful-combatant designations, detention outside Convention frames, narrow grave-breach charging — signal practice drift that will stabilize as informal amendment, or remain correctable deviation?',
    'Longitudinal tracking of detention practice, litigation outcomes, and doctrinal restatements across successive conflicts.',
    'Stabilization of divergent practice would date a type transition — the ceiling eroding toward reciprocity-like operation — far earlier than formal denunciation ever would, and would shift the theater_ratio trajectory upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_trajectory, empirical, 'Whether interpretive erosion of the ceiling stabilizes as informal amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_ceiling_reading_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1955, 0.14).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t1990, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t2001, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2011, 0.27).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t2011, observed).
narrative_ontology:measurement(geneva_ceiling_reading_tr_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(geneva_ceiling_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(geneva_ceiling_reading_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.42).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1955, 0.44).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t1990, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.57).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t2001, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2011, 0.59).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t2011, observed).
narrative_ontology:measurement(geneva_ceiling_reading_be_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(geneva_ceiling_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_ceiling_reading_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.35).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1977, 0.52).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t1990, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t2001, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2011, 0.74).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t2011, observed).
narrative_ontology:measurement(geneva_ceiling_reading_su_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement_basis(geneva_ceiling_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Geneva Conventions' decomposes, under the epsilon-invariance principle, into at least three structurally distinct constraints — readings of one kernel. This file authors the humanitarian-ceiling reading with its own epsilon (referent: the standing regime assessed as an unconditional floor), its own beneficiary/victim structure, and its own classification. The sibling files — conditional_reciprocity_reading and security_maximization_reading — author the same texts under different conditionality premises, producing different victim sets, burden distributions, and extraction profiles. Family linkage runs through network.affects_constraints in all three files; the 1949 texts (highest empirical confidence, least contested) are cited by each downstream reading as evidence for its own interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
