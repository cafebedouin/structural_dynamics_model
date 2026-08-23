% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Parlementaire Remonstrance Authority (Magistrate Reading)
 *   domain: constitutional/historical/political_economy
 *
 * SUMMARY:
 *   The Parlementaire remonstrance right — the power of France's sovereign
 *   courts to refuse registration of royal edicts and present formal
 *   objections — is read here as a fundamental constitutional mechanism
 *   preserving ancient liberties against arbitrary innovation. This
 *   magistrate reading (associated with the Parlements themselves, their
 *   allies in the noblesse de robe, and the classical constitutionalist
 *   tradition of Loisel, Loyseau, and the 18th-century jurisprudence) treats
 *   remonstrance as the guardian of the fundamental laws of the kingdom
 *   against ministerial despotism. The constraint operates on fiscal edicts
 *   primarily: when the Crown attempts new taxes, loans, or financial
 *   restructuring, the Parlements remonstrate, delaying or blocking
 *   registration. The structural delta from the kernel is high extraction on
 *   fiscal reform edicts; the Parlements enter the victim set when overridden
 *   (by lit de justice, exile, or Maupeou's 1771 reform); and the constraint
 *   creates a beneficiary class of tax-exempt magistracy whose privilege
 *   depends on the remonstrance power's survival.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.62).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.71).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Parlementaire Remonstrance Authority (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/historical/political_economy").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '1bc2affb-e0e5-41c5-b9f3-10c94325bc9e').
narrative_ontology:cs_kernel_codification('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', formalized).
narrative_ontology:cs_authority_grounding('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', lineage).
narrative_ontology:cs_interpretation_layer_present('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e').
narrative_ontology:cs_reading_relation('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', foundational, parlements_guard_fundamental_laws).
narrative_ontology:cs_axiom_status(parlements_guard_fundamental_laws, holdable).
narrative_ontology:cs_axiom_grounding('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', parlements_guard_fundamental_laws, conventional).
narrative_ontology:cs_axiom('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', foundational, remonstrance_preserves_ancient_liberties).
narrative_ontology:cs_axiom_status(remonstrance_preserves_ancient_liberties, holdable).
narrative_ontology:cs_axiom_grounding('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', remonstrance_preserves_ancient_liberties, deontological).
narrative_ontology:cs_reference_frame('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', capetian_valois_constitutional_contract).
narrative_ontology:cs_drift_state('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', pre_revolutionary_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1bc2affb-e0e5-41c5-b9f3-10c94325bc9e', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlementaire_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_nobility_with_parlement_seats).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, taxpaying_third_estate).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, reformist_ministries).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, ancient_constitutionalism).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_laws_of_the_kingdom).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, parlementaire_guardian_role).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold venal offices in the Parlements (Paris and provincial) that carry tax exemption, judicial authority, and the right of remonstrance. Register royal edicts and may refuse registration pending remonstrance. Their professional identity and social standing are fused with the office; exit means losing nobility status, tax privilege, and the institutional platform that defines their class. They administer the remonstrance process and collect its protective benefits.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementaire_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlementaire_magistracy, beneficiary).

% Hold lesser venal offices (financial, judicial, administrative) whose tax-exempt status is defended by the Parlements' remonstrance power. They do not set the remonstrance agenda but benefit materially when the Parlements block fiscal edicts that would extend taxation to their offices. Exit requires selling the office (capital loss) or accepting new taxes.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_exempt_office_holders, beneficiary,
    organized, biographical, constrained, national).

% Noble families who have purchased or inherited seats in provincial Parlements. They enjoy local prestige, tax exemption, and a voice in regional remonstrances. Their exit options include retreating to estate management or military service, but the office is a primary vector of family status and fiscal advantage. They coordinate with the Paris Parlement but have independent regional interests.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_nobility_with_parlement_seats, beneficiary,
    powerful, generational, mobile, regional).

% The fiscal apparatus of the Crown. Bears the direct cost of blocked or delayed revenue edicts — each successful remonstrance against a new tax or loan represents foregone revenue. The Treasury can attempt lit de justice (forced registration), exile the Parlement, or negotiate modifications, but each escalation consumes political capital and risks broader resistance. The constraint extracts predictability and timeliness from fiscal operations.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_treasury, payer,
    institutional, biographical, constrained, national).

% The vast majority of subjects who bear the full tax burden (taille, gabelle, aides) while the privileged orders are exempt. When the Parlements block fiscal reform that would broaden the tax base, the existing burden falls disproportionately on them. They have no institutional voice in the remonstrance process, no exit from the fiscal system, and no organized representation. Their resistance manifests as riots, tax evasion, and subsistence crises.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, taxpaying_third_estate, payer,
    powerless, biographical, trapped, national).

% Controllers-General and their bureaus (e.g., Turgot, Necker, Calonne) who attempt fiscal restructuring. They bear the career and policy costs of remonstrance-driven failure. They are excluded from the remonstrance dialogue — the Parlements address the King, not the ministers. Exit means resignation or dismissal; some attempt to co-opt the Parlements through strategic concessions or public opinion campaigns.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, reformist_ministries, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, reformist_ministries, excluded).

% The abstract constitutional authority of the monarchy itself. The remonstrance right structures the King's relationship to his own legislative acts. From this seat, the constraint appears as a structural feature of the French polity — neither purely extractive nor purely coordinative, but a constitutional friction that defines the regime's operating logic. No material exit; the analytical seat observes the system's evolution.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_legitimacy, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional brake on royal legislative action, forcing consultation and deliberation before new fiscal or legal impositions take effect. Creates a venue where the King's edicts encounter institutionalized resistance that claims to represent the kingdom's fundamental laws.
% TRANSFER_FUNCTION: Transfers fiscal predictability and reform capacity from the royal treasury and reformist ministries to the parlementaire magistracy and their allied privileged orders. The remonstrance power converts into tax exemption for office-holders and blocked revenue for the Crown; the third estate pays the residual burden through unchanged or increased direct taxes.
% ABSENT_VOICES: The third estate (peasantry, urban workers, bourgeois not in office) is structurally excluded — they bear the fiscal consequences of remonstrance but have no standing to participate. Would-be reformers within the clergy and nobility who support fiscal equality are marginalized within their own orders. Provincial populations outside Parlement jurisdictions experience the constraint's effects without representation.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the Crown could register fiscal edicts without delay, the venal office system's tax exemption would lose its institutional shield, and the fiscal-military state could centralize revenue extraction. The social structure of the Old Regime — built on the alliance of Crown and privileged orders mediated by the Parlements — would lose a keystone constraint.
% FOUNDING_PROBLEM: The early Capetian and Valois monarchies needed to legitimate taxation beyond feudal dues while preserving the contractual character of the king's relationship with his great officers. Remonstrance emerged as the mechanism by which the King's officers (the Parlements) could signal dissent without denying legitimacy, preserving the fiction of consent in a system where representation was corporate and hierarchical.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — legitimizing royal taxation in a corporate-order society — was resolved by the Revolution's abolition of corporate privilege and the creation of national representation (1789). The Parlements themselves, in their final remonstrances (1787-88), implicitly acknowledged the problem had changed by claiming to defend the nation against ministerial despotism — a pivot noted by contemporaries (Malesherbes, Mirabeau) and modern historians (Doyle, Shennan, Bosher) outside the magistracy.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the systematic diversion of fiscal reform capacity toward privilege preservation: each successful remonstrance against a general tax converts into maintained exemption for office-holders and continued burden on the third estate. Suppression (0.71) is high because the constraint's persistence depends on active institutional resistance — the Parlements must mobilize procedural delays, public opinion campaigns, and noble solidarity to sustain their veto; when the Crown deploys lit de justice or exile, the constraint survives only through collective refusal of service. Theater ratio (0.28) is moderate: the legal forms of remonstrance are genuine deliberative acts, but an increasing share of activity (post-1750) performs constitutional fidelity while protecting material interests. Accessibility collapse (0.45) is partial — alternatives (Estates-General, provincial assemblies, direct royal administration) exist but are structurally inhibited. Resistance (0.68) is high from the Crown and reformist ministries, and from the third estate via subsistence resistance.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat types from the structural data: from the magistracy's seat (beneficiary, identity_locked, institutional power) the constraint should compute as rope or scaffold — genuine coordination of royal authority with corporate consent. From the third estate's seat (payer, trapped, powerless) it computes as snare — pure extraction with no exit. From the Treasury's seat (payer, constrained, institutional) it computes as tangled_rope — coordination function (legitimation) mixed with asymmetric extraction. The magistrate reading's claimed_type (tangled_rope) acknowledges the hybrid nature but weights the coordination function; the crown reading would claim snare. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The parlementaire magistracy is the structural beneficiary (d near 0.0): they administer the constraint, collect its protective rents (tax exemption, status), and their identity is fused with the office (identity_locked exit). Tax-exempt office holders and provincial noblesse de robe are secondary beneficiaries (d ~0.15-0.25): they gain materially but do not set the agenda. The royal treasury and reformist ministries are targets (d ~0.75-0.85): they bear the direct costs of blocked revenue and failed policy, with constrained exit (they can escalate but at escalating political cost). The taxpaying third estate is the deepest target (d ~0.95): trapped, powerless, bearing the residual fiscal burden with no voice. The crown_legitimacy observer seat sits at d=0.5 (symmetric) — the constraint both stabilizes and destabilizes monarchical authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing royal taxation in a corporate society) is dead — the corporate order was abolished in 1789. Yet the constraint persisted for 128 years after its founding logic vanished, maintained by the beneficiary class it created. The mandatrophy is resolved in the historical record (the Revolution dissolved the Parlements), but the reading itself refuses this resolution: the magistrate reading treats the founding problem as live (contested status) by redefining 'arbitrary innovation' to include any fiscal reform threatening their privilege. This is a classic mandatrophy pattern: the constraint's original justification is repurposed to defend the beneficiary class it spawned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_axis_ambiguity,
    'Is this constraint a single reading of the remonstrance_authority kernel, or does it conflate multiple magistrate sub-readings (e.g., Paris vs. provincial Parlements, early vs. late period)?',
    'Disaggregate the remonstrance practice by Parlement jurisdiction and decade; measure ε separately for fiscal vs. judicial vs. religious edicts. If ε varies systematically across sub-domains, the single reading decomposes.',
    'If multiple sub-readings exist, each needs its own constraint story with its own ε, stakeholders, and classification. The current story would be a composite violating ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axis_ambiguity, conceptual, 'Whether the magistrate reading is a single coherent constraint or a family of related constraints.').

omega_variable(
    privilege_vs_liberty_boundary,
    'How much of the remonstrance''s protective effect covers genuine corporate liberties (e.g., provincial estates'' rights, religious minority protections) versus narrow fiscal privilege of the magistracy?',
    'Code a sample of remonstrances (1661-1789) by stated object: fiscal exemption, judicial independence, provincial privileges, religious policy. Measure the proportion that defend non-magistracy interests.',
    'If the liberty defense is substantial, the coordination function is genuine and the tangled_rope claim holds. If marginal, the constraint is a snare with a liberty cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(privilege_vs_liberty_boundary, empirical, 'Whether the constraint''s coordination function extends beyond the magistracy''s material interests.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (royal enforcement capacity: lit de justice, exile, police) or internalized (magistrates'' self-censorship, public opinion internalization, habitus of deference)?',
    'Compare suppression levels during periods of active royal confrontation (1671-73, 1718-20, 1766, 1771-74, 1787-88) vs. quiescent periods. If suppression persists in quiescence without active enforcement, internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — the magistracy carries the constraint''s discipline internally. This would increase effective extraction for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the remonstrance regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1661, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rem_mag_tr_t1661, remonstrance_authority__magistrate_reading, theater_ratio, 1661, 0.12).
narrative_ontology:measurement(rem_mag_tr_t1685, remonstrance_authority__magistrate_reading, theater_ratio, 1685, 0.15).
narrative_ontology:measurement(rem_mag_tr_t1715, remonstrance_authority__magistrate_reading, theater_ratio, 1715, 0.18).
narrative_ontology:measurement(rem_mag_tr_t1750, remonstrance_authority__magistrate_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement(rem_mag_tr_t1770, remonstrance_authority__magistrate_reading, theater_ratio, 1770, 0.25).
narrative_ontology:measurement(rem_mag_tr_t1787, remonstrance_authority__magistrate_reading, theater_ratio, 1787, 0.28).

% Extraction over time
narrative_ontology:measurement(rem_mag_be_t1661, remonstrance_authority__magistrate_reading, base_extractiveness, 1661, 0.35).
narrative_ontology:measurement(rem_mag_be_t1685, remonstrance_authority__magistrate_reading, base_extractiveness, 1685, 0.42).
narrative_ontology:measurement(rem_mag_be_t1715, remonstrance_authority__magistrate_reading, base_extractiveness, 1715, 0.48).
narrative_ontology:measurement(rem_mag_be_t1750, remonstrance_authority__magistrate_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(rem_mag_be_t1770, remonstrance_authority__magistrate_reading, base_extractiveness, 1770, 0.59).
narrative_ontology:measurement(rem_mag_be_t1787, remonstrance_authority__magistrate_reading, base_extractiveness, 1787, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rem_mag_su_t1661, remonstrance_authority__magistrate_reading, suppression_requirement, 1661, 0.45).
narrative_ontology:measurement(rem_mag_su_t1685, remonstrance_authority__magistrate_reading, suppression_requirement, 1685, 0.52).
narrative_ontology:measurement(rem_mag_su_t1715, remonstrance_authority__magistrate_reading, suppression_requirement, 1715, 0.58).
narrative_ontology:measurement(rem_mag_su_t1750, remonstrance_authority__magistrate_reading, suppression_requirement, 1750, 0.63).
narrative_ontology:measurement(rem_mag_su_t1770, remonstrance_authority__magistrate_reading, suppression_requirement, 1770, 0.68).
narrative_ontology:measurement(rem_mag_su_t1787, remonstrance_authority__magistrate_reading, suppression_requirement, 1787, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__magistrate_reading, 0.1).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, venal_office_system).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, royal_fiscal_authority).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, provincial_estates_autonomy).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, crown_legitimacy_architecture).

% DUAL FORMULATION NOTE:
% This constraint is the magistrate_reading of the remonstrance_authority kernel. The crown_reading (remonstrance_authority__crown_reading) treats the same institutional power as an illegitimate veto. The two readings have different ε values (this: 0.62, crown: ~0.25), different victim sets (this: royal_treasury, third_estate; crown: crown_authority, national_interest), and different beneficiary structures (this: tax_exempt_magistracy; crown: none). They are linked as constraint family members via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, institutional, 0.15).
constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, organized, 0.25).
constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, powerful, 0.3).
constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
