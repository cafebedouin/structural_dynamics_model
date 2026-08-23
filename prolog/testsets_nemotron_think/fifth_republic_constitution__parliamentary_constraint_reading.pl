% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Parliamentary Constraint on Presidential Policy Implementation (Fifth Republic)
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint story captures the parliamentary constraint reading of
 *   the French Fifth Republic constitution: the President is a coordinated
 *   executive who requires legislative authorization (via Prime Ministerial
 *   responsibility to the Assembly) for policy implementation. The reading
 *   emphasizes that the 1958 Constitution's architecture — particularly
 *   Articles 20, 21, 49, and 50 — makes the government responsible to
 *   Parliament, not to the President alone. When the Assembly withholds
 *   confidence (Art. 49) or censures the government (Art. 50), the
 *   President's policy agenda is blocked, placing the executive in a
 *   victim/payer position. The legislative majority is the structural
 *   beneficiary, holding the authorization gate. This reading coexists with
 *   the hyper-presidential reading (which reads the same text as granting the
 *   President direct democratic legitimacy) and the cohabitation equilibrium
 *   reading (which sees a negotiated dual executive). The claimed type is
 *   'rope' — genuine democratic coordination — but metrics show modest
 *   extraction and suppression reflecting periods of cohabitation and
 *   presidential resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.45).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Parliamentary Constraint on Presidential Policy Implementation (Fifth Republic)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '3f5c33c7-b2bf-4568-9305-b28ee8f79e8e').
narrative_ontology:cs_kernel_codification('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', formalized).
narrative_ontology:cs_authority_grounding('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', lineage).
narrative_ontology:cs_interpretation_layer_present('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e').
narrative_ontology:cs_reading_relation('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', foundational, parliamentary_confidence_required).
narrative_ontology:cs_axiom_status(parliamentary_confidence_required, holdable).
narrative_ontology:cs_axiom_grounding('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', parliamentary_confidence_required, conventional).
narrative_ontology:cs_axiom('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', foundational, president_subordinate_to_assembly_on_policy).
narrative_ontology:cs_axiom_status(president_subordinate_to_assembly_on_policy, holdable).
narrative_ontology:cs_axiom_grounding('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', president_subordinate_to_assembly_on_policy, conventional).
narrative_ontology:cs_reference_frame('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', contemporary_term_aligned_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3f5c33c7-b2bf-4568-9305-b28ee8f79e8e', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, citizens_voters).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, responsible_government_principle).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, democratic_accountability_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formally heads state and appoints Prime Minister but requires legislative authorization for policy implementation. When Assembly withholds confidence or blocks legislation, the President bears the cost of constrained executive action — cannot implement agenda, may face cohabitation or dissolution. Exit is constrained by fixed term and constitutional role; cannot easily abandon the office or its constraints.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    institutional, biographical, constrained, national).

% Leads government and implements policy but must maintain confidence of the Assembly. The PM is the effective policy executor when parliamentary majority supports the government; during cohabitation, the PM's agenda-setting power expands relative to the President. Exit depends on parliamentary support — loss of confidence forces resignation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).

% Controls legislative agenda and confidence votes, thereby constraining or enabling presidential policy. Benefits from the constitutional requirement that executive action requires legislative authorization — this gives the majority leverage over policy direction and executive appointments. Exit is mobile: majority can shift at elections, and individual deputies can defect.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    organized, biographical, mobile, national).

% Lacks formal gatekeeping power over executive action but can obstruct, publicize, and mobilize. Would object to both presidential overreach and majority tyranny but is structurally excluded from the confidence mechanism that authorizes policy. Exit is constrained by electoral calendar and institutional rules favoring majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_opposition, excluded,
    organized, biographical, constrained, national).

% Adjudicates constitutional disputes between President, Prime Minister, and Parliament. Interprets the scope of presidential vs. parliamentary authority, especially during cohabitation. Does not collect rents or bear costs from the constraint; provides the authoritative reading that stabilizes or shifts the constraint's operation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Benefit from democratic accountability — the constraint ensures executive action reflects legislative will, which in turn reflects electoral mandate. Bear indirect costs when constraint produces gridlock or cohabitation instability. Exit is mobile at elections but constrained between them; no direct institutional exit from the constitutional framework.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens_voters, beneficiary,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures democratic accountability by requiring that executive policy implementation reflect the will of the elected legislature — solves the problem of unchecked executive power in a semi-presidential system.
% TRANSFER_FUNCTION: Transfers policy authorization power from the President to the legislative majority: the President's formal agenda-setting is conditioned on parliamentary confidence, moving effective governing authority to the PM and Assembly majority.
% ABSENT_VOICES: Territorial collectivities (regions, overseas territories) and non-parliamentary social movements — would object to centralization of policy authorization in national legislature but are excluded from the confidence mechanism.
% DISAPPEARANCE_RATIONALE: If the parliamentary confidence requirement vanished overnight, the President would become a de facto sovereign executive able to implement policy without legislative consent — the Fifth Republic would shift toward hyper-presidentialism, the PM would become a mere subordinate, and the constitutional balance would fundamentally reorganize.
% FOUNDING_PROBLEM: The Fourth Republic's chronic governmental instability — frequent cabinet collapses, legislative fragmentation, and executive weakness — which paralyzed policy implementation and undermined democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: De Gaulle's 1958 Bayeux speech and the constitutional debates attest the founding problem was governmental instability. Contemporary constitutional scholars (e.g., Duhamel, Carcassonne) and the Constitutional Council's jurisprudence corroborate that the original instability problem has been substantially solved, but the constraint persists in a form that now addresses a different problem (preventing presidential dominance) — attested by opposition parties and academic critics outside the original benefiting coalition.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.22) because the constraint primarily coordinates democratic accountability rather than extracting rents. Suppression (0.45) reflects the constitutional enforcement machinery (dissolution power, Constitutional Council review) needed to maintain the confidence requirement. Theater ratio (0.18) is low — the parliamentary sessions, confidence votes, and legislative process are functional, not performative. Accessibility collapse (0.52) is moderate: alternatives (presidential decree, Article 16 emergency powers, EU-level governance) exist but are constitutionally bounded. Resistance (0.38) is moderate — Presidents regularly test the constraint's boundaries but operate within the constitutional framework. The temporal series shows extractiveness lowest during strong parliamentary majorities (1980s-1990s), rising during cohabitation periods (1986-88, 1993-95, 1997-2002) and again after the 2000 term alignment reduced cohabitation frequency.
 *
 * PERSPECTIVAL GAP:
 *   From the President's seat (especially under hyper-presidential reading), the constraint appears as extraction — a parliamentary veto on democratic mandate. From the legislative majority's seat, it appears as coordination — democratic accountability mechanism. The engine computes this divergence from the structural data: same constraint, different directionality per seat. The hyper-presidential reading would assign the President d=0.15 (beneficiary) and legislative majority d=0.7 (payer); this reading inverts that. The divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The President is the primary payer (d near target end) when Assembly withholds confidence — bears costs of blocked agenda, forced cohabitation, or dissolution risk. The legislative majority is the primary beneficiary (d near beneficiary end) — holds authorization power and extracts policy concessions. The Prime Minister sits near symmetric (d ~0.5) — agenda-setter when supported by majority, constrained when not. The Constitutional Council is analytical (d=0.5). Citizens are beneficiaries with mobile exit. Legislative opposition is excluded — their exclusion is structural, not a bug.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic instability) is substantially solved — governments no longer collapse weekly. Yet the parliamentary constraint persists and has been repurposed: it now primarily prevents presidential dominance rather than ensuring governmental stability. This is not pure mandatrophy (the constraint still solves a live problem: unchecked executive power) but a shifted function. The founding_problem_status='contested' captures this: the original beneficiaries (proponents of stable government) would say the problem is dead; current beneficiaries (parliamentary majorities checking presidential power) say it's live. Corroboration from outside the beneficiary set (opposition parties, scholars) confirms the shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the parliamentary constraint reading describe a distinct constraint from the hyper-presidential reading, or are they interpretive frames on a single constraint?',
    'Compare ε values across readings: if hyper-presidential reading yields substantially higher extractiveness (President extracts from legislature) while this reading yields low extractiveness (legislature constrains President), they are structurally distinct constraints sharing a kernel. If ε is invariant, they are frames on one constraint.',
    'If distinct constraints, each gets its own classification and the kernel is a family. If one constraint, the classification must reconcile the divergent ε — suggesting the kernel itself is ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel supports multiple ε-invariant constraints or one constraint with contested interpretation.').

omega_variable(
    cohabitation_as_distinct_mode,
    'Is cohabitation a distinct constraint mode (triggering the cohabitation_equilibrium_reading) or a phase of the same parliamentary constraint?',
    'Measure extractiveness and suppression during cohabitation vs. unified majority periods. If metrics shift discontinuously at cohabitation boundaries, it is a mode switch. If they shift continuously, it is a phase.',
    'If mode switch, the kernel has three readings each with distinct metrics. If phase, the parliamentary constraint reading absorbs cohabitation as a variable-intensity phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cohabitation_as_distinct_mode, empirical, 'Whether cohabitation constitutes a structural regime change within the kernel.').

omega_variable(
    term_alignment_drift,
    'Has the 2000 quinquennat reform (aligning presidential and legislative terms) structurally weakened the parliamentary constraint by making cohabitation rare?',
    'Compare pre-2000 and post-2002 extractiveness/suppression trajectories controlling for majority size. If post-2002 shows sustained higher presidential extractiveness and lower parliamentary constraint activation, the reform altered the constraint''s structure.',
    'If confirmed, the constraint has drifted toward hyper-presidential operation without textual change — a practice drift that the parliamentary constraint reading must acknowledge as erosion of its reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_alignment_drift, empirical, 'Whether electoral calendar reform has shifted the constraint''s operational equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frc_pcr_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(frc_pcr_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(frc_pcr_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(frc_pcr_tr_t30, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(frc_pcr_tr_t40, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(frc_pcr_tr_t50, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(frc_pcr_tr_t60, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(frc_pcr_tr_t66, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 66, 0.18).

% Extraction over time
narrative_ontology:measurement(frc_pcr_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(frc_pcr_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(frc_pcr_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(frc_pcr_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(frc_pcr_be_t40, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(frc_pcr_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(frc_pcr_be_t60, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(frc_pcr_be_t66, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 66, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(frc_pcr_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(frc_pcr_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(frc_pcr_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(frc_pcr_su_t30, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(frc_pcr_su_t40, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(frc_pcr_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(frc_pcr_su_t60, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(frc_pcr_su_t66, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 66, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.1).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, french_electoral_system__two_round_majoritarian).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, eu_supranational_governance__domestic_parliamentary_control).

% DUAL FORMULATION NOTE:
% This reading is one of three in the fifth_republic_constitution constraint family. The parliamentary_constraint_reading claims the kernel instantiates a rope (democratic coordination via confidence requirement). The hyper_presidential_reading claims the same kernel instantiates a snare (President extracts from legislature via plebiscitary legitimacy). The cohabitation_equilibrium_reading claims a tangled_rope (dual executive coordinates via negotiated authority but extracts via mutual veto). All three share the kernel_id but author different ε, beneficiaries, victims, and claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, institutional, 0.75).
constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
