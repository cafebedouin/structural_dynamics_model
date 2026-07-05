% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation as Divinely Suspended Duty Pending Messianic Restoration
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates the messianic-suspension reading of the sacrifice
 *   obligation kernel: the commandment to offer sacrifices is held to remain
 *   divinely binding but structurally unfulfillable since the Temple's
 *   destruction, and is neither transformed into a different obligation nor
 *   discharged through study — it is suspended, in abeyance, awaiting
 *   restoration. Under this reading, sustained study of sacrificial law
 *   functions instrumentally: it maintains procedural readiness so that, upon
 *   restoration, performance can resume with fidelity rather than
 *   reconstruction. This is deliberately one reading among four sibling
 *   constraints (study_as_exercise_reading, performance_only_reading,
 *   symbolic_archive_reading) that share the same textual kernel but diverge
 *   on what study accomplishes and whether the obligation is live,
 *   discharged, transformed, or archival. Each sibling is authored as its own
 *   constraint with its own epsilon; this file does not average or hedge
 *   across them.
 *
 * KEY AGENTS:
 *   - study_house_scholars: administer and set the agenda for readiness-maintenance study (institutional/identity_locked)
 *   - future_restoration_generation: hypothetical beneficiary of preserved operational capacity (powerless/analytical, non-existent-yet)
 *   - ordinary_practitioners: bear the diffuse psychological/liturgical cost of living under an unfulfillable obligation (moderate/constrained)
 *   - rival_halakhic_authorities: hold sibling readings excluded from this reading's own framework (organized/constrained)
 *   - religious_studies_observers: analytical seat tracking the doctrine's persistence and function (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation as Divinely Suspended Duty Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'f082cbda-1494-4118-90d8-cb12f0013c3d').
narrative_ontology:cs_kernel_codification('f082cbda-1494-4118-90d8-cb12f0013c3d', fixed_text).
narrative_ontology:cs_authority_grounding('f082cbda-1494-4118-90d8-cb12f0013c3d', lineage).
narrative_ontology:cs_interpretation_layer_present('f082cbda-1494-4118-90d8-cb12f0013c3d').
narrative_ontology:cs_reading_relation('f082cbda-1494-4118-90d8-cb12f0013c3d', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('f082cbda-1494-4118-90d8-cb12f0013c3d', sacrifice_obligation_kernel__performance_only_reading, influences).
narrative_ontology:cs_reading_relation('f082cbda-1494-4118-90d8-cb12f0013c3d', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('f082cbda-1494-4118-90d8-cb12f0013c3d', foundational, obligation_persists_in_abeyance_pending_restoration).
narrative_ontology:cs_axiom_status(obligation_persists_in_abeyance_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('f082cbda-1494-4118-90d8-cb12f0013c3d', obligation_persists_in_abeyance_pending_restoration, deontological).
narrative_ontology:cs_axiom('f082cbda-1494-4118-90d8-cb12f0013c3d', foundational, study_is_instrumental_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_instrumental_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('f082cbda-1494-4118-90d8-cb12f0013c3d', study_is_instrumental_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('f082cbda-1494-4118-90d8-cb12f0013c3d', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('f082cbda-1494-4118-90d8-cb12f0013c3d', post_destruction_exile_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f082cbda-1494-4118-90d8-cb12f0013c3d', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_restoration_generation).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, study_house_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, ordinary_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, temple_service_restorability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the corpus of sacrificial law through continuous study — reconstructing procedural detail, adjudicating disputed points, training successive cohorts — on the premise that the obligation itself is suspended, not extinguished, and that this maintenance work keeps the community capable of resuming performance the moment restoration occurs. They administer the curriculum and set which questions count as live halakhic inquiry versus antiquarian curiosity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, study_house_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, study_house_scholars, beneficiary).

% A not-yet-existing population who would, on this reading, inherit an intact operational tradition capable of resuming sacrificial service without centuries-long reconstruction. They receive the benefit of preserved procedural knowledge but have no voice in how the maintenance work is currently conducted.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_restoration_generation, beneficiary,
    powerless, civilizational, analytical, global).

% Live under a legal system that treats sacrifice as a live obligation held in abeyance rather than dead law — this shapes liturgy, calendar, mourning practices for the Temple's absence, and the framing of certain holidays. They bear the psychological and communal cost of orienting daily and seasonal practice around an obligation they can never discharge, without receiving the operational-readiness benefit directly.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, ordinary_practitioners, payer,
    moderate, biographical, constrained, regional).

% Communities and authorities who hold one of the sibling readings (study-as-exercise, performance-only, or symbolic-archive) are structurally present but not accommodated within this reading's own framework — the suspension reading treats their positions as either premature performance-claims or under-claims that abandon the restoration horizon. They would object that suspension-framing understates or overstates what study accomplishes, but this constraint's internal logic does not make room for their view.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rival_halakhic_authorities, excluded,
    organized, generational, constrained, regional).

% Historians and comparative religion scholars who track how the suspension doctrine has functioned across exile, its relationship to messianic expectation, and whether its persistence tracks genuine communal belief or institutional path-dependency in rabbinic authority structures.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, religious_studies_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves procedural and legal knowledge of sacrificial service across an indefinite interruption so that resumption, if and when it occurs, does not require starting from an archaeological or reconstructed basis — the coordination problem is intergenerational transmission of a capacity that has no current use-case.
% TRANSFER_FUNCTION: Moves scholarly and communal attention and resources into the maintenance of a body of law with no present operational referent, in exchange for the preservation of restorability; the cost is borne diffusely by communities orienting practice around an unfulfillable duty, and the benefit accrues to a hypothetical future generation.
% ABSENT_VOICES: Adherents of the sibling readings — those who hold that study itself fulfills the mitzvah, that only physical performance counts and study is mere preparation, or that the material is a cultural archive with no live legal claim — are not represented within this reading's own commitments; each treats the suspension frame as either an evasion or an overclaim.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine were abandoned, some communities would shift toward the study-as-exercise reading (converting the obligation into a currently dischargeable duty) while others would shift toward symbolic-archive framing (relieving the community of any live obligation). Practitioners and scholars dispute which outcome would follow, which is itself part of why the kernel remains contested rather than settled.
% FOUNDING_PROBLEM: The Temple's destruction removed the physical locus where sacrificial obligation could be performed, creating an acute crisis: was the commandment nullified, transformed, or held pending? The suspension reading was built to preserve the commandment's normative force and the possibility of restoration without asserting that anyone was currently in violation of it.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic and later halakhic authorities within the tradition attest the suspension framing as the operative resolution; however, comparative religion scholars and historians outside the tradition's own authority structure note that suspension-as-abeyance is one of several theologically motivated responses to an irrecoverable practice, and that its persistence over two millennia is at least partly explained by its capacity to sustain rabbinic scholarly authority independent of Temple-based priestly authority — a reading not endorsed by the tradition's own committed adherents.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because under this reading no one is being coerced into discharging a duty they cannot discharge — the obligation is explicitly suspended, and the associated study activity is instrumental maintenance rather than a substitute performance extracting compliance from anyone. Suppression is low-moderate (0.18): there is social and institutional pressure to accept the suspension framing over the sibling readings, but no coercive mechanism preventing individuals from holding a different view privately. Theater ratio is low (0.15) and drifts only slightly upward — the study activity is authored as substantially functional (preserving real procedural knowledge) rather than performative, though a small rising theater component reflects that some fraction of study, over centuries with no restoration in sight, drifts toward commemorative rather than strictly operational content. Accessibility collapse is moderate (0.4): once inside the tradition's own framework the suspension reading is treated as settled, but the existence of three live sibling readings means alternatives have not collapsed at the level of the broader tradition. Resistance is low (0.2): the reading is not without dissent (from adherents of sibling readings) but does not face organized opposition seeking to overturn it from within its own adherent community.
 *
 * PERSPECTIVAL GAP:
 *   From the study_house_scholars' seat, the arrangement is a genuine coordination and preservation function with no victims — a Scaffold or near-Rope structure whose sunset is restoration. From ordinary_practitioners' seat, the same arrangement can register as a low-grade but real cost: organizing communal life for millennia around commemorating a duty that can never be discharged, whose end-condition (restoration) is outside anyone's control. From rival_halakhic_authorities' seat, the reading looks either insufficiently committed (understating what study accomplishes) or overcommitted (overstating that anything remains obligatory at all) — this is not extraction but genuine doctrinal disagreement the engine cannot resolve from structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as future_restoration_generation (the hypothetical population that inherits preserved operational knowledge) and study_house_scholars (who derive institutional continuity, authority, and purpose from administering the study). No victims are declared for this reading specifically, consistent with the expected structural delta: the obligation is in abeyance, not violated, so there is no identifiable party currently being extracted from by this reading's operation. ordinary_practitioners are declared as payers bearing diffuse liturgical/psychological cost, but this is authored as a low-grade cost of orientation, not extraction with an identifiable beneficiary capturing it — hence gain_flow is not declared as concentrated.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's declared sunset is messianic restoration — a condition external to any human institution's control, which is structurally unusual for a scaffold (most sunset clauses are set by the administering party itself). This reading resists mandatrophy in the classic sense (no party is captured collecting rents while claiming transition) precisely because the suspension framing forbids claiming the obligation has been discharged or replaced — study_house_scholars cannot declare victory or absorb the obligation permanently without abandoning the reading itself. The corroboration question — whether the doctrine persists because the founding problem is genuinely still live (Temple absent) or because it sustains scholarly authority independent of priestly authority — is exactly the mandatrophy-adjacent question an outside observer (religious_studies_observers) is positioned to raise but that the tradition's own adherents do not experience as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_extinguishment_ambiguity,
    'Is the obligation genuinely suspended (retaining full normative force, pending restoration) or has it been functionally extinguished while suspension-language is retained for continuity reasons?',
    'Compare halakhic rulings on adjacent questions (e.g., whether failure to rebuild the Temple when politically feasible constitutes violation) across authorities who hold the suspension reading; a consistent refusal to treat non-restoration as violation even when restoration is feasible would suggest functional extinguishment dressed as suspension.',
    'If functionally extinguished, this reading''s low extractiveness score is doing real analytical work masking a de facto convergence with symbolic_archive_reading; if genuinely suspended, the reading is structurally distinct and the low extractiveness is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_extinguishment_ambiguity, conceptual, 'Whether suspension is substantively different from extinguishment-with-continuity-language.').

omega_variable(
    restoration_horizon_credibility,
    'Does the messianic restoration horizon function as a genuinely anticipated future condition, or as a rhetorically indefinite deferral that structurally never resolves?',
    'Examine whether communities holding this reading maintain concrete restoration-readiness practices (updated architectural/genealogical/procedural preparation) versus purely commemorative practices with no operational updating.',
    'A credible, actively-maintained horizon supports the scaffold classification (real transition-orientation); a rhetorically indefinite horizon would push the constraint toward piton (maintained by inertia, sunset clause present in name only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_horizon_credibility, empirical, 'Whether the sunset clause is operationally real or nominal.').

omega_variable(
    scholarly_authority_incentive,
    'To what extent does the suspension reading''s persistence serve the institutional interests of study_house_scholars in maintaining rabbinic authority independent of a restored priestly/Temple hierarchy, versus reflecting sincere theological conviction with no institutional incentive component?',
    'Historical analysis of periods where restoration seemed politically proximate (e.g., certain moments of political upheaval) and whether rabbinic authorities'' doctrinal positions shifted in ways correlated with their own institutional exposure to a restored Temple hierarchy.',
    'If the reading''s persistence tracks institutional self-interest rather than pure theological conviction, the beneficiary declaration for study_house_scholars is under-weighted relative to future_restoration_generation, and the constraint would sit closer to tangled_rope than scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_authority_incentive, conceptual, 'Whether the reading''s persistence is explained by sincere doctrine or institutional incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 80, 0.11).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of sacrifice_obligation_kernel, decomposed per the ε-invariance principle: each reading of 'what study of sacrificial law accomplishes' is structurally distinct (different extractiveness, different beneficiary/victim structure, different relationship between study and the obligation) and is authored as its own constraint story rather than as one story with a measurement parameter. This reading (messianic_suspension_reading) carries the lowest extractiveness of the four because it declares no victim set and treats study as instrumental maintenance rather than substitutive performance or archival repurposing. study_as_exercise_reading would show a different transfer structure (study as currently-dischargeable obligation). performance_only_reading would potentially show higher suppression if it treats study-only communities as in ongoing default of a live physical-performance duty. symbolic_archive_reading would show near-zero suppression and no live legal claim at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
