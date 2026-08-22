% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction Reading of the Constitutional Authority Boundary
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the coordinate-construction reading of the
 *   constitutional authority boundary kernel: each of the three branches
 *   interprets the constitution within its own sphere of operation, and no
 *   single branch holds final, unchallengeable authority over constitutional
 *   meaning. This is structurally distinct from the judicial-supremacy
 *   reading (which vests final interpretive authority in the courts) and the
 *   parliamentary-primacy reading (which subordinates any constitutional text
 *   to the ordinary or entrenched will of the elected legislature) — those
 *   are separate constraint files with their own ε and beneficiary
 *   structures, linked here via network.affects_constraints. Under this
 *   reading, the coordination function (preventing monopolized interpretive
 *   power) is genuine and is bundled with real extraction: institutional
 *   actors gain strategic leverage and insulation from binding resolution,
 *   while litigants and rights-claimants who need finality bear the cost of
 *   protracted, potentially unenforceable outcomes.
 *
 * KEY AGENTS:
 *   - judicial_branch: interprets within cases and controversies, cannot compel enforcement beyond parties
 *   - legislative_branch: can override statutory interpretation, holds appropriations and impeachment leverage
 *   - executive_branch: interprets in the course of enforcement, can assert non-acquiescence
 *   - litigants_seeking_finality: bear the cost of interpretive multiplicity
 *   - minority_rights_claimants: most exposed when political branches contest a favorable judicial ruling
 *   - constitutional_scholars: analytical observers of departmentalism as a structural pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.38).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction Reading of the Constitutional Authority Boundary").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '5f951a20-2160-4b62-bac3-06a8f1e3b668').
narrative_ontology:cs_kernel_codification('5f951a20-2160-4b62-bac3-06a8f1e3b668', formalized).
narrative_ontology:cs_authority_grounding('5f951a20-2160-4b62-bac3-06a8f1e3b668', distributed).
narrative_ontology:cs_reading_relation('5f951a20-2160-4b62-bac3-06a8f1e3b668', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('5f951a20-2160-4b62-bac3-06a8f1e3b668', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('5f951a20-2160-4b62-bac3-06a8f1e3b668', foundational, no_branch_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_branch_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('5f951a20-2160-4b62-bac3-06a8f1e3b668', no_branch_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('5f951a20-2160-4b62-bac3-06a8f1e3b668', foundational, interpretive_authority_is_sphere_bounded_not_universal).
narrative_ontology:cs_axiom_status(interpretive_authority_is_sphere_bounded_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('5f951a20-2160-4b62-bac3-06a8f1e3b668', interpretive_authority_is_sphere_bounded_not_universal, conventional).
narrative_ontology:cs_reference_frame('5f951a20-2160-4b62-bac3-06a8f1e3b668', founding_era_separation_of_powers_equilibrium).
narrative_ontology:cs_drift_state('5f951a20-2160-4b62-bac3-06a8f1e3b668', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5f951a20-2160-4b62-bac3-06a8f1e3b668', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, litigants_seeking_finality).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, regulated_private_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution within cases and controversies, issues binding rulings on parties before it, but cannot compel the political branches to enforce or acquiesce beyond that. Benefits from the coordinate reading because it preserves judicial independence from override by simple statute, but pays a cost in the form of non-enforcement risk and the possibility that its constitutional interpretations are ignored by coordinate branches acting within their own spheres.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, beneficiary).

% Passes statutes it believes are constitutional, can override judicial statutory interpretation (though not most constitutional holdings), and retains appropriations and impeachment powers as leverage against the other branches. Benefits from the coordinate reading because it is not simply bound by judicial constitutional pronouncements in every future legislative act, but faces the same relative uncertainty the other branches do about who prevails in a genuine conflict.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, beneficiary).

% Interprets the constitution in the course of enforcing law, can decline to enforce judicial orders it deems non-binding beyond the parties, and can veto legislation on constitutional grounds. Benefits from departmental interpretive latitude and the historical practice of executive non-acquiescence in certain contexts, but is checked by impeachment, appropriations, and the reputational cost of appearing to defy settled rulings.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, beneficiary).

% Bring constitutional claims expecting a definitive resolution, but under coordinate construction a favorable ruling in one branch's sphere may be undermined or left unenforced by another branch acting on its own reading. They bear the cost of prolonged uncertainty, repeated litigation, and remedies that depend on political-branch cooperation they cannot compel.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, litigants_seeking_finality, payer,
    moderate, biographical, trapped, national).

% Rely disproportionately on judicial constitutional protection against majoritarian action; when the political branches assert coordinate authority to disregard or narrow a favorable ruling, they have no alternative venue with binding finality. Their exit options are essentially foreclosed because the same government structure that would need to enforce their rights is the one contesting whose interpretation controls.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Must comply with regulatory and statutory regimes whose constitutional status can shift depending on which branch's interpretation currently controls a given dispute; they bear compliance costs and legal uncertainty from the absence of a single settled interpretive authority, sometimes structuring conduct around conflicting guidance from different branches simultaneously.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, regulated_private_actors, payer,
    moderate, biographical, constrained, national).

% Study inter-branch interpretive conflict as a structural feature of the constitutional order, documenting historical instances of departmentalism, judicial supremacy assertions, and legislative override attempts without themselves being bound by or benefiting materially from any particular resolution.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributing interpretive authority across three branches prevents any single institution from monopolizing constitutional meaning, forcing sustained inter-branch dialogue and making constitutional change harder to accomplish unilaterally — a genuine check against concentrated interpretive power.
% TRANSFER_FUNCTION: Moves the cost of interpretive uncertainty and remedy-enforcement risk from the institutions (which retain leverage and reputational insulation) onto litigants and rights-claimants who need a definitive, enforceable answer and cannot compel one when branches disagree.
% ABSENT_VOICES: Individuals and groups whose rights depend on prompt, final resolution — particularly minorities relying on judicial protection against majoritarian legislative or executive action — have no seat in the inter-branch negotiation; their claims are adjudicated by branches that may be, or may become, adversarial to each other's rulings on entirely separate institutional grounds.
% DISAPPEARANCE_RATIONALE: If distributed interpretive authority collapsed into a single final arbiter overnight (whichever branch), the entire architecture of checks and balances would restructure: legislative override mechanisms, executive non-acquiescence practices, and judicial independence from political retaliation would all need to be renegotiated around the new monopoly authority, changing incentives for all three branches and for private actors who currently plan around interpretive multiplicity.
% FOUNDING_PROBLEM: The founding-era problem was preventing any one branch — particularly a monarch-like executive or an unchecked legislature — from seizing final authority over the meaning of fundamental law, given fresh memory of unchecked sovereign power.
% FOUNDING_PROBLEM_CORROBORATION: Historical framers' debates (Federalist 78, Madison's separation-of-powers writings) attest the problem as originally conceived; contemporary comparative constitutional scholars outside any single branch's interest attest the problem remains partly live (concentration risk persists) but note the mechanism has also become a source of gridlock and unaccountability that the founders did not fully anticipate — a status neither purely live nor purely dead.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits in the moderate 0.35-0.50 band (authored at 0.42) reflecting genuine inter-branch conflict potential without a monopoly extractor — no single branch captures a disproportionate share of the arrangement's benefits, but the diffuse cost lands on those needing prompt, enforceable resolution. Suppression is moderate (0.38) because dissenting branches retain real recourse (override, veto, non-acquiescence, appropriations) rather than being coercively foreclosed, though the arrangement does suppress the possibility of definitive judicial finality for claimants who have no equivalent leverage. Theater ratio is modest and rising slowly (0.15 to 0.28) reflecting an observed drift where formal invocations of 'coordinate branches' occasionally substitute for substantive resolution of genuine disputes, particularly in areas where political branches decline to enforce disfavored rulings while nominally respecting judicial authority.
 *
 * DIRECTIONALITY LOGIC:
 *   All three branches are declared as both agenda_setters and beneficiaries because each retains institutional leverage that the other two lack full power to override — this is the structural core of coordinate construction. Litigants, minority rights claimants, and regulated private actors are declared as payers because they must operate within a system whose ultimate resolution depends on inter-branch cooperation they cannot compel; minority rights claimants carry the highest exposure given their comparative reliance on judicial protection against majoritarian branches with a track record of resisting expansive judicial constitutional holdings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing sovereign concentration) remains partially live — the coordinate structure genuinely still deters single-branch capture of constitutional meaning. But the founding_problem_status is marked contested rather than dead or live because the mechanism now also produces a distinct harm the founders did not centrally anticipate: prolonged inter-branch stalemate that itself extracts costs from claimants needing timely resolution. Classifying this as tangled_rope (rather than pure rope or pure snare) prevents two mislabeling errors: treating coordinate construction as costless pure coordination (ignoring the payer seats) and treating it as pure extraction with an identifiable capturing beneficiary (there is none — extraction is diffused across three co-equal institutional actors, none of which fully captures the surplus).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_reading_versus_siblings,
    'Is coordinate construction the constitutionally correct reading, or is it a contingent equilibrium that would collapse toward judicial supremacy or parliamentary primacy under sustained political pressure?',
    'This constraint documents ONE reading of the constitutional_authority_boundary kernel, distinct from the judicial_supremacy_reading and parliamentary_primacy_reading (separate constraint files). Resolution would require observing which reading actually governs practice during a genuine three-way interpretive conflict — e.g., a case where the executive refuses enforcement of a Court ruling that Congress has also purported to override by statute.',
    'If sustained practice shows one branch''s interpretation reliably prevailing in conflicts, the coordinate-construction reading becomes descriptively false even where it remains normatively asserted, and the operative constraint shifts toward one of the sibling readings'' structure (different ε, different beneficiary set).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_reading_versus_siblings, conceptual, 'Whether coordinate construction is the live operative reading or a normative claim increasingly contradicted by practice favoring one sibling reading.').

omega_variable(
    diffuse_extraction_versus_capture,
    'Does the absence of a single capturing beneficiary mean the arrangement is genuinely non-extractive at the institutional level, or does diffusion of extraction across three branches simply make capture harder to detect and remedy?',
    'Comparative analysis of remedy outcomes for rights-claimants across jurisdictions with coordinate construction versus jurisdictions with clear judicial or parliamentary supremacy, controlling for underlying rights violations.',
    'If claimants systematically fare worse under coordinate construction due to enforcement uncertainty, the diffuse-beneficiary structure functions as a distributed extraction mechanism despite lacking a single rent-collecting institution, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_extraction_versus_capture, empirical, 'Whether distributed institutional benefit obscures rather than eliminates net extraction from claimants.').

omega_variable(
    departmentalism_versus_settled_practice,
    'Is the coordinate-construction reading itself contested within the constitutional order''s own history (i.e., is ''coordinate construction'' a live doctrinal position or largely superseded by de facto judicial supremacy in practice since Marbury and its progeny)?',
    'Doctrinal and historical survey of instances of explicit non-judicial constitutional interpretation asserted against a judicial holding (e.g., executive non-enforcement episodes, legislative re-passage of struck statutes) versus instances of unchallenged judicial finality.',
    'A predominance of unchallenged judicial finality in the historical record would suggest the coordinate-construction reading, while textually and theoretically available, has been substantially displaced by convergence toward the judicial_supremacy_reading in practice — affecting how much weight this reading''s ε and classification should carry as a description of the operative constraint versus an aspirational or minority doctrinal position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(departmentalism_versus_settled_practice, empirical, 'Whether coordinate construction describes current practice or a historically available but largely dormant doctrinal alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the constitutional_authority_boundary kernel. judicial_supremacy_reading vests final interpretive authority in courts (a concentrated-beneficiary structure with high accessibility_collapse for other branches' contrary readings). parliamentary_primacy_reading subordinates constitutional meaning to legislative sovereignty (a different concentrated-beneficiary structure favoring the legislature). This coordinate_construction_reading is the diffuse-beneficiary alternative: no branch holds final authority, extraction is spread across all three institutional actors rather than captured by one, and ε sits lower (0.42) than either sibling would likely author for their respective concentrated-authority arrangements, reflecting the absence of a monopoly extractor even though genuine cost is imposed on claimants needing finality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
