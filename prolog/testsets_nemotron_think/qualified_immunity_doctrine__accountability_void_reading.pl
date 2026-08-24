% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine — Accountability Void Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   Qualified immunity is a judge-made doctrine that shields government
 *   officials from civil liability unless they violated 'clearly established'
 *   constitutional rights. The accountability_void_reading treats this as a
 *   systematic extraction mechanism: the 'clearly established' standard
 *   functions as a near-absolute bar to liability because it requires victims
 *   to find a prior case with nearly identical facts — an impossible burden
 *   for novel or egregious violations. The doctrine extracts the value of
 *   constitutional rights from victims (who bear harm without remedy) and
 *   transfers it to officers and municipalities (who avoid accountability).
 *   The coordination story — that immunity enables 'vigorous policing' — is
 *   the cover; the operational reality is impunity for constitutional
 *   violations. The constraint is actively enforced by courts at every level;
 *   the Supreme Court refuses to correct misapplications, letting the
 *   extraction machinery run.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.85).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine — Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '71982b6a-d2a6-43c7-9e5d-fecd07c410c5').
narrative_ontology:cs_kernel_codification('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', fixed_text).
narrative_ontology:cs_authority_grounding('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', extraction).
narrative_ontology:cs_interpretation_layer_present('71982b6a-d2a6-43c7-9e5d-fecd07c410c5').
narrative_ontology:cs_reading_relation('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', qualified_immunity_doctrine__constitutional_fidelity_reading, forecloses).
narrative_ontology:cs_axiom('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', foundational, immunity_as_systematic_impunity_mechanism).
narrative_ontology:cs_axiom_status(immunity_as_systematic_impunity_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', immunity_as_systematic_impunity_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', foundational, clearly_established_standard_as_designed_barrier).
narrative_ontology:cs_axiom_status(clearly_established_standard_as_designed_barrier, holdable).
narrative_ontology:cs_axiom_grounding('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', clearly_established_standard_as_designed_barrier, empirically_contingent).
narrative_ontology:cs_axiom('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', secondary, founding_problem_historically_resolved).
narrative_ontology:cs_axiom_status(founding_problem_historically_resolved, holdable).
narrative_ontology:cs_axiom_grounding('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', founding_problem_historically_resolved, empirically_contingent).
narrative_ontology:cs_reference_frame('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', post_reconstruction_federal_official_protection).
narrative_ontology:cs_drift_state('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', contemporary_section_1983_erosion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('71982b6a-d2a6-43c7-9e5d-fecd07c410c5', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipalities).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, municipalities).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, separation_of_powers_judicial_lawmaking).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, official_immunity_common_law_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform constitutional violations (excessive force, false arrest, unlawful search) with near-certain immunity from personal civil liability. The 'clearly established' standard requires victims to find a prior case with nearly identical facts — a bar so high that novel or egregious violations often escape accountability. Officers face no personal financial risk; municipalities indemnify. Their professional incentives align with aggressive enforcement because the downside of constitutional error is effectively zero.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, arbitrage, national).

% Avoid direct financial liability for officers' constitutional violations through the immunity shield. They pay settlements occasionally but the immunity doctrine drastically reduces the volume and value of viable claims. They bear indirect costs (insurance, training, consent decrees) but these are a fraction of what full §1983 exposure would cost. They have political incentives to maintain the doctrine because it shields budgets and police labor relations.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipalities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, municipalities, payer).

% Lobby aggressively to preserve and expand qualified immunity. They frame it as essential for officer recruitment, retention, and 'vigorous policing.' They fund litigation defending the doctrine, file amicus briefs, and exert political pressure on legislators. Their institutional survival depends on delivering legal protection to members; immunity is a core deliverable.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, biographical, arbitrage, national).

% Suffer physical injury, psychological trauma, loss of liberty, or death from constitutional violations. When they sue, courts dismiss at summary judgment because no prior case 'clearly established' the right in the precise factual configuration. They bear the full cost of the violation — medical bills, lost wages, trauma — with zero compensation. They cannot exit the relationship with law enforcement; they are structurally trapped in jurisdictions where the doctrine operates.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims, payer,
    powerless, immediate, trapped, national).

% Bring §1983 actions on behalf of victims. Their cases are systematically filtered out by the 'clearly established' requirement — they must locate a factually near-identical precedent in the same circuit. This forces them to litigate losing cases to create precedent for future plaintiffs, a resource-intensive strategy that few can sustain. Attorneys decline meritorious cases because immunity makes recovery unlikely; the bar chills the entire enforcement ecosystem.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, payer,
    moderate, biographical, constrained, national).

% Created the doctrine in Pierson v. Ray (1967) and hardened it in Harlow v. Fitzgerald (1982) and subsequent cases. They administer the 'clearly established' standard case by case, effectively legislating the scope of constitutional remedies. Lower courts apply the doctrine mechanically; the Supreme Court rarely grants certiorari to correct misapplications, letting percolation failures stand. They benefit institutionally from avoiding a flood of constitutional tort litigation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Has statutory authority to modify or abrogate qualified immunity (it is a judicial gloss on §1983, not a constitutional command). Multiple bills (Ending Qualified Immunity Act, Justice in Policing Act) have been introduced but stall. Structural exclusion: the doctrine insulates itself from legislative correction by framing immunity as a 'judicial interpretation' of legislative intent, making congressional override appear as encroachment on judicial power.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, congress, excluded,
    institutional, generational, constrained, national).

% Produces the empirical and doctrinal critique: Joanna Schwartz's work showing dismissal rates, circuit splits, and the 'clearly established' game; William Baude's historical demonstration that the doctrine lacks statutory or common-law foundation. They document the extraction mechanism but have no enforcement lever. Their work feeds litigation strategy and legislative efforts but does not directly alter the constraint.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legal_academy, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates law enforcement by providing a predictable liability shield, enabling officers to act decisively in ambiguous situations without fear of personal financial ruin from good-faith errors — the protective_scaffold_reading's claimed function.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations from officers and municipalities to victims — victims bear the full harm (physical, financial, psychological) with no remedy; officers and governments avoid financial consequences, professional discipline, and deterrence pressure. The 'clearly established' standard operates as a transfer mechanism: each dismissal on immunity grounds transfers the loss to the victim and insulates the violator.
% ABSENT_VOICES: Victims of constitutional violations who cannot overcome the 'clearly established' bar — disproportionately poor, minority, and mentally ill populations who lack litigation resources. Future victims whose rights will be violated with impunity because no precedent is set when cases are dismissed on immunity grounds (the precedent-generating function of §1983 is disabled). Congress which has authority to modify the doctrine but is structurally excluded by the judiciary's framing of immunity as a separation-of-powers prerogative.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, victims would have viable §1983 claims against officers who violate constitutional rights; municipalities would face direct financial accountability for patterns of misconduct; officers would face personal liability exposure changing the deterrence calculus for excessive force and unlawful searches; the 'clearly established' precedent machinery would collapse and courts would need new standards for official immunity — likely a good-faith defense or statutory qualified immunity with defined scope. The §1983 enforcement ecosystem would reactivate.
% FOUNDING_PROBLEM: Post-Reconstruction concern (1871 Ku Klux Klan Act) that federal officials enforcing civil rights in the South would be harassed by bad-faith litigation in hostile state courts, undermining federal authority.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians William Baude ("Is Qualified Immunity Unlawful?" 2018) and Joanna Schwartz (empirical studies of §1983 dismissal rates) document the doctrine's origin in Pierson v. Ray (1967) as a judicial gloss on the 1871 Act, not in the statute itself. The Reconstruction-era concern is historically resolved; the doctrine expanded far beyond its founding context to cover all state and local officials in all contexts. No source outside the benefiting parties (officers, municipalities, unions) asserts the founding problem remains live.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.88 because the doctrine blocks virtually all §1983 claims at summary judgment — Schwartz's empirical work shows dismissal rates exceeding 90% in some circuits for excessive force claims. Suppression is 0.85 because the constraint's persistence depends on active judicial enforcement: courts police the 'clearly established' boundary aggressively, and the Supreme Court grants certiorari almost exclusively to reverse denials of immunity (protecting officers), not grants (protecting victims). Theater ratio is 0.68 because the 'clearly established' inquiry is performative — courts pretend to search for precedent while knowing the standard is designed to fail. The coordination function (predictable liability rules) exists but is overwhelmed by the extraction function. Accessibility collapse is 0.82 because once a plaintiff understands the 'clearly established' requirement, alternatives vanish — legislative fix is structurally blocked, state-law alternatives are preempted or inadequate. Resistance is 0.55 because civil rights plaintiffs and scholars resist, but the doctrine's institutional insulation (courts, Congress, police unions) is formidable.
 *
 * PERSPECTIVAL GAP:
 *   The protective_scaffold_reading seat (officers, unions, municipalities) experiences the constraint as genuine coordination — a necessary shield against ruinous litigation. The accountability_void_reading seat (victims, plaintiffs, scholars) experiences it as pure extraction — a mechanism that guarantees impunity. The engine computes this divergence from the structural data: same constraint, opposite classifications. The agenda_setter seat (courts) sits between — they built the doctrine and benefit from docket control, but face legitimacy erosion.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers are full beneficiaries (d ≈ 0.05): they collect the immunity subsidy — zero personal liability risk, indemnification guaranteed. Municipalities are beneficiaries with secondary payer role (d ≈ 0.15): they avoid most liability but bear some insurance/training costs. Police unions are pure beneficiaries (d ≈ 0.05): they extract political capital from delivering immunity. Victims are full targets (d ≈ 0.95): trapped, identity-locked (cannot exit relationship with law enforcement), bear full costs. Plaintiffs are targets (d ≈ 0.85): constrained exit (can choose other practice areas but the constraint shapes their entire field). Courts are agenda_setters with analytical exit (d ≈ 0.2): they administer the extraction but are institutionally insulated from its costs. Congress is excluded (d ≈ 0.5): could fix but structural incentives block action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Reconstruction-era harassment of federal officials) is dead — corroborated by historians outside the beneficiary set. The doctrine persists and has expanded far beyond its origin. This is mandatrophy: a coordination scaffold (if it ever was one) that became a snare. The constraint now extracts systematically with no live coordination justification. The 'vigorous policing' rationale is empirically contested (Schwartz shows no deterrence effect on misconduct) and logically circular (immunity removes the deterrence that would make policing constitutional). The classification prevents mislabeling by exposing the beneficiary/victim asymmetry: officers/municipalities collect; victims pay with no exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_immunity,
    'Is qualified immunity a genuine common-law background principle that Congress implicitly ratified, or a purely judicial fabrication with no statutory or historical foundation?',
    'Historical analysis of the 1871 Congress''s intent regarding official immunities; comparison with actual 1871 common law of official immunity (Baude 2018). If no common-law analogue exists for the modern ''clearly established'' standard, the doctrine is constructed extraction.',
    'If constructed, the doctrine has no natural-law claim — it is a judicial policy choice masquerading as interpretation. This strengthens the snare classification and undermines the protective_scaffold_reading''s legitimacy premise. If a genuine background principle exists, the mountain/false_summit_mountain question activates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_immunity, empirical, 'Whether the doctrine has any foundation in pre-1871 common law or statutory text.').

omega_variable(
    clearly_established_coordination_or_extraction,
    'Does the ''clearly established'' standard serve a genuine coordination function (predictable rules for officers) or is it purely an extraction mechanism (a designed-to-fail barrier)?',
    'Empirical study of officer knowledge of §1983 precedent: if officers actually know and are guided by ''clearly established'' law, coordination function exists. If they are unaware and the standard operates only as a judicial filter at summary judgment, it is pure extraction.',
    'If pure extraction, the constraint is a snare with zero coordination residue. If genuine coordination exists, it is a tangled_rope (coordination + extraction). The theater_ratio trajectory (rising from 0.25 to 0.68) suggests the coordination story has decayed into performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_coordination_or_extraction, empirical, 'Whether the central doctrinal mechanism coordinates behavior or merely filters claims.').

omega_variable(
    victim_suppression_structural_vs_internalized,
    'Is the suppression experienced by constitutional violation victims primarily structural (legal barriers, cost of litigation, attorney refusal) or internalized (victims believe they have no rights, distrust the system, self-censor)?',
    'Post-exit trajectory study: track victims who lose on immunity grounds — do they pursue alternative remedies (state court, complaints, media), or do they disengage entirely? If suppression persists after the legal barrier is removed (e.g., in a jurisdiction that abolishes immunity), the internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure — victims carry the suppression with them. This affects the omega-adjusted χ for the victim seat and the piton/theater analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism for the primary victim seat.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change if the kernel is resolved in favor of a sibling reading?',
    'Comparative classification: run the engine on all three readings'' constraint stories. If protective_scaffold_reading computes as rope/tangled_rope and constitutional_fidelity_reading computes as mountain (illegitimate = no constraint), the kernel contest is a classification fork. The accountability_void_reading''s snare classification is stable only if the extraction structure is real independent of the reading''s framing.',
    'If the snare classification holds across readings (i.e., even the protective_scaffold_reading''s metrics show high extraction), the extraction is structural, not framing-dependent. If classification flips with reading, the kernel itself is the site of contestation — the constraint family must be analyzed as a unit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta across sibling readings of the qualified_immunity_doctrine kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(qual_tr_t1975, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.5).
narrative_ontology:measurement(qual_tr_t1990, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement(qual_tr_t2001, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2001, 0.6).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2009, 0.63).
narrative_ontology:measurement(qual_tr_t2018, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2018, 0.66).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(qual_be_t1975, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement(qual_be_t1990, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(qual_be_t2001, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2001, 0.78).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2009, 0.82).
narrative_ontology:measurement(qual_be_t2018, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2018, 0.86).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(qual_su_t1975, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(qual_su_t1990, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(qual_su_t2001, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2001, 0.78).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2009, 0.81).
narrative_ontology:measurement(qual_su_t2018, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, section_1983_enforcement_ecosystem).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, municipal_liability_monell_doctrine).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_union_collective_bargaining_rights).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, fourth_amendment_exclusionary_rule).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, civil_rights_attorney_fee_shifting).

% DUAL FORMULATION NOTE:
% This constraint is one member of the qualified_immunity_doctrine constraint family (kernel_id: qualified_immunity_doctrine). The family has three readings: accountability_void_reading (this file, snare), protective_scaffold_reading (claimed rope/tangled_rope), constitutional_fidelity_reading (claimed mountain — illegitimate doctrine = no constraint). All three share the same doctrinal kernel but instantiate different constraints with different ε, different victim/beneficiary structures, and different classifications. The accountability_void_reading's ε (0.88) diverges sharply from the protective_scaffold_reading's expected ε (~0.3-0.4) and the constitutional_fidelity_reading's ε (~0.0). This is the BGS pattern: same label, structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.15).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, organized, 0.05).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, powerless, 0.95).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, moderate, 0.85).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
