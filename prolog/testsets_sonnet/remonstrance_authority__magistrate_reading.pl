% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Parlementary Remonstrance Right (Magistrate Reading: Guardian of Fundamental Law)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the MAGISTRATE READING of the contested
 *   remonstrance-authority kernel: the Parlements' right to remonstrate
 *   against royal edicts before registration, understood from the
 *   magistracy's own constitutional self-conception — a fundamental mechanism
 *   preserving ancient liberties, customary law, and the kingdom's unwritten
 *   constitution against arbitrary royal innovation. This is deliberately NOT
 *   the crown reading (which treats the same right as an illegitimate
 *   minoritarian veto protecting particularist privilege) — that is a
 *   separate constraint, linked via network edges, not a parameter of this
 *   one. Under this reading, ε is measured against the magistracy's actual
 *   documented behavior: heavy concentration of remonstrance activity on
 *   fiscal edicts touching noble and office-holder tax exemptions, producing
 *   a tax-exempt magistracy beneficiary class and drawing the royal treasury,
 *   fiscal reformers, and non-privileged taxpayers into the victim set when
 *   their preferred edicts are overridden or indefinitely delayed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.62).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.48).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Parlementary Remonstrance Right (Magistrate Reading: Guardian of Fundamental Law)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '8b357352-3a6e-4647-a5a1-01460b759918').
narrative_ontology:cs_kernel_codification('8b357352-3a6e-4647-a5a1-01460b759918', distributed).
narrative_ontology:cs_authority_grounding('8b357352-3a6e-4647-a5a1-01460b759918', lineage).
narrative_ontology:cs_interpretation_layer_present('8b357352-3a6e-4647-a5a1-01460b759918').
narrative_ontology:cs_reading_relation('8b357352-3a6e-4647-a5a1-01460b759918', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('8b357352-3a6e-4647-a5a1-01460b759918', foundational, judicial_magistracy_guards_fundamental_law).
narrative_ontology:cs_axiom_status(judicial_magistracy_guards_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('8b357352-3a6e-4647-a5a1-01460b759918', judicial_magistracy_guards_fundamental_law, conventional).
narrative_ontology:cs_axiom('8b357352-3a6e-4647-a5a1-01460b759918', secondary, registration_consent_binds_royal_will).
narrative_ontology:cs_axiom_status(registration_consent_binds_royal_will, holdable).
narrative_ontology:cs_axiom_grounding('8b357352-3a6e-4647-a5a1-01460b759918', registration_consent_binds_royal_will, conventional).
narrative_ontology:cs_reference_frame('8b357352-3a6e-4647-a5a1-01460b759918', ancient_constitution_customary_law).
narrative_ontology:cs_drift_state('8b357352-3a6e-4647-a5a1-01460b759918', pre_revolutionary_fiscal_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b357352-3a6e-4647-a5a1-01460b759918', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlementary_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, office_holding_nobility_of_the_robe).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_fiscal_privilege_holders).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, non_privileged_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parlement registers royal edicts before they take effect as law and exercises the right of remonstrance — formally objecting to and returning edicts it judges contrary to fundamental law, custom, or the kingdom's ancient constitution. Magistrates hold venal, heritable offices that are themselves often exempt from the very taxes under dispute. They frame remonstrance as sworn duty to protect the realm's fundamental laws from arbitrary royal innovation, and can delay registration indefinitely absent a lit de justice.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementary_magistracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlementary_magistracy, beneficiary).

% Depends on timely registration of fiscal edicts to collect new revenue, especially during wartime or debt crises. Remonstrance delays registration for months or years, forcing the crown to negotiate, modify, withdraw, or force through edicts via lit de justice at high political cost. The treasury bears the fiscal shortfall and borrowing-cost premium generated by the delay.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_treasury, payer,
    institutional, immediate, constrained, national).

% Commoners and non-exempt taxpayers ultimately bear a heavier share of the fiscal burden because remonstrance is disproportionately deployed to defend privileged exemptions (nobility, clergy, office-holders) rather than to protect broad popular liberties. When reform edicts that would have broadened the tax base are blocked, the shortfall is recovered through existing regressive levies on those without standing to remonstrate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, non_privileged_taxpayers, payer,
    powerless, biographical, trapped, national).

% Royal ministers attempting fiscal or administrative reform (e.g., universal land taxes, provincial fiscal equalization) must either win over the Parlements, force registration through a lit de justice risking legitimacy crisis, or abandon reform. Their career and the crown's solvency are both hostage to remonstrance timing.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_reformers, payer,
    powerful, biographical, constrained, national).

% Holds venal magistrate offices that confer noble status, tax exemptions, and remonstrance standing. Benefits directly from the Parlement's power to block edicts that would tax noble land or dilute office-holder privilege, while presenting this defense as principled constitutionalism.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, office_holding_nobility_of_the_robe, beneficiary,
    organized, generational, arbitrage, national).

% Regional estates and privileged corporations whose historic tax arrangements are shielded whenever a Parlement remonstrates against uniform national fiscal edicts. They gain indefinitely from the persistence of a patchwork of exemptions that remonstrance helps preserve.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_fiscal_privilege_holders, beneficiary,
    organized, generational, mobile, regional).

% Study whether remonstrance functioned as genuine constitutional check or as an entrenched veto for a narrow office-holding class, weighing the doctrine's stated purpose against its documented fiscal effects.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, non-violent mechanism for reviewing royal edicts against a body of accumulated fundamental law and custom, preventing purely arbitrary or unvetted legislative action and giving the realm's judicial institutions a structured voice before new law binds the population.
% TRANSFER_FUNCTION: Moves fiscal risk and shortfall from privileged office-holders and provincially exempt groups onto the royal treasury and non-exempt taxpayers, by enabling indefinite delay or defeat of edicts that would equalize or broaden the tax base.
% ABSENT_VOICES: Non-privileged taxpayers who bear the shifted fiscal burden have no standing to remonstrate and are not present in the registration process at all; peasant and urban commoner interests are represented, if at all, only rhetorically by magistrates whose own privileges are frequently the object of the edicts being blocked.
% DISAPPEARANCE_RATIONALE: If remonstrance disappeared overnight, royal edicts would register automatically, removing the primary structural check on crown fiscal action — the crown could tax office-holders and privileged corporations without institutional resistance, magistrate offices would lose much of their political value, and provincial fiscal patchworks would face far more direct pressure to equalize.
% FOUNDING_PROBLEM: Originally a mechanism for the sovereign courts to flag technical or legal defects in edicts before registration, ensuring new law was consistent with prior law and did not contradict itself or established custom — a genuine legal-quality check.
% FOUNDING_PROBLEM_CORROBORATION: Parlementary magistrates themselves attest the founding problem is fully live — that fundamental law requires an independent guardian against arbitrary royal will. Royal fiscal ministers and later constitutional historians, working from treasury records and edict registers outside the magistracy's own self-description, document that by the eighteenth century remonstrance overwhelmingly targeted tax and privilege edicts rather than genuine legal defects, supporting a shifted-function reading from outside the beneficiary class.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that remonstrance's actual historical deployment skews heavily toward blocking tax-base-broadening edicts rather than genuine legal-consistency review — the coordination function (legal-quality check) is real but has been substantially colonized by privilege defense. Suppression (0.48) is moderate: the crown retains the lit de justice override, so the Parlements' power is real but not absolute; magistrates cannot simply block law forever, only delay and raise costs. Theater ratio rises over the measured interval (0.15 to 0.40) as remonstrance increasingly invokes 'fundamental law' rhetoric to cover fiscal self-interest rather than genuine jurisprudential objections — a Goodhart-style substitution of constitutional language for its original legal-review function. Resistance (0.72) is high because both crown reformers and (later) popular fiscal-equality advocates actively contest the practice; accessibility_collapse (0.5) reflects that alternative paths to legal review (royal council, provincial estates) persisted but were increasingly marginalized as remonstrance became the dominant veto point.
 *
 * DIRECTIONALITY LOGIC:
 *   The parlementary magistracy and the office-holding nobility of the robe are the clear structural beneficiaries: they hold the venal offices that confer standing, they are frequently the direct beneficiaries of the tax exemptions being defended, and their exit option is effectively arbitrage — they can shift between judicial, fiscal, and social capital freely. The royal treasury and crown fiscal reformers are targets: they bear the direct cost of blocked or delayed revenue and cannot exit the relationship (the crown needs registration to govern). Non-privileged taxpayers are the most severely targeted: powerless, trapped, and bearing the downstream fiscal burden of privilege-preservation without any standing in the process at all — this is why they enter the victim set even though remonstrance is never nominally 'about' them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification under this reading captures exactly the ambiguity the kernel contest exists to resolve: remonstrance genuinely coordinates a legal-quality function (catching edicts inconsistent with prior law) even as, empirically, its exercise has drifted toward defending a narrow fiscal privilege class. Classifying it purely as coordination (rope) would erase the documented asymmetric extraction from non-privileged taxpayers; classifying it purely as extraction (snare) would erase the genuine constitutional-review function magistrates plausibly still perform in non-fiscal cases. Tangled rope holds both facts without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_law_content_indeterminacy,
    'Is there an actual, ascertainable body of ''fundamental law'' that remonstrance protects, or is ''fundamental law'' a retroactively-invoked label that expands to cover whatever the magistracy currently wishes to block?',
    'Comparative analysis of remonstrance texts across the early (16th-century) versus late (18th-century) periods: if the invoked ''fundamental law'' content is consistent and narrow versus increasingly elastic and fiscal-specific, this indicates genuine constitutional doctrine versus rhetorical cover for privilege defense.',
    'If fundamental law is a stable, ascertainable body of doctrine, the magistrate reading''s coordination claim is substantially stronger and the extractiveness score should be revised downward. If it is elastic and expands opportunistically, the tangled_rope classification undersells how close this sits to a pure snare on the fiscal dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_law_content_indeterminacy, conceptual, 'Whether ''fundamental law'' names a real constraint on remonstrance or an infinitely flexible justificatory label.').

omega_variable(
    kernel_authority_locus_ambiguity,
    'Does legitimate authority to determine what counts as a valid constitutional objection reside in the sworn judicial magistracy (this reading) or in the sovereign''s unilateral legislative will as checked only by estates-general consent (crown reading)?',
    'This is the structural disagreement the kernel contest exists to hold open — it is not resolvable by additional fiscal data because it is a question about where sovereignty properly locates, not a question about what happened. Historical resolution came only through revolutionary rupture (abolition of both Parlements and venal office in 1790), which does not adjudicate the prior contest so much as end it by force.',
    'Adopting the crown reading instead would reclassify the same institutional practice with a substantially different beneficiary/victim structure — royal ministers and the wider reforming public would appear as targets of an illegitimate veto rather than the magistracy appearing as a fiscal-privilege beneficiary class within a partially genuine coordination structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_authority_locus_ambiguity, preference, 'Irreducible disagreement over the locus of constitutional authority, which the kernel-reading split exists to preserve rather than resolve.').

omega_variable(
    magistracy_beneficiary_confound,
    'Given that magistrates simultaneously (a) perform a plausible legal-review function and (b) personally hold tax-exempt venal offices, can these two roles be disentangled in the historical record, or does the office-holding structure make impartial remonstrance structurally impossible regardless of individual magistrate intent?',
    'Case-level comparison of remonstrance outcomes on edicts that did NOT touch office-holder privilege versus edicts that did: if delay/blocking rates are similar, personal interest is not driving the pattern; if fiscal-privilege-touching edicts show systematically higher blocking rates, the confound is structural rather than incidental.',
    'A structural confound would mean the coordination function claimed by the magistrate reading cannot in practice be separated from the extraction, strengthening the case for classifying this reading''s constraint closer to snare than tangled_rope on the fiscal dimension specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magistracy_beneficiary_confound, empirical, 'Whether the venal-office structure makes the magistracy''s dual role as reviewer and beneficiary separable even in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__magistrate_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__magistrate_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(remo_tr_t60, remonstrance_authority__magistrate_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(remo_tr_t80, remonstrance_authority__magistrate_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(remo_tr_t100, remonstrance_authority__magistrate_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__magistrate_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__magistrate_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(remo_be_t60, remonstrance_authority__magistrate_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(remo_be_t80, remonstrance_authority__magistrate_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(remo_be_t100, remonstrance_authority__magistrate_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__magistrate_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__magistrate_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(remo_su_t60, remonstrance_authority__magistrate_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(remo_su_t80, remonstrance_authority__magistrate_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(remo_su_t100, remonstrance_authority__magistrate_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% This story and remonstrance_authority__crown_reading are sibling readings of the single remonstrance_authority kernel (the institutional practice of parlementary remonstrance against royal edicts). They are not the same constraint measured two ways: the magistrate reading treats the practice as fundamentally a legal-review coordination mechanism with a fiscal-privilege extraction layer (tangled_rope), while the crown reading treats the same formal practice as fundamentally illegitimate minoritarian obstruction (expected to compute closer to snare). Each carries its own ε, beneficiary/victim structure, and claimed_type, linked here per the kernel/reading protocol rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
