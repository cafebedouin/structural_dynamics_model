% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   end_of_life_decision_authority; this file generates the sanctity_reading
 *   only: the arrangement in which human life is held to possess value
 *   independent of individual will, and intentional life-ending is prohibited
 *   by criminal law and professional code. The standing arrangement under
 *   contest is the prohibition regime itself, and epsilon below is authored
 *   for THAT arrangement as the sanctity reading assesses it, never for the
 *   authorization frameworks the sibling readings would install. The claim
 *   and the metrics are independent authored facts: the claim (tangled_rope)
 *   states what I believe is structurally true, and the metrics state what I
 *   believe is descriptively true of the regime's operation; where adherents
 *   would claim timeless moral law, the structural record
 *   (jurisdiction-by-jurisdiction variation, enforcement dependence,
 *   organized resistance) is authored honestly and the engine measures any
 *   divergence. Expected structural delta honored: under this reading the
 *   pressured-vulnerable sit in the PROTECTED set rather than the victim set,
 *   the physician role is healer-only, and the cost of individual suffering
 *   is externalized onto the suffering individuals themselves.
 *
 * KEY AGENTS:
 *   - - sanctity_doctrine_institutions: Primary beneficiary (institutional/identity_locked) - doctrine codified into public law, supplies the moral vocabulary and the lobbying muscle
 *   - - vulnerable_elderly_and_disabled_adults: Protected beneficiary (organized/constrained) - the class the rule shields from ambient pressure; its organizations actively defend the rule
 *   - - palliative_care_establishment: Secondary beneficiary (institutional/constrained) - receives patient flow and funding priority anchored on the prohibition
 *   - - competent_terminal_patients_denied_exit: Primary target (powerless/trapped) - bears the override of their end-of-life judgment and the externalized suffering
 *   - - compassionate_family_assistants: Secondary target (moderate/constrained) - bear prosecution risk for assistance, driving some deaths to occur alone
 *   - - physicians_in_ban_jurisdictions: Dual-positioned (institutional/identity_locked) - role boundary protected, conscience and therapeutic options burdened
 *   - - legislatures_and_appellate_courts: Agenda-setter (institutional/mobile) - administers the rule and could revise it by ordinary process
 *   - - autonomy_advocacy_networks: Excluded challenger (organized/constrained) - kept off the operative agenda in most prohibiting jurisdictions
 *   - - bioethics_commissions: Analytical observer (institutional/analytical) - convenes evidence, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.46).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.66).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '95512932-ceac-4487-aca4-874643e5b666').
narrative_ontology:cs_kernel_codification('95512932-ceac-4487-aca4-874643e5b666', formalized).
narrative_ontology:cs_authority_grounding('95512932-ceac-4487-aca4-874643e5b666', lineage).
narrative_ontology:cs_interpretation_layer_present('95512932-ceac-4487-aca4-874643e5b666').
narrative_ontology:cs_reading_relation('95512932-ceac-4487-aca4-874643e5b666', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('95512932-ceac-4487-aca4-874643e5b666', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('95512932-ceac-4487-aca4-874643e5b666', foundational, life_value_will_independent).
narrative_ontology:cs_axiom_status(life_value_will_independent, holdable).
narrative_ontology:cs_axiom_grounding('95512932-ceac-4487-aca4-874643e5b666', life_value_will_independent, deontological).
narrative_ontology:cs_axiom('95512932-ceac-4487-aca4-874643e5b666', foundational, intentional_life_ending_categorical_violation).
narrative_ontology:cs_axiom_status(intentional_life_ending_categorical_violation, holdable).
narrative_ontology:cs_axiom_grounding('95512932-ceac-4487-aca4-874643e5b666', intentional_life_ending_categorical_violation, deontological).
narrative_ontology:cs_axiom('95512932-ceac-4487-aca4-874643e5b666', secondary, healer_role_excludes_killing).
narrative_ontology:cs_axiom_status(healer_role_excludes_killing, holdable).
narrative_ontology:cs_axiom_grounding('95512932-ceac-4487-aca4-874643e5b666', healer_role_excludes_killing, conventional).
narrative_ontology:cs_reference_frame('95512932-ceac-4487-aca4-874643e5b666', hippocratic_absolute_prohibition).
narrative_ontology:cs_drift_state('95512932-ceac-4487-aca4-874643e5b666', contemporary_maid_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('95512932-ceac-4487-aca4-874643e5b666', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_elderly_and_disabled_adults).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, competent_terminal_patients_denied_exit).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, compassionate_family_assistants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, physicians_in_ban_jurisdictions).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, physicians_in_ban_jurisdictions).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, healer_role_absolute_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Churches, denominational bioethics centers, and allied moral institutions teach that human life is inviolable and may not be intentionally ended. The criminal prohibition enacts their teaching into public law; they supply witnesses, lobbying, and much of the moral vocabulary that legislative debate draws on. Their institutional authority is bound up with the rule's retention, and abandoning it would require revising doctrines they regard as constitutive of who they are.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Older and disabled people who depend on others for daily care. Their organizations argue that a legal route to death changes the ambient message sent to people whose care is expensive or burdensome: that their death is an option others may quietly prefer. They testify, litigate, and campaign for the prohibition's retention and report feeling safer under it; individually they remain dependent on caregivers and on public attitudes they do not control.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_elderly_and_disabled_adults, beneficiary,
    organized, biographical, constrained, global).

% Hospice and palliative medicine grew on the premise that suffering can be met with care rather than death. The prohibition channels terminally ill patients into their services and anchors their funding arguments. The specialty also carries costs: some patients refuse palliative routes outright, and clinicians absorb the moral weight of symptoms they cannot fully relieve.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment, beneficiary,
    institutional, generational, constrained, national).

% Adults with grievous, irremediable conditions who retain decision-making capacity and judge that their remaining life holds more suffering than meaning for them. The law overrides that judgment wherever they live under the prohibition. Their lawful options are continued endurance or refusal of treatment where permitted; the unlawful options carry consequences they will not impose on others, so some travel abroad at great cost, and some end their lives alone and earlier than a regulated route would allow, precisely to protect relatives from investigation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, competent_terminal_patients_denied_exit, payer,
    powerless, biographical, trapped, national).

% Spouses, parents, and friends who help a dying relative act on a settled wish: researching options, accompanying travel, or in rare cases assisting directly. Assistance short of travel exposes them to prosecution for aiding suicide, and several have faced trial after acts of mercy. Most respond by distancing themselves from the act, which is one reason some patients die alone.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, compassionate_family_assistants, payer,
    moderate, biographical, constrained, national).

% Licensed doctors practice under a professional norm that they heal and do not kill. The prohibition protects that boundary and the trust it purchases with vulnerable patients entering care. The same norm binds them when a patient begs for release: they must refuse, watch the suffering continue, and sometimes lose the patient to an unregulated death. Professional identity and the rule are fused; a doctor who crossed the line would lose the license that constitutes the career.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians_in_ban_jurisdictions, beneficiary,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, physicians_in_ban_jurisdictions, payer).

% Parliaments and highest courts in prohibiting jurisdictions set and revise the rule. They face recurring private-member bills and constitutional challenges; several have declined to schedule authorization bills for debate, and a number of courts have upheld the prohibition while urging legislatures to reconsider. They can change the rule by ordinary process, and some peer jurisdictions already have.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, legislatures_and_appellate_courts, agenda_setter,
    institutional, generational, mobile, national).

% Right-to-die organizations and allied campaigners press for competent adults to hold the end-of-life authority this arrangement assigns to the collective. In most prohibiting jurisdictions their bills never reach a hearing and their framing is kept outside the operative settlement; their access runs through occasional court challenges and through jurisdictions that have already changed course. Where authorization passes, the population they serve leaves this rule's scope entirely.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% State-appointed and academic bioethics bodies convene evidence, hear the affected seats, and publish recommendations on end-of-life law. Several have recommended authorizing frameworks with safeguards; legislatures have often shelved the reports. They hold no enforcement power and collect nothing from the rule's operation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: keeping decision-making around death free of coercive pressure on people whose care is costly, and maintaining a bright-line professional norm that healers do not kill, which vulnerable patients rely on when they enter care. Stated without evaluation of whether the price charged to competent sufferers is worth the protection bought.
% TRANSFER_FUNCTION: Moves end-of-life decision authority from competent suffering individuals to the collective moral and legal order; moves the residual burden of end-of-life suffering back onto the individuals themselves and onto relatives who risk prosecution if they help; moves enforcement discretion over compassionate assistance to prosecutors and courts.
% ABSENT_VOICES: Competent dying patients themselves are rarely seated at legislative hearings in prohibiting jurisdictions; committees hear clergy, disability organizations, and palliative specialists far more often than the people the rule governs most directly. Secular citizens who reject the doctrinal basis live under its codification without a dedicated voice. Where they are: outside the hearing rooms, with access running through occasional court challenges and the rare private member's bill.
% DISAPPEARANCE_RATIONALE: Medical practice, prosecutorial priorities, and end-of-life politics in prohibiting jurisdictions are organized around the prohibition. Removing it overnight would force immediate choices about who may assist dying, on what conditions, and with what oversight; the vacuum would fill with improvised practice or emergency legislation, and the protective anxieties the rule answers would demand new institutional answers rather than evaporating.
% FOUNDING_PROBLEM: The arrangement was built to prevent the strong from ending the lives of the weak: a history of coerced mercy killings, eugenic programs, and the wartime record of state killing of the disabled made an absolute professional and legal bar on intentional life-ending the load-bearing wall of medical trust.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights scholarship and post-war medical historiography, sources outside the doctrinal beneficiary set, corroborate that the coercive-killing problem was real and remains a live worry; the same sources dispute whether an absolute prohibition remains the right instrument now that checkpointed authorization exists in peer jurisdictions. Denominational bodies attest the problem as fully live; autonomy-side scholars attest it as substantially answered by safeguards elsewhere. No seat attests it as simply dead.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).
:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the prohibition's costs concentrate on a small, identifiable class (competent sufferers denied exit, relatives facing prosecution) while its returns are diffuse protection; authored from the reading's own lights but not its apologetic register, since even the reading's own literature concedes the hard-case cost even while denying it is wrongful taking. Suppression 0.66 is a raw structural property, unscaled by power or scope: criminal liability for assistance, professional discipline, and prosecution of family members are the rule's load-bearing machinery. Theater 0.37: the active/passive line-drawing (withdrawal of treatment permitted, lethal means barred) is increasingly maintained rhetorically as practice converges across the line, but the enforcement apparatus itself remains functional. Accessibility_collapse 0.35: alternatives persist and are known (jurisdictions with authorized frameworks, Dignitas-style travel, treatment refusal), so understanding the rule does not close the option space. Resistance 0.68: sustained constitutional litigation, repeated legislative campaigns, and physician civil disobedience. The measurement series run on one shared eight-point grid (1960-2025) so every tracked metric is authored at every examined time point; the rising suppression_requirement series models the documented dynamic in which eroding consensus forced FORMAL enforcement to intensify (prosecutions, disciplinary actions, defensive litigation) even as the rule's territorial coverage shrank.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. From the doctrine institutions' position the rule is sacred obligation, and extraction is invisible by stipulation: protection all the way down. From the denied patient's position the same rule is a total override of their final act, enforced by threat against anyone who would help. Physicians straddle: their professional identity is constituted by the boundary the rule draws, and the same boundary costs them patients they cannot relieve. Legislatures experience a manageable controversy they can defer. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: vulnerable classes and doctrine institutions sit near the subsidized end, amplified for doctrine institutions by identity_locked exit (their identity is fused with the rule). Victims derive high directionality: denied patients combine powerless power with trapped exit, sitting nearest the full-target end; family assistants add constrained exit at moderate power. Physicians are declared beneficiary with identity_locked exit, keeping them toward the beneficiary end, with the secondary payer role recording the conscience burden the derivation would otherwise miss. The agenda-setter sits near symmetric-administrative: it bears political cost and collects legitimacy. No directionality_overrides are used: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate (never again the coerced killing of the weak) has not died: coercive-pressure worries remain empirically live, which is why this is authored contested rather than dead, and why the rule is not a resolved mandatrophy. But the mandate's SELF-ENFORCING era has ended: what was once near-consensus now requires active enforcement against organized resistance, and the reading's adherents increasingly maintain it theatrically at the edges (the active/passive line) while practice converges beneath it. Classification prevents mislabeling in both directions: calling the rule pure coordination ignores the identifiable victims who bear its concentrated costs; calling it pure extraction ignores the genuine protective function that disability advocates, outside any doctrinal beneficiary set, defend on their own behalf. The tangled_rope claim keeps both faces visible. Note also why the receipt surface does not make this a piton: fixing is prohibitively costly for the agenda-setter relative to benefit, and no seat captures the extracted value itself, but the rule is ACTIVELY DEFENDED by concentrated doctrinal beneficiaries rather than persisting by neglect, which disqualifies the inertial reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which reading of the end_of_life_decision_authority kernel should govern a jurisdiction: this sanctity reading, the autonomy sibling, or the vulnerability-protection sibling?',
    'Adoption decisions by legislatures and appellate courts; not resolvable from inside this story. Each sibling is authored as its own constraint with its own epsilon, beneficiaries, and victims.',
    'Autonomy adoption moves the pressured-vulnerable into the victim set and dissolves the healer-only role; vulnerability-protection adoption replaces the absolute bar with checkpoints and shrinks the victim set to checkpoint-failure cases; sanctity retention preserves the victim set authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    intrinsic_value_metaphysics,
    'Is the will-independent value of human life a discovered moral fact or a constructed commitment?',
    'Cross-tradition convergence analysis and metaethical inquiry: convergence among independent traditions would support discovery; persistent divergence tracked to social position would support construction.',
    'If discovered, the rule approaches natural-law status and resistance reads as error; if constructed, its persistence turns entirely on enforcement and coalition maintenance, and the extractive accounting governs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_metaphysics, conceptual, 'Whether the reading''s foundational premise is bedrock or construction.').

omega_variable(
    coercive_pressure_prevalence,
    'How prevalent is actual coercive pressure on elderly and disabled people in authorization regimes compared with prohibiting regimes?',
    'Comparative longitudinal data across Benelux, Canada, and holdout jurisdictions: non-voluntary and involuntary ending rates, elder-abuse reporting, safeguard-failure audits.',
    'Low prevalence in authorization regimes would undermine the rule''s protective coordination function and push the classification toward extraction-dominant; high prevalence confirms the coordination function is genuine and the tangled structure is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_pressure_prevalence, empirical, 'Empirical ground truth of the protective claim that anchors the rule''s coordination side.').

omega_variable(
    externalized_suffering_scale,
    'How much additional suffering does the prohibition impose through denied exits, solitary unregulated deaths, and unsafe means?',
    'Epidemiology of solo and unregulated deaths among terminal patients in prohibiting jurisdictions; assisted-dying travel statistics; coroner findings on mercy killings prosecuted.',
    'Large scale raises the effective burden on the payer seats and strengthens the asymmetric-cost reading of the rule; negligible scale supports the protection-first reading and lowers effective extraction for the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_suffering_scale, empirical, 'Magnitude of the externalized cost the structural delta assigns to suffering individuals.').

omega_variable(
    act_omission_line_theater,
    'Does the active/passive line (treatment withdrawal permitted, lethal means barred) track a stable moral distinction or performative maintenance?',
    'Physician moral-psychology studies and outcome-equivalence data comparing withdrawal of life-sustaining treatment with requested ending at matched prognoses.',
    'If the line is mostly performative, the rule''s ceremonial share rises and its maintenance drifts toward inertia; if robust, the line is functional moral architecture and the theater ratio is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(act_omission_line_theater, conceptual, 'Status of the rule''s most conspicuous line-drawing apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t1960, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement_basis(eol_sanctity_tr_t1960, observed).
narrative_ontology:measurement(eol_sanctity_tr_t1975, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement_basis(eol_sanctity_tr_t1975, observed).
narrative_ontology:measurement(eol_sanctity_tr_t1990, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(eol_sanctity_tr_t1990, observed).
narrative_ontology:measurement(eol_sanctity_tr_t1997, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement_basis(eol_sanctity_tr_t1997, observed).
narrative_ontology:measurement(eol_sanctity_tr_t2002, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2002, 0.26).
narrative_ontology:measurement_basis(eol_sanctity_tr_t2002, observed).
narrative_ontology:measurement(eol_sanctity_tr_t2015, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement_basis(eol_sanctity_tr_t2015, observed).
narrative_ontology:measurement(eol_sanctity_tr_t2021, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2021, 0.34).
narrative_ontology:measurement_basis(eol_sanctity_tr_t2021, observed).
narrative_ontology:measurement(eol_sanctity_tr_t2025, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2025, 0.37).
narrative_ontology:measurement_basis(eol_sanctity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t1960, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement_basis(eol_sanctity_be_t1960, observed).
narrative_ontology:measurement(eol_sanctity_be_t1975, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1975, 0.24).
narrative_ontology:measurement_basis(eol_sanctity_be_t1975, observed).
narrative_ontology:measurement(eol_sanctity_be_t1990, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1990, 0.29).
narrative_ontology:measurement_basis(eol_sanctity_be_t1990, observed).
narrative_ontology:measurement(eol_sanctity_be_t1997, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1997, 0.32).
narrative_ontology:measurement_basis(eol_sanctity_be_t1997, observed).
narrative_ontology:measurement(eol_sanctity_be_t2002, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2002, 0.35).
narrative_ontology:measurement_basis(eol_sanctity_be_t2002, observed).
narrative_ontology:measurement(eol_sanctity_be_t2015, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(eol_sanctity_be_t2015, observed).
narrative_ontology:measurement(eol_sanctity_be_t2021, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2021, 0.43).
narrative_ontology:measurement_basis(eol_sanctity_be_t2021, observed).
narrative_ontology:measurement(eol_sanctity_be_t2025, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2025, 0.46).
narrative_ontology:measurement_basis(eol_sanctity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t1960, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement_basis(eol_sanctity_su_t1960, observed).
narrative_ontology:measurement(eol_sanctity_su_t1975, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement_basis(eol_sanctity_su_t1975, observed).
narrative_ontology:measurement(eol_sanctity_su_t1990, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement_basis(eol_sanctity_su_t1990, observed).
narrative_ontology:measurement(eol_sanctity_su_t1997, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1997, 0.38).
narrative_ontology:measurement_basis(eol_sanctity_su_t1997, observed).
narrative_ontology:measurement(eol_sanctity_su_t2002, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement_basis(eol_sanctity_su_t2002, observed).
narrative_ontology:measurement(eol_sanctity_su_t2015, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(eol_sanctity_su_t2015, observed).
narrative_ontology:measurement(eol_sanctity_su_t2021, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement_basis(eol_sanctity_su_t2021, observed).
narrative_ontology:measurement(eol_sanctity_su_t2025, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(eol_sanctity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'end-of-life decision authority' decomposes into three structurally distinct constraints (sanctity, autonomy, vulnerability-protection readings) with different epsilons, victim sets, and enforcement profiles over the shared referent of standing end-of-life arrangements. The sanctity reading is historically upstream: its absolute bar shaped the checkpoint design the vulnerability-protection reading later proposed, and its doctrine supplies the vocabulary every sibling must accept or reject. Each family file links the other two via affects_constraints; epsilon is invariant within each file because each instantiates exactly one reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
