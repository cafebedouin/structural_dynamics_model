% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity as Systematic Impunity Mechanism
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   This story instantiates the accountability_void_reading of the qualified
 *   immunity kernel: the doctrine functions as a systematic extraction
 *   mechanism that guarantees officer and municipal impunity for
 *   constitutional violations by erecting a near-absolute evidentiary bar
 *   (the 'clearly established law' standard) that dismisses claims before the
 *   underlying constitutional question is ever adjudicated. This is a
 *   distinct constraint from the constitutional_fidelity_reading (which
 *   contests the doctrine's judicial legitimacy independent of outcomes) and
 *   the protective_scaffold_reading (which holds the doctrine is necessary
 *   and proportionate protection for good-faith discretionary action). Each
 *   reading has a different ε: this reading's ε is high because it measures
 *   outcome-level impunity for victims with a live, unremedied constitutional
 *   injury; the protective_scaffold reading would measure a much lower ε
 *   because it evaluates the same doctrine as calibrated protection against
 *   frivolous litigation, not extraction from injured parties. The
 *   constitutional_fidelity reading measures a third quantity entirely — the
 *   doctrine's textual/statutory pedigree — which is largely orthogonal to
 *   outcome extraction. These are not the same constraint measured three
 *   ways; they are three constraints sharing one textual label.
 *
 * KEY AGENTS:
 *   - civil_rights_plaintiffs: primary target (powerless/trapped) — bears dismissal without remedy
 *   - excessive_force_victims: primary target (powerless/trapped) — bears physical and financial harm uncompensated
 *   - law_enforcement_officers_facing_liability: primary beneficiary (organized/arbitrage) — shielded from personal liability
 *   - police_unions: organized beneficiary and agenda-influencer (organized/arbitrage) — lobbies to preserve the shield
 *   - federal_judiciary: agenda-setter (institutional/analytical) — created and administers the doctrine's discretionary scope
 *   - civil_rights_bar: excluded (moderate/constrained) — priced out of bringing meritorious claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.79).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity as Systematic Impunity Mechanism").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers_facing_liability).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers_and_budgets).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, excessive_force_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, wrongfully_detained_individuals).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, families_of_decedents_in_police_custody).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, good_faith_immunity_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, clearly_established_law_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Files a Section 1983 suit alleging a constitutional violation by a government officer. Must clear the 'clearly established law' bar — a prior case with nearly identical facts — before the court will even reach whether a constitutional right was violated. No prior identical case exists for most novel fact patterns, so the claim is dismissed before discovery, before any factual record is built, and before liability is ever adjudicated. Has no alternative forum: the doctrine is federal and uniform across circuits in its basic operation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Suffered physical injury from an officer's use of force later conceded or found unreasonable, but the case is dismissed on immunity grounds because no prior case held that particular force, in that particular configuration of facts, unconstitutional. Bears medical costs, lost wages, and psychological harm with no compensatory mechanism, since the officer is shielded and the municipality is often separately insulated by Monell doctrine.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, excessive_force_victims, payer,
    powerless, biographical, trapped, national).

% Detained, searched, or arrested without the constitutionally required basis. Even where a court agrees the detention was unconstitutional, immunity frequently bars damages because the specific unconstitutionality was not 'beyond debate' at the time. Absorbs the harm of the violation with no offsetting recovery and no mechanism to deter recurrence against them specifically.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, wrongfully_detained_individuals, payer,
    powerless, biographical, trapped, national).

% Pursue wrongful death claims after a death in custody or during an encounter with police. Face the same clearly-established-law threshold; where prior cases involve slightly different circumstances (a different weapon, a different position of the body, a different verbal warning), courts routinely find the law was not clearly established and grant immunity, ending the case without a jury ever hearing the facts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, families_of_decedents_in_police_custody, payer,
    powerless, generational, trapped, national).

% Named individually in a civil rights suit for an on-duty action. Qualified immunity operates as a near-categorical shield: the officer need not show the conduct was lawful, only that no prior case put them on clear notice it was unlawful. This produces near-total insulation from personal financial liability regardless of the severity or clarity of the violation, and removes the primary financial incentive to alter conduct.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers_facing_liability, beneficiary,
    organized, biographical, arbitrage, national).

% Municipalities and their insurers bear the financial exposure of policing but benefit from immunity's dismissal-before-discovery effect: fewer cases proceed to the point of establishing municipal liability, fewer settlements are compelled, and budget exposure to civil rights litigation is systematically suppressed relative to a world without the doctrine.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers_and_budgets, beneficiary,
    institutional, generational, arbitrage, national).

% Lobbies extensively to preserve the doctrine, funds litigation defending it, and opposes state and federal reform efforts that would narrow or abolish it. Frames the doctrine publicly as necessary for officer morale and recruitment, while its material effect is removing the personal financial stakes of misconduct for members.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, police_unions, agenda_setter).

% Created and continues to administer the doctrine through case law, including the discretion (since Pearson v. Callahan, 2009) to skip the merits question entirely and rule on 'clearly established law' alone — a discretion that entrenches the shield by preventing new precedent from ever clearly establishing a right, since courts need never reach the merits to grant immunity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Attorneys who would litigate meritorious constitutional claims decline to take cases as a matter of economic reality — the doctrine's dismissal rate makes contingency-fee civil rights litigation against government officers financially unviable for all but the most extreme fact patterns, meaning most violations are never brought at all and never enter the public record.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_bar, excluded,
    moderate, biographical, constrained, national).

% Retain the authority to abolish or narrow qualified immunity by statute (as Colorado and New Mexico have done at the state level) but have largely declined to act federally despite repeated legislative proposals, leaving the judicially created doctrine in place absent congressional correction.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, state_legislatures_and_congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers_facing_liability).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its stated form, the doctrine coordinates officer decision-making under uncertainty by protecting good-faith judgment calls made in fast-moving, ambiguous situations from being second-guessed with the benefit of hindsight and subjected to ruinous personal liability.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations from the individual officers and municipalities who commit or authorize them to the individuals who are shot, beaten, wrongfully detained, or killed — who absorb the physical, financial, and psychological harm with no offsetting recovery mechanism.
% ABSENT_VOICES: Civil rights plaintiffs whose specific fact patterns lack a prior identical precedent are never heard on the merits at all — their claims are dismissed at the threshold, so the courts that shape the doctrine's application rarely hear from the class of victims most harmed by its threshold effect. The civil rights bar that would otherwise bring these cases is structurally priced out by the doctrine's dismissal economics before a complaint is even filed.
% DISAPPEARANCE_RATIONALE: If qualified immunity were abolished tomorrow, Section 1983 suits would proceed to merits and discovery at dramatically higher rates, municipal insurance costs and settlement volumes would rise substantially, officer conduct standards would be tested against ordinary constitutional tort principles instead of a novelty-of-precedent bar, and a body of case law establishing constitutional rights would begin accumulating in a domain where it has been deliberately prevented from accumulating for decades — the entire ecosystem of policing litigation, insurance, and internal discipline would reorganize.
% FOUNDING_PROBLEM: Officials performing discretionary government functions needed protection from being sued into paralysis for good-faith judgment calls made under uncertainty, particularly amid a wave of Reconstruction-era and civil-rights-era litigation against government officials.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the political spectrum (including Justice Sotomayor's dissents, the Cato Institute, and the ACLU jointly on separate occasions) and empirical studies of Section 1983 dismissal rates attest that the doctrine has drifted from protecting good-faith judgment calls to categorically barring recovery even where courts find the underlying conduct plainly unconstitutional, because the 'clearly established' requirement as applied by circuit courts demands near-identical precedent that rarely exists for any given fact pattern. This corroboration comes from academics, dissenting jurists, and cross-ideological advocacy organizations outside the beneficiary set of officers, unions, and municipal budgets.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.88 at interval end) because the doctrine's operative effect — dismissal prior to merits adjudication in the overwhelming majority of contested-fact cases — transfers the cost of proven or plausible constitutional violations from officers and municipalities onto injured individuals with no compensatory mechanism. Suppression is authored high (0.79) because the doctrine actively forecloses the very precedent-generation process that could narrow its own future application: Pearson v. Callahan's discretion to skip the merits question means courts can perpetually decline to clearly establish new rights, structurally preventing the doctrine's own bar from ever lowering. Theater ratio is authored substantial and rising (0.62) because the doctrine's stated justification — protecting good-faith judgment calls under uncertainty — increasingly diverges from its applied function, which shields conduct courts themselves sometimes find plainly unconstitutional; the 'good faith' framing performs a coordination story that the accountability_void reading holds no longer matches the doctrine's actual operation. Accessibility collapse is high (0.81): once a claim fails the clearly-established-law threshold there is no alternative federal remedy path (Bivens has been narrowed nearly to nonexistence for analogous federal-officer claims), and state tort remedies are frequently barred by separate sovereign or official immunity doctrines. Resistance is substantial (0.71), reflecting sustained scholarly, judicial-dissent, and advocacy pressure for reform, including bipartisan legislative proposals and state-level abolition in Colorado and New Mexico.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers facing potential liability are the clearest structural beneficiaries: the doctrine's operation removes their personal financial exposure regardless of the severity of the underlying violation, and their exit options are effectively arbitrage-grade (the shield travels with them across jurisdictions and case types). Municipal insurers and police unions benefit derivatively — the doctrine suppresses settlement volume and litigation costs system-wide. Plaintiffs and their families are the clear targets: trapped in the sense that there is no forum-shopping or alternative remedy path once the federal claim is dismissed, and the injury (physical harm, wrongful death, unlawful detention) is not undone by any other mechanism. The federal judiciary sits as agenda-setter rather than beneficiary or victim — it administers the doctrine but does not collect from its operation, which is why it is listed separately from the beneficiary/victim arrays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting good-faith official discretion from paralyzing litigation exposure — is assessed as dead in this reading: contemporary application routinely bars recovery even where courts explicitly find the underlying conduct unconstitutional, which is a different function than protecting genuine good-faith uncertainty. Corroboration for this obsolescence claim comes from outside the beneficiary set (academic empirical studies of dismissal rates, cross-ideological advocacy groups, and dissenting justices), which is the evidentiary structure the R5 genealogy question requires — a genealogy claim corroborated only by officers, unions, or municipal defendants would be self-serving and would not meet the bar. This reading does not deny that SOME version of good-faith protection could serve a live coordination function (that possibility is precisely what the protective_scaffold_reading investigates as its own constraint); it asserts that the doctrine AS CURRENTLY APPLIED has drifted into an accountability void that exceeds any defensible protective function, and treats that drift as the structurally dominant fact for this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_standard_calibration,
    'Is the ''clearly established law'' threshold, as currently applied by circuit courts, a reasonable calibration of good-faith protection, or has it drifted into a near-absolute bar that forecloses adjudication of most meritorious claims?',
    'Empirical review of Section 1983 dismissal rates and outcomes across circuits, comparing cases dismissed on immunity grounds against the severity/clarity of the alleged underlying violation, and comparing pre- and post-Pearson v. Callahan merits-reaching rates.',
    'If the threshold is found to function as a near-categorical bar disconnected from case severity, this supports the accountability_void reading''s ε as descriptively accurate; if it is found to track genuine good-faith ambiguity proportionately, this would favor the protective_scaffold reading and suggest this story''s ε is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_standard_calibration, empirical, 'Whether the clearly-established-law bar functions as calibrated protection or categorical impunity.').

omega_variable(
    kernel_reading_selection_basis,
    'Given that qualified immunity is a single kernel with at least three defensible readings (accountability_void, constitutional_fidelity, protective_scaffold), what determines which reading a given court, legislature, or commentator adopts, and does that selection track evidence or institutional interest?',
    'Comparative analysis of which institutional actors (judiciary, police unions, civil rights bar, legislatures) advance which reading, and whether reading adoption correlates with structural position (beneficiary vs. victim) rather than with independent evaluation of the doctrine''s empirical operation.',
    'If reading selection tracks structural interest rather than evidence, this would suggest the kernel contest itself is partly an artifact of extraction dynamics rather than a purely epistemic disagreement — relevant to how much weight any single reading''s claimed_type should carry in isolation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether kernel-reading selection across institutional actors tracks structural interest or independent evidence.').

omega_variable(
    bivens_narrowing_interaction,
    'How much of the accountability void measured here is attributable to qualified immunity specifically versus the near-total narrowing of Bivens remedies for federal officers, which operates as a parallel and compounding bar?',
    'Decompose federal civil rights case outcomes by claim type (state officer Section 1983 vs. federal officer Bivens) and isolate the marginal dismissal contribution of qualified immunity from the marginal contribution of Bivens narrowing.',
    'If Bivens narrowing accounts for a large share of the observed accountability gap for federal officers, this constraint''s ε (scoped to qualified immunity specifically) may need adjustment or a sibling constraint story for Bivens narrowing should be authored and linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bivens_narrowing_interaction, empirical, 'Disentangling qualified immunity''s contribution to accountability void from Bivens remedy narrowing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(qual_tr_t0, observed).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(qual_tr_t10, observed).
narrative_ontology:measurement(qual_tr_t20, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(qual_tr_t20, observed).
narrative_ontology:measurement(qual_tr_t30, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(qual_tr_t30, observed).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement_basis(qual_tr_t40, observed).
narrative_ontology:measurement(qual_tr_t50, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(qual_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(qual_be_t0, observed).
narrative_ontology:measurement(qual_be_t10, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(qual_be_t10, observed).
narrative_ontology:measurement(qual_be_t20, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(qual_be_t20, observed).
narrative_ontology:measurement(qual_be_t30, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(qual_be_t30, observed).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement_basis(qual_be_t40, observed).
narrative_ontology:measurement(qual_be_t50, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement_basis(qual_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(qual_su_t0, observed).
narrative_ontology:measurement(qual_su_t10, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(qual_su_t10, observed).
narrative_ontology:measurement(qual_su_t20, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(qual_su_t20, observed).
narrative_ontology:measurement(qual_su_t30, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(qual_su_t30, observed).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement_basis(qual_su_t40, observed).
narrative_ontology:measurement(qual_su_t50, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(qual_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the qualified_immunity_doctrine kernel. accountability_void_reading (this file) measures outcome-level extraction from injured plaintiffs; protective_scaffold_reading measures the same doctrine's calibration as officer protection against frivolous litigation; constitutional_fidelity_reading measures the doctrine's judicial-vs-legislative legitimacy independent of outcomes. All three share the textual label 'qualified immunity' but instantiate structurally distinct claims with distinct ε values, distinct beneficiary/victim structures, and distinct classifications — per the ε-invariance principle, they are authored as three linked files rather than one file with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
