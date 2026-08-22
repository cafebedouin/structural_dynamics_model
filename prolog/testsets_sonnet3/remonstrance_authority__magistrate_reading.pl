% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading): Ancient Liberties Against Arbitrary Fiscal Innovation
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the magistrate reading of the
 *   remonstrance-authority kernel: the claim, made from within the sovereign
 *   courts' own self-understanding, that the right to remonstrate against
 *   royal edicts before registration is a fundamental constitutional
 *   mechanism descending from ancient liberties, checking arbitrary
 *   innovation by the crown. Under this reading the Parlements are the
 *   coordinating, liberty-preserving body; the crown's fiscal edicts are the
 *   potential arbitrary innovation being checked. But the same story that
 *   authors this coordination function must also author, honestly, what the
 *   metrics show: an increasingly extractive arrangement in which the
 *   magistracy's tax-exempt privileges and venal office values are precisely
 *   what the remonstrance power is deployed to protect, especially on fiscal
 *   edicts, and in which unrepresented taxpayers and the third estate bear
 *   the resulting shortfall. This is the SAME kernel the crown_reading also
 *   addresses, but crown_reading is a separate constraint with its own ε and
 *   its own beneficiary/victim structure — the two are not combined or
 *   averaged here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.62).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.45).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading): Ancient Liberties Against Arbitrary Fiscal Innovation").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '38d9699f-5093-4381-9b5f-a9409783095c').
narrative_ontology:cs_kernel_codification('38d9699f-5093-4381-9b5f-a9409783095c', distributed).
narrative_ontology:cs_authority_grounding('38d9699f-5093-4381-9b5f-a9409783095c', lineage).
narrative_ontology:cs_interpretation_layer_present('38d9699f-5093-4381-9b5f-a9409783095c').
narrative_ontology:cs_reading_relation('38d9699f-5093-4381-9b5f-a9409783095c', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('38d9699f-5093-4381-9b5f-a9409783095c', foundational, registration_review_is_constitutional_check).
narrative_ontology:cs_axiom_status(registration_review_is_constitutional_check, holdable).
narrative_ontology:cs_axiom_grounding('38d9699f-5093-4381-9b5f-a9409783095c', registration_review_is_constitutional_check, conventional).
narrative_ontology:cs_axiom('38d9699f-5093-4381-9b5f-a9409783095c', secondary, ancient_liberties_bind_present_sovereign).
narrative_ontology:cs_axiom_status(ancient_liberties_bind_present_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('38d9699f-5093-4381-9b5f-a9409783095c', ancient_liberties_bind_present_sovereign, conventional).
narrative_ontology:cs_reference_frame('38d9699f-5093-4381-9b5f-a9409783095c', ancient_constitution_registration_check).
narrative_ontology:cs_drift_state('38d9699f-5093-4381-9b5f-a9409783095c', eighteenth_century_fiscal_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38d9699f-5093-4381-9b5f-a9409783095c', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlement_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_estates).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, unrepresented_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, third_estate_commoners).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_law_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, ancient_constitution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold venal, heritable offices in the sovereign courts and exercise the right to remonstrate against royal edicts before registering them into law, repeatedly delaying or blocking fiscal reforms including new taxes, forced loans, and attempts to curtail their own tax exemptions. Their offices, purchased and passed through families, are constituted by this power; surrendering it would dissolve their institutional identity, not merely cost them income.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlement_magistrates, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlement_magistrates, beneficiary).

% Needs revenue for war and administration and repeatedly drafts fiscal edicts that the Parlements refuse to register, forcing recourse to lettres de jussion, lits de justice, or exile of magistrates to force registration. Each remonstrance cycle delays revenue by months or years and forces the crown to negotiate away portions of the reform to obtain registration.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_treasury, payer,
    institutional, biographical, constrained, national).

% Bear whatever tax burden survives the negotiated compromise between crown and magistracy, without a seat at either table. When magistrates block reforms that would have broadened the tax base by ending exemptions, the shortfall is recovered through indirect taxes and tailles that fall disproportionately on them.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, unrepresented_taxpayers, payer,
    powerless, biographical, trapped, regional).

% Carry the base of the fiscal burden under a system the Parlements defend as 'ancient constitution' but which preserves exemptions for nobility and office-holders. Have no formal remonstrance channel of their own and experience the magistrates' defense of 'liberties' as a defense of privilege that keeps their own burden fixed and rising.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, third_estate_commoners, payer,
    powerless, generational, trapped, national).

% Purchased offices whose value depends on the fiscal and honorific privileges (including tax exemption) the Parlements' remonstrance power protects. Can sell, bequeath, or leverage their offices as financial assets; their exit option is to convert the office's protected status into capital rather than to leave the arrangement.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, venal_office_holders, beneficiary,
    organized, civilizational, arbitrage, national).

% Regional bodies whose own fiscal privileges and consultative role are reinforced whenever a Parlement successfully asserts that royal edicts require registration and consent, since a precedent limiting crown unilateralism protects their analogous claims.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_estates, beneficiary,
    organized, generational, constrained, regional).

% Design fiscal reforms intended to address structural deficits and would argue, if the magistrate framing were not dominant in the historical record, that remonstrance functions as an unaccountable veto by an unelected, self-perpetuating body rather than as representation of any broader interest — this is the crown_reading, treated here as excluded from this constraint's own frame.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_ministers, excluded,
    institutional, biographical, constrained, national).

% Assess competing claims about whether the Parlements' remonstrance right descended from genuine ancient constitutional constraint or was a constructed doctrine legitimating a venal magistracy's self-interest, drawing on registers, remonstrance texts, and comparative institutional history.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlement_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, textual channel through which a body with specialized legal knowledge can review royal edicts for conformity with prior law and precedent before those edicts bind the realm, creating a check against purely unilateral fiscal or legal innovation.
% TRANSFER_FUNCTION: Moves fiscal burden away from the tax-exempt magistracy, venal office-holding class, and privileged provinces, and onto unrepresented taxpayers and the third estate, by blocking or diluting reforms that would have broadened or equalized the tax base; also moves negotiating leverage from the crown to the magistracy each time registration is contested.
% ABSENT_VOICES: Unrepresented taxpayers and third estate commoners have no remonstrance channel of their own and are not consulted when the Parlements negotiate the terms of registration with the crown; crown ministers who view the arrangement as obstruction rather than representation are excluded from the magistrate reading's own self-account of the right.
% DISAPPEARANCE_RATIONALE: If remonstrance vanished overnight, the crown could register fiscal edicts unilaterally, the magistracy's leverage over reform negotiations would collapse, venal offices would lose much of their protective value, and provincial estates would lose a precedent supporting their own consultative claims — the entire negotiated fiscal settlement between crown and privileged orders would have to be renegotiated from a different baseline.
% FOUNDING_PROBLEM: Medieval and early modern monarchs needed a mechanism to prevent purely arbitrary royal lawmaking; requiring sovereign courts to verify and register edicts against existing law provided a check rooted in judicial review of legal conformity rather than direct political representation.
% FOUNDING_PROBLEM_CORROBORATION: The magistrates themselves attest the founding problem — arbitrary royal innovation — remains live into the eighteenth century, citing continuity of legal doctrine since the medieval councils. Independent constitutional historians and contemporaneous crown ministers attest that by the eighteenth century the right had shifted from checking arbitrary law to defending the fiscal privileges of a self-perpetuating venal office-holding class, a reading corroborated by the pattern of remonstrances concentrated overwhelmingly on tax and exemption edicts rather than on procedural or rights violations.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises across the interval (0.30 to 0.62) because remonstrance is disproportionately deployed against fiscal reform edicts specifically, and each successful block preserves exemptions for a narrowing, increasingly self-interested magistracy while the tax base that could have absorbed reform shifts onto those without a remonstrance channel. Theater ratio is moderate and rising (0.12 to 0.30): genuine legal-conformity review persists alongside a growing share of remonstrance activity that is procedurally elaborate but substantively defends fixed privilege. Suppression is moderate (0.45 at end) — this is not primarily a coercive constraint against the magistrates themselves; the suppression that exists falls on the crown's ability to legislate unilaterally and, downstream, on the excluded taxpayers who have no analogous channel.
 *
 * PERSPECTIVAL GAP:
 *   From the magistrates' own seat (the seat this story authors from), remonstrance is a constitutional safeguard whose steady erosion would itself be the arbitrary innovation. From the crown treasury's seat the same mechanism is a recurring, costly obstruction to needed reform. From the unrepresented taxpayers' seat it is neither — it is simply a negotiation between two privileged parties whose outcome they bear without having been consulted. The engine should compute different per-seat types from these structural positions even though only one claimed_type is authored for the constraint as a whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Parlement magistrates and venal office-holders sit near the beneficiary end: their offices' value and their exemptions are the thing the right protects, and their exit options (identity-locked for magistrates whose institutional identity IS the office; arbitrage for office-holders who can sell or leverage the office as an asset) both keep them structurally on the collecting side. The crown treasury is a payer at the institutional level — constrained but not powerless, since it retains countervailing tools (lits de justice, exile) even though each use is costly. Unrepresented taxpayers and third estate commoners are the clearest targets: powerless, trapped, bearing the residual fiscal burden with no remonstrance channel of their own — the coordination the magistrates provide for themselves is not extended to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing captures the mandatrophy risk directly: the magistrates attest the original problem (arbitrary royal lawmaking) remains live, while historians and crown ministers attest the problem has substantially receded into ordinary fiscal negotiation and the arrangement now persists chiefly to protect the magistracy's own privileged position. This is exactly the mismatch pattern the R5 interview is built to surface: founding_problem_status is contested, and the disappearance_verdict (world_rearranges) shows real dependent arrangements exist — which is consistent with either an authentic living constitutional check OR a captured privilege structure that has become load-bearing precisely because so much fiscal and status architecture now depends on it. Classifying this as tangled_rope rather than snare or mountain keeps both possibilities open at the structural level: there IS a genuine coordination function (review of edicts against prior law) but it now runs alongside asymmetric extraction (protection of a narrowing beneficiary class), and it requires active enforcement (the whole apparatus of lits de justice and exile exists because compliance is not voluntary from the crown's side, and privilege-protection is not voluntary from the taxpayers' side).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_constitution_vs_constructed_privilege,
    'Is the remonstrance right a genuine surviving fragment of an ancient constitutional check on royal power, or a doctrine substantially constructed and elaborated by a venal magistracy to legitimate its own fiscal privileges?',
    'Comparative textual analysis of remonstrance content across centuries: if the proportion of remonstrances addressing procedural/rights violations versus fiscal exemption issues shifted markedly toward fiscal self-interest over time, that supports the constructed-privilege reading; continuity of substantive concerns supports the ancient-constitution reading.',
    'If constructed, this reading''s claimed coordination function is substantially cover, and the constraint drifts from tangled_rope toward snare; if genuinely ancient and substantively continuous, the coordination function is real and the tangled_rope classification (genuine check + captured privilege riding alongside it) is the more defensible middle position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_constitution_vs_constructed_privilege, conceptual, 'Whether remonstrance is authentic ancient constitutional constraint or constructed doctrine legitimating magistrate privilege.').

omega_variable(
    kernel_reading_divergence_crown_magistrate,
    'This constraint is the magistrate_reading of the remonstrance_authority kernel; the crown_reading of the same kernel treats the identical historical procedure as an illegitimate minoritarian veto. Where exactly does the disagreement between the two readings locate itself structurally?',
    'The disagreement is not about what happened (both readings share the same registers and remonstrance texts) but about (a) whether the magistracy''s authority to review edicts descends from genuine representative or constitutional function versus purchased office, and (b) whether blocking fiscal edicts constitutes preserving liberty or obstructing legitimate sovereign reform. Resolving this requires a normative judgment about the legitimacy of venal officeholding as a basis for constitutional authority, not further historical fact-finding alone.',
    'Under the magistrate reading (this story), the Parlements are a beneficiary class with a genuine but partially captured coordination function (tangled_rope). Under the crown reading, the same historical procedure would likely author lower coordination legitimacy and a beneficiary set essentially identical to the victim set the magistrate reading treats as protected, pushing that sibling constraint toward snare. The two readings do not average; each is authored independently per the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence_crown_magistrate, conceptual, 'Locates the structural disagreement between the magistrate and crown readings of the shared remonstrance_authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__magistrate_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__magistrate_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(remo_tr_t60, remonstrance_authority__magistrate_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(remo_tr_t80, remonstrance_authority__magistrate_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(remo_tr_t100, remonstrance_authority__magistrate_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__magistrate_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__magistrate_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(remo_be_t60, remonstrance_authority__magistrate_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(remo_be_t80, remonstrance_authority__magistrate_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(remo_be_t100, remonstrance_authority__magistrate_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__magistrate_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__magistrate_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(remo_su_t60, remonstrance_authority__magistrate_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(remo_su_t80, remonstrance_authority__magistrate_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement(remo_su_t100, remonstrance_authority__magistrate_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, venal_office_market).

% DUAL FORMULATION NOTE:
% This constraint and remonstrance_authority__crown_reading are two readings of the single remonstrance_authority kernel, decomposed per the ε-invariance principle rather than averaged. This story (magistrate_reading) authors ε=0.62 from the magistracy's own self-understanding of its constitutional function, with the Parlements and venal office-holders as beneficiaries and the crown treasury plus unrepresented taxpayers as victims. The sibling crown_reading would author its own ε and its own beneficiary/victim structure from the crown's perspective on the same underlying historical procedure — very likely with a different, probably higher, ε for the same fiscal edicts, and with the Parlements appearing on the victim/target side of that story's coordination claim rather than the beneficiary side. Both stories should list each other in affects_constraints to preserve the kernel linkage for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
