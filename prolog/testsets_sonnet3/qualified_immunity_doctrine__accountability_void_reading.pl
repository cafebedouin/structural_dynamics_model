% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity as Systematic Impunity Extraction Mechanism
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   This story instantiates the accountability_void_reading of the qualified
 *   immunity kernel: the doctrine as it has operated since Harlow v.
 *   Fitzgerald (1982) reformulated qualified immunity around the 'clearly
 *   established law' standard. Under this reading, the doctrine is not a
 *   modest good-faith shield but a systematic extraction mechanism that
 *   transfers the cost of constitutional violations from officers and
 *   municipalities to victims, while insulating the actors who commit the
 *   violations from both financial liability and, frequently, any formal
 *   judicial finding of wrongdoing at all. The doctrine's 'clearly
 *   established law' requirement has hardened over four decades into what
 *   critics document as a near-absolute bar: because courts may dismiss on
 *   immunity grounds without reaching the merits, and because 'clearly
 *   established' requires closely analogous prior precedent, the doctrine
 *   reproduces its own scarcity of precedent — a self-reinforcing extraction
 *   loop. This is one of three readings of the qualified immunity kernel; it
 *   shares the kernel with constitutional_fidelity_reading (which argues the
 *   doctrine is illegitimate on separation-of-powers/textualist grounds
 *   regardless of policy outcome) and protective_scaffold_reading (which
 *   holds the doctrine is a necessary shield enabling effective policing).
 *   Each reading authors its own ε, beneficiary/victim structure, and
 *   classification; they are linked as siblings under the shared kernel, not
 *   merged into one story.
 *
 * KEY AGENTS:
 *   - civil_rights_plaintiffs: powerless/trapped — bears the constitutional injury and the near-impossibility of judicial remedy
 *   - law_enforcement_officers: organized/arbitrage — shielded from personal liability regardless of severity of violation absent closely analogous precedent
 *   - police_unions: organized/arbitrage — actively lobbies to preserve and expand the doctrine
 *   - federal_and_state_appellate_courts: institutional/analytical — administers the standard and self-reinforces the precedent scarcity that sustains it
 *   - municipal_insurers_and_budgets: institutional/arbitrage — captures the fiscal benefit of suppressed liability exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.79).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity as Systematic Impunity Extraction Mechanism").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '74e95ab6-2b1b-492d-8edd-1d5d014068bb').
narrative_ontology:cs_kernel_codification('74e95ab6-2b1b-492d-8edd-1d5d014068bb', formalized).
narrative_ontology:cs_authority_grounding('74e95ab6-2b1b-492d-8edd-1d5d014068bb', lineage).
narrative_ontology:cs_interpretation_layer_present('74e95ab6-2b1b-492d-8edd-1d5d014068bb').
narrative_ontology:cs_reading_relation('74e95ab6-2b1b-492d-8edd-1d5d014068bb', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('74e95ab6-2b1b-492d-8edd-1d5d014068bb', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('74e95ab6-2b1b-492d-8edd-1d5d014068bb', foundational, remedy_access_is_constitutive_of_the_right).
narrative_ontology:cs_axiom_status(remedy_access_is_constitutive_of_the_right, holdable).
narrative_ontology:cs_axiom_grounding('74e95ab6-2b1b-492d-8edd-1d5d014068bb', remedy_access_is_constitutive_of_the_right, deontological).
narrative_ontology:cs_axiom('74e95ab6-2b1b-492d-8edd-1d5d014068bb', foundational, clearly_established_standard_functions_as_de_facto_absolute_bar).
narrative_ontology:cs_axiom_status(clearly_established_standard_functions_as_de_facto_absolute_bar, holdable).
narrative_ontology:cs_axiom_grounding('74e95ab6-2b1b-492d-8edd-1d5d014068bb', clearly_established_standard_functions_as_de_facto_absolute_bar, empirically_contingent).
narrative_ontology:cs_reference_frame('74e95ab6-2b1b-492d-8edd-1d5d014068bb', harlow_good_faith_shield_standard).
narrative_ontology:cs_drift_state('74e95ab6-2b1b-492d-8edd-1d5d014068bb', contemporary_post_2020_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('74e95ab6-2b1b-492d-8edd-1d5d014068bb', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers_and_budgets).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, excessive_force_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, wrongfully_detained_individuals).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, families_of_decedents_in_police_encounters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Files a Section 1983 claim after suffering a constitutional violation at the hands of a government actor. Must clear the 'clearly established law' bar — a prior case with materially identical facts — before ever reaching the merits of whether their rights were violated. Bears the cost of the constitutional injury and the litigation itself, often unable to find counsel because the doctrine makes recovery so uncertain. Has no alternative forum; the federal claim is the only vehicle for this kind of relief.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Suffers physical injury from an officer's use of force. Even where a court agrees the force was excessive, the case is routinely dismissed because no prior published decision addressed the precise combination of facts presented. Cannot choose which court decides, cannot select the officer who responds, cannot negotiate around the doctrine — it applies regardless of the severity of the violation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, excessive_force_victims, payer,
    powerless, biographical, trapped, national).

% Detained, searched, or arrested without lawful basis. Seeking damages requires proving both the violation and its clear prior establishment; the doctrine's granularity requirement (facts must match closely, not just legal principle) means novel or first-instance violations are functionally unwinnable regardless of merit.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, wrongfully_detained_individuals, payer,
    powerless, biographical, trapped, national).

% Pursues wrongful-death claims after a fatal encounter. Faces the same clearly-established-law bar in cases with the highest stakes; the decedent cannot testify, and the doctrine's fact-specificity requirement makes precedent from even closely analogous fatal encounters insufficient if the details differ. Absorbs the loss with no path to accountability or systemic change through the courts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, families_of_decedents_in_police_encounters, payer,
    powerless, generational, trapped, national).

% Named individually in a civil rights suit but shielded from personal liability by the doctrine in the overwhelming majority of cases, regardless of the severity of the underlying conduct, as long as no sufficiently similar precedent existed beforehand. Faces no personal financial exposure even when a court finds a constitutional violation occurred, because the two questions (violation vs. clearly established) are decided separately and the second nearly always controls the outcome.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, arbitrage, national).

% Lobbies aggressively to preserve and expand the doctrine, funds amicus briefs defending it before appellate courts, and opposes state and federal legislation that would narrow or eliminate it. Benefits from the doctrine's chilling effect on litigation, which reduces both individual member exposure and pressure for departmental reform.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, police_unions, agenda_setter).

% Municipalities and their insurers avoid the bulk of potential settlement and judgment costs because the doctrine screens out most claims before liability is ever assessed. This suppresses the fiscal signal that might otherwise drive departmental policy reform, training investment, or personnel accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers_and_budgets, beneficiary,
    institutional, generational, arbitrage, regional).

% Adjudicates the 'clearly established law' standard case by case, deciding whether prior precedent was specific enough to have put the officer on notice. Courts frequently resolve cases on immunity grounds alone, declining to reach the constitutional merits — which itself prevents new precedent from ever becoming 'clearly established,' perpetuating the doctrine's bar in future cases.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_and_state_appellate_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Attorneys and advocacy organizations that would litigate more civil rights claims absent the doctrine's chilling effect on case viability and fee recovery. Their voice is present in briefing and legislative testimony but has no vote in the doctrine's judicial maintenance or elimination — that authority rests entirely with courts that benefit from docket reduction the doctrine provides.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_bar_and_legal_aid_organizations, excluded,
    moderate, generational, constrained, national).

% Some states have enacted statutory alternatives to qualified immunity for state-law claims, but these do not reach federal Section 1983 claims, which remain the primary vehicle for constitutional violations. Reform advocates are structurally excluded from altering the federal doctrine itself, which is judicially created and can only be undone by the Supreme Court or federal legislation neither has yet delivered.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, state_legislatures_reform_advocates, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, diffuse).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its stated form, the doctrine is meant to solve a genuine problem: without some liability shield, officers might hesitate in split-second, high-stakes situations for fear of personal financial ruin over legally ambiguous conduct. This reading does not deny that a coordination problem could exist in principle.
% TRANSFER_FUNCTION: The doctrine moves the cost of constitutional violations from the officers and municipalities that commit them to the individuals who suffer them. Plaintiffs absorb physical injury, wrongful detention, and death without compensation or acknowledgment of wrongdoing; officers and their employing institutions are relieved of both financial liability and, because merits are frequently unreached, of any formal finding that a violation occurred at all.
% ABSENT_VOICES: Civil rights plaintiffs' bar and legal aid organizations are present in briefing but have no authority over the doctrine's maintenance. The individuals actually injured — dead, injured, or wrongfully detained — are, in the fatal cases, permanently unable to testify to their own encounter, and in all cases lack any institutional lever to alter the standard that dismissed their claim.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, Section 1983 litigation would proceed to the merits far more often; officers and municipalities would face meaningfully increased financial exposure for constitutional violations; insurance costs and settlement practices would shift; and departments would likely see altered incentives toward training, use-of-force policy, and internal discipline. The volume of case law establishing what conduct violates the Constitution would also grow rapidly, since courts currently often skip that question.
% FOUNDING_PROBLEM: The doctrine originated to protect good-faith government officials from personal liability for reasonable, good-faith actions in performing discretionary duties, particularly where the law was genuinely unsettled.
% FOUNDING_PROBLEM_CORROBORATION: Federal appellate judges across the ideological spectrum (including sitting circuit judges in published opinions and concurrences), the Cato Institute, the ACLU, and multiple bipartisan congressional reform proposals have documented that the doctrine as currently applied under the 'clearly established law' standard extends far beyond good-faith protection, foreclosing merits review even in cases of clear, serious misconduct. This corroboration comes from sources outside the beneficiary set: judges who apply the doctrine but publicly criticize its scope, and civil liberties organizations across the political spectrum.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.88) because the doctrine's operation systematically transfers the cost of proven constitutional violations to those who suffered them, with officers and municipalities bearing minimal financial consequence in the overwhelming majority of cases. Suppression is authored high (0.79) and rising over the interval because the doctrine's enforcement mechanism — courts declining to reach the merits, thereby preventing new 'clearly established' precedent from forming — actively closes off the very channel (accumulated case law) that would loosen the bar over time. This is a self-reinforcing suppression ratchet, not a static barrier. Theater ratio is moderate (0.42): the doctrine retains some genuine good-faith-protection function in truly novel-law cases, but a growing share of its operation is invoked in cases involving conduct that is plainly and seriously wrongful, where the 'clearly established' framing functions as pretextual cover for foreclosing relief. Accessibility collapse is high (0.72) because once a plaintiff understands the doctrine, there is no practical alternative federal vehicle for the underlying constitutional claim. Resistance is high (0.68), reflecting sustained, well-organized reform efforts (bipartisan legislation, appellate judges' own published criticism, sustained advocacy) that have nonetheless failed to dislodge the doctrine federally.
 *
 * PERSPECTIVAL GAP:
 *   From the officer/beneficiary seat, the doctrine functions as protective infrastructure enabling confident performance of duties. From the payer seat, the identical structure functions as an accountability void with no remedy path. The appellate court seat experiences the doctrine as a docket-management tool that also, structurally, participates in its own perpetuation by declining to generate the precedent that would loosen it. The engine computes these divergent per-seat classifications from the same structural facts; this story does not average across the readings — it authors ONE reading (accountability_void) as its own ε-invariant constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers, police unions, and municipal insurers are coded as beneficiaries with arbitrage-level exit because the doctrine can be invoked repeatedly across cases without cost to the invoking party, and the entities that would bear liability absent the doctrine face no comparable structural exposure. Civil rights plaintiffs, excessive force victims, wrongfully detained individuals, and families of decedents are coded with trapped exit and powerless standing: they cannot select their circumstances, cannot negotiate around the doctrine, and have no forum substitute. The directionality derivation places the payer stakeholders near the full-target end of d and the beneficiary stakeholders near the full-beneficiary end — this is a direct read of the beneficiary/victim declarations, not an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting good-faith discretionary action under genuinely unsettled law — is authored as dead under this reading: the doctrine now forecloses relief even where courts find clear and serious misconduct, because 'clearly established' has hardened into a fact-specific precedent-matching requirement that outpaces the rate at which precedent can accumulate (partly because courts skip the merits). The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges is exactly the capture/zombie signature this reading asserts: an arrangement whose stated purpose no longer describes its operation, but whose removal would still visibly reorganize outcomes — because what it is currently doing (suppressing liability) is a real, ongoing function, just not the one it was built to perform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_ratio_unresolved,
    'What fraction of qualified immunity''s current caseload involves genuinely novel legal questions (where a good-faith shield serves its stated function) versus cases of established, serious misconduct where ''clearly established'' functions as pretextual bar?',
    'Systematic empirical coding of dismissed Section 1983 cases distinguishing genuinely unsettled-law dismissals from cases where the underlying conduct was later or elsewhere recognized as clearly wrongful, absent the precedent-matching technicality.',
    'A high ratio of genuine novel-law dismissals would support the protective_scaffold_reading''s characterization of the same kernel; a low ratio (most dismissals involving conduct that is substantively wrongful but technically unprecedented) would corroborate this reading''s extraction characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_unresolved, empirical, 'Whether the doctrine''s caseload is dominated by genuine novel-law protection or extraction-pattern dismissals of clear misconduct.').

omega_variable(
    self_reinforcing_precedent_scarcity,
    'Is the doctrine''s persistence causally dependent on courts declining to reach constitutional merits (thereby preventing new ''clearly established'' precedent), making the doctrine self-perpetuating rather than merely persistent?',
    'Longitudinal analysis of the rate at which courts reach the merits question versus dismiss on immunity grounds alone, correlated with the rate of new ''clearly established'' precedent formation across circuits.',
    'If confirmed, the doctrine is not merely extractive in outcome but structurally self-reinforcing — an extraction mechanism that actively forecloses its own future narrowing, which would sharpen the classification toward snare with mandatrophy characteristics rather than a stable tangled coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_reinforcing_precedent_scarcity, empirical, 'Whether merits-avoidance causally perpetuates the precedent scarcity that sustains the doctrine''s bar.').

omega_variable(
    reading_selection_conceptual_framing,
    'Is the choice to treat qualified immunity as fundamentally about extraction outcomes (this reading) versus fundamentally about judicial legitimacy of source (constitutional_fidelity_reading) itself a defensible single framing, or does the doctrine require both framings held simultaneously to be fully characterized?',
    'This is not resolvable by further empirical data alone — it is a conceptual question about which structural feature of the doctrine (its outcomes vs. its judicial-legislative authorization) is the primary object of critique. Legal scholarship treats both as live, independent critiques.',
    'If both framings are held as jointly necessary rather than alternative readings, that would argue for treating the two readings as strongly coupled (high inferred_coupling) rather than merely coexisting siblings — though the ε-invariance principle still requires them as separate constraint files.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_conceptual_framing, conceptual, 'Whether the extraction-outcome framing and the judicial-legitimacy framing of the same kernel are independent or jointly necessary critiques.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement_basis(qual_tr_t1982, observed).
narrative_ontology:measurement(qual_tr_t1990, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement_basis(qual_tr_t1990, observed).
narrative_ontology:measurement(qual_tr_t2001, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(qual_tr_t2001, observed).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2009, 0.35).
narrative_ontology:measurement_basis(qual_tr_t2009, observed).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2017, 0.4).
narrative_ontology:measurement_basis(qual_tr_t2017, observed).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(qual_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement_basis(qual_be_t1982, observed).
narrative_ontology:measurement(qual_be_t1990, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement_basis(qual_be_t1990, observed).
narrative_ontology:measurement(qual_be_t2001, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2001, 0.71).
narrative_ontology:measurement_basis(qual_be_t2001, observed).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2009, 0.79).
narrative_ontology:measurement_basis(qual_be_t2009, observed).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2017, 0.85).
narrative_ontology:measurement_basis(qual_be_t2017, observed).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.88).
narrative_ontology:measurement_basis(qual_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement_basis(qual_su_t1982, observed).
narrative_ontology:measurement(qual_su_t1990, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement_basis(qual_su_t1990, observed).
narrative_ontology:measurement(qual_su_t2001, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement_basis(qual_su_t2001, observed).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement_basis(qual_su_t2009, observed).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2017, 0.74).
narrative_ontology:measurement_basis(qual_su_t2017, observed).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.79).
narrative_ontology:measurement_basis(qual_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.05).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, section_1983_civil_rights_remedy).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_use_of_force_accountability).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the qualified immunity kernel. constitutional_fidelity_reading shares the same underlying doctrine but authors ε from a judicial-legitimacy-of-source lens rather than an outcome-extraction lens; protective_scaffold_reading authors ε near-zero, treating the doctrine as genuine coordination infrastructure with officers and departments as legitimate beneficiaries and no true victim class. All three are linked here per the ε-invariance decomposition principle — they are not merged, and no single ε value is claimed to represent 'the' qualified immunity doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
