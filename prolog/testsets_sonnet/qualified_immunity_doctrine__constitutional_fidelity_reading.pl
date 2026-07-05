% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity as Judicially Fabricated Ultra Vires Doctrine
 *   domain: constitutional_law/civil_rights/judicial_authority
 *
 * SUMMARY:
 *   This story instantiates the constitutional fidelity reading of the
 *   qualified immunity kernel: the doctrine is illegitimate not because of
 *   its policy effects on officers or victims, but because it was fabricated
 *   by the judiciary without constitutional or statutory warrant. Section
 *   1983, enacted in 1871, contains no immunity language; the Supreme Court
 *   invented the 'good faith' defense in Pierson v. Ray (1967) and hardened
 *   it into the 'clearly established law' test in Harlow v. Fitzgerald
 *   (1982), then continued elaborating it for decades without ever grounding
 *   it in text, structure, or history. On this reading, the relevant
 *   extraction is not officer-on-victim (that is the
 *   accountability_void_reading, a separate constraint) and the relevant
 *   coordination is not officer protection (that is the
 *   protective_scaffold_reading, also separate) — the relevant transfer is
 *   institutional: interpretive authority moves from Congress and the
 *   statutory text to the judiciary, which then insulates its own creation
 *   from ordinary legislative correction because courts, not Congress,
 *   control the doctrine's boundaries. The beneficiary is the judiciary
 *   itself, which gains expanded common-law-making authority over a federal
 *   remedial statute; both officers and victims are, on this reading, denied
 *   a legitimate legal framework — officers get an unstable pseudo-protection
 *   with no genuine pedigree, and victims get no remedy Congress actually
 *   withheld.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Judicially Fabricated Ultra Vires Doctrine").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/judicial_authority").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'c50e5f9a-46e8-4824-8d60-f589852887da').
narrative_ontology:cs_kernel_codification('c50e5f9a-46e8-4824-8d60-f589852887da', formalized).
narrative_ontology:cs_authority_grounding('c50e5f9a-46e8-4824-8d60-f589852887da', extraction).
narrative_ontology:cs_interpretation_layer_present('c50e5f9a-46e8-4824-8d60-f589852887da').
narrative_ontology:cs_reading_relation('c50e5f9a-46e8-4824-8d60-f589852887da', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('c50e5f9a-46e8-4824-8d60-f589852887da', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('c50e5f9a-46e8-4824-8d60-f589852887da', foundational, judicial_authority_requires_textual_or_constitutional_warrant).
narrative_ontology:cs_axiom_status(judicial_authority_requires_textual_or_constitutional_warrant, holdable).
narrative_ontology:cs_axiom_grounding('c50e5f9a-46e8-4824-8d60-f589852887da', judicial_authority_requires_textual_or_constitutional_warrant, conventional).
narrative_ontology:cs_axiom('c50e5f9a-46e8-4824-8d60-f589852887da', foundational, policy_desirability_cannot_cure_ultra_vires_lawmaking).
narrative_ontology:cs_axiom_status(policy_desirability_cannot_cure_ultra_vires_lawmaking, holdable).
narrative_ontology:cs_axiom_grounding('c50e5f9a-46e8-4824-8d60-f589852887da', policy_desirability_cannot_cure_ultra_vires_lawmaking, deontological).
narrative_ontology:cs_reference_frame('c50e5f9a-46e8-4824-8d60-f589852887da', statutory_text_1871_congressional_enactment).
narrative_ontology:cs_drift_state('c50e5f9a-46e8-4824-8d60-f589852887da', post_harlow_clearly_established_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c50e5f9a-46e8-4824-8d60-f589852887da', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, appellate_court_system).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_seeking_clear_rules).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_statutory_text).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, separation_of_powers_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created the qualified immunity test in Pierson v. Ray (1967) and expanded it through Harlow v. Fitzgerald (1982) and subsequent 'clearly established law' jurisprudence without a textual hook in 42 U.S.C. Section 1983, which contains no immunity provision. Controls the doctrine's continued elaboration, can narrow or abolish it at will, and has repeatedly declined to revisit it despite dissents from within its own ranks (Justice Thomas, Justice Sotomayor) questioning its legitimacy. Bears none of the doctrine's costs and gains expanded interpretive authority over the scope of a statute Congress wrote as an unqualified damages remedy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Bring Section 1983 claims for constitutional violations and have their suits dismissed at summary judgment when no prior case with materially identical facts exists, regardless of whether the violation was obvious or egregious. Cannot appeal to Congress to fix the doctrine because the doctrine is judge-made, not statutory, and cannot litigate around it because every circuit applies some version of the same court-invented test. Have no forum in which to contest the doctrine's textual legitimacy except the same judiciary that created it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Operate under a doctrine that is supposed to give them fair notice of what conduct is unlawful but instead produces an unpredictable, circuit-by-circuit patchwork of 'clearly established law' holdings that even legal specialists struggle to apply prospectively. On the constitutional fidelity reading, officers are not net beneficiaries of a legitimate protective rule — they are relying on a rule with no lawful pedigree, which offers no genuine notice function and could be withdrawn or reconstructed at any time by the same court that fabricated it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_seeking_clear_rules, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_seeking_clear_rules, excluded).

% Enacted Section 1983 in 1871 with no immunity language and has never amended the statute to codify qualified immunity, despite decades of judicial elaboration of the doctrine in its name. Repeated legislative reform proposals (the George Floyd Justice in Policing Act and various state-level abolitions) show live legislative interest, but Congress has not acted at the federal level, leaving the judiciary as the doctrine's sole author and sole potential undoer.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, civilizational, constrained, national).

% The statutory text of Section 1983 and the enacting Congress's evident purpose (unqualified liability for constitutional deprivations under color of law) are the interpretive baseline against which the constitutional fidelity reading measures the doctrine's illegitimacy. Neither is an actor; both are cited as the standard the judiciary is held to have departed from.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_and_1871_congress, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_and_1871_congress).

% Analyze the doctrine's textual and historical pedigree and conclude, across the ideological spectrum (Baude, Schwartz, Reinert, and Justice Thomas himself), that qualified immunity as currently constructed cannot be derived from the 1871 statute or from a coherent common-law immunity tradition the statute was meant to preserve. Their scholarship is the primary corroborating source for this reading, external to both plaintiffs seeking damages and officers seeking protection.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, originalist_legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None cognizable on this reading. The doctrine does not solve a genuine coordination problem between officers and the public; it substitutes judicial policy preference for a legislative or constitutional judgment that was never made. Any coordination benefit claimed (predictability for officers) is illusory because the doctrine's core test is itself unpredictable and continuously re-litigated.
% TRANSFER_FUNCTION: Moves adjudicative authority over the scope of civil rights liability from Congress (the statute's author) to the federal judiciary, and moves the practical remedy away from plaintiffs whose constitutional rights were violated, without any corresponding transfer of accountability to a legitimate rulemaking body.
% ABSENT_VOICES: Congress, as the 1871 enacting body, is functionally absent from the doctrine's ongoing elaboration; it never authorized the immunity and has not been forced to ratify or reject it through the ordinary legislative process. The text of the statute itself has no voice in a body of law that has departed from it.
% DISAPPEARANCE_RATIONALE: If judicially fabricated qualified immunity were withdrawn (as opposed to a legislatively enacted substitute), Section 1983 would revert to something closer to its unqualified text: courts would adjudicate constitutional violations under ordinary tort-like standards without a 'clearly established law' threshold. Whether this produces more or less accountability is a separate policy question the fidelity reading brackets; the immediate rearrangement is institutional — the judiciary's self-granted interpretive gloss disappears and Congress would face pressure to legislate a replacement framework explicitly.
% FOUNDING_PROBLEM: The doctrine was constructed to give officials 'fair warning' before subjecting them to damages liability, addressing a perceived unfairness in applying newly announced constitutional rules retroactively to good-faith conduct.
% FOUNDING_PROBLEM_CORROBORATION: Originalist and textualist scholars across the political spectrum (Baude, Schwartz, and Justice Thomas in his Ziglar v. Abbasi concurrence) attest that the doctrine's justification was manufactured after the fact and has no statutory or common-law basis, corroborating the fidelity reading from outside both the judiciary that authored the doctrine and the plaintiff/officer parties it directly affects. The judiciary itself has not offered this corroboration; the fidelity reading rests on external doctrinal history, not judicial self-justification.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.58 by interval end: the extraction here is not primarily monetary but jurisdictional — the judiciary's accretion of interpretive control that displaces the coordinate branch's textual choice. Suppression (0.71) reflects that no litigant, officer, or member of Congress has a working channel to force judicial reconsideration of the doctrine's textual basis; stare decisis and the doctrine's self-referential case law make internal correction effectively closed. Theater ratio rises to 0.62 because an increasing share of the doctrine's judicial defense consists of stare decisis and reliance-interest rhetoric rather than engagement with the textual objection itself — later opinions increasingly perform legitimacy rather than establish it. Accessibility collapse (0.60) and resistance (0.55) are moderate: the doctrine has not fully foreclosed alternatives (state-level abolition, scholarly campaigns, occasional judicial dissent) but active resistance from within the legal academy and dissenting justices has not moved the doctrine's core.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits at the beneficiary end: it authored the doctrine, controls its elaboration, and bears no cost from maintaining it while gaining expanded interpretive reach over a statute Congress wrote without qualification. Civil rights plaintiffs sit at the target end: trapped, powerless, and bearing the doctrine's practical costs in dismissed claims, with no venue to contest its textual legitimacy other than the judiciary that created it. Officers seeking genuinely clear rules are also, on this reading, payers rather than beneficiaries — the 'protection' the doctrine offers is doctrinally unstable and offers no real notice function, so officers relying on it are relying on a fabrication rather than a legitimate legal shield. Congress and the enacting text are excluded voices: the doctrine operates in their name but without their authorization.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists conflating the doctrine's policy merits with its legitimacy. A constraint can be popular, can produce outcomes some observers regard as socially desirable, and still be illegitimate if it lacks constitutional or statutory grounding — that is precisely the claim this reading makes and the accountability_void_reading and protective_scaffold_reading do not. The fidelity reading's founding_problem status is authored as contested rather than dead, because on this reading the doctrine was never solving a problem Congress or the Constitution assigned to the judiciary in the first place; there is no lapsed original function to point to, only an ultra vires exercise of interpretive power that has since ossified through stare decisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_silence_vs_implied_authorization,
    'Does Section 1983''s textual silence on immunity mean Congress affirmatively rejected any immunity defense, or does it mean Congress intended to preserve common-law immunities that existed in 1871 and simply did not need to codify them?',
    'Historical analysis of 1871 common-law immunity doctrines and legislative history of the Civil Rights Act of 1871 (the Ku Klux Klan Act); scholarly consensus among legal historians on what background immunities, if any, were understood to survive the statute''s enactment.',
    'If Congress intended to preserve some common-law immunity, a narrower, historically grounded immunity doctrine could be legitimate even on a fidelity-focused reading — only the judiciary''s post-1967 expansion into the ''clearly established law'' test would remain illegitimate. If Congress intended no such preservation, the entire doctrine is ultra vires from its 1967 origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_silence_vs_implied_authorization, empirical, 'Whether 1871 statutory silence implies rejection or preservation of common-law immunity.').

omega_variable(
    judicial_beneficiary_versus_ideological_beneficiary,
    'Is ''the judiciary'' as an institution actually the beneficiary of the doctrine''s persistence, or is the true beneficiary a specific judicial philosophy (managerial docket-control, deference to law enforcement) that happens to be implemented through the judiciary but could be held by other actors?',
    'Compare judicial behavior across ideologically diverse judges and circuits: if the doctrine''s application correlates more with docket-management incentives (case dismissal reduces trial burden) than with substantive law-enforcement deference, the institutional-capacity account is stronger than the ideological one.',
    'If institutional capacity (not ideology) drives persistence, the doctrine is better modeled as a piton-like inertial artifact of judicial workload management rather than a deliberate extraction of interpretive power; this reading''s claim that the judiciary is the concentrated beneficiary would need refinement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_beneficiary_versus_ideological_beneficiary, conceptual, 'Whether the judiciary benefits as an institution or as a proxy for docket-management incentives.').

omega_variable(
    cs_framing_kernel_versus_legitimacy_claim,
    'Should the kernel here be treated as the underlying statutory text (Section 1983) or as the layered legitimacy claim the judiciary has constructed on top of it (the doctrine of qualified immunity itself, treated as if it were the operative law)?',
    'Track which framing legal actors actually argue from: litigants briefing ''clearly established law'' treat the doctrine''s own case law as the operative kernel, while originalist critics treat the 1871 text as the true kernel the doctrine has drifted from.',
    'Framing the kernel as the statutory text supports this reading''s illegitimacy claim (drift from an authoritative source); framing the kernel as the accumulated case law itself would make the doctrine self-legitimating by its own internal consistency, closer to the protective_scaffold_reading''s implicit premise. This story adopts the statutory-text framing because it is the fidelity reading''s defining commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_legitimacy_claim, conceptual, 'Alternative framings of the operative kernel: enacted text versus accumulated doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(qual_tr_t0, observed).
narrative_ontology:measurement(qual_tr_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 11, 0.42).
narrative_ontology:measurement_basis(qual_tr_t11, observed).
narrative_ontology:measurement(qual_tr_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 22, 0.48).
narrative_ontology:measurement_basis(qual_tr_t22, observed).
narrative_ontology:measurement(qual_tr_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 33, 0.53).
narrative_ontology:measurement_basis(qual_tr_t33, observed).
narrative_ontology:measurement(qual_tr_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 44, 0.58).
narrative_ontology:measurement_basis(qual_tr_t44, observed).
narrative_ontology:measurement(qual_tr_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 55, 0.62).
narrative_ontology:measurement_basis(qual_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(qual_be_t0, observed).
narrative_ontology:measurement(qual_be_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 11, 0.3).
narrative_ontology:measurement_basis(qual_be_t11, observed).
narrative_ontology:measurement(qual_be_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 22, 0.4).
narrative_ontology:measurement_basis(qual_be_t22, observed).
narrative_ontology:measurement(qual_be_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 33, 0.48).
narrative_ontology:measurement_basis(qual_be_t33, observed).
narrative_ontology:measurement(qual_be_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 44, 0.54).
narrative_ontology:measurement_basis(qual_be_t44, observed).
narrative_ontology:measurement(qual_be_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 55, 0.58).
narrative_ontology:measurement_basis(qual_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(qual_su_t0, observed).
narrative_ontology:measurement(qual_su_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 11, 0.5).
narrative_ontology:measurement_basis(qual_su_t11, observed).
narrative_ontology:measurement(qual_su_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 22, 0.58).
narrative_ontology:measurement_basis(qual_su_t22, observed).
narrative_ontology:measurement(qual_su_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 33, 0.63).
narrative_ontology:measurement_basis(qual_su_t33, observed).
narrative_ontology:measurement(qual_su_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 44, 0.68).
narrative_ontology:measurement_basis(qual_su_t44, observed).
narrative_ontology:measurement(qual_su_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 55, 0.71).
narrative_ontology:measurement_basis(qual_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'qualified immunity' per the ε-invariance principle. The accountability_void_reading treats immunity as an extraction mechanism with officers as concentrated beneficiaries and victims as the payer class (a tangled_rope or snare depending on enforcement data). The protective_scaffold_reading treats immunity as a legitimate coordination mechanism protecting good-faith law enforcement (a rope or scaffold, contingent on a sunset/reform mechanism). This constraint_fidelity_reading brackets both policy questions and asks only whether the doctrine has constitutional or statutory pedigree, concluding it does not — its beneficiary set (the judiciary as an institution) and its extraction mechanism (jurisdictional/interpretive, not officer-victim wealth transfer) are structurally distinct from both siblings. All three share the same underlying kernel (the judicially constructed immunity doctrine as it operates in Section 1983 litigation) but instantiate different ε values, different beneficiary/victim sets, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
