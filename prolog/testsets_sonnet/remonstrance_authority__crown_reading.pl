% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Parlementary Remonstrance as Minoritarian Fiscal Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   In the crown reading of the remonstrance kernel, the parlements' right to
 *   remonstrate against royal edicts before registration is treated not as a
 *   fundamental constitutional check but as an illegitimate minoritarian veto
 *   exercised by a narrow class of venal officeholders and their allied
 *   privileged orders. From this seat, remonstrance's coordination story —
 *   that it screens edicts for legal regularity — is cover for its actual
 *   operation: repeated blocking or dilution of fiscal and administrative
 *   reforms whose costs would have fallen on magistrates, office-holders, and
 *   privileged provinces, and whose absence instead falls on the general
 *   taxpaying populace and the Crown's creditors. The Crown itself enters the
 *   victim set here: its capacity to govern coherently is what the
 *   remonstrance power obstructs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.71).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.62).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Parlementary Remonstrance as Minoritarian Fiscal Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '7d81adef-52d1-4916-923a-4c8a0cd7717f').
narrative_ontology:cs_kernel_codification('7d81adef-52d1-4916-923a-4c8a0cd7717f', distributed).
narrative_ontology:cs_authority_grounding('7d81adef-52d1-4916-923a-4c8a0cd7717f', distributed).
narrative_ontology:cs_reading_relation('7d81adef-52d1-4916-923a-4c8a0cd7717f', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('7d81adef-52d1-4916-923a-4c8a0cd7717f', foundational, unified_sovereign_legislative_will_supreme).
narrative_ontology:cs_axiom_status(unified_sovereign_legislative_will_supreme, holdable).
narrative_ontology:cs_axiom_grounding('7d81adef-52d1-4916-923a-4c8a0cd7717f', unified_sovereign_legislative_will_supreme, conventional).
narrative_ontology:cs_axiom('7d81adef-52d1-4916-923a-4c8a0cd7717f', secondary, venal_office_privilege_lacks_constitutional_standing).
narrative_ontology:cs_axiom_status(venal_office_privilege_lacks_constitutional_standing, holdable).
narrative_ontology:cs_axiom_grounding('7d81adef-52d1-4916-923a-4c8a0cd7717f', venal_office_privilege_lacks_constitutional_standing, deontological).
narrative_ontology:cs_reference_frame('7d81adef-52d1-4916-923a-4c8a0cd7717f', absolute_sovereign_legislative_supremacy).
narrative_ontology:cs_drift_state('7d81adef-52d1-4916-923a-4c8a0cd7717f', pre_1789_fiscal_crisis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7d81adef-52d1-4916-923a-4c8a0cd7717f', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, parlementaire_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_privileged_orders).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_fiscal_administration).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_creditors).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, unrepresented_taxpaying_populace).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, unified_sovereign_legislative_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attempts to register and enforce royal edicts (new taxes, debt restructuring, administrative reform) that require parlementary registration to take legal effect. Each remonstrance delays or blocks revenue collection at moments of fiscal crisis, forcing the Crown into lits de justice, exile of magistrates, or negotiated concessions. The Crown cannot simply route around the parlement without escalating to a constitutional confrontation it may not win cleanly.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_fiscal_administration, payer,
    institutional, generational, constrained, national).

% Hold venal, heritable offices that entitle them to review and remonstrate against royal edicts before registration. They frame remonstrance as guardianship of fundamental law, but the offices themselves are purchased property whose value and prestige depend on the remonstrance power persisting. They face essentially no personal fiscal exposure from blocking taxes that would fall on others.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlementaire_magistrates, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, parlementaire_magistrates, beneficiary).

% Purchased judicial and administrative offices whose market value is propped up by the parlements' capacity to resist reforms (including reforms that would abolish venality itself). Remonstrance functions as insurance on their capital investment in office.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_office_holders, beneficiary,
    powerful, generational, arbitrage, regional).

% Nobility and clergy whose tax exemptions and local privileges are repeatedly defended by sympathetic parlements invoking 'fundamental law' against royal attempts at uniform taxation. They rely on the parlements to stall or dilute reforms that would erode their particular exemptions.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_privileged_orders, beneficiary,
    organized, generational, mobile, regional).

% Hold royal debt whose servicing depends on the Crown's ability to raise and collect revenue. Repeated remonstrance-driven delays in tax registration degrade the Crown's fiscal credibility and, downstream, the security of their loans; they have no seat in the remonstrance process and no direct recourse against the parlements that produce the delay.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_creditors, payer,
    moderate, biographical, trapped, national).

% Bears the taxes that survive registration, plus the costs of the fiscal shortfalls and improvised expedients (venal office sales, forced loans, currency manipulation) the Crown resorts to when remonstrance blocks orderly reform. Has no formal standing in the remonstrance exchange between Crown and parlement and no voice in what 'fundamental law' is invoked to protect.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, unrepresented_taxpaying_populace, payer,
    powerless, biographical, trapped, national).

% Crown ministers who design tax and administrative reforms (uniform land tax, abolition of venality, standardized weights and measures) but must route every measure through parlementary registration. Their technocratic case for reform is repeatedly recast by the parlements as tyrannical innovation, and they have no independent forum to make their argument stick without royal escalation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, reforming_ministers, excluded,
    powerful, biographical, constrained, national).

% Assess, after the fact, whether the remonstrance power functioned as a check on arbitrary rule or as a mechanism by which a narrow, self-perpetuating class defended its fiscal privileges against a sovereign trying to govern a fiscally coherent state.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, parlementaire_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, remonstrance provides a review step that could catch genuinely arbitrary or legally defective royal acts before they take force — a coordination function analogous to judicial review of legislative regularity.
% TRANSFER_FUNCTION: The arrangement moves fiscal burden away from the offices, orders, and provinces the parlements are staffed by and sympathetic to, and onto the general taxpaying populace and the Crown's creditors, by blocking or diluting reforms that would have taxed privilege more evenly or restructured venal offices.
% ABSENT_VOICES: The unrepresented taxpaying populace and the Crown's creditors have no seat in the remonstrance exchange; reforming ministers are structurally routed through the very body whose members' privileges the reforms would curtail, so their case is heard only as filtered through hostile review.
% DISAPPEARANCE_RATIONALE: If remonstrance disappeared overnight (as it effectively did after 1789), royal edicts would take immediate legal effect on promulgation; venal office values tied to obstruction capacity would collapse, provincial fiscal exemptions defended through parlementary sympathy would lose their institutional shield, and fiscal reform could proceed without the systematic multi-year delays that characterized late Bourbon finance.
% FOUNDING_PROBLEM: Originally, remonstrance addressed a genuine problem: preventing the Crown from issuing edicts that contradicted existing registered law without any check, and ensuring new law was formally consistent with the body of registered law the parlement administered.
% FOUNDING_PROBLEM_CORROBORATION: Royal ministers (Turgot, Necker, Calonne) attested repeatedly, in official correspondence and reform memoranda, that the legal-consistency function had been supplanted by defense of fiscal and status privilege; foreign observers of French finance (e.g., in comparative assessments of English vs. French credit markets) independently attributed France's chronic fiscal fragility partly to unpredictable registration delays. The parlements themselves, the primary beneficiaries, are the main source asserting the founding problem remains live.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.42 → 0.71) as venality and privilege become increasingly entangled with the remonstrance function and as fiscal crises multiply the stakes of each blocked reform. Suppression is substantial but lower than extraction (peaking at 0.62) because the Crown retained formal tools (lits de justice, exile, forced registration) to override remonstrance when it chose to escalate — the suppression the parlements exercise is legal-procedural obstruction, not physical coercion, and the Crown's own countervailing enforcement machinery limits how total the parlements' control ever became. Theater ratio rises moderately (0.2 → 0.4) as the 'fundamental law' rhetoric increasingly outpaces any genuine legal-consistency review function.
 *
 * DIRECTIONALITY LOGIC:
 *   Parlementaire magistrates, venal office holders, and provincial privileged orders are declared beneficiaries: their offices, capital, and exemptions are defended by the very structure being evaluated, and their exit options are arbitrage-grade (they can shift assets, negotiate individually, or wait out royal pressure) — this pushes their derived directionality toward the beneficiary end. The royal fiscal administration, Crown creditors, and unrepresented taxpaying populace are declared victims: the administration is institutionally constrained (it cannot simply dissolve the parlements without immense cost), creditors are trapped by existing debt exposure, and the populace is fully trapped with no formal standing at all. This is the inverse of the magistrate reading, where the Crown would be read as the agenda-setter and the parlements as defenders of victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking arbitrary royal edicts against a coherent body of registered law — is assessed as dead in this reading well before the interval's end: by the 18th century, remonstrance was functioning primarily to defend particularist fiscal privilege rather than legal consistency, yet the arrangement persisted because dismantling it required a constitutional confrontation the Crown repeatedly deferred. This is the tangled_rope signature rather than pure snare: the arrangement did once solve a coordination problem (legal-consistency review) and elements of that function persisted in form even as the substance shifted to extraction — hence active enforcement (registration procedure, lits de justice) is required to keep the extraction running.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remonstrance_kernel_reading_divergence,
    'Is the remonstrance power correctly read as an illegitimate minoritarian veto protecting particularist privilege (this story), or as a legitimate constitutional check preserving ancient liberties against arbitrary royal innovation (the magistrate_reading sibling)?',
    'No single historical fact resolves this; it depends on which theory of sovereign legitimacy is adopted (unified legislative will vs. layered customary/fundamental law) and on empirical assessment of whether specific remonstrances tracked genuine legal defects or tracked office-holder and privileged-order fiscal interest. Case-by-case study of remonstrance content against subsequent fiscal outcomes would shift confidence but not settle the underlying legitimacy question.',
    'If the magistrate reading is credited, the Crown becomes the agenda-setter/beneficiary and the parlements'' defense function becomes the coordination story; the victim set shifts to those harmed by unchecked royal fiscal innovation. The two readings produce opposite beneficiary/victim assignments from the same institutional facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remonstrance_kernel_reading_divergence, conceptual, 'Which reading of the remonstrance kernel is structurally correct — crown or magistrate.').

omega_variable(
    reform_content_neutrality,
    'Were specific remonstrances (e.g., against Turgot''s or Calonne''s reforms) driven predominantly by defense of legal consistency, or predominantly by defense of magistrate/order fiscal interest?',
    'Close reading of individual remonstrance texts against the specific fiscal content of the edicts they opposed, cross-referenced with which social groups bore the cost of each blocked reform.',
    'A finding of predominant fiscal self-interest strengthens this crown reading''s tangled_rope/high-ε classification; a finding of genuine legal-consistency concern in a substantial share of cases would suggest this reading overstates extraction relative to the magistrate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_content_neutrality, empirical, 'Whether individual remonstrances were substantively fiscal-interest-driven or legal-consistency-driven.').

omega_variable(
    crown_as_victim_ambiguity,
    'Is it coherent to classify the Crown — itself an extractive fiscal apparatus toward the general populace — as a ''victim'' of remonstrance, or does this obscure that both Crown and parlements extract from the same taxpaying base by different mechanisms?',
    'Compare fiscal outcomes for the populace under counterfactual unchecked royal taxation versus the actual remonstrance-mediated regime — did blocked reforms net protect or harm the populace relative to what unconstrained royal fiscal policy would have produced?',
    'If unchecked royal taxation would have been worse for the populace, the Crown''s ''victim'' status here is a narrow institutional-capacity claim, not a populist one, and the populace''s placement as a distinct victim group (rather than a Crown ally) needs qualification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_as_victim_ambiguity, conceptual, 'Whether Crown-as-victim and populace-as-victim are compatible claims or in tension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(remo_tr_t8, remonstrance_authority__crown_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(remo_tr_t16, remonstrance_authority__crown_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(remo_tr_t24, remonstrance_authority__crown_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(remo_tr_t32, remonstrance_authority__crown_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(remo_be_t8, remonstrance_authority__crown_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(remo_be_t16, remonstrance_authority__crown_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(remo_be_t24, remonstrance_authority__crown_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(remo_be_t32, remonstrance_authority__crown_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(remo_su_t8, remonstrance_authority__crown_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(remo_su_t16, remonstrance_authority__crown_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(remo_su_t24, remonstrance_authority__crown_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(remo_su_t32, remonstrance_authority__crown_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, venal_office_system).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, bourbon_fiscal_credit_crisis).

% DUAL FORMULATION NOTE:
% This story and remonstrance_authority__magistrate_reading are two readings of the same kernel (remonstrance_authority): the formal institutional fact of parlementary review-before-registration. They carry different ε (this reading's extractiveness rises to 0.71 by interval end, reflecting the crown's view of rent-protection; the magistrate reading would author a lower ε reflecting genuine constitutional-check function) and inverted beneficiary/victim sets. Per the ε-invariance principle, these are not the same constraint measured two ways — they are two distinct constraints sharing a contested institutional kernel, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
