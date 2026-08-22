% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   A categorical prohibition on intentional life-ending, covering assisted
 *   dying and euthanasia alike, enforced through criminal law and
 *   professional discipline, grounded in this reading's axiom that human life
 *   has intrinsic value no individual preference can override. The story is
 *   authored from the sanctity_reading's seat: one reading of the
 *   end_of_life_authority kernel, with the autonomy_reading and the
 *   slippery_slope_mechanism as sibling constraints in separate files. Per
 *   the epsilon-referent rule, epsilon is authored for the standing
 *   arrangement under contest, the prohibition regime itself, as the sanctity
 *   reading assesses it: the reading holds the regime protective rather than
 *   extractive, concedes its real costs (denied options, regressive burden,
 *   enforcement exposure), and denies those costs are wrongful takings. The
 *   claim/metrics split is deliberate: the reading claims moral bedrock,
 *   while the structural data names enforced machinery, identifiable
 *   beneficiaries, and cost-bearing classes. The engine measures that
 *   divergence rather than the story reconciling it. The legalization wave
 *   across a growing minority of jurisdictions has made the regime's burden
 *   visibly regressive: affluent patients buy assisted death abroad while
 *   poor and disabled patients are fully bound, and the relatives who help
 *   them face prosecution. KEY AGENTS (by structural relationship):
 *   pressured_vulnerable_patients (powerless/trapped, dual
 *   protected-and-burdened class), competent_suffering_patients
 *   (powerless/constrained, primary cost-bearing class),
 *   mobile_wealthy_patients (moderate/arbitrage, nominal payers with
 *   effective exit), compassionate_family_assisters (powerless/trapped,
 *   enforcement-exposed), disability_rights_community (organized/mobile,
 *   beneficiary), religious_institutions (institutional/mobile, beneficiary),
 *   medical_profession_majority (institutional/constrained, dual beneficiary
 *   and bound party), autonomy_conscientious_physicians
 *   (moderate/constrained, excluded dissent), legislature_and_courts
 *   (institutional/mobile, agenda setter), bioethics_commissions
 *   (institutional/analytical, observer).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.28).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.65).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, mountain).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life Categorical Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).
domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, 'd575969d-4ad6-4951-b8e5-fd9d6717a074').
narrative_ontology:cs_kernel_codification('d575969d-4ad6-4951-b8e5-fd9d6717a074', formalized).
narrative_ontology:cs_authority_grounding('d575969d-4ad6-4951-b8e5-fd9d6717a074', lineage).
narrative_ontology:cs_interpretation_layer_present('d575969d-4ad6-4951-b8e5-fd9d6717a074').
narrative_ontology:cs_reading_relation('d575969d-4ad6-4951-b8e5-fd9d6717a074', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d575969d-4ad6-4951-b8e5-fd9d6717a074', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('d575969d-4ad6-4951-b8e5-fd9d6717a074', foundational, consent_cannot_legitimize_intentional_killing).
narrative_ontology:cs_axiom_status(consent_cannot_legitimize_intentional_killing, holdable).
narrative_ontology:cs_axiom_grounding('d575969d-4ad6-4951-b8e5-fd9d6717a074', consent_cannot_legitimize_intentional_killing, deontological).
narrative_ontology:cs_axiom('d575969d-4ad6-4951-b8e5-fd9d6717a074', secondary, physician_role_limited_to_life_preservation).
narrative_ontology:cs_axiom_status(physician_role_limited_to_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('d575969d-4ad6-4951-b8e5-fd9d6717a074', physician_role_limited_to_life_preservation, conventional).
narrative_ontology:cs_reference_frame('d575969d-4ad6-4951-b8e5-fd9d6717a074', inviolability_of_human_life_norm).
narrative_ontology:cs_drift_state('d575969d-4ad6-4951-b8e5-fd9d6717a074', contemporary_post_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d575969d-4ad6-4951-b8e5-fd9d6717a074', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_rights_community).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_profession_majority).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressured_vulnerable_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, mobile_wealthy_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, compassionate_family_assisters).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, medical_profession_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elderly, disabled, and economically precarious patients making end-of-life decisions inside relationships of dependency, with family caregivers, care institutions, and heirs whose interests can diverge from their own. The categorical rule means no one may lawfully end their lives at any request, so a request produced by pressure has no legal channel and they never have to prove their refusal was free. The same rule binds them personally: if their own wish, formed freely under suffering, is to die, it cannot be honored anywhere they can reach, since they lack the money and mobility to travel to permissive jurisdictions, and relatives who might help them face prosecution. Their position is dual: shielded from other people's pressure, bound by other people's certainty.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressured_vulnerable_patients, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, pressured_vulnerable_patients, payer).

% Patients with decision-making capacity experiencing suffering they judge unbearable, who want the option of an assisted death and are denied it by the categorical rule. The rule overrides their preference by design, regardless of individual preference. What flows from them: continued suffering they would end, loss of control over the circumstances and timing of their death, and in some cases recourse to risky self-administration or clandestine assistance. Their exit is limited: some could travel to permissive jurisdictions if they had the money, most cannot, and none can exit the reach of the rule within their own jurisdiction.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_suffering_patients, payer,
    powerless, biographical, constrained, global).

% Affluent patients subject to the same prohibition at home who can purchase what the rule withholds: right-to-die organizations and clinics in permissive jurisdictions provide assisted death to foreign nationals. The rule's practical weight on them is a travel bill and a logistics problem rather than a categorical bar, so they carry the regime's formal cost while escaping its practical one. They are the visible edge of the asymmetry that makes the burden fall hardest on those with the least.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, mobile_wealthy_patients, payer,
    moderate, biographical, arbitrage, global).

% Spouses, partners, and adult children who help a suffering loved one die at that person's insistent request and face criminal investigation and prosecution as a result. Their exposure is retroactive and unavoidable: the act cannot be undone and the relationship cannot be exited. Enforcement selects them one by one while the rule's public justification speaks in general terms, and their cases supply the regime's most visible human cost.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, compassionate_family_assisters, payer,
    powerless, biographical, trapped, national).

% Organized advocacy groups, many led by disabled people, that treat the bright-line rule as the main legal barrier against a culture of devalued disabled life: they argue that once assisted death is an option, disability itself starts to read as a reason to die. They gain standing, coalition partners, and a legal anchor from the rule's persistence, and they campaign actively to keep or extend it. Their position is chosen and defensible: they litigate, lobby, and enter or exit coalitions at will.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_rights_community, beneficiary,
    organized, generational, mobile, global).

% Churches and religious bodies whose doctrine holds the end of life under divine rather than personal authority. The categorical rule codifies their teaching in secular law; they supply much of the political muscle that maintains it and treat its erosion as doctrinal defeat. Their commitment predates and would outlast any particular legislature, and they operate across jurisdictions, shifting resources to wherever the rule is contested.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% Physicians and medical bodies whose traditional identity, healers who preserve life and never intentionally end it, is secured by the rule. Patients can trust that their doctor is not a possible instrument of their death, and the profession is spared the conflict of caring for and killing the same patient. The binding side: the rule forecloses practices some physicians and patients would choose, requires the profession to police its own members, and leaves doctors managing suffering they cannot offer to end. The role travels with the profession across borders, so exit would mean exiting medicine's identity, not merely its jurisdiction.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_profession_majority, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, medical_profession_majority, payer).

% A minority of physicians who judge that respecting a competent patient's settled wish for death can be part of care, and who would provide it under regulated conditions. Within the regime the rule sustains, their position is not a dissent to be weighed but a violation to be prevented: they cannot practice their conviction, risk discipline for advocating it at the bedside, and enter the policy conversation mainly as objects of regulation rather than as parties.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_conscientious_physicians, excluded,
    moderate, biographical, constrained, national).

% The legislatures and courts that enact, maintain, revisit, or repeal the prohibition. They set its scope, its penalties, and its exceptions, and they respond to litigation and electoral pressure: several have moved from prohibition to regulated permission in recent decades, others have re-entrenched the ban. Their position is jurisdictional and revisable; they can change the rule at the cost of political conflict, not structural impossibility.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislature_and_courts, agenda_setter,
    institutional, generational, mobile, national).

% State ethics commissions, royal commissions, and advisory bodies that investigate end-of-life policy, take evidence from all the other seats, and publish analyses of coercion risk, safeguard design, and practice in permissive jurisdictions. They decide nothing and bear nothing; their reports nonetheless shape what the agenda-setters treat as established.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem in end-of-life decision-making: when intentional death is a lawful option, each decision to die is made under conditions of pain, dependency, care burden, inheritance, and disability devaluation in which an expressed wish may not be an authentic one, and no case-by-case procedure can reliably detect pressure applied inside close relationships. A bright-line rule removes the decision point altogether: no request can be acted on, so no request needs to be audited, and vulnerable people are protected without anyone having to prove coercion. The same rule maintains a unified medical role, the physician as preserver of life, that patients can trust unconditionally.
% TRANSFER_FUNCTION: Transfers end-of-life decision authority from individual patients and their families to the legal-moral order: the question of whether this life may be ended is moved out of private hands and answered categorically. It also fixes the distribution of continuing costs, so the suffering, care burden, and expense of prolonged dying stay with patients and families instead of being terminable by choice, and it concentrates enforcement exposure on the relatives and physicians who assist despite the rule.
% ABSENT_VOICES: The competent suffering patients are formally present, as litigants, survey respondents, and witnesses, but the rule's own formula overrides them regardless of individual preference, so their voice enters the conversation only to be set aside by design. Genuinely absent: patients who have lost capacity and are represented only by proxies; dissenting physicians, whose position is treated as a compliance problem rather than a position; and dying people in jurisdictions where the ban is entrenched with no review mechanism, who never get a hearing at all.
% DISAPPEARANCE_RATIONALE: Jurisdictions that have dropped the rule show what follows: regulated assisted death is legislated within years, physician practice reorganizes around it, safeguard machinery is built, and disability and religious opposition reorganizes against the new arrangement. If the prohibition vanished everywhere overnight, the end-of-life order would rearrange along the lines already visible in the permissive jurisdictions, and jurisdictions that value the ban would have to rebuild it deliberately rather than inherit it.
% FOUNDING_PROBLEM: The prohibition is ancient, with Hippocratic, religious, and common-law lines converging on it, but in its modern form it was built to solve a specific problem: that legalizing intentional death opens a channel through which elderly, disabled, and economically dependent people can be pressured, subtly or openly, into agreeing to die, and that no procedural safeguard can fully detect pressure applied inside relationships of dependency. Its second founding purpose is role protection: keeping the medical profession on the preserving side of the line between healing and killing.
% FOUNDING_PROBLEM_CORROBORATION: The coercion problem's reality is corroborated from outside the beneficiary coalition: autonomy-side legislators build elaborate safeguard machinery precisely because they concede the risk is real, the elder-abuse and caregiving-burden literature documents pressure inside dependency relationships, and permissive jurisdictions' own commission reports record documented and suspected pressure cases. What no outside source attests is the problem's sufficiency: every non-beneficiary source that concedes the risk also holds it manageable by safeguards, so the founding problem's existence is corroborated while its categorical weight remains the contest itself.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_authority__sanctity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.28 is reading-indexed over the prohibition regime as referent: the sanctity seat concedes the regime's real costs, including competent patients' overridden wishes, the arbitrage asymmetry, and prosecutions of helpers, but holds them justified prices of protection rather than wrongful takings, so epsilon sits far below what the payer seats would author for the same arrangement. Suppression 0.65 is a raw, unscaled structural measure of the regime's coercive machinery: criminal statutes, licensing discipline, and active prosecution; only extractiveness is scaled by directionality and scope, not suppression. Theater_ratio 0.33: most enforcement remains operative, since prosecutions and discipline are real, but a growing share of the regime's activity is declarative, restating the principle, symbolically charging compassion cases, reaffirming oaths, as the legalization wave proceeds. Accessibility_collapse 0.45 and resistance 0.7 are honest for a contested construct: alternatives persist, including foreign clinics, permissive jurisdictions, palliative sedation, and litigation, and organized resistance has repeatedly moved legislatures and courts. The claimed mountain with emerges_naturally true is the reading's own structural claim: the intrinsic value of life is held to be a feature of moral reality rather than an enactment, while the beneficiary and victim declarations describe the regime's actual party structure; the divergence between claim and computed classification is the datum this story exists to record. The measurement series share one grid, points 0 to 30 at steps of 6, mapping the modern debate era from near-universal uncontested prohibition to the present contested patchwork. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the regime needed little active enforcement at consensus baseline (0.35) and progressively more as court challenges, autonomy movements, and legislative repeals mounted (0.65 at present); the rising series is enforcement intensification against resistance, not a change in the rule itself. Receipt surface: gain_flow diffuse is an affirmative checked claim, made after re-reading every stakeholder situation: the regime's costs, foregone options and prolonged suffering, are not received by any named seat as gain; religious institutions, disability organizations, and the medical majority benefit from the rule without converting the denied class's costs into receipts. fixing_cost cheap: the agenda-setter seat has repeatedly removed the prohibition by ordinary legislation and court-forced revision where majorities formed, so removal is politically costly but structurally feasible.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the beneficiary seats, religious institutions (institutional/mobile), disability rights organizations (organized/mobile), and the medical majority (institutional/constrained), the regime is protective coordination and role security, and the denied class's costs are the necessary price of a bright line. From the cost-bearing seats, competent suffering patients (powerless/constrained) and prosecuted helpers (powerless/trapped), the same structure is a categorical override of their most consequential preference, enforced by criminal process. The pressured-vulnerable seat computes from both sides at once: shielded from coercion it cannot disprove, bound by a denial it cannot exit. The mobile-wealthy seat formally belongs to the payer class but its arbitrage exit puts it near the protected end: same rule, different effective bind. The agenda-setter seat experiences the regime as revisable policy, not moral bedrock, having repealed the ban where majorities formed. Coalition note: the powerless victim classes are not without recourse; patient litigants and disability-led coalitions have won the decisive court victories that forced the regime onto the agenda-setter's docket, which is why resistance sits high despite individually powerless seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: religious institutions and disability organizations collect standing, doctrinal order, and coalition position without running the enforcement; the medical majority collects role security and patient trust while also being bound by the rule, hence the secondary role. Victim declarations map to real costs: competent suffering patients bear overridden preference and prolonged suffering; the pressured-vulnerable bear the regime's regressive burden, no arbitrage exit, exposed helpers, overridden genuine wishes, while simultaneously receiving its protection, which is why they appear in both arrays. Exit structure differentiates seats the derivation would otherwise merge: arbitrage-grade exit places the mobile-wealthy formal payers near the beneficiary end; trapped exit places helpers and the pressured-vulnerable at the full-target end. No directionality overrides are authored: the derivation from beneficiary/victim declarations plus exit options already separates every seat this story needs to distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the regime as pure coordination, the sanctity seat's temptation, erases the denied class's costs and the enforcement machinery that sustains the line. Reading it as pure extraction, the autonomy seat's temptation, erases the genuine protective function that motivated the rule and still operates. The structural data, beneficiaries with a real coordination claim, victims with real costs, and active enforcement, supports the hybrid computation, and the engine's per-seat classifications will register the divergence the two advocacy seats deny. On genealogy: the founding problem, coercion inside dependency and role protection, is corroborated as real from outside the beneficiary coalition, since even autonomy-side legislators build safeguards because they concede the risk, but its sufficiency is the contest itself, so founding_problem_status is contested rather than dead; with disappearance_verdict world_rearranges, since jurisdictions demonstrably reorganize when the ban falls, the status and verdict pairing stays short of the dead-mandate capture flag while keeping the obsolescence question honestly open. Mandatrophy is not resolved: the protective mandate is arguably as live as it ever was, and this story does not declare it spent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_mountain_vs_constructed_prohibition,
    'Is the categorical prohibition a genuine moral mountain, an expression of life''s intrinsic value that would hold regardless of enforcement, or a constructed legal constraint, maintained by enforcement, from which identifiable parties benefit?',
    'Cross-jurisdictional persistence test: track whether condemnation of assisted killing and physician practice remain unchanged in jurisdictions that repeal the legal ban. If social condemnation and professional practice track the law''s repeal, the prohibition is constructed and enforced; if they hold independently of the law, the moral-mountain reading gains support.',
    'If the mountain reading holds, the prohibition certifies as natural law and the beneficiary declarations are incidental alignments; if not, the false-summit path reclassifies it as an enforced coordination structure with cost-bearers, and the beneficiary/victim structure becomes operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_mountain_vs_constructed_prohibition, conceptual, 'Whether the sanctity prohibition is natural moral law or an enforced construction with beneficiaries.').

omega_variable(
    coercion_prevalence_and_detectability,
    'How prevalent is pressure on elderly, disabled, and economically dependent people to consent to death, and how detectable is it case-by-case? The reading''s victim-set claim rests on coercion being common and undetectable by procedure.',
    'Longitudinal study of permissive regimes'' official reports for documented and suspected coercion; elder-abuse prevalence studies in end-of-life caregiving; comparison of coercion indicators before and after legalization within jurisdictions.',
    'High undetectable coercion validates the bright-line design and the protective reading of the rule; low coercion means the categorical ban overprotects, imposing severe costs on autonomous patients to prevent a rare harm, and shifts the computed classification toward the cost-bearers'' seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_prevalence_and_detectability, empirical, 'Prevalence and detectability of end-of-life coercion.').

omega_variable(
    regressive_exit_asymmetry,
    'Does the prohibition''s practical burden fall regressively, with affluent patients buying assisted death abroad while poor and disabled patients are fully bound and their helpers prosecuted?',
    'Data on foreign-jurisdiction assisted-death travel by income; prosecution records of assisted-dying cases analyzed by class of defendant and decedent; jurisdictional comparisons of effective access.',
    'If regressive, the same rule binds different seats at different strengths, a class-differentiated constraint, which strengthens the cost-bearer seats'' divergence and complicates the reading''s claim that the burden is a universal moral discipline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regressive_exit_asymmetry, empirical, 'Whether the prohibition''s burden varies by wealth via arbitrage exit.').

omega_variable(
    victim_set_boundary_is_the_kernel_disagreement,
    'This constraint is one reading of the end_of_life_authority kernel, the sanctity_reading. The kernel''s readings disagree precisely on the victim-set boundary: this reading places the pressured-vulnerable (elderly, disabled, economically disadvantaged at coercion risk) in the victim set as the class whose victimization the categorical ban prevents, and holds the competent suffering patients'' denied preference among the costs it justifies; the autonomy_reading would dissolve the categorical bar, make the denied patients its central injured class, and re-sort the pressured-vulnerable as a safeguard-managed risk class; the slippery_slope_mechanism reading predicts the autonomy arrangement''s victim set empirically expands to incompetent and non-terminal populations. Which victim-set composition is structurally accurate is the unresolved contest.',
    'Not resolvable within this story: the readings are separate constraints with separate epsilon values and separate victim sets, linked via the network. Resolution proceeds by the corpus comparing the three stories'' computed classifications against longitudinal jurisdictional data.',
    'Adopting the autonomy reading''s victim set would move this constraint''s classification toward the payer seats'' computed type and dissolve the protective-coordination function; adopting the slippery-slope delta would extend this reading''s victim set to incompetent and non-terminal populations and strengthen the categorical design''s justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_boundary_is_the_kernel_disagreement, conceptual, 'Kernel-contest omega: the victim-set boundary is where the three readings of end_of_life_authority structurally diverge.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the prohibition''s suppressive force is structural (criminal statutes, licensing discipline, prosecution) and how much is internalized (physicians'' and patients'' internalized sanctity norms that would persist after repeal)?',
    'Post-repeal practice trajectories: in jurisdictions that legalized, track how many physicians volunteer to provide assisted death, how long conscience-clause dominance persists, and whether patient uptake remains far below eligibility. Persistence of internal restraint after the legal barrier falls indicates the internalized component.',
    'A large internalized component means the constraint''s suppression outlives its legal form, so repeal under-delivers the option the payer seats expect and the constraint''s effective force exceeds the statutory measure; a small component means the legal machinery is the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the prohibition''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t6, end_of_life_authority__sanctity_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(end__tr_t12, end_of_life_authority__sanctity_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(end__tr_t18, end_of_life_authority__sanctity_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.33).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(end__be_t6, end_of_life_authority__sanctity_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(end__be_t12, end_of_life_authority__sanctity_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(end__be_t18, end_of_life_authority__sanctity_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(end__su_t6, end_of_life_authority__sanctity_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(end__su_t12, end_of_life_authority__sanctity_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(end__su_t18, end_of_life_authority__sanctity_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The colloquial label end-of-life authority decomposes into at least three structurally distinct constraints: a categorical prohibition grounded in life's intrinsic value (this story), an autonomy-grounded right to assisted death, and an empirical expansion hypothesis about permissive frameworks. Each carries its own epsilon, victim set, and classification; they are linked here rather than merged because a single story would need observer-dependent epsilon, violating epsilon-invariance. The sanctity reading influences the slippery-slope reading, since its institutions resource and cite expansion tracking, and coexists with the autonomy reading across jurisdictions and professional coalitions; neither sibling's file should restate this one's victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
