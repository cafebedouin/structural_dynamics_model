% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection — Harm-Limited Reading
 *   domain: constitutional law/political philosophy/speech regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested First Amendment
 *   speech-protection kernel: the harm-limited reading, on which
 *   constitutional protection yields when expression causes demonstrable,
 *   unconsented-to harm. Per the epsilon-invariance discipline, the sibling
 *   readings (absolutist_reading, categorical_balancing_reading) are separate
 *   constraints in separate files; nothing about them is averaged into this
 *   story. Epsilon's referent is the standing arrangement this reading puts
 *   in place — a speech order bounded by an evidence-gated harm threshold —
 *   assessed by this reading's own lights: the reading endorses the boundary,
 *   so its authored epsilon reflects the bounded, purposeful cost the rule
 *   imposes on harm-causing expression, not the cost profile of the rival
 *   orders. Structurally the reading both coordinates (a predictable,
 *   principled line that preserves broad protection while answering real
 *   injury) and takes asymmetrically (a defined class of speakers bears
 *   liability, injunction, and sanction costs that flow to protected
 *   targets), under active judicial enforcement — hence the tangled-rope
 *   claim. Time points index years since approximately 1965 (interval 0-60
 *   approximates 1965-2025), spanning the civil-rights-era demand that
 *   targeting expression not ride on free-speech protection through
 *   contemporary harassment and disinformation-harm debates. KEY AGENTS (by
 *   structural relationship): - vulnerable_minority_targets: primary
 *   beneficiary (powerless/trapped) — receives protection and recourse at the
 *   harm boundary - minority_advocacy_organizations: secondary beneficiary
 *   (organized/mobile) — collects standing, funding, and doctrinal wins -
 *   ordinary_speakers: residual beneficiary (moderate/mobile) — retains
 *   near-full protection behind the bright line - harm_causing_speakers:
 *   primary payer (moderate/constrained) — bears liability and sanction at
 *   the boundary - federal_judiciary: agenda setter
 *   (institutional/constrained) — administers demonstrability and consent
 *   determinations - absolutist_civil_libertarians: excluded voice
 *   (organized/mobile) — categorical-protection premise foreclosed inside
 *   this frame - constitutional_scholars: analytical observer
 *   (analytical/analytical) — maps the kernel contest
 *
 * KEY AGENTS:
 *   - vulnerable_minority_targets: primary beneficiary (powerless/trapped) — receives protection and recourse at the harm boundary
 *   - minority_advocacy_organizations: secondary beneficiary (organized/mobile) — collects standing, funding, and doctrinal wins from the reading's adoption
 *   - ordinary_speakers: residual beneficiary (moderate/mobile) — keeps near-full protection behind the evidence-gated line
 *   - harm_causing_speakers: primary payer (moderate/constrained) — bears liability, injunction, and sanction costs at the boundary
 *   - federal_judiciary: agenda setter (institutional/constrained) — decides what counts as demonstrable harm and unconsented exposure
 *   - absolutist_civil_libertarians: excluded voice (organized/mobile) — their categorical premise has no standing inside this reading's framework
 *   - constitutional_scholars: analytical observer (analytical/analytical) — maps the three-reading contest and supplies the harm-evidence literature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.45).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.38).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection — Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional law/political philosophy/speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'b0cf9239-dedc-4574-b8d3-c1e2fef775eb').
narrative_ontology:cs_kernel_codification('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', fixed_text).
narrative_ontology:cs_authority_grounding('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', lineage).
narrative_ontology:cs_interpretation_layer_present('b0cf9239-dedc-4574-b8d3-c1e2fef775eb').
narrative_ontology:cs_reading_relation('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', foundational, protection_yields_on_demonstrable_unconsented_harm).
narrative_ontology:cs_axiom_status(protection_yields_on_demonstrable_unconsented_harm, holdable).
narrative_ontology:cs_axiom_grounding('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', protection_yields_on_demonstrable_unconsented_harm, deontological).
narrative_ontology:cs_axiom('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', secondary, harm_demonstrability_makes_boundary_administrable).
narrative_ontology:cs_axiom_status(harm_demonstrability_makes_boundary_administrable, holdable).
narrative_ontology:cs_axiom_grounding('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', harm_demonstrability_makes_boundary_administrable, empirically_contingent).
narrative_ontology:cs_reference_frame('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', millian_harm_bounded_liberty).
narrative_ontology:cs_drift_state('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', contemporary_us_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0cf9239-dedc-4574-b8d3-c1e2fef775eb', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minority_targets).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, minority_advocacy_organizations).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, ordinary_speakers).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, millian_harm_principle).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups routinely targeted by threatening, demeaning, or harassing expression — racial, religious, and sexual minorities, and individuals singled out for abuse. When expression directed at them causes demonstrable injury and they did not consent to the exposure, this reading gives them a principled claim to regulatory recourse: injunctions, damages, code enforcement. Leaving the discursive environment is not realistic — the targeting follows membership, not venue — so their protection depends entirely on the boundary holding.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minority_targets, beneficiary,
    powerless, generational, trapped, national).

% Civil-rights and anti-defamation organizations that litigate for and publicize harm-based speech regulation. The reading validates their core claim that expression can injure; they collect standing, funding, and doctrinal wins from its adoption without administering the boundary themselves. Their alternative — litigating under a balancing regime — is slower, less predictable, and more resource-intensive.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, minority_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% The large majority of speakers whose expression never demonstrably harms an unconsenting other. They keep essentially full protection, and the reading's bright, evidence-gated line shields them from the case-by-case value judgments a balancing regime would subject them to. Their exposure is limited to the rare occasion their own speech crosses the harm line.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, ordinary_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Speakers whose expression demonstrably injures unconsenting others — from street harassers and anonymous intimidators to broadcasters, officials, and mass platforms whose content measurably harms audiences or targeted groups. Under this reading they lose protection at the harm boundary: liability, injunctions, code sanctions. The class is heterogeneous in power and resources; the binding costs fall hardest on those without the means to relocate expression to permissive venues or to litigate the demonstrability of their harm. Exit means self-censorship at the boundary, moving to permissive forums, or contesting the harm finding itself.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers, payer,
    moderate, biographical, constrained, national).

% Courts administer the boundary: they decide what counts as 'demonstrable,' what exposure was 'unconsented,' and whether the harm shown suffices to strip protection. Every application runs through their evidentiary standards; they carry the administrability burden and the legitimacy risk of declaring expression harmful. Precedent and appointment politics constrain them; they cannot exit the adjudicative role, and each ruling reshapes the boundary for every other seat.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Speech-protective organizations and jurists holding that protection is categorical. Inside this reading's framework their premise has no standing — the harm-yield rule presupposes what they deny — so their objection registers nowhere in the arrangement's own terms, even though they remain loud in public debate and would dismantle the boundary if they held the adjudicative pen. Their practical outlet is persuasion, appointment politics, and litigation in the sibling frames.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_civil_libertarians, excluded,
    organized, generational, mobile, national).

% Academic observers who map the three readings of the speech-protection kernel, trace doctrinal drift, and supply the harm-evidence literature the boundary depends on. They collect nothing and pay nothing under the arrangement; their analyses cut both for and against the reading, and comparative work across jurisdictions informs whether the harm-bounded frame gains or loses ground.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, vulnerable_minority_targets).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the boundary problem for a free-speech order: how to protect expression broadly while giving recourse to those demonstrably injured by it. The harm threshold replaces open-ended value weighing with an evidence-gated line, giving ordinary speakers predictability about what is protected and giving targets a principled claim rather than a discretionary plea.
% TRANSFER_FUNCTION: Moves regulatory protection (and with it liability exposure and dignitary recognition) from speakers whose expression demonstrably harms unconsenting others to the targets of that expression; moves damages and sanction costs from harm-causing speakers to injured parties; moves adjudicative authority over speech injuries to courts armed with harm-evidence standards.
% ABSENT_VOICES: Absolutist civil libertarians — their categorical premise is foreclosed inside this reading's framework, so their objection has no standing within the arrangement's own terms even though they remain vocal outside it. Also absent: boundary-zone speakers who lack the resources to contest a 'demonstrability' finding against them, and communities whose harm experiences never reach adjudication because they lack access to the evidentiary process the boundary runs on.
% DISAPPEARANCE_RATIONALE: If the harm-yield rule vanished overnight, targeted groups would lose their principled recourse against demonstrably injurious expression, advocacy litigation strategies built on the reading would collapse, employment and educational harassment frameworks would lose their constitutional anchor, and the contested space would reorganize around the sibling readings — either absolutist expansion of the protected set or proliferation of case-by-case balancing.
% FOUNDING_PROBLEM: Built to solve the collision between expressive liberty and the injuries expression inflicts: the failure of purely categorical protection to answer harms borne by those with the least power to answer speech with speech. The lineage runs from Mill's treatment of liberty's limits through civil-rights-era demands that threatening and demeaning targeting not ride on free-speech protection, to contemporary harassment and disinformation-harm claims.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Mill articulated the harm limit from a liberty-first seat with no stake in modern anti-subordination claims; courts across ideological stripes concede narrow harm carve-outs (true threats, incitement, defamation), attesting the liberty/injury collision is real even while disputing this reading's breadth; and legislative fact-finding on workplace and educational harassment predates and proceeds independently of minority-advocacy sponsorship. Stated plainly: no corroborator attests that THIS reading's specific threshold (demonstrable plus unconsented) is the correct line — corroboration covers the founding problem, not the solution's correctness.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).
:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type tangled_rope: the reading has a genuine coordination function (an evidence-gated harm threshold replaces open-ended value balancing, giving ordinary speakers predictability and targets a principled claim) AND asymmetric cost-bearing through the same structure (harm-causing speakers lose protection; the gains land on targets), held together by active judicial enforcement — without courts applying the boundary, neither the coordination nor the cost allocation holds. Metrics describe actual operation: extractiveness 0.45 — real but bounded taking, gated on demonstrated harm and unconsented exposure, purposeful rather than rent-seeking; suppression 0.38 — coercive capacity concentrated at the boundary, with rival readings alive in public discourse (accessibility_collapse 0.48: granting that demonstrated harm matters collapses the absolutist alternative for anyone who accepts it, but categorical balancing remains fully available); theater_ratio 0.20 — adjudicated harm-finding is mostly functional, with a growing performative stratum of contested expert demonstration; resistance 0.62 — sustained speaker-side and civil-libertarian opposition, judicial reluctance, and academic critique. The measurement series share one grid (decades since ~1965): extraction and enforcement capacity rose as the reading gained traction from civil-rights-era demands through harassment-code and platform-era harm debates; theater rose with the expertization of harm contests. Suppression_requirement is tracked deliberately: this story's enforcement story is precisely the maturation of harm-demonstration infrastructure (social science of harassment, intimidation, and audience effects), so the enforcement-capacity trajectory is the dynamic being traced. Suppression is authored as raw structure; only extractiveness is scaled by directionality and scope downstream. Boltzmann coordination_type is enforcement_mechanism: the primary coordination function is a legal boundary administered through adjudication; the type default floor applies, no override.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From harm_causing_speakers the boundary is confiscatory at the margin — protection they held under the absolutist frame is taken the moment an adversary demonstrates harm. From vulnerable_minority_targets the same boundary is overdue protection of last resort against injury they cannot answer with more speech. From federal_judiciary it is an administrability burden — every application turns on contestable 'demonstrable' and 'unconsented' determinations, with legitimacy risk attached to each finding. From absolutist_civil_libertarians it is a category error: the kernel's text says 'no law.' Same structure, four incompatible experiences; the engine computes the divergence from power, exit, and directional data rather than from this prose.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the subsidized end: vulnerable_minority_targets (beneficiary, powerless, trapped — the boundary delivers protection they cannot obtain otherwise, d near 0.0); minority_advocacy_organizations likewise derive low d as collectors of standing and doctrinal wins; ordinary_speakers derive low d strongly, since the bright line shields them from discretionary balancing while costing them almost nothing. harm_causing_speakers carry the payer role with constrained exit — they cannot leave the jurisdiction's speech order and their expression is the regulated object — placing them near the full-target end (d near 1.0), where effective extraction is amplified. federal_judiciary administers without collecting rents; its d sits mid-range, reflecting cost-bearing (administrability, legitimacy risk) without receipt. National spatial scope modestly amplifies verification difficulty for harm claims. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabels. Proponents narrate the reading as pure coordination: nobody loses except those injuring others. Opponents narrate it as pure censorship: a license to suppress disfavored speech. Both erase structure. Tangled rope holds both truths: the coordination function is real (predictability for the many, recourse for the targeted) and the taking is real (a defined speaker class pays through the same line that coordinates everyone else), sustained by active enforcement. Mandatrophy: the founding problem — expressive liberty colliding with injuries the injured cannot answer — remains live (founding_problem_status: live), so no zombie declaration fires; the status x verdict pair (live x world_rearranges) raises no capture flag. The risk to watch is drift, not obsolescence: if harm-demonstrability degrades into discretionary assertion (see omega harm_demonstrability_contest), the same structure would operate increasingly as suppression with coordination as cover. Fixing-cost note: removing the boundary (reverting toward absolutism) would be prohibitive for whoever held the pen — decades of reliance by protected classes, an entrenched harm-evidence adjudicative apparatus, and the political cost of stripping recognized recourse — hence fixing_cost: prohibitive alongside a named receipt seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the first_amendment_speech_protection kernel — what structurally changes if a sibling reading is classified instead?',
    'Comparative classification across the three reading files (absolutist_reading, categorical_balancing_reading, harm_limited_reading): same referent topic, divergent beneficiary/victim sets and epsilon values.',
    'Under the absolutist reading the harm-yield taking disappears entirely (epsilon near zero for the protection rule itself; the payer class shrinks to speakers inside narrow historical carve-outs); under categorical balancing the taking distributes continuously across case-by-case judgments instead of concentrating at a demonstrated-harm threshold. Resolution happens by cross-file comparison, never by adjusting this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: this file is one of three readings; siblings are separate constraints.').

omega_variable(
    harm_demonstrability_contest,
    'Can ''demonstrable'' harm be established reliably enough that the boundary tracks real injury rather than adversarial expert assertion?',
    'Longitudinal audit of adjudicated harm findings against subsequent replication and meta-analysis of the underlying evidentiary base.',
    'If demonstrability fails, the boundary becomes a discretionary instrument — effective extraction rises and operation drifts toward coerced silence with coordination as cover; if robust, the taking stays bounded and the tangled-rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstrability_contest, empirical, 'Reliability of the evidentiary gate that defines this reading.').

omega_variable(
    unconsented_exposure_scope,
    'What counts as unconsented-to exposure — does entering or remaining in a shared discursive space count as consent, shrinking the regulated set?',
    'Conceptual analysis anchored in adjudicated cases: compare outcomes where courts treated mere presence in a forum as consent against outcomes treating exposure imposed on captive or targeted audiences as unconsented.',
    'Broader consent definitions shrink the payer class and lower epsilon; narrower definitions expand both. The reading''s reach is underdetermined by its own formula until this is settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconsented_exposure_scope, conceptual, 'Scope ambiguity in the consent half of the harm boundary.').

omega_variable(
    viewpoint_capture_risk,
    'Does the harm boundary operate viewpoint-neutrally, or does enforcement concentrate on speakers holding disfavored views?',
    'Audit of enforced harm findings and sanctioned speakers by viewpoint and speaker class across jurisdictions that adopt harm-based limits.',
    'Concentrated enforcement would indicate majoritarian capture riding on the coordination form — effective operation shifting toward enforced silence despite the genuine boundary; neutral enforcement supports the dual-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viewpoint_capture_risk, empirical, 'Whether the boundary''s costs concentrate on disfavored speakers.').

omega_variable(
    doctrine_gap_trajectory,
    'Will US doctrine converge toward the harm-bounded reference frame (expanding regulable ground around demonstrated harm) or recede further from it?',
    'Track doctrinal development: new unprotected-category rulings, harassment-doctrine expansion, and how courts treat empirically demonstrated speech harms.',
    'Convergence restores the reading''s grip on practice; recession confines it to academic advocacy and eventually dates its reference frame — changing drift-state inputs and long-run classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_gap_trajectory, empirical, 'Trajectory of the gap between the Millian frame and actual doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'First Amendment speech protection.' The label conflates three structurally distinct claims with different epsilon, beneficiary structures, and failure modes: absolutist_reading (categorical protection; negligible taking from speakers, maximal exposure for targeted listeners), categorical_balancing_reading (continuous case-by-case tradeoffs; distributed, discretionary taking), and this harm_limited_reading (threshold taking at a demonstrated-harm boundary; concentrated on harm-causing speakers, delivering protection to vulnerable targets). This file instantiates the third member. Upstream members influence it: absolutist carve-outs (true threats, incitement, defamation) define the residual protected core this reading contracts, and balancing jurisprudence supplies the adjudicative habits the threshold displaces. Family links run through network edges in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
