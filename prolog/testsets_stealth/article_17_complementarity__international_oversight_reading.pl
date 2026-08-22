% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity as Accountability Trigger (International Oversight Reading)
 *   domain: international law / criminal justice / state sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute makes the International Criminal Court a
 *   court of last resort: cases are inadmissible where a state with
 *   jurisdiction is willing and able genuinely to investigate or prosecute.
 *   This story instantiates ONE reading of that clause — the international
 *   oversight reading — under which complementarity operates as an
 *   accountability trigger: a low admissibility threshold, intervention
 *   whenever domestic proceedings lack independence or genuine intent,
 *   intensified cooperation demands, and a victim set expanded to include
 *   defendants processed through symbolic or sham prosecutions. The
 *   colloquial label covers two structurally distinct arrangements; per the
 *   epsilon-invariance principle this file authors epsilon only for the
 *   oversight reading, and the sibling file
 *   (article_17_complementarity__national_primacy_reading) authors the
 *   sovereignty-protection reading with its own epsilon, beneficiaries, and
 *   victims. The two are linked through network.affects_constraints as a
 *   constraint family. Beneficiaries under this reading are atrocity
 *   survivors in complicit or failed states, advocacy networks, and
 *   transitional governments shedding uncarryable dockets; the parties who
 *   pay are complicit executives, national judiciaries whose work is
 *   second-guessed, and accused persons facing parallel exposure.
 *
 * KEY AGENTS:
 *   - international_criminal_court: agenda setter ([institutional]/[constrained]) — administers the admissibility gate and collects the transferred authority
 *   - atrocity_survivor_communities: primary beneficiary ([powerless]/[trapped]) — the accountability claim the mechanism serves
 *   - complicit_state_executives: primary target ([institutional]/[constrained]) — lose case control when proceedings are found non-genuine
 *   - national_judiciaries_of_targeted_states: secondary target ([moderate]/[identity_locked]) — professional identity fused with national legal sovereignty
 *   - accused_perpetrators: direct target ([powerful]/[constrained]) — liberty and forum exposure ride on which court tries them
 *   - un_security_council: selective enforcer ([institutional]/[arbitrage]) — refers and defers while remaining largely insulated
 *   - nonparty_great_powers: excluded objectors ([powerful]/[arbitrage]) — reached at the margins without ever consenting
 *   - human_rights_advocacy_networks: mobilized beneficiary ([organized]/[mobile]) — supplies evidence, communications, and arrest pressure
 *   - post_conflict_transitional_governments: incidental beneficiary ([moderate]/[constrained]) — offload dockets they cannot carry
 *   - comparative_international_lawyers: analytical observer ([analytical]/[analytical]) — maps the jurisprudence without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.52).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity as Accountability Trigger (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international law / criminal justice / state sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'c56a2698-1742-42e7-97c6-e3c282373d62').
narrative_ontology:cs_kernel_codification('c56a2698-1742-42e7-97c6-e3c282373d62', fixed_text).
narrative_ontology:cs_authority_grounding('c56a2698-1742-42e7-97c6-e3c282373d62', lineage).
narrative_ontology:cs_interpretation_layer_present('c56a2698-1742-42e7-97c6-e3c282373d62').
narrative_ontology:cs_reading_relation('c56a2698-1742-42e7-97c6-e3c282373d62', article_17_complementarity__national_primacy_reading, forecloses).
narrative_ontology:cs_axiom('c56a2698-1742-42e7-97c6-e3c282373d62', foundational, domestic_impunity_triggers_international_jurisdiction).
narrative_ontology:cs_axiom_status(domestic_impunity_triggers_international_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('c56a2698-1742-42e7-97c6-e3c282373d62', domestic_impunity_triggers_international_jurisdiction, deontological).
narrative_ontology:cs_axiom('c56a2698-1742-42e7-97c6-e3c282373d62', foundational, sham_proceedings_equivalent_to_nonprosecution).
narrative_ontology:cs_axiom_status(sham_proceedings_equivalent_to_nonprosecution, holdable).
narrative_ontology:cs_axiom_grounding('c56a2698-1742-42e7-97c6-e3c282373d62', sham_proceedings_equivalent_to_nonprosecution, empirically_contingent).
narrative_ontology:cs_reference_frame('c56a2698-1742-42e7-97c6-e3c282373d62', no_impunity_guardianship_framework).
narrative_ontology:cs_drift_state('c56a2698-1742-42e7-97c6-e3c282373d62', contemporary_admissibility_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c56a2698-1742-42e7-97c6-e3c282373d62', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_survivor_communities).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, post_conflict_transitional_governments).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_state_executives).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_judiciaries_of_targeted_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, accused_perpetrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, no_impunity_principle).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, individual_criminal_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 17 and rules on admissibility challenges; opens investigations on state or Security Council referral or on its own initiative; issues warrants and requests surrender. Has no police force: every arrest, every piece of evidence, and every unit of funding depends on state cooperation. Each situation it accepts enlarges its docket, staffing, and standing.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, international_criminal_court, beneficiary).

% Live in the territories where mass atrocities occurred, under governments that participated in, ordered, or shielded the violence. Domestic courts offer no realistic path to accountability. Participation runs through victim counsel and the victims' trust fund; relocating away from the local order the proceedings concern is rarely possible.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_survivor_communities, beneficiary,
    powerless, biographical, trapped, regional).

% Document atrocities, file communications, brief the chambers, and campaign for arrests. Operate across borders with staff and funders distributed worldwide; their caseload and donor base grow with each situation the court accepts.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Inherit collapsed courts, destroyed archives, and insecure witness pools after civil war. Trying atrocity cases domestically would exceed their capacity for years; handing situations to the international forum relieves them of cases they cannot carry while keeping cooperation channels and donor relationships open.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, post_conflict_transitional_governments, beneficiary,
    moderate, biographical, constrained, national).

% Ordered, tolerated, or shielded the atrocities, then staged domestic proceedings calculated to close the door on outside review. When the international chamber finds those proceedings not genuine, they lose custody of the case, face surrender requests touching officials and allies, and weigh treaty withdrawal against reputational and financial costs.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_state_executives, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, complicit_state_executives, agenda_setter).

% Judges and prosecutors whose dockets are re-examined by an external chamber. Their careers, pensions, and professional standing sit inside the national legal order; a finding that their work lacks independence or genuine intent lands as an institutional demotion of the profession they belong to.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_judiciaries_of_targeted_states, payer,
    moderate, biographical, identity_locked, national).

% Commanders and officials facing warrants. Patron states shelter them day to day, but warrant listings shrink their travel, freeze assets, and bar them from forums they once frequented. Which forum tries them determines defense strategy, sentence exposure, and legacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, accused_perpetrators, payer,
    powerful, biographical, constrained, continental).

% Can refer situations arising in territories of states that never joined the treaty, and can defer any case for renewable twelve-month periods. Permanent members use these levers selectively; their own personnel and client states stay largely out of reach absent consent.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Never ratified the statute. Broad readings of unwillingness and inability reach their soldiers, intelligence officers, and partner governments through referrals and territory-based jurisdiction. Their objections arrive as funding cuts, visa restrictions, and bilateral pressure rather than as arguments inside the interpretive process.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, nonparty_great_powers, excluded,
    powerful, generational, arbitrage, global).

% Track admissibility rulings across chambers and decades, mapping how threshold interpretations move authority between national and international levels. Publish critiques and defenses of the guardianship frame; collect nothing and pay nothing in the proceedings.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, comparative_international_lawyers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates adjudicative jurisdiction over genocide, crimes against humanity, and war crimes between national and international levels, guaranteeing a backstop forum when the territorial system is complicit, collapsed, or captured — a forum-allocation problem solved once centrally instead of ad hoc after each atrocity.
% TRANSFER_FUNCTION: Moves case control and adjudicative authority from national systems to the international court when genuineness fails; moves surrender obligations and cooperation costs onto states; moves liberty and reputational costs onto officials and commanders previously shielded by office.
% ABSENT_VOICES: Non-party great powers and the constituencies of accused persons would object that the reading binds those who never consented and reopens matters national systems deemed closed; governments charging selectivity press the same objection from another direction. They appear only as resistance — funding cuts, withdrawal, non-cooperation — not as voices inside the interpretive process, which is staffed by the court, states parties, and advocacy coalitions.
% DISAPPEARANCE_RATIONALE: If the oversight reading vanished overnight, every pending sham prosecution would stand as final, victors' justice would consolidate in each post-conflict state, survivors in complicit states would lose their forum of last resort, and the cooperation architecture of warrants, surrender requests, and non-cooperation referrals would dissolve — the accountability order would reorganize around purely voluntary state consent.
% FOUNDING_PROBLEM: Post-Nuremberg and post-Cold-War atrocity impunity: heads of state and commanders answered to no one when territorial courts were complicit or destroyed, as Yugoslavia and Rwanda demonstrated in the 1990s.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by Security Council and Human Rights Council commissions of inquiry (Darfur, Syria, Myanmar), the jurisprudential record of the ad hoc tribunals for Yugoslavia and Rwanda, and the historical consensus that produced the Rome Conference. Survivor and advocacy attestations exist, but the status does not rest on them.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the reading transfers case control, surrender obligations, and reputational cost onto identifiable state actors whenever genuineness fails, and the threshold sits low enough that intervention is routine rather than exceptional. Suppression is 0.52 as a raw structural property — unscaled by power or scope in the engine's arithmetic: the cooperation regime, warrant listings, and the non-party referral path coerce compliance, but genuine domestic prosecution remains an open alternative and treaty withdrawal exists, so alternatives are narrowed rather than closed. Accessibility collapse is correspondingly moderate (0.42) and resistance high (0.72): Kenya, Burundi, the Philippines, and several great powers pushed back openly. Theater ratio 0.34 reflects a real trigger function increasingly accompanied by symbolic activity — situations opened without arrest prospects, complementarity litigation consumed for signaling. All three temporal series run on one shared seven-point grid spanning 2002-2026 (statute entry into force to present); endpoint values equal the base_properties scalars. gain_flow names the court itself: the transferred authority, docket, and budget demonstrably accrue to that seat, which is why it carries a beneficiary secondary role alongside agenda_setter. fixing_cost is prohibitive: reversing the reading requires either treaty amendment by seven-eighths of states parties or a judicial about-face against an entrenched interpretive coalition — costs exceeding the diffuse benefit to any would-be fixer.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the court's seat the arrangement is a guardianship it administers in good faith; from complicit executives' seat it is an external seizure of a case they had closed; from national judiciaries' seat it is a verdict on their profession's integrity issued by outsiders; from survivors' seat it is the only functioning path to accountability. One nominal legal order, four different lived constraints. The engine computes per-seat classifications from power, exit, and role data; divergence between the payer seats and the beneficiary/agenda seats is the expected output, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Survivor communities, advocacy networks, and transitional governments are declared beneficiaries — d sits near the beneficiary end, and effective extraction dampens or inverts toward subsidy for them. Complicit executives, national judiciaries, and accused perpetrators are declared victims — d sits near the target end, amplified by constrained or identity-locked exit: executives cannot shed treaty obligations cheaply, judiciaries cannot exit their professional identity, accused cannot exit warrant reach. The court holds an agenda-setter seat with a beneficiary secondary role — it both runs the gate and collects the gains. The Security Council's arbitrage position (wields the tool, largely immune from it) and the non-party powers' excluded position are carried structurally; no directionality_overrides are needed because the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereign impunity for atrocity, demonstrated from Nuremberg through the 1990s — is live, so no mandatrophy declaration is authored and no sunset clause exists. The classification guards against mislabeling in both directions: reading the arrangement as pure coordination ignores that its costs concentrate on identifiable parties (executives, judiciaries, accused) while its benefits spread diffusely; reading it as pure extraction ignores that it solves a real collective-action problem no national system can solve alone — the impunity gap. Tangled rope is the honest middle: genuine coordination function, asymmetric incidence, active enforcement required. Should the founding problem die — genuine domestic prosecution capacity emerging globally — the arrangement would drift toward piton, maintained theatrically by an interpretive community whose identity fuses with the guardianship role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of kernel article_17_complementarity (reading: international_oversight_reading). What structurally changes under the sibling national_primacy_reading?',
    'Author the sibling story with its own epsilon, beneficiary set (state executives, national judiciaries), higher admissibility threshold, and reversed burden; compare computed classifications across the pair.',
    'Under the sibling reading epsilon drops substantially, the victim set narrows to proven-sham scenarios, and the classification trends toward a protection-framed profile; cross-reading comparison isolates how much of this story''s extraction is reading-indexed rather than text-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-indexed classification of a shared fixed text.').

omega_variable(
    genuineness_standard_objectivity,
    'Can lack of independence, impartiality, or genuine intent be assessed without importing the reviewing body''s own political and professional values?',
    'Cross-chamber consistency analysis of admissibility outcomes on comparable fact patterns; inter-rater reliability studies across benches and eras.',
    'If the standard is indeterminate, threshold-height disputes reduce to preference, and the effective threshold becomes a function of who reviews rather than of the text — raising theater and destabilizing every seat''s computed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_standard_objectivity, conceptual, 'Whether the genuineness standard is determinate enough to anchor a stable threshold.').

omega_variable(
    threshold_vs_cooperation_binding_constraint,
    'Is the mechanism''s effectiveness limited by the admissibility threshold height or by arrest and cooperation capacity?',
    'Compare deterrence and outcome rates across situations that differ in cooperation posture but not threshold treatment; natural experiments from non-cooperation episodes (Kenyatta, Al-Bashir travel).',
    'If cooperation is the binding constraint, lowering the threshold further adds symbolic rather than functional activity — theater_ratio rises without accountability gains, and the piton drift hypothesis strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vs_cooperation_binding_constraint, empirical, 'Which bottleneck — threshold or enforcement capacity — governs real-world effect.').

omega_variable(
    enforcement_selectivity_asymmetry,
    'Does the broad reading apply symmetrically across powerful and weak states, or does effective intervention concentrate on weak-state elites while powerful-state impunity persists?',
    'Audit of situations opened versus declined, and warrants executed versus outstanding, coded by target-state power and great-power alignment.',
    'Asymmetric application concentrates effective extraction on weak-state seats while strong-state seats approach immunity, redistributing chi across the stakeholder surface and feeding the selectivity objection that drives resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_asymmetry, empirical, 'Whether the guardianship operates uniformly or tracks power asymmetries.').

omega_variable(
    duplicate_proceeding_cost_allocation,
    'How much of the burden falling on accused persons is legitimate accountability versus duplicative or redundant prosecution cost across forums?',
    'Track ne bis in idem applications and cumulative-exposure outcomes for defendants touched by both national and international proceedings.',
    'Shifts the accused seat''s effective extraction between a justified component (accountability) and a wasteful component (duplication), changing whether that seat''s costs support or undercut the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duplicate_proceeding_cost_allocation, preference, 'Normative weighting of parallel-proceeding burdens on the accused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_ovsrd_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t0, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t4, article_17_complementarity__international_oversight_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t4, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t8, article_17_complementarity__international_oversight_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t8, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t12, article_17_complementarity__international_oversight_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t12, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t16, article_17_complementarity__international_oversight_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t16, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t20, observed).
narrative_ontology:measurement(art17_ovsrd_tr_t24, article_17_complementarity__international_oversight_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(art17_ovsrd_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(art17_ovsrd_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(art17_ovsrd_be_t0, observed).
narrative_ontology:measurement(art17_ovsrd_be_t4, article_17_complementarity__international_oversight_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(art17_ovsrd_be_t4, observed).
narrative_ontology:measurement(art17_ovsrd_be_t8, article_17_complementarity__international_oversight_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(art17_ovsrd_be_t8, observed).
narrative_ontology:measurement(art17_ovsrd_be_t12, article_17_complementarity__international_oversight_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(art17_ovsrd_be_t12, observed).
narrative_ontology:measurement(art17_ovsrd_be_t16, article_17_complementarity__international_oversight_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(art17_ovsrd_be_t16, observed).
narrative_ontology:measurement(art17_ovsrd_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(art17_ovsrd_be_t20, observed).
narrative_ontology:measurement(art17_ovsrd_be_t24, article_17_complementarity__international_oversight_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(art17_ovsrd_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(art17_ovsrd_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(art17_ovsrd_su_t0, observed).
narrative_ontology:measurement(art17_ovsrd_su_t4, article_17_complementarity__international_oversight_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(art17_ovsrd_su_t4, observed).
narrative_ontology:measurement(art17_ovsrd_su_t8, article_17_complementarity__international_oversight_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(art17_ovsrd_su_t8, observed).
narrative_ontology:measurement(art17_ovsrd_su_t12, article_17_complementarity__international_oversight_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement_basis(art17_ovsrd_su_t12, observed).
narrative_ontology:measurement(art17_ovsrd_su_t16, article_17_complementarity__international_oversight_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement_basis(art17_ovsrd_su_t16, observed).
narrative_ontology:measurement(art17_ovsrd_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(art17_ovsrd_su_t20, observed).
narrative_ontology:measurement(art17_ovsrd_su_t24, article_17_complementarity__international_oversight_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(art17_ovsrd_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 17 complementarity' decomposes into two structurally distinct readings of one fixed text. This file (international_oversight_reading) authors epsilon for the accountability-trigger arrangement: low threshold, prosecutor-favorable burden, expanded victim set including sham-prosecution scenarios. The sibling (national_primacy_reading) authors epsilon for the sovereignty-protection arrangement: presumptive national adequacy, state-favorable burden, narrow victim set. The coupling runs through shared admissibility jurisprudence: each chamber ruling citing the oversight frame raises the sibling reading's repudiation pressure, and each state-pushback victory for the primacy frame raises this reading's drift magnitude. The two files must be read as a pair; neither epsilon is valid for the other's constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
