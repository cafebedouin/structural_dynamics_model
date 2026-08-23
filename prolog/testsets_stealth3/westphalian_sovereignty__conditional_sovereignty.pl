% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Responsibility-to-Protect Threshold Doctrine
 *   domain: international law/political philosophy/global governance
 *
 * SUMMARY:
 *   The Westphalian sovereignty kernel, the norm that states hold supreme
 *   authority within their territory and external interference is
 *   illegitimate, is read three ways in contemporary international order.
 *   This story instantiates the conditional reading: sovereignty entails
 *   responsibility to one's population, and when a state manifestly fails to
 *   protect against genocide, ethnic cleansing, or crimes against humanity,
 *   the international community may act, coercively where necessary, through
 *   Security Council authorization. Codified after the ICISS report (2001) in
 *   the 2005 World Summit Outcome paragraphs 138-139, the reading converts
 *   the absolute non-interference shield into a conditional grant whose
 *   threshold is adjudicated, in practice, by the five veto-holding permanent
 *   members of the Security Council. The structural consequence:
 *   intervention-capable powers and the humanitarian advocacy sector gain
 *   legitimating doctrine and agenda-setting influence, while every sovereign
 *   state becomes a conditional duty-bearer whose internal conduct is
 *   permanently eligible for international review. The doctrine's protective
 *   content is genuine (the absolute reading demonstrably shielded Rwanda and
 *   Srebrenica) and its adjudication asymmetry is equally genuine: the states
 *   most exposed to the threshold have no vote on it, and the states that
 *   administer it have exempted themselves from it. This file is one reading
 *   of a contested kernel; the absolute and graduated readings are separate
 *   constraints, linked in network.affects_constraints, with their own
 *   extraction profiles. KEY AGENTS (by structural relationship): -
 *   unsc_permanent_members: agenda setter (institutional/arbitrage) —
 *   adjudicates the atrocity threshold through veto-holding Council control;
 *   self-exempt from the conditionality it administers -
 *   intervention_coalition_leaders: primary beneficiary (powerful/arbitrage)
 *   — collect legitimating doctrine and operational freedom; may decline to
 *   act at will - humanitarian_advocacy_organizations: secondary beneficiary
 *   (organized/mobile) — documentation gains legal standing; funding and
 *   influence flow to the evidentiary arm - weak_sovereign_states: primary
 *   target (moderate/constrained) — bear permanent eligibility for
 *   international review of internal conduct - pariah_target_states:
 *   most-exposed target (powerless/trapped) — the states the threshold
 *   machinery is aimed at; isolation and justification compound -
 *   at_risk_civilian_populations: protected class, dual position
 *   (powerless/trapped) — intended beneficiaries when action works,
 *   cost-bearers when it fails or never comes - global_south_regional_powers:
 *   resistant payers (organized/constrained) — bear precedent risk; organized
 *   the responsibility-while-protecting corrective after Libya -
 *   international_humanitarian_law_scholars: analytical observer
 *   (analytical/analytical) — sees the full structure from professional
 *   distance - nonconsenting_target_populations: excluded seat
 *   (powerless/trapped) — the civilians whose protection is invoked hold no
 *   vote in any body deciding their case
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.55).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Responsibility-to-Protect Threshold Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international law/political philosophy/global governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '3d9f7837-15d1-435d-a073-549f1a176cc2').
narrative_ontology:cs_kernel_codification('3d9f7837-15d1-435d-a073-549f1a176cc2', formalized).
narrative_ontology:cs_authority_grounding('3d9f7837-15d1-435d-a073-549f1a176cc2', extraction).
narrative_ontology:cs_interpretation_layer_present('3d9f7837-15d1-435d-a073-549f1a176cc2').
narrative_ontology:cs_reading_relation('3d9f7837-15d1-435d-a073-549f1a176cc2', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('3d9f7837-15d1-435d-a073-549f1a176cc2', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('3d9f7837-15d1-435d-a073-549f1a176cc2', foundational, sovereignty_entails_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('3d9f7837-15d1-435d-a073-549f1a176cc2', sovereignty_entails_responsibility_to_protect, deontological).
narrative_ontology:cs_axiom('3d9f7837-15d1-435d-a073-549f1a176cc2', secondary, atrocity_threshold_suspends_non_intervention).
narrative_ontology:cs_axiom_status(atrocity_threshold_suspends_non_intervention, holdable).
narrative_ontology:cs_axiom_grounding('3d9f7837-15d1-435d-a073-549f1a176cc2', atrocity_threshold_suspends_non_intervention, instrumental).
narrative_ontology:cs_reference_frame('3d9f7837-15d1-435d-a073-549f1a176cc2', sovereignty_as_responsibility_framework).
narrative_ontology:cs_drift_state('3d9f7837-15d1-435d-a073-549f1a176cc2', post_libya_overreach_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3d9f7837-15d1-435d-a073-549f1a176cc2', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_coalition_leaders).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, humanitarian_advocacy_organizations).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, weak_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, pariah_target_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, global_south_regional_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, sovereignty_as_responsibility).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_rights_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments holding permanent seats and veto power over Council action. They decide, case by case, whether a state's conduct crosses the threshold that opens its internal affairs to collective measures, and each can block any measure directed at itself or its allies. Which situations get named, which get sanctioned, and which get force authorizations is settled in their chamber. Exit from the arrangement is not a question for them: they wrote its terms and can block any revision of them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments with the military and logistical capacity to conduct humanitarian operations abroad. When the Council authorizes action they execute it; when it does not, some have acted anyway under claimed humanitarian necessity. The doctrine supplies them a legitimating vocabulary for projecting force, and invoking it costs them little at home when operations go well. Their exit is easy: they may simply decline to act, as most did over Syria.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_coalition_leaders, beneficiary,
    powerful, biographical, arbitrage, global).

% International NGOs, commissions of inquiry, and advocacy networks that document atrocities and campaign for external response. The doctrine converts their documentation into legal standing: their reports feed threshold determinations and their campaigns gain a recognized hook in Council debates. Funding, access, and institutional influence flow to organizations positioned as the arrangement's evidentiary arm. They can redirect effort to other causes at will.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, humanitarian_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% Small and mid-sized states without nuclear weapons, permanent seats, or patron protection. Their domestic conduct is permanently eligible for international review; aid, loans, and diplomatic standing arrive with governance and human-rights conditions attached. Leaving the arrangement would mean leaving the UN system, international finance, and treaty networks, a cost no state has paid. They defend the older non-interference norm diplomatically but lack the weight to restore it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, weak_sovereign_states, payer,
    moderate, generational, constrained, global).

% Governments under sanctions or facing credible discussion of external action, the states the threshold machinery is actually aimed at. They hold no veto, command no coalition, and have no exit: the more they are isolated, the more their internal conduct is cited as grounds for the isolation. Their populations often suffer under both their rule and the measures taken against it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, pariah_target_states, payer,
    powerless, biographical, trapped, regional).

% Civilians in states undergoing atrocity or civil war, for whose protection the doctrine is nominally exercised. When authorized action arrives in time and stays limited, they receive the protection the arrangement promises; when action arrives late, overshoots into regime change, or never arrives, they bear the consequences, as in Libya's collapse or Syria's abandoned besieged cities. They cannot leave the territory in question except as refugees, and they hold no vote in any body deciding their case.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations, payer).

% Larger developing states such as Brazil, India, and South Africa, with regional weight but no permanent veto. They accepted a narrowed version of the doctrine at the 2005 World Summit while organizing against its open-ended use: after Libya they sponsored the responsibility-while-protecting initiative demanding Council oversight of mandated operations. They carry the precedent risk that a normalized doctrine will someday be applied to their own internal conduct; their exit is the same as any state's, theoretically available and practically ruinous.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, global_south_regional_powers, payer,
    organized, generational, constrained, continental).

% Academics, jurists, and UN legal advisers who track the doctrine's codification, invocation, and erosion. They see the whole structure: the Charter text, the 2005 paragraphs, the practice record from Kosovo through Ukraine. Their stake is reputational and professional; they can study the arrangement from any jurisdiction and owe it nothing.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_humanitarian_law_scholars, observer,
    analytical, biographical, analytical, global).

% Civilians in states under external-action debate whose consent is claimed by everyone and polled by no one. Advocates invoke their protection; their own government claims to speak for them; neither the Council nor any coalition consults them. They appear in the record as the people being protected while bearing the actual costs of sieges, sanctions, and strikes.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, nonconsenting_target_populations, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, unsc_permanent_members).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the international system a conditional, adjudicated mechanism for responding to genocide, ethnic cleansing, and crimes against humanity when the territorial state is unwilling or unable to act, pooling threshold determination in the Security Council and operational capacity in member coalitions in place of the pure non-interference rule that shielded perpetrators.
% TRANSFER_FUNCTION: Moves decision authority over a state's internal conduct from that state to the Security Council and intervention-capable powers whenever the atrocity threshold is adjudicated met; moves the costs of coercive action onto target-state territory and populations; confers legitimating doctrine and agenda-setting influence on the adjudicating and intervening seats.
% ABSENT_VOICES: The civilians whose protection is invoked hold no seat: the Council speaks for them, their government claims to speak for them, and neither consults them (the nonconsenting_target_populations seat). Smaller states without Council membership likewise have no vote on the threshold that converts their own sovereignty; the 2005 bargain was negotiated by a wider but still unrepresentative assembly.
% DISAPPEARANCE_RATIONALE: Peacekeeping mandates, targeted-sanctions regimes, ad hoc criminal tribunals, and every post-2005 Council resolution invoking protection hang on the conditional reading. If it vanished overnight, ongoing mandates would lose their legal basis, the absolute non-interference reading would govern by default, atrocity response would revert to unauthorized coalition politics, and weak states would regain the absolute shield while losing the uneven protection it purchases.
% FOUNDING_PROBLEM: Absolute sovereignty shielded mass atrocity: the Holocaust proceeded under non-interference norms, and Rwanda 1994 and Srebrenica 1995 showed the shield still operating half a century later. The founding problem was how to keep sovereignty's peace-preserving function while removing its use as a shield for systematic crimes against a state's own population.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Global South governments that bear the autonomy costs accepted the narrowed 2005 codification (Brazil, India, South Africa and the African Union endorsed paragraphs 138-139 while disputing open-ended application), attesting the underlying problem from a paying seat. The independent post-Rwanda and post-Srebrenica inquiry record documents the founding failure. No seat outside the beneficiary set attests that the doctrine as practiced solves the problem; the corroborating sources attest the problem while disputing the remedy.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.40 (moderate): the arrangement transfers real decision authority (a state's internal conduct becomes internationally reviewable once the threshold is adjudicated met), but its protective content is genuine and the reading's own tradition registers the asymmetric adjudication as a defect rather than a feature. Suppression 0.55, authored as a raw structural property (the engine scales only extractiveness): the coercive machinery (sanctions regimes, force authorizations, criminal referrals, conditionality in finance) is real and state exit from the normative order is practically unavailable, but veto politics blocks much enforcement and application is selective. Theater 0.45: a substantial performative layer (never-again rhetoric, commissions whose findings go unenforced, protection language appropriated for non-protective ends as in Crimea 2014) sits atop real function (peacekeeping deployments, the initial civilian-protection effect in Libya 2011, sanctions that bind). Accessibility_collapse 0.55: alternatives exist (nuclear deterrence, patron alliances, regional blocs) but no state can practically exit the UN-centered normative order, and the absolute reading's shield is no longer available to any government that accepted the 2005 paragraphs. Resistance 0.60: organized sovereignty-defense diplomacy is a standing feature of the system (the responsibility-while-protecting initiative, BRICS and Global South pushback, the post-Libya freeze on new protection mandates).
 *   
 *   Claim: tangled_rope, refining the manifest's snare hypothesis. The claim is authored from structural belief, the metrics from descriptive belief, and neither was tuned to the other or to a predicted engine output. The coordination function is not cover: the absolute reading demonstrably shielded Rwanda and Srebrenica, and the conditional reading's protective content was accepted at the 2005 Summit by the very states that bear its autonomy costs. Decompose test: strip the adjudication asymmetry and a genuine protective coordination mechanism remains, which is hybrid structure rather than cover. But the asymmetry is real: the threshold's operative meaning is set by five self-exempt governments and the costs fall on states without vetoes. Both tangled_rope conditions hold: genuine coordination and asymmetric extraction through the same structure, actively enforced.
 *   
 *   Temporal arc on one shared grid: a codification ratchet from 1999 to 2011 (Kosovo practice, ICISS, the 2005 codification, the first full exercise in Libya) raises extraction toward its 2014 peak; the post-Libya backlash then narrows application and erodes the doctrine's credibility, with theater peaking in 2017 as rhetoric detaches from action before partially receding. Suppression_requirement is tracked because the story's enforcement machinery genuinely changed: it built through 2011, then its legitimacy eroded and application narrowed after the Libya overreach. The arc is rise-peak-partial-retreat, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the P5 seat the arrangement is a coordination mechanism it administers: it wrote the terms, holds the veto, and bears almost none of the conditionality. From the pariah-state seat the same arrangement is enforced exposure with no exit: the more isolated the state, the more its conduct justifies the isolation. Weak states without vetoes compute a standing conditionality accepted at a bargaining table they did not control. At-risk populations compute protection whose arrival, scope, and aftermath are decided entirely elsewhere: beneficiaries when it works, cost-bearers when it fails. Global South regional powers compute precedent risk: a normalized doctrine aimed today at pariahs could cite their own internal conduct tomorrow. Same-level dynamics matter here: all states are formal sovereign equals under the Charter, yet capacity differentiates exit completely (permanent members hold vetoes, regional powers hold organization and size, small states hold neither), so identical legal status produces radically different exposure. The engine derives this per-seat divergence from the role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: the P5 (collecting discretionary adjudication authority), coalition leaders (collecting legitimating doctrine), advocacy organizations (collecting standing and funding), and at-risk populations (the intended protected class) sit at the beneficiary end. Victim declarations: weak states, pariah states, and Global South regional powers sit at the target end, with pariah states highest (powerless and trapped, the doctrine's actual aiming point) and Global South powers high but moderated by organized capacity. Exit modulation: P5 arbitrage (they can always block application to themselves) pushes them to the extreme beneficiary end; trapped pariah states push to the extreme target end. Spatial scope is global, so verification of systematic violations is contested at planetary scale, which amplifies effective extraction modestly for all target seats. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate every seat the derivation must distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty shielding mass atrocity) is live (Myanmar, Tigray, Sudan, Syria), so the doctrine has not outlived its function and the mandate is not resolved. The classification prevents two mislabels. Reading the arrangement as pure coordination would erase the adjudication asymmetry: who decides systematic is five self-exempt governments, and that fact does real structural work. Reading it as pure extraction would erase the protective content the absolute reading's failures demonstrated; Rwanda is the counterfactual the doctrine was built against. The tangled_rope classification holds both. The theater trajectory is the early-warning for mandatrophy: if the founding problem were genuinely solved while the doctrine's rhetoric persisted unbacked by action, theater_ratio would climb past 0.5 and the arrangement would drift toward theatrical maintenance of a solved problem; the 2017 theater peak is the shape of that risk, currently receded but not eliminated. Coalition potential among the paying seats partially materialized (the responsibility-while-protecting initiative) and is the main channel through which the extraction component could be renegotiated rather than merely endured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the conditional_sovereignty reading of the westphalian_sovereignty kernel; how would the classification shift under the sibling readings?',
    'Generate the sibling stories (westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty) as separate files and compare per-seat classifications. The disagreement is located in what sovereignty IS: unconditional shield, conditional grant, or capacity-graded spectrum, which changes the beneficiary/victim structure entirely.',
    'Under the absolute reading, this same arrangement (external conditionality) is itself the violation: intervention advocates become violators, states become right-holders, and the arrangement''s epsilon would be authored near zero by absolutists assessing non-intervention. Under the graduated reading the exposed set widens to all low-capacity states regardless of conduct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-indexed classification of a contested sovereignty kernel; sibling readings instantiate different constraints.').

omega_variable(
    threshold_adjudication_politics,
    'Is the atrocity threshold that triggers legitimate intervention adjudicated by juridical criteria or by great-power discretion?',
    'Comparative process-tracing of threshold application across cases (Kosovo, Libya, Syria, Myanmar, Yemen, Ukraine): whether documented violations of similar scale receive similar treatment and what explains the differences.',
    'Purely political adjudication concentrates the arrangement''s operative content in five self-exempt governments and pushes the extraction component toward the pure-extraction end; juridically constrained adjudication strengthens the coordination component and holds the hybrid classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_adjudication_politics, empirical, 'Whether the trigger threshold is law or power in practice.').

omega_variable(
    net_protection_outcome,
    'Does conditional sovereignty protect at-risk populations net of intervention''s own costs (Libya''s post-intervention collapse, civilian casualties, regional destabilization)?',
    'Systematic outcome studies comparing atrocity trajectories in intervened, non-intervened, and counterfactual-modeled cases, using population-level welfare measures rather than doctrinal success metrics.',
    'If net outcomes are negative, the protective coordination function is degraded and the arrangement''s justification collapses toward cover for power projection, moving the classification toward the pure-extraction end; if net-positive, the coordination function is real and load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_protection_outcome, empirical, 'Whether the protective function delivers net protection to the populations it names.').

omega_variable(
    selectivity_intrinsic_vs_contingent,
    'Is the asymmetric application of the doctrine (strong states self-exempt, weak states exposed) intrinsic to a Council-adjudicated structure, or contingent on the current distribution of power?',
    'Institutional analysis under counterfactual capacity distributions: whether any adjudication structure with enforcement concentrated in few hands can avoid self-exemption, plus historical evidence from capacity shifts (post-1945 decolonization, post-1991 unipolarity).',
    'If intrinsic, the asymmetry is structural, removable only by redesign, and the hybrid classification is stable; if contingent, the arrangement could drift toward symmetric coordination as power diffuses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selectivity_intrinsic_vs_contingent, empirical, 'Whether selectivity is a structural feature or a circumstantial one.').

omega_variable(
    global_south_consent_authenticity,
    'Does the 2005 Global South acceptance of the codified doctrine represent genuine consent to conditionality or acquiescence under power asymmetry?',
    'Archival and diplomatic-record analysis of the 2005 World Summit negotiations, plus subsequent behavior: responsibility-while-protecting sponsorship, General Assembly debates, voting patterns on protection mandates.',
    'Genuine consent strengthens the arrangement''s coordination claim (broad ownership of the bargain); acquiescence weakens it, making the protective consensus a negotiated surrender and shifting weight toward the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_consent_authenticity, conceptual, 'Authenticity of the cross-bloc bargain underlying the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1999, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1999, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1999, 0.3).
narrative_ontology:measurement(west_tr_t2002, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2002, 0.32).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2008, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.4).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2014, 0.45).
narrative_ontology:measurement(west_tr_t2017, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2017, 0.5).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(west_tr_t2023, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2023, 0.47).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t1999, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1999, 0.3).
narrative_ontology:measurement(west_be_t2002, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2002, 0.33).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement(west_be_t2008, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.44).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2014, 0.46).
narrative_ontology:measurement(west_be_t2017, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2020, 0.43).
narrative_ontology:measurement(west_be_t2023, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2025, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1999, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1999, 0.45).
narrative_ontology:measurement(west_su_t2002, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2002, 0.48).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(west_su_t2008, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(west_su_t2017, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2017, 0.57).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(west_su_t2023, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label Westphalian sovereignty covers three structurally distinct claims about what sovereignty is. This file instantiates the conditional reading (codified: ICISS 2001; 2005 World Summit Outcome paragraphs 138-139). The absolute reading is its foreclosed sibling within any single legal framework: a state cannot simultaneously hold that external interference is categorically illegitimate and that it is legitimate at an atrocity threshold. The graduated reading is a downstream proposal whose legitimacy conditions the conditional codification changed: once sovereignty's content became internationally specifiable, capacity-graded variants became arguable. Each reading carries its own epsilon, beneficiary/victim structure, and classification; the family is linked so contamination and drift propagate visibly across the kernel. Sibling constraint IDs follow this file's kernel__reading naming pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
