% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Reading of Article 51 Self-Defense
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Article 51 self-defense
 *   kernel: the expansive preventive reading, under which self-defense
 *   extends to preemptive and preventive force against non-state actors and
 *   emerging threats when the acting state itself demonstrates necessity. The
 *   standing arrangement under contest — and the ε referent — is that
 *   permissive self-judged regime as it actually operates in state practice
 *   since 2001: each capable state publishes its own necessity justification,
 *   strikes first when it judges the threshold met, and faces no external
 *   adjudication of the claim. The arrangement retains a genuine coordination
 *   residue (a shared legal vocabulary for force against threats the Charter
 *   framework did not contemplate, and a justification surface that
 *   coalitions and rivals can contest) while its operative incidence is
 *   sharply asymmetric: freedom of action concentrates in states with
 *   power-projection capacity, and the physical and institutional costs fall
 *   on target-region populations, host-state sovereignty, and the Security
 *   Council's authorization function. Claim and metrics are authored
 *   independently: claimed_type is my structural judgment (tangled_rope —
 *   both a coordination function and asymmetric extraction, actively enforced
 *   through legal advocacy and precedent-building); the metrics describe the
 *   arrangement's observed operation. Sibling readings of the same kernel —
 *   the narrow armed-attack reading and the unable-unwilling doctrine — are
 *   separate constraint stories with their own ε values; this file authors
 *   only the expansive reading and keeps its ε invariant.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: primary beneficiary and agenda-setter (institutional power / arbitrage exit) — self-judges necessity, collects freedom of action, sets interpretive precedent through practice
 *   - defense_industrial_sector: secondary beneficiary (powerful / arbitrage) — collects procurement demand from open-ended emerging-threat operations
 *   - target_region_populations: primary target (powerless / trapped) — bears the physical costs of self-judged force; holds no seat in the governing discourse
 *   - host_states_of_alleged_threats: secondary target (moderate / constrained) — sovereignty breached by operations conducted without consent
 *   - un_security_council: institutional payer (institutional / identity_locked) — its exclusive authorization function is what each self-judged invocation depletes
 *   - small_and_middle_powers: diffuse payers (moderate / constrained) — rely on the collective-security system, can never invoke the reading themselves
 *   - international_court_of_justice: excluded counterweight (institutional / constrained) — holds the narrow-reading jurisprudence no self-judging state treats as operative
 *   - international_legal_scholarship: analytical observer (analytical / analytical) — documents each invocation against Charter text and precedent without enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.6).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '4a61f76a-b541-4d9b-8013-b086462d418b').
narrative_ontology:cs_kernel_codification('4a61f76a-b541-4d9b-8013-b086462d418b', fixed_text).
narrative_ontology:cs_authority_grounding('4a61f76a-b541-4d9b-8013-b086462d418b', practice).
narrative_ontology:cs_interpretation_layer_present('4a61f76a-b541-4d9b-8013-b086462d418b').
narrative_ontology:cs_reading_relation('4a61f76a-b541-4d9b-8013-b086462d418b', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('4a61f76a-b541-4d9b-8013-b086462d418b', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('4a61f76a-b541-4d9b-8013-b086462d418b', foundational, inherent_right_extends_to_emerging_threats).
narrative_ontology:cs_axiom_status(inherent_right_extends_to_emerging_threats, holdable).
narrative_ontology:cs_axiom_grounding('4a61f76a-b541-4d9b-8013-b086462d418b', inherent_right_extends_to_emerging_threats, deontological).
narrative_ontology:cs_axiom('4a61f76a-b541-4d9b-8013-b086462d418b', foundational, necessity_self_judged_by_threatened_state).
narrative_ontology:cs_axiom_status(necessity_self_judged_by_threatened_state, holdable).
narrative_ontology:cs_axiom_grounding('4a61f76a-b541-4d9b-8013-b086462d418b', necessity_self_judged_by_threatened_state, conventional).
narrative_ontology:cs_reference_frame('4a61f76a-b541-4d9b-8013-b086462d418b', inherent_right_self_judged_necessity).
narrative_ontology:cs_drift_state('4a61f76a-b541-4d9b-8013-b086462d418b', contemporary_post_iraq_backlash, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4a61f76a-b541-4d9b-8013-b086462d418b', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, host_states_of_alleged_threats).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, small_and_middle_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with global or regional power-projection capacity that invoke the expansive reading to justify strikes and operations against non-state actors and emerging threats before any armed attack occurs. Each invocation requires them to publish a necessity justification of their own drafting — intelligence assessments and legal memos they author and assess themselves. What flows to them is freedom of action: the option to strike first without seeking anyone's authorization. What flows from them is the force itself and the precedent each operation sets. Their exit would be unilateral self-restraint — renouncing an option their domestic politics and threat assessments punish — and no external body can compel it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter).

% Contractors and arms manufacturers whose order books depend on the tempo and duration of military operations. Open-ended campaigns against emerging threats — counterterrorism operations, missile-defense buildups, long-duration deployments — sustain procurement demand that a force posture confined to responding to actual attacks would flatten. They participate in the discourse as suppliers of the capability assessments that feed necessity claims. Demand is global and diversified across clients, so their position is comfortable under any reading that keeps operations running.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Civilians in the regions where preventive strikes and operations land — parts of the Middle East, the Sahel, South Asia. They bear the physical costs of force authorized under self-judged necessity: casualties, displacement, destroyed infrastructure, and the recurring insecurity of living under a doctrine that permits strike-first operations against threats assessed elsewhere. They hold no seat in the legal discourse that governs the force used against them and no exit from the territory the operations occur in.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, target_region_populations, excluded).

% States whose territory hosts or is alleged to host the non-state actors that preventive operations target — Pakistan, Lebanon, Syria, Yemen, and peers. They bear sovereignty violations: operations conducted without consent, sometimes without notification. Their formal equality under the Charter gives them a voice in the General Assembly, but their practical options are limited to protest, asymmetric response, or negotiated toleration; few can deter or adjudicate the force used on their territory.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, host_states_of_alleged_threats, payer,
    moderate, biographical, constrained, regional).

% The collective-security organ whose exclusive competence to authorize force the expansive reading routes around. Each self-judged invocation depletes the precedent value of its authorization function: states learn that authorization is optional when a capable state judges necessity for itself. The Council cannot abandon its Charter role — its authority IS that role — and its veto politics prevent it from either endorsing or formally repudiating the reading in most cases.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council, payer,
    institutional, generational, identity_locked, global).

% States without power-projection capacity that rely on the collective-security system for protection against larger neighbors. The expansive reading offers them nothing they can use — they cannot self-judge their way to preventive war against anyone — while exposing them to preventive operations by capable states and to the precedent that force-rights track capacity. They consistently vote for narrow-reading formulations in the General Assembly. Their exit is the system itself: they cannot opt out of the Charter order they depend on.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, small_and_middle_powers, payer,
    moderate, generational, constrained, regional).

% The principal judicial organ whose jurisprudence (Nicaragua 1986, Oil Platforms 2003) holds that force requires an actual or imminent armed attack and that necessity claims are judicially reviewable. Self-judging states neither accept its compulsory jurisdiction over force decisions nor treat its determinations as operative constraints; its rulings accumulate on the record while the practice they address continues. It would adjudicate the necessity claims if given the seat; the reading's structure leaves it none.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_court_of_justice, excluded,
    institutional, generational, constrained, global).

% Academic international lawyers, most of whom hold the narrow or unable-unwilling positions and document each expansive invocation against the Charter text and ICJ precedent. They produce the analytical record that any eventual settlement of the doctrine must answer, but hold no enforcement capacity; their objection is persistent, published, and without operative effect on self-judging states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared legal vocabulary and focal point for decisions about force against threats the Charter framework did not clearly contemplate — transnational non-state actors and emerging (pre-attack) threats — and channels force justifications into a common discourse of necessity, proportionality, and reporting, enabling coalition formation and diplomatic contest over any given use of force.
% TRANSFER_FUNCTION: Moves decision-rights over war and peace from the collective-security organ (Security Council authorization) to each militarily capable state (self-judged necessity); moves the physical costs of preventive force onto target-region populations and the sovereignty of host states; moves procurement demand to the defense industrial sector; moves interpretive authority away from the ICJ toward acting states' own legal advisers.
% ABSENT_VOICES: Target-region populations have no seat in the discourse that authorizes force against them; the ICJ holds the narrow-reading jurisprudence but self-judging states neither accept its jurisdiction over force decisions nor its determinations as binding; General Assembly majorities that repeatedly reaffirm the narrow reading lack any enforcement instrument. Their objections are on the record and operative nowhere.
% DISAPPEARANCE_RATIONALE: Capable states could no longer lawfully claim preventive force: the narrow reading would govern, force would route through Council authorization at higher legitimacy cost or proceed extra-legally with real coalition consequences, the Council's authorization function would regain precedent value, and defense procurement profiles tied to open-ended emerging-threat operations would contract. Force tempo, institutional authority, and legal argument all depend on the reading — the world rearranges without it.
% FOUNDING_PROBLEM: The Charter's self-defense framework was drafted around interstate armed attacks. By the 1990s and 2000s the live threat set had shifted: transnational terrorism culminating in the 9/11 attacks, WMD proliferation to so-called rogue states, and non-state actors operating from permissive or failed territory — threats that do not present a clean Article 51 trigger and that capable states argued could not be waited out.
% FOUNDING_PROBLEM_CORROBORATION: The threat's liveness is corroborated from outside the benefiting parties: Security Council resolutions 1368 and 1373 (2001) recognized international terrorism as a threat to international peace and security; the UN High-Level Panel on Threats, Challenges and Change (2004) — an independent body — attested the emerging-threat problem as real while explicitly rejecting unilateral preventive force and endorsing only Council-authorized collective prevention; ICJ jurisprudence and the overwhelming majority of international legal scholarship likewise attest the problem while rejecting the self-judged remedy. Corroboration of the problem is broad; corroboration of THIS reading as its solution comes almost exclusively from the capable states that practice it.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 because the arrangement's operative test — necessity — is demonstrated by the actor itself and adjudicated by no one: the 2003 Iraq case showed a demonstrated-necessity claim can be wholly unmoored from fact (the WMD intelligence base collapsed) with zero legal consequence to the claimant, and the subsequent two decades normalized strike-first operations across multiple host states on self-certified assessments. Suppression is authored at 0.60 as a raw structural property (unscaled — only extractiveness is scaled by directionality and scope downstream): the arrangement holds by displacing the Council's authorization role and by leaving ICJ determinations without operative effect on self-judging states, but the narrow reading remains live formal law, so the suppression is contested rather than total. Theater_ratio at 0.48 reflects the growing ritualization of the 'necessity demonstrated' requirement — intelligence dossiers, Security Council presentations, post-hoc legal memos — that participants increasingly treat as legitimacy cover rather than decision input; it approaches but does not cross the Goodhart threshold. Accessibility_collapse at 0.45: the alternatives (Council authorization, narrow-reading restraint) remain partially accessible as formal law even as they decay in practice for states that have adopted the reading. Resistance at 0.62: ICJ jurisprudence, recurring General Assembly majorities, Non-Aligned declarations, and the weight of legal scholarship actively contest the reading — sustained, published, and non-dispositive because enforcement capacity sits with the self-judging states. All three measurement series run on one shared time grid (T=0/5/10/15/20/25, i.e., 2001–2026). The suppression_requirement series is deliberately non-monotonic (assertion 2001–2003, post-Iraq backlash decay, partial rehabilitation through the ISIS era and beyond) — that is the enforcement-capacity story, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat and the payer seats compute different constraints from the same norm. From militarily_capable_states' position the arrangement is the minimum legal order compatible with survival against threats that do not wait for an armed attack — the narrow reading looks, from that seat, like a suicide pact. From target_region_populations' and host_states_of_alleged_threats' position the same arrangement is a license for the powerful to strike first on self-certified evidence, with the demonstration requirement functioning as paperwork after the decision. The same-level lateral dynamic is equally sharp: small_and_middle_powers and militarily_capable_states hold formally identical sovereignty under the Charter — equal votes, equal formal rights — but the self-judged standard distributes force-rights strictly by capacity, so two states at the same nominal level experience opposite constraints. Coalition potential for the powerless victims is structurally blocked: target populations hold no seat in the governing discourse at all, and host-state coalitions (Non-Aligned Movement, Arab Group) produce declarations without enforcement instruments, so the resistance that exists cannot convert into constraint on the self-judgers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. militarily_capable_states sit at the beneficiary end (d near 0): the arrangement subsidizes their freedom of action, and their arbitrage-grade exit — invoking the expansive reading for themselves while condemning rivals' preventive strikes in the same register — damps their effective extraction toward subsidy. defense_industrial_sector sits at the extreme beneficiary end: it collects demand without administering anything. At the target end: target_region_populations (trapped, powerless) sit near full target — they bear the force with no exit and no seat; host_states_of_alleged_threats (constrained, moderate) sit high; un_security_council sits near full target because its identity-locked exit (its authority IS its Charter function) places it among the trapped — an agent that cannot leave what is being consumed from it. small_and_middle_powers are high-d payers with no reciprocal benefit: the reading gives them nothing they can use and exposes them to its operation. international_court_of_justice and international_legal_scholarship hold excluded/observer seats — the ICJ bears erosion of its adjudicative function without collecting or paying directly; scholarship is symmetric-analytical. Per-seat effective extraction will therefore diverge sharply from base ε: amplified for the trapped and identity-locked targets, damped into subsidy for the arbitrage-grade beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both misdirections. Against pure-rope labeling: naming the victims (target populations, host states, the Council's authorization function) keeps the asymmetric incidence visible — the coordination story of law-governed force does not describe the whole structure, and the self-judged standard is the hinge where coordination tips into extraction. Against pure-snare labeling: the founding problem is genuinely live and is corroborated by parties outside the beneficiary set — the 2004 UN High-Level Panel attested the emerging-threat problem as real while rejecting the unilateral remedy — so the reading is not cover-only extraction riding a fictitious problem; it addresses a real Charter gap. The R5 mismatch check returns clean: founding_problem_status = live with disappearance_verdict = world_rearranges, so no zombie flag — the arrangement is not being maintained past a dead mandate. The live risk is drift, not obsolescence: if the necessity demonstration completes its ritualization (theater_ratio sustaining above 0.5) while self-judgment remains the only check, the coordination residue thins and the structure slides toward pure extraction; the measurement series is built to catch exactly that crossing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the Article 51 self-defense kernel (expansive_preventive_reading). Do the sibling readings — narrow_armed_attack_reading and unable_unwilling_doctrine_reading — correctly instantiate the same kernel text, and which structural element (the trigger, or the locus of the necessity judgment) does the live contest actually turn on?',
    'Consolidation of state practice and opinio juris, ICJ jurisprudence on necessity and imminence, and whether capable states ever accept external adjudication of their own necessity claims.',
    'Under the narrow reading the beneficiary/victim structure inverts — capable states lose the preventive license and target-region populations gain the protection this arrangement charges them for; under the unable-unwilling reading a host-state unwillingness/unability element is inserted, moderating extraction while preserving self-judgment. This story''s ε (0.72) is valid only for the expansive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: which reading of Article 51 governs, and on which structural element the readings diverge.').

omega_variable(
    necessity_self_judgment_circularity,
    'Does the self-judged necessity standard impose any operative check on the acting state, or is it structurally vacuous — is there any recorded case in which a capable state concluded its own necessity was not satisfied and abstained on that ground?',
    'Comparative analysis of force decisions against the internal record: declined strikes, declassified legal memos, and ex-post assessments (e.g., the Iraq WMD postmortems) measuring whether the demonstration requirement ever bit.',
    'If the standard is structurally vacuous, the coordination residue thins toward zero and the arrangement trends toward pure extraction (cover-only); if self-judgment carries real decision weight (declined operations, internal vetoes), the tangled_rope claim holds with the measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_circularity, empirical, 'Whether ''necessity demonstrated'' by the acting state functions as a check or as ritual.').

omega_variable(
    emerging_threat_epistemics,
    'Are emerging threats epistemically tractable — can necessity against a future or materializing threat ever be demonstrated rather than asserted, or is the demonstration requirement categorically unable to bear evidentiary weight for preventive (as opposed to preemptive) force?',
    'Ex-post validation of invoked threats across the case record: what share of demonstrated-necessity claims were vindicated by subsequent findings (Iraq''s WMD: none; other cases mixed).',
    'If systematically untrackable, every preventive invocation rests on an untestable claim and the arrangement''s measured extraction understates its operation — no external party could ever falsify a necessity claim, which is the structure of a license, not a standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_threat_epistemics, empirical, 'Epistemic tractability of demonstrated necessity against emerging threats.').

omega_variable(
    small_state_invocation_test,
    'Does the expansive reading produce any diffuse benefit that reaches small and middle powers — do they ever invoke it for their own protection — or is invocation exclusively a capability of the militarily capable, making the beneficiary structure purely concentrated?',
    'Survey of state practice and General Assembly voting: whether any state without power-projection capacity has invoked preventive self-defense, and the voting record on narrow-reading formulations.',
    'If small states never invoke it and consistently vote against it, the arrangement''s benefit structure is purely concentrated in the capable — the coordination story serves only those who can use it, and the classification sits nearer the extraction pole than the base metrics alone indicate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_state_invocation_test, empirical, 'Whether the reading''s benefits diffuse beyond the capable states or concentrate entirely in them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__expansive_preventive_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__expansive_preventive_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__expansive_preventive_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(arti_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(arti_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(arti_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% Article 51 self-defense is one kernel (a fixed Charter text) that decomposes into three structurally distinct constraints per the ε-invariance principle: this expansive preventive reading (high ε — licenses self-judged force; victims: target-region populations, host states, Council authority), narrow_armed_attack_reading (low ε — confines force to response to actual/imminent armed attack; protective of the same seats this reading charges), and unable_unwilling_doctrine_reading (intermediate hybrid). The readings differ on two structural elements — the trigger and the locus of the necessity judgment — so each gets its own ε, beneficiaries, victims, and classification rather than one story with a measurement parameter. Upstream/downstream structure within the family: this reading's self-judgment apparatus is the load-bearing precedent the hybrid doctrine borrows, and the narrow reading's formal authority (ICJ jurisprudence, GA majorities) is what this reading's enforcement machinery must displace.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
