% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Article 17 Complementarity — International Oversight Reading (Accountability-Trigger Guardian)
 *   domain: international law / criminal justice / state sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute makes the International Criminal Court's
 *   jurisdiction conditional: the Court may act only where a state is
 *   'unwilling or unable genuinely' to investigate or prosecute. This story
 *   instantiates the international-oversight reading of that clause —
 *   complementarity as an accountability-trigger mechanism, the Court as
 *   guardian against impunity, and 'unwilling' read broadly enough to capture
 *   victor's justice and elite self-shielding. Per the epsilon-referent rule
 *   for kernel readings, the metrics below describe the standing arrangement
 *   under contest — the complementarity regime as it actually operates, with
 *   its contested threshold, state-controlled domestic proceedings, and
 *   cooperation-dependent enforcement — assessed by this reading's own
 *   lights. From that seat the regime is a genuine coordination mechanism
 *   (jurisdiction allocation, double-jeopardy avoidance, safe-haven closure)
 *   whose routine output is impunity for the perpetrators best positioned to
 *   shield themselves. The sibling reading,
 *   article_17_complementarity__national_primacy_reading, is a separate
 *   constraint with its own epsilon; the contest between readings is carried
 *   in the omega variables and the cs_structure block, not folded into this
 *   story's classification.
 *
 * KEY AGENTS:
 *   - complicit_state_elites: Primary extraction-side beneficiary (institutional power / constrained exit) — controls domestic dockets and collects the impunity the default-forum rule provides
 *   - atrocity_victims_complicit_states: Primary target (powerless / trapped) — bears impunity and symbolic-prosecution theater; contingent beneficiary when the broad reading prevails
 *   - atrocity_victims_failed_states: Beneficiary (powerless / trapped) — receives the only accountability available when the state cannot act
 *   - surrendered_accused: Payer (powerless / trapped) — bears the enforcement edge: detention and second prosecution
 *   - icc_office_of_the_prosecutor: Agenda-setter (institutional / constrained) — owns the docket and the admissibility litigation, owns no arrests
 *   - assembly_of_states_parties: Agenda-setter (institutional / constrained) — owns amendment, elections, and budget; its members are also the regime's subjects
 *   - un_security_council: Agenda-setter with beneficiary secondary role (institutional / arbitrage) — referral, deferral, and rival-tribunal power; permanent-five shield
 *   - victors_justice_prosecutors: Beneficiary (moderate / constrained) — runs one-sided dockets validated by the adequacy presumption
 *   - nonparty_great_powers: Excluded (powerful / arbitrage) — bound by referrals, absent from governance, armed with sanctions and vetoes
 *   - human_rights_organizations: Analytical observer (organized / analytical) — documents and advocates, holds no decision seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.64).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.6).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity — International Oversight Reading (Accountability-Trigger Guardian)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international law / criminal justice / state sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'da602ff3-fde0-4c11-9048-3122aa5e89b8').
narrative_ontology:cs_kernel_codification('da602ff3-fde0-4c11-9048-3122aa5e89b8', fixed_text).
narrative_ontology:cs_authority_grounding('da602ff3-fde0-4c11-9048-3122aa5e89b8', lineage).
narrative_ontology:cs_interpretation_layer_present('da602ff3-fde0-4c11-9048-3122aa5e89b8').
narrative_ontology:cs_reading_relation('da602ff3-fde0-4c11-9048-3122aa5e89b8', article_17_complementarity__national_primacy_reading, forecloses).
narrative_ontology:cs_axiom('da602ff3-fde0-4c11-9048-3122aa5e89b8', foundational, impunity_triggers_international_jurisdiction).
narrative_ontology:cs_axiom_status(impunity_triggers_international_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('da602ff3-fde0-4c11-9048-3122aa5e89b8', impunity_triggers_international_jurisdiction, deontological).
narrative_ontology:cs_axiom('da602ff3-fde0-4c11-9048-3122aa5e89b8', foundational, selective_victors_justice_constitutes_unwillingness).
narrative_ontology:cs_axiom_status(selective_victors_justice_constitutes_unwillingness, holdable).
narrative_ontology:cs_axiom_grounding('da602ff3-fde0-4c11-9048-3122aa5e89b8', selective_victors_justice_constitutes_unwillingness, deontological).
narrative_ontology:cs_reference_frame('da602ff3-fde0-4c11-9048-3122aa5e89b8', impunity_guardian_baseline).
narrative_ontology:cs_drift_state('da602ff3-fde0-4c11-9048-3122aa5e89b8', contemporary_admissibility_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da602ff3-fde0-4c11-9048-3122aa5e89b8', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_victims_failed_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, complicit_state_elites).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, atrocity_victims_complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, surrendered_accused).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_victims_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victors_justice_prosecutors).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, complementarity_principle).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, positive_complementarity_catalysis_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior political and military officials in states where atrocity crimes were committed by their side or under their command. They control which files are opened, which are shelved, and which defendants are amnestied; selective prosecutions of the defeated side proceed while their own conduct is never charged. The treaty regime treats their national proceedings as the default forum and steps in only if those proceedings are shown to be hollow, so the practical effect of the arrangement they administer is that their exposure to any prosecution at all approaches zero. Leaving the arrangement would mean accepting jurisdiction over their own conduct, which no holder of this position has done voluntarily.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_state_elites, beneficiary,
    institutional, biographical, constrained, national).

% Survivors of atrocities in states whose courts, police, and archives collapsed in the conflict — no domestic forum exists to charge anyone. Their only route to any accounting runs through the international court's decision to open a situation and through foreign organizations that document what local institutions cannot. When the court acts they receive investigations, trials, and reparations proceedings no one else will provide; when it does not, they wait. Physically leaving the country as refugees removes them from the remedy without removing the crime from their lives.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_victims_failed_states, beneficiary,
    powerless, generational, trapped, regional).

% Survivors of atrocities in states whose governments or victorious factions shield the perpetrators. Their cases end in symbolic charges, amnesties, or closed files presented to the world as domestic justice. They carry the cost of that arrangement — no accounting, no reparations, perpetrators in office — while benefiting only in the rare instances when the international court overrides the domestic forum and forces a real prosecution. They have no standing to start a case themselves and no vote in the body that sets the admissibility threshold.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_victims_complicit_states, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, atrocity_victims_complicit_states, beneficiary).

% Individuals handed over to the international court after domestic proceedings were judged inadequate or after their own state referred them. They sit in detention for years, face prosecution for conduct a national court already charged or declined to charge, and answer with defense resources a fraction of the prosecution's. They cannot leave custody and cannot choose the forum that judges them.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, surrendered_accused, payer,
    powerless, biographical, trapped, local).

% The organ that decides which situations to examine, litigates whether national proceedings are genuine, and issues the warrants. It has no police: every arrest, every witness, every document must come from a state choosing to cooperate, and when states refuse it can only report the refusal to the Assembly or the Security Council. Its docket decisions are the operative content of the admissibility regime.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, generational, constrained, global).

% The treaty body of states that joined the Rome Statute. It amends the Statute, elects the judges and the Prosecutor, sets the budget, and receives the non-cooperation reports. Its consensus practice makes moving the admissibility threshold in either direction practically impossible, and its members are the same governments whose conduct the regime polices — they sit as legislators and appear as subjects.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).

% Fifteen states, five of them permanent, holding the power to refer situations to the court (as with Darfur and Libya, binding states that never joined the treaty), to defer any investigation for a year at a time, and to create rival ad hoc tribunals instead. The permanent members can veto any referral touching themselves or their clients, and most of them never accepted the court's jurisdiction over their own nationals. They shape the regime's reach from outside its membership.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, un_security_council, beneficiary).

% National prosecutors in post-conflict states who charge only the defeated side's fighters and officials while the files on their own side's conduct remain unopened. Their dockets are presented — and under the prevailing default accepted — as genuine domestic justice. A reading that counted one-sided prosecution as unwillingness would make their dockets the paradigm case for international takeover.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victors_justice_prosecutors, beneficiary,
    moderate, biographical, constrained, national).

% Major military powers that never joined the Rome Statute but whose nationals, allies, and operations can still be reached through Security Council referral or through crimes committed on member territory. They hold no seat in the treaty body that governs the regime, yet they answer its reach with sanctions on court officials, cooperation bans, and bilateral immunity agreements. They argue that a low admissibility threshold turns the court into an instrument against them, and they have the leverage to make that argument expensive.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, nonparty_great_powers, excluded,
    powerful, generational, arbitrage, global).

% International organizations that document atrocities, submit briefs in admissibility proceedings, and campaign for the court to override domestic shielding. They hold no decision seat: they can place a situation on the Prosecutor's desk and shape public legitimacy, but the threshold, the docket, and the budget are set elsewhere. Their monitoring is the main external check on whether domestic proceedings are real.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, complicit_state_elites).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates criminal jurisdiction over genocide, crimes against humanity, and war crimes between national courts and a single standing international court: national proceedings take precedence when they are real, the international court backstops when they are absent or hollow, and the same person is not prosecuted twice for the same conduct. It replaces the case-by-case ad hoc tribunal model, which was slow, selective, and required a fresh Security Council act for each conflict.
% TRANSFER_FUNCTION: When the admissibility trigger fires, custody, prosecution, and punishment move from the state to the international court, and arrest, evidence, and witness obligations move onto states. When the trigger does not fire, the justice claims of victims remain parked with domestic institutions controlled by the same elites the claims implicate — which functions, in practice, as a transfer of impunity protection to those elites and a transfer of the cost of that protection to victims.
% ABSENT_VOICES: Victim communities have no standing to trigger admissibility, to appeal an inadmissibility ruling, or to initiate a case in their own name; they reach the process only through the Prosecutor's discretion and NGO intermediation. Non-party great powers are bound by Security Council referrals but hold no seat in the treaty body that sets the threshold. The two excluded constituencies object from opposite directions — victims that the threshold is too high, non-party powers that it is too low — and neither is in the room where it is set.
% DISAPPEARANCE_RATIONALE: If complementarity disappeared overnight, the jurisdictional settlement of international criminal law dissolves with it: either every state with custody must prosecute everything, producing double-jeopardy conflicts and extradition warfare, or there is no backstop and complicit states return to unreviewable impunity. The Court's docket, the cooperation web, and the domestic-prosecution incentive the regime generates would all have to re-form around something else.
% FOUNDING_PROBLEM: Standing impunity for atrocity crimes after the ad hoc tribunal era: states would not ratify a court with primary jurisdiction over their own nationals, so the Rome Conference built complementarity as the ratifiable compromise — national courts first by default, an international guardian at the margin, and a treaty obligation to cooperate with the guardian when it acts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Security Council's own referrals of Darfur (2005) and Libya (2011) attest that state failure persists wherever the permanent members permit acknowledgment; UN Commission of Inquiry reports on Syria, Myanmar, and Ethiopia document mass atrocity crimes with no genuine domestic prosecution; and several governments — Uganda, the Democratic Republic of the Congo, the Central African Republic, Mali — referred their own situations to the court, an admission from the state itself that its institutions could not or would not act. No corroborator is fully disinterested — Council referrals track permanent-member interests — but the documentary record is not produced by the elites the arrangement shields.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.64 is the oversight reading's honest assessment of the standing arrangement: the Court has delivered a small set of genuine convictions (Ntaganda, Ongwen, Blé Goudé, Al Hassan) and real catalytic effects in some self-referral states, but the dominant output across the docket is unprosecuted atrocity — two decades of unexecuted Darfur warrants, the collapse of the Kenya cases against sitting elites, the Afghanistan investigation opened and then deprioritized. Suppression 0.60 is authored as a raw structural property and is not scaled by power or scope in the engine's arithmetic (only extractiveness is scaled, by directionality and scope); it records that the regime actively forecloses rival accountability routes — ad hoc tribunals (Security-Council veto-blocked), universal jurisdiction (delegitimized by the primacy norm the regime itself affirms), and victims' own standing (none). Theater 0.45 records symbolic domestic prosecutions accepted as genuine, warrants issued and never executed, and situations opened and stalled — the guardianship is partly performance. Accessibility collapse 0.55: once victims understand the regime, alternatives are mostly closed but not completely — the rare universal-jurisdiction convictions (Habré, the Koblenz Syria cases) show exits that function occasionally. Resistance 0.65: sustained non-cooperation findings, Assembly budget standoffs, sanctions on court personnel, great-power non-membership. The three measurement series share one grid (t = 0, 4, 8, 12, 16, 20, mapping roughly 2002–2022) so no metric is sampled against a substituted end-state value; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity dynamics — the cooperation machinery hardening as state resistance grows — not merely extraction drift. On the receipt surface: the gains of the arrangement's non-operation accrue demonstrably to the complicit-elite seat (they keep office, liberty, and assets), which is why gain_flow names that seat rather than diffuse; the cost of fixing is prohibitive for every seat that could fix it (Assembly consensus, Council veto, a Prosecutor with no compulsion power).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from the same structure. From the agenda-setter seats (Prosecutor, Assembly) the regime is a coordination achievement built against state resistance — a backstop that fires when it can. From the payer seats it divides: complicit-state victims experience a guardian that mostly does not fire while their perpetrators remain in office, and surrendered accused experience the enforcement edge directly — years of detention and a second prosecution. From the extraction-side beneficiary seat (complicit elites, victors' prosecutors) the same arrangement is a sovereignty-respecting order that validates their dockets. Among the institutional agenda-setters themselves — nominally the same power level — exit options differ sharply: the Prosecutor is constrained (no police, total cooperation dependence), the Assembly is constrained (consensus rules bind it), and the Security Council holds arbitrage (it can refer, defer, or bypass the Court with ad hoc tribunals), which is why the Council's exposure to the regime it administers is the lowest of the three.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Complicit state elites and victors'-justice prosecutors sit near the beneficiary pole: the arrangement subsidizes their impunity, and their exit (accepting jurisdiction over their own conduct) is one no holder takes. Failed-state victims sit low as well — they collect the accountability no domestic forum can provide. Complicit-state victims are the structural targets: they pay in impunity and theater prosecutions, and their benefit is contingent on a broad reading that has not prevailed in the case law; their declared secondary beneficiary role reflects the real convictions the regime has occasionally delivered them, but their net position is payer. Surrendered accused are pure targets of the enforcement edge. The Prosecutor and Assembly derive mid-range directionalities from their institutional positions — they administer the arrangement and absorb its cooperation failures; the Council's secondary beneficiary role (the permanent-five shield) pulls it toward the beneficiary end while its arbitrage exit keeps its exposure low. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place each seat correctly, and the override surface is too coarse (power-atom-wide) to improve on that placement without mislabeling seats that share a power atom. Note also the coalition question for the powerless seats: victims' natural coalition power is structurally blocked — they cannot initiate cases, cannot appeal admissibility rulings, and reach the process only through the Prosecutor's discretion — which is why powerless here does not aggregate into organized.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim keeps both halves of the structure visible and prevents the two standard misreadings. A pure-extraction reading would license scrapping the backstop and re-creating the ad hoc tribunal gap the regime was built to close — but the coordination function (jurisdiction allocation, double-jeopardy avoidance, safe-haven closure) is real and load-bearing. A pure-coordination reading would license treating impunity as coordination overhead — but the extraction is real and concentrated: the justice claims of victims are absorbed by institutions controlled by the elites those claims implicate. The founding problem is live (impunity persists at scale), so no mandatrophy declaration is authored; the regime has not outlived its function, it under-delivers on it. The theater series is the early-warning line: if theater_ratio crosses 0.5, the question stops being 'why does the guardian under-deliver' and becomes 'is guardianship now mostly performance' — the degraded-inertia question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the article_17_complementarity kernel (the international-oversight reading); what would the sibling national_primacy_reading change structurally if it prevailed?',
    'Author the sibling story and compare: the sibling moves the admissibility threshold up, flips the burden to the Court to prove sham proceedings, contracts the victim set to cases of total judicial collapse, and removes victor''s justice and elite immunity from the trigger''s scope.',
    'Under the sibling reading this story''s epsilon falls sharply — the standing arrangement reads as sovereignty-respecting coordination with a narrow backstop, the impunity value attributed to complicit elites dissolves into the presumption of domestic adequacy, and the beneficiary/victim structure inverts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the Article 17 kernel; the sibling reading flips threshold, burden, and victim set.').

omega_variable(
    inability_unwillingness_boundary,
    'How much of the regime''s non-intervention record reflects genuine state incapacity (unable) versus elite shielding (unwilling)?',
    'Case-by-case capacity assessment across the docket: judicial infrastructure, witness security, and budget on one side; selective file-closure, amnesty patterns, and perpetrators remaining in office on the other.',
    'If incapacity dominates, the regime is closer to a capacity-building backstop with modest overhead; if shielding dominates, the impunity component is the regime''s principal output and the extraction profile hardens toward the pure-extraction end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inability_unwillingness_boundary, empirical, 'Unable-versus-unwilling composition of the docket the Court has declined or failed to reach.').

omega_variable(
    threshold_location_jurisprudence,
    'Where does the operative admissibility threshold actually sit in the Appeals Chamber''s case law — the narrow line (unwillingness requires specific intent to shield perpetrators; inability requires near-total collapse) or the broad line this reading asserts (absence of independence or genuine intent suffices)?',
    'Systematic analysis of the Article 17 and Article 19 decisions (Lubanga, Katanga, Gaddafi, Muthaura, and subsequent admissibility challenges) against the Statute''s drafting history.',
    'A narrow threshold confirms the practice_drift finding against this reading''s reference frame and places more of the victim cost on the standing arrangement; a broad threshold would partially vindicate the guardian function and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_location_jurisprudence, empirical, 'Location of the operative unwillingness threshold in the admissibility case law.').

omega_variable(
    positive_complementarity_trajectory,
    'Does the regime catalyze genuine domestic accountability capacity — the transitional trajectory its drafters advertised — or ossify domestic systems into permanent dependence on a backstop that rarely fires?',
    'Compare domestic prosecution rates for atrocity crimes in states parties before and after situations were opened, controlling for conflict intensity and aid flows.',
    'A catalytic record would re-date this constraint''s temporal profile toward transitional support — a mechanism with a real sunset condition as domestic capacity matures; an ossifying record confirms the standing-arrangement reading and the extraction profile authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(positive_complementarity_trajectory, empirical, 'Catalytic versus ossifying effect of the regime on domestic accountability capacity.').

omega_variable(
    p5_structural_lock,
    'Is the regime''s enforcement ceiling structurally locked by the permanent members'' referral, deferral, and non-membership position — a design feature of the Statute rather than a contingent political state?',
    'Not resolvable by observation within the current treaty design; watch for structural change — any permanent member ratifying, referral practice diversifying beyond permanent-member interests, or Assembly-of-States-Parties assertiveness against the Council.',
    'If the lock is structural, the cost of fixing stays prohibitive under either reading and the extraction ceiling is a design constant; if it is contingent, a coalition shift could lower the ceiling without amending the text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_structural_lock, conceptual, 'Whether the great-power veto structure irreducibly caps the regime''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_intl_oversight_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(art17_intl_oversight_tr_t4, article_17_complementarity__international_oversight_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(art17_intl_oversight_tr_t8, article_17_complementarity__international_oversight_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(art17_intl_oversight_tr_t12, article_17_complementarity__international_oversight_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(art17_intl_oversight_tr_t16, article_17_complementarity__international_oversight_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(art17_intl_oversight_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(art17_intl_oversight_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(art17_intl_oversight_be_t4, article_17_complementarity__international_oversight_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(art17_intl_oversight_be_t8, article_17_complementarity__international_oversight_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(art17_intl_oversight_be_t12, article_17_complementarity__international_oversight_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(art17_intl_oversight_be_t16, article_17_complementarity__international_oversight_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(art17_intl_oversight_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(art17_intl_oversight_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(art17_intl_oversight_su_t4, article_17_complementarity__international_oversight_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(art17_intl_oversight_su_t8, article_17_complementarity__international_oversight_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(art17_intl_oversight_su_t12, article_17_complementarity__international_oversight_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(art17_intl_oversight_su_t16, article_17_complementarity__international_oversight_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(art17_intl_oversight_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'complementarity' (Article 17, Rome Statute) covers two structurally distinct constraints that this corpus models as separate stories of one kernel: this oversight reading (low threshold, guardian function, victims as the beneficiary class, epsilon for the standing arrangement authored high) and article_17_complementarity__national_primacy_reading (high threshold, sovereignty shield, states and accused as the protected class, epsilon authored low). The epsilon values diverge because the readings locate the regime's costs differently — impunity borne by victims here, intervention burdens borne by states and accused there. Each story carries its own beneficiaries, victims, metrics, and classification; the affects_constraints edge preserves the shared-kernel family link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
