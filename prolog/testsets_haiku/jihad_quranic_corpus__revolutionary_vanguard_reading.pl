% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Jihad: Individual Obligation Against Apostate Authority
 *   domain: religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates the revolutionary vanguard reading of
 *   the Quranic jihad corpus. Under this reading, jihad is reinterpreted as
 *   an immediate individual obligation (fard 'ayn) that attaches to every
 *   Muslim directly when certain emergency conditions are met, bypassing the
 *   classical Islamic state-monopoly on legitimate force. The reading employs
 *   takfir (declaration of apostasy) to reframe Muslim rulers who do not
 *   implement full Islamic law or who occupy foreign territory as apostate
 *   authorities no longer commanding religious obedience. It invokes
 *   emergency jurisprudence (dharurat) to override classical jurisprudential
 *   safeguards — including the requirement for an imam (legitimate
 *   authority), conditions for just cause, and categorical protection of
 *   non-combatants. The victim set expands to include not only occupation
 *   forces but the apostate rulers and, through a doctrine of collective
 *   guilt, civilians under their authority who do not resist. This reading is
 *   authored here as the standing arrangement under contest — the structural
 *   dynamics as the reading's own frame instantiates them — not as evaluated
 *   against the defensive or legalist readings. The reading is held as a live
 *   position by vanguard movements and contested by classical jurisprudential
 *   authorities and by other readings; the corpus contains it as a factual
 *   constraint in contemporary Islamic political theology, not as a matter
 *   the story adjudicates.
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard_actors (organizational, moderate-to-institutional power; identity_locked exit; immediate-to-biographical time horizon; bypass state monopoly on takfir authority)
 *   - apostate_muslim_rulers (institutional power; powerful enough to enforce state authority but structurally positioned as apostates by the reading's takfir doctrine)
 *   - classical_islamic_jurists_and_authorities (institutional power; defenders of state monopoly on force and ijtihad; read as impediments to the reading's immediate obligation)
 *   - muslim_civilians_under_occupation (powerless-to-organized; trapped or identity_locked exit; classified as either supporters of apostate regimes or themselves combatants via collective guilt)
 *   - occupation_forces_and_foreign_states (institutional/powerful; structural antagonists; legitimacy target for the reading's emergency doctrine)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.87).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Jihad: Individual Obligation Against Apostate Authority").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '2326e428-0843-401a-aa3b-72e14ea66bc7').
narrative_ontology:cs_kernel_codification('2326e428-0843-401a-aa3b-72e14ea66bc7', fixed_text).
narrative_ontology:cs_authority_grounding('2326e428-0843-401a-aa3b-72e14ea66bc7', lineage).
narrative_ontology:cs_interpretation_layer_present('2326e428-0843-401a-aa3b-72e14ea66bc7').
narrative_ontology:cs_reading_relation('2326e428-0843-401a-aa3b-72e14ea66bc7', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('2326e428-0843-401a-aa3b-72e14ea66bc7', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_axiom('2326e428-0843-401a-aa3b-72e14ea66bc7', foundational, takfir_decentralized_individual_authority).
narrative_ontology:cs_axiom_status(takfir_decentralized_individual_authority, holdable).
narrative_ontology:cs_axiom_grounding('2326e428-0843-401a-aa3b-72e14ea66bc7', takfir_decentralized_individual_authority, deontological).
narrative_ontology:cs_axiom('2326e428-0843-401a-aa3b-72e14ea66bc7', foundational, emergency_jurisprudence_overrides_classical_safeguards).
narrative_ontology:cs_axiom_status(emergency_jurisprudence_overrides_classical_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('2326e428-0843-401a-aa3b-72e14ea66bc7', emergency_jurisprudence_overrides_classical_safeguards, empirically_contingent).
narrative_ontology:cs_reference_frame('2326e428-0843-401a-aa3b-72e14ea66bc7', classical_imam_monopoly_framework).
narrative_ontology:cs_drift_state('2326e428-0843-401a-aa3b-72e14ea66bc7', contemporary_vanguard_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2326e428-0843-401a-aa3b-72e14ea66bc7', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_under_occupation).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupied_populations_generic).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurists_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_islamic_jurists_and_authorities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_under_occupation_or_apostate_rule).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupation_forces_and_foreign_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decentralized actors (individual mujahideen, cells, movements) who adopt the vanguard reading and interpret fard 'ayn as binding them immediately, regardless of state authorization. They enforce takfir doctrine against apostate rulers and classical jurists, expand targeting criteria to civilians under collective guilt doctrine, and maintain internal discipline through the reading's jurisprudential frame. Their exit would constitute apostasy within the reading's own logic, making departure catastrophically costly at the identity and social level.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_actors, agenda_setter,
    organized, biographical, identity_locked, continental).

% State authorities (governments, militaries) of Muslim-majority nations classified by the reading as apostate for failing to implement full Islamic law or for cooperating with occupation. They lose monopoly on legitimate force, face existential threat from the vanguard's actions, and cannot reform into legitimacy without surrendering sovereignty or adopting the reading's jurisprudence wholesale. Their options are suppress the vanguard (reinforcing the 'apostate' designation) or yield authority.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_rulers, payer,
    institutional, generational, trapped, national).

% Scholars, legal authorities, and institutional Islam (Al-Azhar, official muftis, madhab leaders) who defend classical jurisprudential constraints (imam requirement, non-combatant immunity, takfir conditions). They are positioned as payers because their authority is overridden by the vanguard reading's claim to direct Quranic interpretation, and their jurisprudential framework is actively suppressed as an obstacle to revolutionary action. Their exit options are constrained: endorsing the vanguard reading undermines their own jurisprudential authority; rejecting it publicly marks them as enemies of resistance.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_islamic_jurists_and_authorities, payer,
    institutional, generational, constrained, global).

% Ordinary Muslims living under occupation or under regimes the reading classifies as apostate. They are reclassified as combatants via collective guilt doctrine or required to support vanguard action. They cannot opt out without marking themselves as complicit with apostate authority (in the reading's frame) or risking vanguard targeting. Their exit options are trapped: staying means bearing the collective guilt classification; leaving the territory is flight; resistance from within is constrained by the vanguard's security and discipline requirements.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_under_occupation_or_apostate_rule, payer,
    powerless, biographical, trapped, regional).

% External military and state actors (foreign armies, imperial powers, regional hegemons) whose occupation the reading treats as a triggering condition for the fard 'ayn obligation. They bear the costs of the vanguard's armed action and face the reading's de-legitimization as non-Muslim occupiers. Their exit options are constrained by geopolitical commitment and perceived security interests; they cannot easily withdraw without ceding territory or admitting strategic defeat.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupation_forces_and_foreign_states, payer,
    institutional, biographical, constrained, global).

% Muslims who accept Islamic obligation to resist occupation but question the vanguard reading's decentralized takfir, emergency jurisprudence, and collective guilt doctrine. They would advocate for classical jurisprudential constraints (imam authority, non-combatant immunity, proportionality) but are excluded from the vanguard's decision-making by the reading's frame that positions them as insufficiently committed or compromised by association with classical authorities. Their absence from the vanguard's jurisprudential deliberation means their objections do not constrain targeting or application of takfir.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, adherents_questioning_the_reading, excluded,
    moderate, biographical, identity_locked, global).

% External academic, legal, and policy analysts observing the reading's instantiation in practice. They document empirical effects, compare the reading to classical jurisprudence and to other readings, and identify the structural dynamics of extraction and suppression. They have no decision-making power within the constraint but provide documentation and critique.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, non_muslim_observers_and_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading does not produce a genuine coordination function in the classical sense. Rather, it coordinates decentralized action against apostate rulers and occupation by reinterpreting fard 'ayn as individual obligation and takfir as decentralized authority. It 'solves' the collective-action problem of how resistance can occur when state authority is captured or occupying — by transferring authority to the vanguard and to individual Muslims acting on the reading's jurisprudence. This is coordination-as-extraction: the coordination it produces is coercive alignment behind the reading's interpretation, not voluntary cooperation.
% TRANSFER_FUNCTION: Moves authority, legitimacy, and the right to deploy force from state institutions and classical jurists to decentralized vanguard actors and individual adherents. Transfers compliance burden from state law to the reading's jurisprudence. Extracts behavioral compliance (participation in or support for vanguard action, acceptance of collective guilt classification) from all Muslims under the reading's jurisdictional scope. Transfers targeting authorization from state military command to individual judgment and cell-level ijithad.
% ABSENT_VOICES: Classical jurists who would defend non-combatant immunity, state authorities who would claim legitimate monopoly on force, Muslims who would advocate for defensive-only or legalist readings, occupation-subject populations who would reject collective guilt classification, and diaspora communities who would resist vanguard framing of their situation as requiring armed struggle. These voices are excluded by the vanguard reading's takfir doctrine and emergency jurisprudence, which position them as obstacles to resistance or as apostates. Their absence means the reading's expansion of victims and suppression of alternatives proceeds without internal check from these constituencies.
% DISAPPEARANCE_RATIONALE: If the vanguard reading vanished, the world would partially rearrange and partially stay the same depending on the observer seat. For vanguard-adherent movements, the disappearance would eliminate the jurisprudential justification for decentralized takfir and individual fard 'ayn, requiring reversion to either classical jurisprudence (imam requirement, state monopoly) or the defensive reading (spiritual struggle emphasis). For occupied populations, it would remove the collective guilt classification and decentralized obligation to support vanguard action, reducing extraction. For classical jurists and state authorities, it would restore authority and jurisprudential consensus. However, the underlying conflict structures (occupation, apostate governance) would remain, and the defensive or legalist readings would likely become available alternatives rather than representing world-unchanged. The contest is whether the reading is essential to the conflict or a particular jurisprudential frame on a persistent structural problem.
% FOUNDING_PROBLEM: Muslim lands are occupied by foreign military forces, and Muslim-majority states are governed by rulers who do not implement full Islamic law or who cooperate with occupying powers. Classical Islamic jurisprudence, by the reading's assessment, has been compromised by state capture and institutional accommodation with these rulers, leaving the classical state-centered authorization structures unavailable for legitimate resistance. The founding problem is how to resist occupation and apostasy when state authority is not available or is positioned as the antagonist.
% FOUNDING_PROBLEM_CORROBORATION: The vanguard reading's adherents and various Islamic political movements attest that occupation and apostate governance are live problems requiring action. Classical jurists and mainstream Islamic authorities attest that the founding problem is overstated — Islamic jurisprudence provides resources for resistance without decentralizing takfir or eliminating non-combatant immunity. Occupation-subject populations and international legal observers contest whether the reading's scope of the problem (expansion of victims, elimination of safeguards) accurately reflects the underlying conflict. No party outside the vanguard movement attests that the founding problem requires the specific jurisprudential solution (decentralized fard 'ayn, emergency doctrine overrides) the reading proposes. The corroboration is one-directional: vanguard movements attest their founding problem; others dispute both the problem's scope and the reading's solution.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87) because the reading extracts compliance with decentralized vanguard jurisprudence that overrides individual agency and classical safeguards, expanding the obligation (fard 'ayn) to act or bear collective guilt. Suppression is very high (0.92) because the reading requires elimination of alternative jurisprudential framings (the defensive and legalist readings) and suppression of state authority structures that would otherwise constrain individual action — exit from the reading's framework is identity_locked (rejecting the vanguard's fard 'ayn is framed as Islamic apostasy itself). Theater ratio is low (0.22) because the reading treats its jurisprudential claims as substantive obligations with material consequences, not as performative theater; however, the ratio is not near-zero because empirical implementation often involves ideological maintenance and rhetorical invocation of emergency conditions that may persist beyond any objective threat level. Measurement series shows extractiveness and suppression rising over the interval: as the reading becomes institutionalized within vanguard movements, the suppression requirement increases to maintain internal discipline and prevent defection to classical jurisprudence, and the extractiveness of the obligation deepens as the vanguard consolidates control and expands targeting criteria. Theater ratio increases modestly because performative invocation of emergency and takfir doctrine becomes necessary to sustain the reading's legitimacy when empirical conditions improve or occupation does not end.
 *
 * PERSPECTIVAL GAP:
 *   From the vanguard seat, the reading is an urgent jurisprudential correction to classical authorities who have accommodated apostate rulers and abandoned the obligation to resist occupation. The constraint appears as a restoration of true Islamic law against institutional corruption. From the classical jurists' seat, the same reading is an erosion of legal order and replacement of schooled jurisprudence with individual qiyas (analogy) and takfir unmoored from classical conditions. From the civilian seat, the reading is coercive reclassification into combatancy without choice or redress. From the apostate ruler's seat, it is a delegitimization doctrine that transfers authority to unaccountable non-state actors. These perspectives do not compute to different types via the engine's per-seat mechanics because the core structural asymmetry (decentralized authority extraction, zero beneficiaries, universal suppression of alternatives) is invariant across seats. The divergence is in how each seat experiences the legitimacy and necessity of the constraint — the engine's directionality and effectiveness computation should show all seats experiencing high extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholders benefit from this constraint in the classical sense — no party receives rents or collects systematically from the reading's operation (no `beneficiaries` array is populated). The vanguard actors who instantiate the reading bear costs (risk of death, internal discipline, ideological purity enforcement) and distribute costs to others (civilians, state authorities, opposition forces). This is structurally a pure extraction mechanism: it compels compliance via decentralized authority (takfir doctrine) without compensation. The classical jurists are positioned as payers in the sense that their authority is overridden and their jurisprudential framework is suppressed. Apostate rulers are victims because they lose monopoly on authority and face existential threat. Civilians are victims because they are classified into combatancy without consent and become subject to targeting. The directionality is uniformly high d (near 1.0) for all named stakeholders except the vanguard actors themselves, whose d is ambiguous: they are both the enforcers (low d ordinarily) and the subjects of the identity-locked obligation (high d). No directionality override is needed because the structural data (victims, no beneficiaries, suppression of alternatives, decentralized enforcement via takfir) already grounds a uniformly extractive directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a mandatrophy candidate that the classical-to-revolutionary transition reveals. The reading's founding problem is legitimate — occupation of Muslim lands and governance by rulers that classical Islamic law itself treats as illegitimate under certain conditions. However, the reading's solution (decentralized takfir, emergency jurisprudence, collective guilt targeting) persists even after the foundational problem's parameters shift: when occupation ends, apostate rulers are replaced, or emergency conditions decay, the vanguard reading does not automatically revert to classical jurisprudence. Instead, the emergency doctrine becomes institutionalized as permanent, and the takfir scope expands to encompass opposition within the vanguard's own ranks. Measurement data show extractiveness and suppression rising over time despite no corresponding change in the external threat level, suggesting the reading's jurisprudence has become self-reinforcing rather than problem-responsive. This is the signature of mandatrophy: the constraint persists because the reading's authority structure (decentralized vanguard) benefits from the emergency state, not because the empirical conditions warrant it. The remedy — restoration of classical jurisprudential constraints and return of state monopoly on force — is blocked by the identity_locked exit: rejecting the vanguard reading is coded as apostasy within the vanguard frame, making reversal catastrophically costly for adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_legitimacy_scope,
    'Who authorizes takfir (declaration of apostasy) and against whom does it justly apply? Does the reading''s takfir scope encompass only explicit theological apostasy, or administrative governance failure, or both?',
    'Textual analysis of Quranic and hadith precedent; comparison across classical madhabs (schools) on takfir doctrine; empirical study of how contemporary vanguard movements apply takfir in practice.',
    'Narrow scope (explicit doctrinal apostasy only) substantially reduces victim set and restrains decentralization. Broad scope (governance failure → takfir) expands victims exponentially and eliminates any structural constraint on individual vanguard action. This is the pivot between emergency jurisprudence and blanket authorization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(takfir_legitimacy_scope, conceptual, 'The scope of legitimate takfir authority determines whether the reading enables restrained emergency action or unlimited decentralized violence.').

omega_variable(
    emergency_doctrine_durational_boundary,
    'Does emergency jurisprudence (dharurat) that overrides classical safeguards apply only during acute occupation/oppression, or does the reading treat emergency as a permanent state?',
    'Textual exegesis of the reading''s foundational works; longitudinal study of how practitioners transition between emergency and post-emergency jurisprudence; analysis of whether restoration conditions are ever reached or perpetually deferred.',
    'If emergency is temporary and conditional, the constraint''s extractiveness and suppression should decay as conditions improve — measurement series would show trajectory toward classical jurisprudence. If emergency is permanent (the reading''s implicit default), suppression and extractiveness remain high indefinitely and the constraint approaches piton (inertial maintenance of emergency doctrine long after original justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_doctrine_durational_boundary, empirical, 'Whether emergency jurisprudence is a temporary override or a permanent legitimacy foundation.').

omega_variable(
    civilian_combatancy_collective_guilt_mechanism,
    'By what structural principle does the reading classify civilians as combatants? Is it political identity (subjects of apostate state), institutional participation (taxation/conscription support), or intrinsic collective guilt (the reading''s own framing)?',
    'Detailed textual analysis of how the reading establishes civilian status; comparison with other armed conflict traditions'' combatancy criteria; study of how vanguard movements apply the doctrine to decide targeting.',
    'If civilians are combatants by institutional participation, targeting is narrower (those who directly support the authority). If by political identity alone, all subjects are valid targets and the victim set becomes the entire population under apostate rule. Collective guilt narrows neither — it suspends ordinary immunity categorically. The mechanism determines whether the reading is restrained revolutionary action or indiscriminate extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_combatancy_collective_guilt_mechanism, conceptual, 'The principle by which the reading categorizes civilians as legitimate targets.').

omega_variable(
    state_monopoly_elimination_vacuum_structure,
    'Does the reading''s bypass of state authority replace state monopoly with a different structure (decentralized but rule-bound vanguard), or does it eliminate the monopoly entirely and leave individual ijithad as the only authority?',
    'Analysis of how the reading defines constraints on individual action post-takfir; study of whether vanguard movements that adopt this reading establish internal discipline structures; comparison with other decentralized authority systems.',
    'Structured decentralization (vanguard discipline, ijithad within bounds) could retain some constraint on extraction. Pure elimination of monopoly leaves each actor as sole arbiter of fard ''ayn for themselves — maximum decentralization, maximum extractiveness, no institutional check on takfir or targeting. This is the reading''s most destabilizing structural variant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_elimination_vacuum_structure, empirical, 'Whether the reading replaces state monopoly with a different authority structure or eliminates monopoly entirely.').

omega_variable(
    kernel_contest_reading_distinction,
    'This reading coexists with defensive_spiritual_reading and expansionist_legalist_reading as three live positions in Islamic jurisprudential discourse. Are these readings genuinely incommensurable (each would foreclose the others if both held sway in a single framework), or do they represent different but coexistent policy choices held by different parties?',
    'Structural analysis of whether each reading''s foundational axioms logically entail the rejection of the others'' core premises; empirical study of whether any Islamic authority structure or movement has held elements of multiple readings simultaneously or sequentially.',
    'If incommensurable, classify reading_relations as forecloses. If coexistent, classify as coexists_with. The relation type shapes the engine''s handling of cross-reading contamination and foreclosure dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_distinction, conceptual, 'The logical and institutional relationship between the three competing readings of the jihad kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(jiha_tr_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jiha_tr_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(jiha_be_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(jiha_be_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(jiha_be_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 25, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(jiha_su_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(jiha_su_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(jiha_su_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 25, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.18).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_takfir_doctrine).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_jurisprudence_override).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested jihad_quranic_corpus kernel. The same source Quranic material (Quran 2:216, 9:5, 9:29, etc.) is read differently by the defensive_spiritual_reading and the expansionist_legalist_reading constraints. Each reading instantiates a different constraint with a different epsilon, different victims, and different structural dynamics. The three readings are linked via network.affects_constraints to indicate family membership, not equivalence. Each story is written only for its own reading, without averaging or hedging across readings — this story's epsilon (0.87) reflects the revolutionary vanguard reading's own measure of extraction, not a blended measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
