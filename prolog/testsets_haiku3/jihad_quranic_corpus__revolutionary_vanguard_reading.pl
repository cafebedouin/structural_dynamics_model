% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)
 *   domain: religious_jurisprudence/political_theology
 *
 * SUMMARY:
 *   The revolutionary vanguard reading of jihad as immediate individual
 *   obligation (fard 'ayn) against apostate rulers and occupiers operates by
 *   decentralizing Islamic jurisprudential authority through takfir doctrine
 *   and emergency jurisprudence. Unlike classical Islamic law's constraints
 *   (imam authority, non-combatant immunity, proportionality, invitation
 *   before attack), this reading claims that individual Muslims are obligated
 *   to act independently against designated targets, suspending traditional
 *   safeguards. The kernel is the Quranic corpus and the jurisprudential
 *   tradition it grounds; the reading interprets this kernel through the lens
 *   of revolutionary obligation rather than defensive constraint or legalist
 *   conditions. The reading's structural delta: apostate Muslim rulers and
 *   occupier states enter the beneficiary column (targets for removal);
 *   vanguard leadership becomes the distributed authority; ordinary Muslims
 *   in vanguard territories become moral conscripts; civilians are designated
 *   as collectively guilty through proximity or complicity. This constraint
 *   story models ONE reading of the contested kernel—the revolutionary
 *   vanguard instantiation—not the defensive spiritual reading or the
 *   expansionist legalist reading (which are other constraints in the
 *   family). The authored claim and metrics are independent: the constraint
 *   is CLAIMED as snare (extraction via decentralized authority bypassing
 *   state and classical jurisprudence) while the metrics describe high
 *   extractiveness (0.87), maximal suppression (0.92), and low theater
 *   (functional rather than performative). The engine computes divergence; no
 *   reconciliation is applied.
 *
 * KEY AGENTS:
 *   - revolutionary_vanguard_leadership: distributes takfir authority and targeting; claims fard 'ayn obligation on believers; benefits from moral urgency and expanded scope of legitimate targets
 *   - apostate_muslim_rulers: designated targets; lose monopoly on Islamic legitimacy; bear institutional delegitimization and violent pressure
 *   - occupier_states: designated targets; face decentralized armed opposition framed as religious obligation; constrained by claim that resistance to vanguard is itself apostasy
 *   - civilian_population: implicitly targeted via collective guilt; designated as legitimate targets through proximity/complicity to apostate rulers or occupiers; lack exit options
 *   - orthodox_islamic_scholars: subject to takfir for maintaining classical jurisprudential constraints; bear intellectual and physical threat; their exit requires abandoning scholarly tradition
 *   - international_law_advocates: observe the constraint's operation as breach of laws of war; analytical seat producing counter-verdicts that feed state policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.87).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '2e9e2964-25f8-4853-a92c-ac46d0fff3bf').
narrative_ontology:cs_kernel_codification('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', fixed_text).
narrative_ontology:cs_authority_grounding('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', extraction).
narrative_ontology:cs_interpretation_layer_present('2e9e2964-25f8-4853-a92c-ac46d0fff3bf').
narrative_ontology:cs_reading_relation('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', foundational, individual_obligation_supersedes_organizational_hierarchy).
narrative_ontology:cs_axiom_status(individual_obligation_supersedes_organizational_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', individual_obligation_supersedes_organizational_hierarchy, empirically_contingent).
narrative_ontology:cs_axiom('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', foundational, classical_jurisprudential_constraints_enable_oppression).
narrative_ontology:cs_axiom_status(classical_jurisprudential_constraints_enable_oppression, holdable).
narrative_ontology:cs_axiom_grounding('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', classical_jurisprudential_constraints_enable_oppression, empirically_contingent).
narrative_ontology:cs_axiom('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', secondary, takfir_authority_decentralized_to_believers).
narrative_ontology:cs_axiom_status(takfir_authority_decentralized_to_believers, holdable).
narrative_ontology:cs_axiom_grounding('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', takfir_authority_decentralized_to_believers, deontological).
narrative_ontology:cs_reference_frame('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', quranic_obligation_unmediated_by_state).
narrative_ontology:cs_drift_state('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', contemporary_state_securitization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e9e2964-25f8-4853-a92c-ac46d0fff3bf', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupier_states).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_population_as_collective_guilt_bearers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, orthodox_islamic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, ordinary_muslims_in_vanguard_territories).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, ordinary_muslims_in_vanguard_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinates decentralized cells interpreting Quranic obligation; declares takfir against rulers and occupiers; authorizes emergency jurisprudence that suspends classical safeguards; frames armed action as obligatory rather than optional. Leadership benefits from the framework's moral urgency and from the expansion of their authority into theological interpretation and targeting authorization. Exit would require renouncing the foundational theological claim.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership, agenda_setter,
    organized, biographical, trapped, global).

% Designated as apostates by takfir; targeted as immediate obligation for removal or coercion; bear violent pressure and institutional delegitimization via the revolutionary reading. Their capacity to defend themselves is constrained by the reading's claim that resistance to the vanguard is itself apostasy. Their only options are capitulation, securitization against the threat, or assertion of counter-takfir claims.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_muslim_rulers, payer,
    powerful, biographical, constrained, national).

% Designated as targets of immediate individual obligation; targeted by decentralized cells claiming religious authorization; bear operational and reputational costs from the constraint's expansion of combatancy. Their options are withdrawal (renouncing occupation), intensified securitization, or counter-designation of the vanguard as non-combatants (whose killing therefore violates international law).
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupier_states, payer,
    powerful, generational, constrained, global).

% Implicitly designated as legitimate targets via collective guilt mechanism (supporting or not resisting apostate rulers or occupiers constitutes complicity). Civilians lack exit options from the territories where they live or from the status categories (nationality, proximity to targets) that place them at risk. Their vulnerability is heightened by the suspension of classical non-combatant immunity doctrines.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_population_as_collective_guilt_bearers, payer,
    powerless, immediate, trapped, regional).

% Subject to takfir themselves for maintaining classical jurisprudential constraints (non-combatant immunity, imam authority, proportionality requirements). Bear intellectual and physical threats for upholding traditional safeguards. Their exit would require abandoning the Islamic scholarly tradition they are embedded in; their resistance is framed as apostasy rather than legitimate disagreement.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, orthodox_islamic_scholars, payer,
    moderate, generational, identity_locked, global).

% Caught between the vanguard's moral imperative (framed as obligatory on every Muslim) and the costs of participation or refusal. Participation exposes them to state counter-measures; refusal exposes them to takfir accusations and social sanction within vanguard-controlled spaces. Their identity as Muslims is weaponized to claim their assent. Geographic mobility is constrained by the territorial scope of the conflict.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, ordinary_muslims_in_vanguard_territories, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, ordinary_muslims_in_vanguard_territories, beneficiary).

% Would argue for historical contextualization of Quranic jihad passages, reinterpretation via modern human-rights frameworks, and rejection of takfir as a tool of religious authority. Structurally excluded from the vanguard's theological conversation because their interpretive premises would delegitimize the reading's foundational claims. Their voices are absent from spaces where the revolutionary reading is authoritative.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, liberal_islamic_reformers, excluded,
    moderate, generational, mobile, global).

% Document the constraint's operation as a breach of laws of war (non-combatant immunity, targeting of civilians, absence of proportionality review). Their role is analytical: they take testimony, conduct investigations, and produce verdicts that feed state counter-measures and humanitarian advocacy. They have no seat in the vanguard's decision-making.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, international_law_advocates, observer,
    institutional, generational, analytical, global).

% The Quranic text itself (the kernel) is treated as vindicated by the vanguard reading's operational interpretation. This non-agent entry records that the constraint operates by claiming to implement the corpus correctly and by reinterpreting contested passages (e.g., Q. 9:5, the 'Verse of the Sword') as authorizing the vanguard's decentralized targeting. The corpus does not collect rents but its authority is the legitimacy foundation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, quranic_corpus_as_kernel, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jihad_quranic_corpus__revolutionary_vanguard_reading, quranic_corpus_as_kernel).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Decentralized mobilization of Muslims toward removal of apostate rulers and occupiers; coordination of individual obligatory action via shared takfir doctrine and emergency jurisprudence; replacement of state monopoly on authority to declare war with distributed vanguard authority.
% TRANSFER_FUNCTION: Transfers moral legitimacy from state-based classical jurisprudence to decentralized vanguard cells; transfers targeting authority from imam/sultan to any believer meeting the vanguard's theological criteria; transfers immunity from non-combatants to expanded combatant category via collective guilt; transfers lives and security from civilian populations designated as collectively complicit.
% ABSENT_VOICES: Liberal Islamic reformers advocating historical contextualization and human-rights reinterpretation; Sunni and Shia classical jurists affirming imam authority and non-combatant immunity; victims of vanguard attacks who cannot testify; territories where the vanguard operates with state-imposed information restriction; international humanitarian law advocates whose interpretive framework is pre-rejected as Western imperialism.
% DISAPPEARANCE_RATIONALE: If the revolutionary vanguard reading of jihad obligation disappeared, apostate rulers would lose delegitimization pressure from internal religious authority; occupier states would face reduced non-state armed opposition justified by Islamic law; classical Islamic jurisprudence and non-combatant immunity would re-consolidate as the authoritative interpretation; ordinary Muslims would cease receiving claims that armed participation is fard 'ayn (individually obligatory); territories currently organized around vanguard authority would reorganize around state or alternative institutional authority.
% FOUNDING_PROBLEM: Islamic territories are governed by rulers who have abandoned Islamic law; foreign occupiers control Muslim lands; classical jurisprudence's constraints (imam authority, proportionality, non-combatant immunity) are read as complicity with oppression; individual Muslims lack direct moral agency to resist.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard ideologues attest the founding problem is live and urgent. Classical Islamic scholars attest the founding problem is mischaracterized — Islamic law permits legitimate resistance to oppression within jurisprudential constraints, not by abandoning them. Victims of vanguard attacks (families of casualties, survivors) attest the problem the reading was built to solve does not justify targeting civilians. Independent historians and religious scholars note that the revolutionary vanguard reading crystallized in specific mid-20th-century geopolitical contexts (colonialism, state nationalism, Qutb's writings) rather than arising from the Quranic corpus itself; corroboration comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.87) because the constraint transfers targeting authority, moral legitimacy, and civilian immunity status from classical jurisprudence and state authority to decentralized vanguard cells. The measurement series shows extraction rising from 0.68 (initial contestation when the reading first crystallized mid-20th century) to 0.87 (contemporary saturation when the reading has spread across multiple organizational networks and geopolitical contexts). Suppression is maximal (0.92) because the constraint's persistence depends on sustained coercion: dissenting voices (classical scholars, reformers) face takfir; civilians under vanguard control cannot publicly reject the obligation; participation is framed as mandatory rather than chosen. Suppression increases from 0.71 to 0.92 across the interval, tracking the vanguard's institutional maturation and the hardening of enforcement mechanisms (internal discipline, punishment for those who refuse mobilization, targeting of internal dissenters). Theater ratio is low (0.18) because the reading's functional purpose (mobilizing armed action against specified targets) is its primary activity, not performance. Theater remains low and stable because even defensive communications (justification, recruitment rhetoric) are structurally necessary to maintain the authority claims, not merely theatrical ornament. The one-time grid ensures every metric is authored at every examined time point: (t=0,9,18,27,36,45) creates a 6-point series per metric, enabling lifecycle analysis of the reading's consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The vanguard leadership's seat and the civilian victim's seat compute radically different type classifications from the same structural data. From the vanguard leadership's position, the arrangement is genuine coordination (mobilizing believers toward liberation) enforced by justified obligation. From the civilian's position, the same structure operates as extraction: moral urgency is imposed externally, participation is coerced via takfir threat, and targeting designation is made without their consent. The leadership experiences the constraint as moral clarity and distributed authority; civilians experience it as coercive vulnerability and identity-weaponization. The engine computes these divergences from the power/exit/scope differences (leadership: organized/trapped/global vs. civilians: powerless/trapped/regional) and from the beneficiary/victim declarations. The authored claim (snare) aligns with the victim perspective; the leadership would claim rope (genuine coordination). This divergence is diagnostic: the engine's per-seat computation flags where the parties' experiential worlds have diverged so radically that a single shared framework cannot describe both truthfully.
 *
 * DIRECTIONALITY LOGIC:
 *   The vanguard leadership sits near the beneficiary end (d ≈ 0.15): they distribute authority, frame themselves as implementing obligation, and benefit from the moral urgency that justifies their organizational role. Apostate rulers and occupier states sit near the target end (d ≈ 0.85): they are designated as removal targets, lose legitimacy within the reading's frame, and bear operational pressure. Ordinary Muslims in vanguard territories sit near symmetric-with-asymmetric-costs (d ≈ 0.6): they benefit nominally from the reading's claim to liberate Islamic territories, but they bear the costs of participation pressure, civilian targeting risk, and takfir threat if they refuse. Their exit is identity-locked (renouncing Islam as a believer, or fleeing), not mobile. Orthodox scholars sit at high target (d ≈ 0.75): their traditional authority is displaced, they face takfir accusations, their classical safeguards are overridden, yet they cannot exit without renouncing their scholarly tradition. Civilians designated as collectively guilty sit at extreme target (d ≈ 0.95): they bear targeting risk from the vanguard, lack any seat in the decision-making, and cannot exit the status categories that place them at risk (nationality, proximity, civilian status). This seat divergence is the signature of extraction: the vanguard leadership's low d contrasts sharply with the powerless civilians' extreme d, both mediated through the same decentralized theological framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The revolutionary vanguard reading stands on the claim that individual Muslims are obligated by Quranic obligation to resist apostate rule and occupation. However, this claim rests on an interpretation of the Quranic corpus that classical Islamic jurisprudence explicitly rejects. The mandate is: 'Remove rulers who have abandoned Islam and occupiers of Muslim lands through Islamic obligation.' The function that mandate originally served was: 'Mobilize resistance to oppressive rule and foreign occupation using moral authority.' Over 70+ years (from mid-20th-century crystallization to present), the mandate has persisted but the function has mutated. Contemporary observations: (a) The reading is now used to mobilize violent action against civilians designated as collectively guilty, not merely against rulers and occupiers. (b) The reading is enforced via takfir against dissenters within Muslim communities, not merely against external targets. (c) The reading has become the justification for internal hierarchies (vanguard leadership) that replicate the centralized authority structure it originally opposed. The mandate has outlived its original problem-solving function and now primarily serves the organizational and ideological interests of the vanguard leadership. This is mandatrophy: the constraint persists because an institutional leadership profits from maintaining the obligation's scope, not because the obligation solves the coordination problem it was built for. The commentary in six_questions.founding_problem_status='contested' and disappearance_verdict='world_rearranges' corroborates this: if the reading disappeared, territories would reorganize, but apostate rulers and occupiers would NOT disappear (only the Islamic law justification for their removal would); the vanished reading would be replaced by nationalist resistance, secular anti-imperialism, or other frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_authority_decentralization_naturalness,
    'Is the decentralization of takfir authority and targeting decisions a natural consequence of the Quranic obligation, or is it a strategic institutional choice by the vanguard leadership?',
    'Textual analysis comparing Quranic verses on obligation and authority; historical analysis of when and why takfir decentralization emerged in vanguard ideology; comparison to classical jurisprudence''s centralization of takfir in qualified scholars; ethnographic study of vanguard networks'' actual decision-making (centralized vs. decentralized).',
    'If decentralization is natural consequence of the obligation, the reading''s claim to authenticity strengthens. If decentralization is strategic choice, it supports the snare classification: the reading''s primary function is expanding the vanguard leadership''s de facto authority by distributing it as obligation. The latter is consistent with high extractiveness and mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takfir_authority_decentralization_naturalness, empirical, 'Whether takfir decentralization is obligatory or strategic.').

omega_variable(
    collective_guilt_mechanism_legitimacy,
    'Is the designation of civilians as collectively guilty through proximity/complicity an inevitable implication of the obligation to resist apostate rulers, or is it an unnecessary expansion driven by vanguard leadership''s targeting interests?',
    'Textual analysis of Quranic verses on collective responsibility and non-combatant immunity; comparison of the reading''s targeting practice to its stated jurisprudential justification; documentation of instances where the vanguard restricted or expanded collective guilt designation based on operational need; testimony from classical Islamic scholars on whether collective guilt designation violates consensus jurisprudence.',
    'If collective guilt is inevitable, part of the extractiveness may be justified as coordination cost. If unnecessary expansion, it confirms high extractiveness is driven by leadership interests in expanding the target set beyond rulers and occupiers. The vanguard''s practice has expanded collective guilt designation over time (trajectory visible in measurements: theater_ratio remains low because targeting is functional, not because justification is rigorous).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_guilt_mechanism_legitimacy, empirical, 'Whether civilian targeting via collective guilt is obligatory or optional.').

omega_variable(
    emergency_jurisprudence_permanence,
    'Are the suspension of classical jurisprudential safeguards (imam authority, non-combatant immunity, proportionality) intended as emergency measures pending the establishment of Islamic governance, or as permanent replacement of classical jurisprudence?',
    'Analysis of vanguard ideological texts and practical decisions: do they describe classical safeguards as to-be-reinstated post-victory, or as permanently superseded? Comparison of vanguard networks that have established territorial control (e.g., Daesh, Taliban) to determine whether they reinstated classical jurisprudence or maintained emergency framework. Interviews with vanguard theoreticians on post-victory legal structure.',
    'If emergency measures are transitional, the reading''s classification as snare might shift toward tangled_rope (extraction justified as temporary coordination cost for liberation). If permanent replacement, snare classification is confirmed: the reading''s primary function is expanding the leadership''s authority indefinitely. Available evidence suggests the framework tends toward permanent replacement: vanguard-controlled territories rarely reinstate classical jurisprudence, and ideological texts describe classical safeguards as institutional constraints that enabled oppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_jurisprudence_permanence, empirical, 'Whether emergency jurisprudence is transitional or permanent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.92) primarily structural (coercive infrastructure preventing exit) or internalized (identity-fusion making exit appear as identity-dissolution)?',
    'Post-exit trajectory analysis: do defectors from vanguard territories report continued suppression pressure (external threats, family sanction) or ongoing self-imposed suppression (guilt, identity dysphoria, sense of apostasy)? Longitudinal study of defectors over 2+ years; comparison of those who relocated outside vanguard-influenced communities vs. those who stayed but refused participation.',
    'If suppression is primarily structural, it can be reduced by state counter-measures (securitization, territory denial). If primarily internalized, counter-measures may actually strengthen suppression (external threat reinforces identity-fusion). Effective intervention strategies differ radically: structural suppression requires exit facilitation and alternative community space; internalized suppression requires identity-reframing support (theological education, community reintegration, psychological support for identity dysphoria).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Composition of suppression mechanism.').

omega_variable(
    quranic_corpus_authenticity_interpretation,
    'Does the revolutionary vanguard reading''s interpretation of the Quranic corpus represent the most faithful reading, a plausible alternative, or a distortion driven by geopolitical context?',
    'Peer-reviewed Islamic scholarship comparing textual basis for the three readings (defensive, expansionist, revolutionary); historical analysis of Quranic interpretation traditions across 1400 years; documentation of when revolutionary vanguard reading crystallized (mid-20th century) and what geopolitical events co-occurred (colonialism, state nationalism, Qutb''s writings); comparison to classical jurisprudence''s textual analysis.',
    'If the reading is most faithful, the kernel is validly instantiated and alternative readings are distortions. If it is a plausible alternative, all three readings are equally legitimated by the corpus (coexistence confirmed). If it is a distortion, the reading''s authority rests on geopolitical positioning rather than textual authenticity (supports snare classification: the constraint persists via institutional power, not via compelling interpretation). Preliminary scholarship suggests the revolutionary reading crystallized in mid-20th century (Qutb era) in response to specific anti-colonial contexts, suggesting geopolitical determination rather than textual inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_corpus_authenticity_interpretation, conceptual, 'Textual authenticity of the revolutionary vanguard reading.').

omega_variable(
    kernel_contest_foreclosure_boundaries,
    'Do the three readings of the jihad quranic corpus logically foreclose each other, or do they coexist as live alternatives held by different parties within Islam?',
    'Formal logical analysis of the three readings'' core premises: does affirming one necessarily deny another? Empirical observation: do contemporary Islamic communities hold multiple readings simultaneously (different scholars holding different views within the same organizational structure), or are readings segregated by organizational faction? Ethnographic study of scholarly debate: is debate framed as logical refutation or as interpretive pluralism?',
    'If readings foreclose each other logically, one reading''s dominance would eliminate the others (only defensive, expansionist, or revolutionary would remain viable). If readings coexist, all three could persist in different sectors of Islamic thought. Contemporary evidence shows coexistence (all three readings are live in different organizational contexts), suggesting logical compatibility rather than foreclosure. This affects how the readings are related in cs_structure.reading_relations: coexists_with vs. forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_boundaries, conceptual, 'Logical structure of the three readings'' relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t9, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 9, 0.11).
narrative_ontology:measurement(jiha_tr_t18, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(jiha_tr_t27, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 27, 0.17).
narrative_ontology:measurement(jiha_tr_t36, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 36, 0.19).
narrative_ontology:measurement(jiha_tr_t45, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 45, 0.18).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(jiha_be_t9, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 9, 0.76).
narrative_ontology:measurement(jiha_be_t18, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(jiha_be_t27, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 27, 0.85).
narrative_ontology:measurement(jiha_be_t36, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 36, 0.87).
narrative_ontology:measurement(jiha_be_t45, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 45, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(jiha_su_t9, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 9, 0.82).
narrative_ontology:measurement(jiha_su_t18, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(jiha_su_t27, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 27, 0.91).
narrative_ontology:measurement(jiha_su_t36, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 36, 0.92).
narrative_ontology:measurement(jiha_su_t45, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 45, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.14).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% The revolutionary vanguard reading is one of three sibling readings of the contested JIHAD_QURANIC_CORPUS kernel. The three readings share a kernel (the Quranic text and jurisprudential tradition) but instantiate different constraints via different interpretive frames: (1) the defensive spiritual reading emphasizes jihad al-nafs and defensive limitation; (2) the expansionist legalist reading emphasizes governance extension with jurisprudential conditions; (3) the revolutionary vanguard reading (this constraint) emphasizes decentralized individual obligation overriding classical safeguards. The ε values differ substantially: defensive spiritual (~0.15, genuine coordination with classical constraints), expansionist legalist (~0.35, coordination with extractive overlay from governance expansion), revolutionary vanguard (~0.87, pure extraction via decentralized authority and collective guilt). The readings form a constraint family linked by their shared kernel and by their institutional competition for interpretive authority. Each story carries its own claimed_type (defensive=rope, expansionist=tangled_rope, revolutionary=snare) derived from its structural properties, not from averaging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
