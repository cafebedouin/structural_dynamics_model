% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Hybrid Statehood Criteria (Normative Legitimacy Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the HYBRID READING of the
 *   montevideo_statehood_criteria kernel: statehood recognition requires both
 *   the objective Montevideo criteria (territory, permanent population,
 *   government, capacity for diplomatic relations) AND normative legitimacy
 *   (demonstrated liberal democratic governance, human rights compliance,
 *   non-aggression). This reading is one of three competing framings of the
 *   same international legal commitment. The hybrid reading embeds a
 *   normative gate into what the declaratory reading treats as an objective
 *   determination. The reading is operationalized through UN admission
 *   voting, regional organization practice, and the rhetoric of conditional
 *   recognition. It gained institutional force during the post-Cold War
 *   liberal internationalist expansion (1989–2005) and continues to shape
 *   recognition disputes (Kosovo, Palestine, Hong Kong autonomy, Catalonia,
 *   Taiwan, Transnistria). The constraint's persistence depends on active
 *   suppression of competing readings and maintenance of the normative
 *   criteria as binding in practice, even though international law formally
 *   treats them as discretionary.
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: institutional power, agenda-setting, beneficiary through normative justification for recognition denial and intervention authority
 *   - non_liberal_secessionists: moderate power, trapped by normative gate that conditions statehood on governance model adoption
 *   - existing_state_community: institutional power, collective agent making recognition decisions, retains discretion under hybrid frame
 *   - peripheral_aspirant_entities: powerless, trapped by requirement to adopt external governance model for recognition
 *   - declaratory_reading_advocates: organized observers contesting the normative layer as neo-colonial imposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Hybrid Statehood Criteria (Normative Legitimacy Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '46b07309-3f89-4db5-8046-99268e30f121').
narrative_ontology:cs_kernel_codification('46b07309-3f89-4db5-8046-99268e30f121', fixed_text).
narrative_ontology:cs_authority_grounding('46b07309-3f89-4db5-8046-99268e30f121', extraction).
narrative_ontology:cs_interpretation_layer_present('46b07309-3f89-4db5-8046-99268e30f121').
narrative_ontology:cs_reading_relation('46b07309-3f89-4db5-8046-99268e30f121', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('46b07309-3f89-4db5-8046-99268e30f121', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('46b07309-3f89-4db5-8046-99268e30f121', foundational, human_rights_democratic_governance_as_statehood_prerequisite).
narrative_ontology:cs_axiom_status(human_rights_democratic_governance_as_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('46b07309-3f89-4db5-8046-99268e30f121', human_rights_democratic_governance_as_statehood_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('46b07309-3f89-4db5-8046-99268e30f121', foundational, liberal_internationalism_legitimate_authority_grounding).
narrative_ontology:cs_axiom_status(liberal_internationalism_legitimate_authority_grounding, holdable).
narrative_ontology:cs_axiom_grounding('46b07309-3f89-4db5-8046-99268e30f121', liberal_internationalism_legitimate_authority_grounding, deontological).
narrative_ontology:cs_reference_frame('46b07309-3f89-4db5-8046-99268e30f121', liberal_democratic_internationalist_legitimacy_gate).
narrative_ontology:cs_drift_state('46b07309-3f89-4db5-8046-99268e30f121', contemporary_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46b07309-3f89-4db5-8046-99268e30f121', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_successor_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, peripheral_aspirant_entities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_governance_as_prerequisite).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, human_rights_compliance_as_legitimacy_gate).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, international_normative_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the recognition vote through UN General Assembly and Security Council, plus de facto veto through major-power consensus. They set the normative criteria (human rights, democracy, non-aggression) and apply them selectively to exclude non-liberal entities while granting recognition to allied states with similar violations. They benefit from the gate by maintaining ideological homogeneity in the state community and gaining rhetorical cover for intervention in non-liberal states' internal affairs. They can exit the constraint by adopting the declaratory reading (purely objective criteria), but they choose not to because exit would deprive them of gatekeeping power.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, beneficiary).

% NGOs, international law scholars, and human rights commissions that advocate for embedding rights protections into statehood criteria. They benefit from the hybrid reading because it gives them legal leverage: they can invoke the normative gate to demand that aspirant entities adopt human rights protections as a condition of recognition. They have mobile exit options (they can switch between readings if the political climate changes) and global reach. They are genuinely organized and resourced, unlike the powerless aspirant entities they advocate for.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Movements and provisional governments seeking statehood on the basis of ethno-national, religious, or ideological identity that does not fit the liberal democratic template (e.g., Hamas Palestine, Hezbollah-affiliated entities, Islamic Emirate of Afghanistan, Catalonia with non-liberal constitutional aspirations). They meet the objective Montevideo criteria but are trapped by the normative gate: they cannot adopt liberal democracy without dissolving their political identity. Recognition is withheld pending governance transformation. Exit options are constrained: they can attempt to suppress their political identity and adopt liberal institutions (a form of structural death), remain in legal limbo, or pursue armed struggle. The constraint suppresses their political expression by making statehood conditional on ideological conformity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    moderate, biographical, identity_locked, regional).

% Existing recognized states (e.g., Russia, China, Turkey, Saudi Arabia) that do not meet the liberal democracy or human rights criteria and face conditional recognition renewal, regime-change pressure, and exposure to humanitarian intervention rhetoric justified by the normative gate. Their statehood is not formally revoked, but it is delegitimized by the hybrid reading. They are powerful enough to resist external pressure sometimes, but the normative gate provides legal cover for sanctions, isolation, and military intervention. Exit options are constrained: they can attempt democratization (which may threaten elite power) or remain under delegitimization pressure. The constraint functions as an extraction mechanism that allows the liberal coalition to justify coercive measures.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_successor_states, payer,
    powerful, generational, constrained, global).

% Stateless or quasi-sovereign communities seeking self-determination and statehood (e.g., indigenous territories, post-conflict regions, colonized peoples) that meet or could meet objective Montevideo criteria but cannot adopt liberal democratic governance quickly due to lack of resources, historical trauma, or cultural incompatibility. They are trapped by the normative gate: they need statehood for self-determination but the recognition system requires governance transformation before admission. Exit from the constraint is impossible without abandoning statehood claims. They are powerless in recognition votes and cannot negotiate the normative criteria. The constraint enforces a specific governance model as the price of sovereignty.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, peripheral_aspirant_entities, payer,
    powerless, biographical, trapped, local).

% The collective body of recognized states that makes recognition decisions through UN admission voting and regional organization practice. Under the hybrid reading, the community retains discretionary power to condition recognition on normative criteria. Individual states within the community have divided interests: liberal democracies prefer the hybrid gate; non-liberal states oppose it; non-aligned states are split. The community is the mechanism through which the constraint is operationalized, not a unified actor. Its composition itself is shaped by the constraint: new statehood admissions depend on meeting the normative gate, so non-liberal entities are less likely to be admitted, making the community more homogeneously liberal over time.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, existing_state_community, agenda_setter,
    institutional, civilizational, analytical, global).

% International law scholars, human rights institutions, and liberal states' foreign policy establishments that advocate for humanitarian intervention as a response to human rights violations. They benefit from the hybrid reading because it provides normative authority for intervention: entities that violate human rights criteria lose statehood legitimacy and become candidates for external intervention. The normative gate creates the legal pretext for regime change and humanitarian military operations. They have analytical exit options (they could adopt the declaratory reading and abandon intervention rhetoric) but they choose not to because intervention authority serves geopolitical interests.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates, agenda_setter).

% Communities that articulate self-determination claims (indigenous peoples, stateless ethnic minorities, post-conflict populations) but are structurally absent from statehood recognition votes. They have no seat at the recognition table and no ability to contest the normative criteria applied to them. Their absence is not accidental but structural: the recognition system is controlled by existing states, which do not extend voting rights to non-state actors. The normative gate is imposed on them by external decision-makers without consultation or consent. They would object to the criteria if present but are kept out by the same power structure that enforces the gate.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, indigenous_and_stateless_peoples, excluded,
    powerless, generational, trapped, local).

% International legal scholars (primarily from post-colonial and non-aligned states), some political theorists, and non-liberal state governments that contest the hybrid reading's normative layer as neo-colonial gatekeeping. They argue statehood determination should be based solely on the objective Montevideo criteria (territory, population, government, capacity for relations) and that normative requirements constitute illegitimate subjective veto power for liberal democracies. They mount organized resistance through academic publishing, UN floor advocacy, and institutional practice in their own states, but they are outweighed in the liberal-dominated international institutional structure. Their analytical exit options are open (they could switch readings), but their political position is constrained by power dynamics.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_reading_advocates, observer,
    organized, generational, analytical, global).

% International law scholars and some state diplomats who argue that statehood is determined by recognition from existing states (constitutive reading), not by text or objective criteria. They occupy a middle position: they acknowledge the hybrid reading's normative concerns but argue the solution is recognizing the actual practice of state recognition rather than imposing external criteria. They resist the hybrid reading's attempt to bind recognition to abstract principles and propose that law is what states do, not what texts say. They have analytical options and organized capacity for advocacy but lack dominant institutional position.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, constitutive_reading_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single international statehood recognition framework: determines which entities are eligible for UN membership, treaty-making capacity, and standing in international institutions. Solves the collective-action problem of distinguishing legitimate states from non-state entities without a centralized global authority.
% TRANSFER_FUNCTION: Transfers recognition authority from the entity seeking statehood to the liberal democratic state coalition, which exercises discretionary gatekeeping power over admission. Extraction flows from non-liberal entities (who face withholding of recognition and exposure to intervention rhetoric) to liberal democracies (who gain normative justification for gatekeeping and intervention authority) and human rights advocates (who gain a legal lever for conditioning recognition on governance compliance).
% ABSENT_VOICES: Indigenous and stateless peoples seeking self-determination would object if present: the recognition system is designed without their participation, and the normative criteria are imposed on them by external decision-makers. Post-colonial and non-aligned states contest the normative layer as neo-colonial but are present as outvoted observers. Non-liberal governance movements reject the liberal democracy requirement as cultural imperialism but are present as targets rather than participants.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished and recognition reverted to objective criteria (declaratory reading), statehood determination would become less discretionary but non-liberal entities would face fewer barriers to admission. The liberal coalition would lose normative justification for gatekeeping and intervention. If the entire recognition system vanished (reverting to constitutive reading or no system), the state community would rely on practice rather than criteria, and non-aligned states argue this would restore pluralism. Disappearance is contested because the viability of alternatives depends entirely on which reading one accepts.
% FOUNDING_PROBLEM: The post-Cold War liberal internationalist movement sought to embed human rights and democratic governance requirements into statehood determination to prevent non-liberal movements from gaining statehood and using international standing to promote non-liberal agendas globally. The founding problem was the fear that recognizing illiberal states would legitimize authoritarianism and human rights abuse internationally.
% FOUNDING_PROBLEM_CORROBORATION: Liberal internationalist scholars and human rights advocates attest the founding problem remains live: recognizing illiberal states without governance conditions would enable authoritarian expansion and regional aggression (cite: interventions in Bosnia, Kosovo, Libya justified on humanitarian grounds). Post-colonial and non-aligned states attest the founding problem is exaggerated and serves as pretext for neo-colonial gatekeeping; they note that liberal democracies have committed equivalent or greater atrocities without losing recognition (cite: US and European colonial histories, Saudi Arabia's continued recognition despite human rights violations). The International Court of Justice's Kosovo advisory opinion (2010) shows the Court unable to resolve which reading is correct: it declined to rule definitively on whether human rights and democracy are prerequisites for statehood, instead suggesting the criteria are applied by practice. Corroboration is divided between the two frames with no decisive arbiter.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, contested).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the normative gate systematically favors liberal democracies and allows them to deny statehood to entities that meet objective criteria. Suppression is higher (0.72) because the constraint requires active enforcement: maintaining the normative criteria as binding despite their absence from the Montevideo text, blocking rival readings' operationalization, and managing recognition votes to enforce the gate. Theater is moderate (0.41) because the human rights and democracy justifications are partly genuine (liberal states do sanction genuine atrocities) but partly performative (the criteria are applied inconsistently, with geopolitical allies receiving exceptions). The measurement series show extraction and suppression rising sharply across the post-Cold War period (1989–2005), corresponding to liberal internationalist institutional consolidation. The temporal profile reveals the constraint was originally weaker (objective criteria only) and gained extractive force as the normative layer was embedded into practice by dominant states. Resistance has remained steady (~0.58) despite rising suppression because challenger readings (declaratory and constitutive) maintain organized advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (non-liberal secessionists, peripheral aspirants, authoritarian successors) and the agenda-setter seats (liberal democracies, existing state community) should compute as different types. From the payer perspective, the constraint is a snare: recognition is withheld, suppression is active, alternatives are collapsed (they cannot get statehood without adopting mandated governance). From the beneficiary perspective, the constraint is a rope: genuine coordination (statehood recognition system) with aligned incentives (enforce liberal norms). The claim is tangled_rope (mixed justification + asymmetric extraction), but the per-seat computation should show the payer seats computing toward snare and the beneficiary seats computing toward rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The liberal democratic coalition benefits from the constraint without running most of it operationally (they are beneficiaries and agenda-setters collectively, though individually they are institutional actors with high power). Their directionality is low (~0.2): they face no effective suppression, no withholding of recognition, no exit pressure — they are the system. Non-liberal secessionists face high directionality (~0.85): they are suppressed by the normative gate, recognition is withheld pending governance change, and exit requires abandoning their political project. Peripheral aspirant entities face the highest directionality (~0.95): they are powerless, trapped, with constrained exit (they cannot exit the recognition system without losing statehood claims entirely). The existing state community collectively faces low d (they make the rules), but individual non-liberal states within that community face higher d as the constraint exposes them to intervention rhetoric. Humanitarian intervention advocates are beneficiaries (they gain normative cover), so they have low d. Declaratory reading advocates are not suppressed but are organizationally constrained; they resist but lose institutional votes, giving them moderate-to-high d (~0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing abusive regimes from gaining statehood) is contested in status. The hybrid reading advocates argue it remains live and requires normative gatekeeping. The declaratory reading advocates argue the problem has been solved (genocides are deterred by other means) and the normative gate now persists as rent collection for the liberal coalition. The constraint does not exhibit classic mandatrophy (function intact, persistence unjustified) but rather what might be called 'reading drift': the same Montevideo text is read differently over time, with the normative layer added progressively. The measurement series show theater rising from 0.12 to 0.41, indicating growing performative content relative to functional necessity — a signal that the founding problem may be degraded. The disappearance_verdict is contested because the constraint's necessity depends entirely on which reading is accepted: if declaratory reading is correct, removing the normative layer would restore proper law; if the hybrid reading is correct, removal would catastrophically degrade the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_textual_absence,
    'Are human rights compliance and liberal democratic governance legitimate statehood criteria, or are they neo-colonial impositions absent from the original Montevideo text?',
    'Archival historical analysis of the Montevideo Convention''s drafting (1933) vs. post-Cold War reinterpretations; comparative legal analysis of how different state blocs read the text; documentation of when and why normative criteria entered practice.',
    'If the criteria are legitimate (embedded implicitly in ''government'' or ''capacity for relations''), the hybrid reading is proper law enforcement. If they are impositions (read into the text later), the constraint is revealed as extractive gatekeeping and the declaratory reading is restored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_criteria_textual_absence, empirical, 'Whether normative criteria were always part of statehood determination or added by liberal internationalists.').

omega_variable(
    liberal_consistency_in_normative_application,
    'Are the normative criteria (human rights, democracy, non-aggression) applied consistently across all statehood candidates, or selectively based on geopolitical alignment?',
    'Systematic audit of recognition votes and rhetoric: compare treatment of candidates with similar human rights profiles but different geopolitical positions (e.g., Hamas Palestine vs. Saudi Arabia membership; Taiwan vs. Vietnam statehood aspirations).',
    'Consistent application would support the hybrid reading as principled gatekeeping. Selective application would reveal the normative criteria as post-hoc justification for geopolitical veto power, converting the constraint from tangled rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_consistency_in_normative_application, empirical, 'Whether the normative gate is applied impartially or masks geopolitical extraction.').

omega_variable(
    cultural_pluralism_vs_liberal_uniformity,
    'Can the liberal democratic and human rights criteria coexist with genuine cultural, religious, and political pluralism in governance, or do they encode liberal monism?',
    'Comparative governance analysis: identify non-liberal systems that meet functional equivalents of liberal criteria (peaceful transfers of power, accountability, rights protection through different institutional forms) and test whether they receive recognition parity.',
    'If pluralism is possible, the constraint can be salvaged as coordination. If liberal forms are strictly required, the constraint encodes cultural domination and the victim set expands to include all non-liberal governance models.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_pluralism_vs_liberal_uniformity, conceptual, 'Whether the normative criteria allow genuine institutional pluralism or require liberal-democratic convergence.').

omega_variable(
    humanitarian_intervention_cover_story,
    'Does the normative legitimacy gate provide genuine protection against atrocity, or does it create a legal pretext for humanitarian intervention that is actually motivated by geopolitics?',
    'Historical analysis of interventions justified via human rights/democracy criteria: compare states where intervention occurred with states where equivalent atrocities were ignored; analyze decision-making evidence from interventionist powers.',
    'If protection is genuine, the constraint serves coordination. If it is pretextual, the constraint becomes a snare that uses human rights language to rationalize power-based intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_cover_story, empirical, 'Whether humanitarian intervention rhetoric is principal motivation or post-hoc justification.').

omega_variable(
    reading_contest_foreclosure,
    'Can the three readings (hybrid, declaratory, constitutive) coexist indefinitely, or is one reading structurally destined to foreclose the others?',
    'Institutional pressure tracking: monitor whether liberal democracies gradually institutionalize the hybrid reading (e.g., via UN Charter amendment, ICJ precedent, regional practice coordination) or whether declaratory reading advocates force recognition consensus.',
    'If the hybrid reading achieves formal codification, the contest ends and extraction becomes openly institutionalized. If declaratory reading gains ground, the normative layer would be delegitimized and recognition would revert to objective criteria.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the three readings will eventually resolve or remain permanently contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement_basis(mont_tr_t1945, observed).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(mont_tr_t1960, observed).
narrative_ontology:measurement(mont_tr_t1989, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1989, 0.28).
narrative_ontology:measurement_basis(mont_tr_t1989, observed).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(mont_tr_t2005, observed).
narrative_ontology:measurement(mont_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(mont_tr_t2015, observed).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(mont_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(mont_be_t1945, observed).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement_basis(mont_be_t1960, observed).
narrative_ontology:measurement(mont_be_t1989, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement_basis(mont_be_t1989, observed).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement_basis(mont_be_t2005, observed).
narrative_ontology:measurement(mont_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(mont_be_t2015, observed).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(mont_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1945, 0.38).
narrative_ontology:measurement_basis(mont_su_t1945, observed).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1960, 0.46).
narrative_ontology:measurement_basis(mont_su_t1960, observed).
narrative_ontology:measurement(mont_su_t1989, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1989, 0.58).
narrative_ontology:measurement_basis(mont_su_t1989, observed).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(mont_su_t2005, observed).
narrative_ontology:measurement(mont_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(mont_su_t2015, observed).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(mont_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_legal_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, liberal_internationalism_institutional_practice).

% DUAL FORMULATION NOTE:
% This constraint is part of the montevideo_statehood_criteria constraint family (three readings of one kernel). Each reading has a distinct epsilon and beneficiary/victim structure. The hybrid_reading (this story) encodes normative criteria into statehood determination, creating high extraction against non-liberal entities. The declaratory_reading treats the Montevideo criteria as purely objective, making statehood determination formally non-discretionary. The constitutive_reading locates statehood determination in the actual recognition practice of existing states, bypassing text entirely. The three readings coexist as competing institutional framings; no single reading has achieved formal dominance, though the hybrid reading has grown institutionally powerful since 1989. All three share the same kernel text but produce different constraint structures; they affect each other through institutional influence (adoption of one reading changes the legitimacy conditions for the others) rather than logical foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
