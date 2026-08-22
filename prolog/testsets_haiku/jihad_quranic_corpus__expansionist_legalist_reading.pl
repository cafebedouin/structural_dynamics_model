% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Jihad as Obligation to Establish Islamic Governance (Expansionist Legalist Reading)
 *   domain: religious_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the expansionist legalist reading of the
 *   jihad quranic corpus: an Islamic jurisprudential doctrine that obligates
 *   Muslims to establish Islamic governance in territories currently outside
 *   dar al-Islam (the domain of Islam), subject to specific rule-bound
 *   conditions. The reading permits offensive military campaigns by the
 *   caliph/Islamic state authority, provided: (1) non-Muslims are invited to
 *   Islam first; (2) declaration is made by legitimate state authority
 *   (imam), not decentralized actors; (3) campaigns observe proportionality
 *   and protections for non-combatants; (4) conquered populations transition
 *   to dhimmi status or conversion. This reading coexists with the
 *   defensive-only reading and the revolutionary-vanguard reading—all three
 *   claim the same quranic and hadith sources, but parse the obligation,
 *   conditions, and authority differently. The expansionist reading
 *   legitimates conquest within a legalist framework; it is neither anarchic
 *   nor purely reactive.
 *
 * KEY AGENTS:
 *   - Caliphate/Islamic state authority: holds monopoly on jihad declaration and interprets conditions of legitimate expansion.
 *   - Jurists legitimating expansion: provide doctrinal authority and institutional embedding; benefit from state patronage and policy influence.
 *   - Non-Muslim populations: face conversion, subordination (dhimmi), or elimination of sovereignty; trapped by power asymmetries.
 *   - Dissident jurists: marginalized by state monopoly on interpretation; bear costs of dissent without compensating influence.
 *   - Comparative legal scholars: analytical seat; document textual sources and track historical patterns without participating in the framework's internal legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad as Obligation to Establish Islamic Governance (Expansionist Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'f3811ec3-a906-414f-bf85-abb1be590c80').
narrative_ontology:cs_kernel_codification('f3811ec3-a906-414f-bf85-abb1be590c80', fixed_text).
narrative_ontology:cs_authority_grounding('f3811ec3-a906-414f-bf85-abb1be590c80', lineage).
narrative_ontology:cs_interpretation_layer_present('f3811ec3-a906-414f-bf85-abb1be590c80').
narrative_ontology:cs_reading_relation('f3811ec3-a906-414f-bf85-abb1be590c80', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3811ec3-a906-414f-bf85-abb1be590c80', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('f3811ec3-a906-414f-bf85-abb1be590c80', foundational, imam_monopoly_on_jihad_declaration).
narrative_ontology:cs_axiom_status(imam_monopoly_on_jihad_declaration, holdable).
narrative_ontology:cs_axiom_grounding('f3811ec3-a906-414f-bf85-abb1be590c80', imam_monopoly_on_jihad_declaration, deontological).
narrative_ontology:cs_axiom('f3811ec3-a906-414f-bf85-abb1be590c80', foundational, offensive_expansion_permissible_under_conditions).
narrative_ontology:cs_axiom_status(offensive_expansion_permissible_under_conditions, holdable).
narrative_ontology:cs_axiom_grounding('f3811ec3-a906-414f-bf85-abb1be590c80', offensive_expansion_permissible_under_conditions, conventional).
narrative_ontology:cs_reference_frame('f3811ec3-a906-414f-bf85-abb1be590c80', quranic_textual_sufficiency).
narrative_ontology:cs_drift_state('f3811ec3-a906-414f-bf85-abb1be590c80', contemporary_state_practice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3811ec3-a906-414f-bf85-abb1be590c80', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_or_islamic_state_authority).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_outside_dar_islam).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, internal_dissenting_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, jurists_legitimating_expansion).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, conquered_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dissident_jurists_defending_narrower_jihad).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_fighters).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, quranic_supremacy_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, imam_monopoly_on_legitimate_force).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, progressive_expansion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims sole authority to declare jihad, interpret conditions of just warfare, and direct military campaigns to establish Islamic governance in territories deemed dar al-harb (domain of war). Administers the jurisprudential framework that legitimates offensive expansion under rule-bound conditions. The authority is bound by the conditions (invitation first, proportionality, imam declaration) but those conditions preserve the institutional monopoly itself — they do not permit decentralized or popular declaration.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_or_islamic_state_authority, agenda_setter,
    institutional, civilizational, trapped, universal).

% Jurists who endorse the expansionist legalist reading gain interpretive authority, influence over state policy, and vindication of their doctrinal positions as campaigns proceed. Their reading becomes institutionalized in fatwa offices, military chaplaincies, and educational curricula. They benefit from the state's power while providing intellectual legitimacy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, jurists_legitimating_expansion, beneficiary,
    institutional, civilizational, constrained, universal).

% Targeted populations face the choice of conversion, submission (dhimmi status with special taxes and restrictions), or armed resistance. The jurisprudential framework is structured so their only legal exits within the system are subordination or elimination of political independence. Geographic and military barriers trap them; the invitation-to-Islam requirement is formally inclusive but practically non-negotiable given power asymmetries.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_outside_dar_islam, payer,
    powerless, biographical, trapped, universal).

% Populations brought into dar al-Islam through conquest gain formal membership in the Islamic community and access to Islamic law, but lose political independence and bear the costs of military campaigns and taxation to support further expansion. They are both targets of the campaigns and, after incorporation, stakeholders in the expanding state structure.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_muslim_populations, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, conquered_muslim_populations, beneficiary).

% Jurists who argue for defensive-only or purely spiritual jihad readings are suppressed through institutional authority, loss of patronage, or takfir accusations. They bear the cost of dissent without the power to redirect policy — their alternative readings are marginalized by the state's monopoly on legitimate interpretation and force.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dissident_jurists_defending_narrower_jihad, payer,
    moderate, generational, constrained, universal).

% Conscripted or volunteer soldiers bear direct costs of campaigns (injury, death, displacement). They are bound by religious obligation (as framed by authority) and military law. Exit from the obligation is theologically costly (accusation of unfaith) and practically dangerous (desertion penalties).
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rank_and_file_fighters, payer,
    powerless, immediate, trapped, local).

% Scholars and analysts from outside the Islamic jurisprudential tradition study the expansionist reading, document its textual sources, trace its historical application, and debate whether it accurately represents classical Islamic law or is a selective modern reconstruction. They occupy an analytical seat and feed evidence into historical and comparative analysis.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_legal_scholars, observer,
    organized, generational, analytical, universal).

% International law frameworks (UN Charter, laws of armed conflict) classify expansionist jihad campaigns as wars of conquest or aggression, not legitimate self-defense. These authorities are structurally excluded from the Islamic jurisprudential framework's internal legitimacy conversation — they operate under a competing authority structure and have no voice in how the expansionist reading's conditions are interpreted.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, international_legal_authorities, excluded,
    institutional, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_or_islamic_state_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Structures the use of force under Islamic authority by requiring state/caliph declaration, limiting campaigns to rule-bound conditions (invitation to Islam first, proportionality, treatment of captives), and integrating military expansion with legal and administrative incorporation into dar al-Islam. This solves the coordination problem of preventing uncontrolled violence and ensuring conquered territories receive Islamic governance rather than remaining stateless.
% TRANSFER_FUNCTION: Moves political sovereignty, territorial control, and tax revenue from non-Muslim or competing Muslim authorities to the Islamic state/caliphate. In parallel, it moves interpretive authority and religio-political legitimacy from dispersed jurists to the institutional authority. Rank-and-file fighters transfer risk and life; dissident jurists transfer intellectual autonomy.
% ABSENT_VOICES: Non-Muslim populations who would be conquered have no seat in the jurisprudential framework and cannot negotiate the conditions of the invitation-to-Islam phase. Jurists defending purely defensive jihad are suppressed by institutional monopoly. Secular and international-law frameworks are formally excluded from the conversation and cannot object from within the Islamic tradition.
% DISAPPEARANCE_RATIONALE: If the expansionist legalist reading were repudiated and replaced by the defensive reading, Islamic state military doctrine would shift from offensive campaigns to defensive response. Territories currently under expansion pressure would retain independence or face only defensive fortification, not incorporation. The caliphate's territorial reach and tax base would shrink; jurists would lose institutional influence over foreign policy; the theological justification for military campaigns would vanish. The geopolitical and theological order would reorganize around a defensive rather than expansionist framework.
% FOUNDING_PROBLEM: Early Islamic political theology confronted the problem of how to organize Muslim political community globally when Muslims lived under non-Muslim rule or alongside non-Muslim majorities. The expansionist reading answers: through graduated campaigns to establish Islamic governance, structured by jurisprudential conditions that prevent anarchic violence while permitting systematic state-sponsored expansion.
% FOUNDING_PROBLEM_CORROBORATION: Islamic State and certain modern jihadist movements cite this reading as foundational to their campaigns. Mainstream contemporary Islamic states (Saudi Arabia, Egypt, Turkey, Iran) formally endorse Islamic law but do NOT currently pursue expansionist jihad campaigns, suggesting the founding problem (establishing global Islamic governance through coordinated expansion) is treated as superseded or postponed. Historical scholars document that the expansionist reading emerged prominently in classical jurisprudence (Shafi'i, Maliki schools) but coexisted with more restrictive readings. No external authority (comparative legal scholars, international law bodies) attests the founding problem as still-live in the form the expansionist reading addresses — the testimony for 'live' status comes only from jihadist movements themselves.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval, asymptoting around point 40. Early extractiveness is moderate because the reading's legitimating conditions (invitation, proportionality, imam authority) are genuinely constraining and carry real legal weight in early institutional contexts. As campaigns proceed and the framework matures, extractiveness rises because: (1) the conditions become increasingly ritualistic—invitations are formulaic and rejected, proportionality claims expand to justify larger campaigns, imam authority is consolidated and less subject to internal dissent; (2) the suppression machinery hardens—dissident jurists face greater institutional pressure, alternative readings are pushed to margins, and captured legal scholars reinforce the expansion doctrine. Theater ratio shows similar rise (0.18→0.41), indicating that performative elements (ritual invitations, proportionality rhetoric) increase as the genuine constraints of the conditions erode. Suppression requirement stays consistently high (0.55→0.72) because the reading's persistence depends on actively suppressing alternative interpretations and preventing dissent from delegitimizing campaigns. The asymptote (extraction plateaus around 0.68) reflects equilibrium: further extraction requires either territorial saturation or institutional collapse; the reading stabilizes at a point where institutional capture is deep but still rationalizable within legal form.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (caliphate authority) and the beneficiary (legitimating jurists) experience this as genuine coordination with rule-bound constraints: they author and enforce the conditions, and those conditions prevent anarchic violence. They compute low extraction because they frame the extraction (territorial expansion, tax revenue, religio-political authority) as the legitimate fruit of lawful governance, not as outside benefit. The payer seats (non-Muslim populations, dissident jurists, rank-and-file fighters) experience high extraction because the conditions are presented as rule-bound but administered by the very authority that benefits from their violation. From the payer perspective, the framework legitimates what would otherwise be naked conquest; the conditions provide rhetorical cover rather than substantive constraint. The engine computes this divergence from the structural data: beneficiary seats with powerful institutional position and monopoly on interpretation derive low d (beneficiary end); payer seats with powerless or trapped exit derive high d (target end). The divergence is the structural point—the same constraint computed as coordination-like from one seat and extraction-like from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphate authority: powerful, institutional, trapped (legitimacy is tied to the interpretation itself), benefits from institutional monopoly on interpretation → d near 0.1-0.2 (beneficiary end). Legitimating jurists: institutional, constrained (lose patronage if they dissent), gain influence and doctrinal vindication → d near 0.15-0.25. Non-Muslim populations: powerless, trapped (geographic/military barriers, power asymmetry), bear direct costs of conquest and subordination → d near 0.85-0.95 (full target end). Dissident jurists: moderate power (have scholarly standing), constrained (career risk, institutional pressure), bear costs of dissent without influence → d near 0.7-0.8. Rank-and-file fighters: powerless, trapped (religious obligation, military law), immediate horizon, bear direct risk → d near 0.9. The directionality profile is heavily skewed toward extraction: beneficiaries are few and institutionally consolidated; targets are many and dispersed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to organize Muslim political community globally and establish Islamic governance where absent—is contested in its current live status. The reading claims the problem is still-live and urgent; mainstream contemporary Islamic states formally accept the reading but do not pursue active expansionist campaigns, suggesting either the problem is treated as dead (governance is established sufficiently) or the response is postponed. The expansionist reading is thus subject to mandatrophy risk: if the founding problem is declared dead (Islamic governance is adequately established regionally, global expansion is not the primary obligation), the reading's legitimacy collapses. The measurement trajectory shows extractiveness stabilizing around 0.68, not rising toward 1.0—this suggests the constraint is not pure Snare but Tangled Rope with a genuine (though increasingly ritualized) coordination component. If extractiveness were to rise further, it would indicate mandatrophy resolution (the coordination function has atrophied and pure extraction remains). The current reading sits in the contested zone: it claims coordination (rule-bound expansion) but is administered by a beneficiary with monopoly on interpretation, creating asymmetric extraction under the guise of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invitation_performativity_ambiguity,
    'Does the ''invitation to Islam'' requirement (the condition that non-Muslims be offered conversion/submission before combat) function as a genuine legal constraint on campaigns, or is it a ritualized prerequisite that has become purely performative and does not substantively alter the decision to proceed?',
    'Comparative case study of historical and contemporary campaigns: examine instances where invitations were refused and the authority decided NOT to proceed, versus instances where invitations were made and ignored and campaigns proceeded anyway. Document the proportion of cases where refusal of the invitation altered the authority''s decision.',
    'If invitations are purely performative (rarely refused, never alter decisions), extractiveness classification should rise toward pure Snare; the coordination function collapses and only extraction remains. If invitations are genuine constraints (authorities sometimes postpone or cancel campaigns when invitations are refused), the Tangled Rope classification holds and the coordination component persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(invitation_performativity_ambiguity, empirical, 'Whether the invitation-to-Islam condition is a genuine legal constraint or a ritualized cover.').

omega_variable(
    proportionality_specification_gap,
    'The reading invokes proportionality as a constraint, but the jurisprudential texts do not specify operational measures of proportionality (number of casualties, damage to civilian infrastructure, territorial scope). Is proportionality a substantive constraint or a subjective principle interpreted by the authority that benefits from expansion?',
    'Textual analysis of classical and modern Islamic jurisprudence tracing how proportionality is operationalized in fiqh. Survey of contemporary Islamic authorities'' actual judgments on proportionality in specific campaigns.',
    'If proportionality is operationally empty (no measurable standards), it functions as rhetorical cover rather than constraint; extractiveness rises and the reading moves toward Snare. If measurable standards exist (codified limits on civilian casualties, geographic scope, duration), the coordination function is more robust and Tangled Rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_specification_gap, empirical, 'Whether proportionality is a substantive constraint or a subjective cover.').

omega_variable(
    imam_monopoly_consolidation,
    'Historically, has the ''imam monopoly'' on jihad declaration remained meaningful (decentralized actors regularly challenged it, authorities sometimes deferred to popular will), or has it consolidated into institutional lock-in (alternative declarations are suppressed, state authority is never overridden)?',
    'Historical trajectory study: compare cases from early Islamic state period, classical era, Ottoman period, and modern state formations. Document instances of successful decentralized jihad declarations that rivaled state authority, and track when such instances ceased.',
    'If monopoly has consolidated (alternatives are now suppressed by state power), the reading reflects Snare-like extraction: the imam authority forecloses alternatives through force, not through legitimacy. If the monopoly is contested (decentralized actors still successfully challenge it), the coordination function is real and Tangled Rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imam_monopoly_consolidation, empirical, 'Whether the imam monopoly is genuine authority or consolidated power lock-in.').

omega_variable(
    founding_problem_live_status_contestation,
    'Is the founding problem—organizing Muslim political community globally and establishing Islamic governance where absent—still live (a current obligation) or dead (a historical project that has been substantially accomplished)?',
    'Survey of contemporary Islamic jurisprudential consensus: do mainstream Islamic scholars, contemporary governments, and Islamic institutions treat the expansionist obligation as active, or as postponed/superseded? Compare ratios of expansionist vs. defensive readings endorsed in fatwa collections, educational curricula, and state military doctrines.',
    'If consensus is that the founding problem is dead or sufficiently addressed, the reading should be reclassified as a historical artifact or a Piton (maintained theatrically by fringe actors). If expansionist obligation is still treated as live by mainstream authorities, the constraint remains active. This directly tests mandatrophy: has the reading''s justification outlived its function?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_status_contestation, empirical, 'Whether the expansionist jihad obligation is a live contemporary duty or a historical artifact.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the expansionist reading logically foreclose the revolutionary-vanguard reading (which denies state monopoly and treats individual judgment as sufficient jihad), or do they coexist as distinct framings that different actors hold simultaneously?',
    'Analyze whether a single institutional actor (e.g., Islamic State, a particular jurist school) can coherently hold both the state-monopoly principle and the individual-judgment principle, or whether holding one necessarily rules out holding the other. If an actor has shifted from one reading to the other, document whether the shift was internal contradiction resolution or strategic reframing.',
    'If the readings foreclose each other, they are genuinely competing frameworks and only one can be institutionally implemented. If they coexist (held by different factions simultaneously), they are readings of the same kernel occupied by different parties. This affects the network topology and classification of the sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the expansionist and revolutionary readings are logically foreclosed or merely coexisting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(jiha_tr_t0, observed).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(jiha_tr_t10, observed).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(jiha_tr_t20, observed).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(jiha_tr_t30, observed).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(jiha_tr_t40, observed).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(jiha_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(jiha_be_t0, observed).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(jiha_be_t10, observed).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(jiha_be_t20, observed).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(jiha_be_t30, observed).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(jiha_be_t40, observed).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(jiha_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(jiha_su_t0, observed).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(jiha_su_t10, observed).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(jiha_su_t20, observed).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(jiha_su_t30, observed).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(jiha_su_t40, observed).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(jiha_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the same textual corpus. The expansionist_legalist_reading is linked to its siblings defensive_spiritual_reading and revolutionary_vanguard_reading via the network.affects_constraints array. All three share the same referent (the quranic and hadith sources on jihad) but parse the obligation, authority structure, and scope of legitimate jihad differently. The ε-invariance principle applies: each reading has a distinct ε value (this reading: 0.68 at interval end; defensive reading would show lower extraction; revolutionary reading would show extraction concentrated on state authority rather than non-Muslim populations). Decomposition follows from the recognition that changing the interpretive reading changes the constraint's beneficiary/victim structure, administrative conditions, and classification, not merely the observer's perspective—thus, three separate constraints, three separate files, linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
