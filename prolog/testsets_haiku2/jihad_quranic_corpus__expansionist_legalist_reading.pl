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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Jihad as Obligation for Expansionist Governance (Legalist Reading)
 *   domain: religious_jurisprudence/political_theology
 *
 * SUMMARY:
 *   The expansionist-legalist reading interprets Quranic corpus and prophetic
 *   precedent as establishing an obligation for Islamic states to propagate
 *   Islamic governance into territories where it is absent. The reading is
 *   jurisprudentially structured: it requires imam/caliph authority (not
 *   individual declaration), prior invitation to Islam, assessment of
 *   proportionality and military capacity, and offers non-Muslims choice
 *   between conversion, tributary status, or combat. This reading coexists
 *   with defensive-spiritual readings (jihad as internal struggle + defensive
 *   response only) and revolutionary-vanguard readings (individual obligation
 *   via takfir, bypassing state authority). The expansionist-legalist reading
 *   legitimates systematic conquest within a rule-bound framework; the other
 *   readings constrain or reframe the obligation entirely. This JSON
 *   instantiates ONLY the expansionist-legalist reading as a clean, stable
 *   constraint — the other readings are separate constraint stories, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - State/Caliph authority: declares and conditions expansion obligation; controls interpretation and deployment
 *   - Islamic jurists and scholars: authorize conditions (imam competence, proportionality, invitation protocol); delegate legitimacy
 *   - Muslim combatants: bear military cost; benefit spiritually and materially from conquest
 *   - Non-Muslim populations in target territories: face invitation/conversion/tributary/combat choice; structurally external to authority but targets of obligation
 *   - Rival Islamic authorities: excluded from declaring jihad by monopoly-authority doctrine; cannot exit without rejecting reading
 *   - Defensive-spiritual reading scholars: dispute the legitimacy of expansionist campaigns; excluded from authority structure
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
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad as Obligation for Expansionist Governance (Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '4479ce4e-5510-4f96-9ec4-11795d673f09').
narrative_ontology:cs_kernel_codification('4479ce4e-5510-4f96-9ec4-11795d673f09', fixed_text).
narrative_ontology:cs_authority_grounding('4479ce4e-5510-4f96-9ec4-11795d673f09', lineage).
narrative_ontology:cs_interpretation_layer_present('4479ce4e-5510-4f96-9ec4-11795d673f09').
narrative_ontology:cs_reading_relation('4479ce4e-5510-4f96-9ec4-11795d673f09', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('4479ce4e-5510-4f96-9ec4-11795d673f09', jihad_quranic_corpus__revolutionary_vanguard_reading, influences).
narrative_ontology:cs_axiom('4479ce4e-5510-4f96-9ec4-11795d673f09', foundational, expansion_obligation_primary).
narrative_ontology:cs_axiom_status(expansion_obligation_primary, holdable).
narrative_ontology:cs_axiom_grounding('4479ce4e-5510-4f96-9ec4-11795d673f09', expansion_obligation_primary, empirically_contingent).
narrative_ontology:cs_axiom('4479ce4e-5510-4f96-9ec4-11795d673f09', foundational, imam_authority_monopoly_legitimate).
narrative_ontology:cs_axiom_status(imam_authority_monopoly_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4479ce4e-5510-4f96-9ec4-11795d673f09', imam_authority_monopoly_legitimate, deontological).
narrative_ontology:cs_axiom('4479ce4e-5510-4f96-9ec4-11795d673f09', secondary, humanitarian_conditions_binding).
narrative_ontology:cs_axiom_status(humanitarian_conditions_binding, overridden).
narrative_ontology:cs_axiom_grounding('4479ce4e-5510-4f96-9ec4-11795d673f09', humanitarian_conditions_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('4479ce4e-5510-4f96-9ec4-11795d673f09', quranic_systematic_expansion_mandate).
narrative_ontology:cs_drift_state('4479ce4e-5510-4f96-9ec4-11795d673f09', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4479ce4e-5510-4f96-9ec4-11795d673f09', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, state_caliph_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_governance_expansion).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_in_unconquered_territories).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, rival_islamic_states_claiming_caliphate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists_and_scholars).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_combatants_and_soldiers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, muslim_combatants_and_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares and administers the obligation to pursue jihad as systematic expansion of Islamic governance. Sets the conditions: invitation to Islam first, assessment of imam competence and state readiness, proportionality judgment. Collects legitimacy from the jurisprudential framework and (historically) territorial gains and tribute revenue. Exit would require abandoning the foundational claim of authority to interpret divine mandate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, state_caliph_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Face obligation of invitation to Islam, then choice between conversion, tributary status (dhimmi), or combat. The reading's legal framework offers this structured choice; their actual exit options depend on geography, military capacity, and whether the invitation phase is actually executed. Non-compliance with the obligation (failure to mount invitation) is theoretically a breach; non-compliance with the choice itself (refusing conversion or tributary status) invokes combat justification.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_in_unconquered_territories, payer,
    powerless, biographical, trapped, regional).

% Benefit from the jurisprudential framework by defining the conditions under which jihad is legitimate. They interpret Quranic corpus, Hadith, precedent, and necessity. The reading empowers them to set standards for imam authority, assess proportionality, and adjudicate when conditions are met. Their power is delegated by political authority but derives legitimacy from scholarly consensus (ijma') and established schools (madhabs). Exit from the framework would require surrendering scholarly authority within Islamic law.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists_and_scholars, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists_and_scholars, agenda_setter).

% Bear the cost of military campaigns (risk to life, conscription, displacement) in service of the expansion obligation. They may benefit spiritually from fulfilling the obligation and materially from conquest booty distribution (historically). Their exit from individual campaigns is constrained by state authority; exit from the entire obligation is framed as apostasy or shirking, identity-dissolving for those whose religious identity is constituted through Islamic duty.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_combatants_and_soldiers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, muslim_combatants_and_soldiers, beneficiary).

% Are excluded from the authority to declare jihad and set conditions for expansion. The reading's requirement for imam authority (singular caliph or unifying authority) means competing claims to caliphate are structurally excluded from legitimacy. They may dispute the interpretation but cannot escape the framework's authority-monopoly without rejecting the reading itself. Historical fragmentation of caliphate authority created a persistent tension: multiple powers claiming right to declare jihad, but the reading's framework designates only one as legitimate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rival_islamic_states_claiming_caliphate, excluded,
    institutional, civilizational, trapped, global).

% Respond to the obligation's activation (invasion, ultimatum, invitation phase) without being structural parties to the jurisprudential framework. Their agency enters through acceptance/rejection of conversion or tributary status, or through military defense. From outside the framework, they observe and counter the reading's implementation but cannot participate in its authority structure.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_rulers_and_states, observer,
    institutional, civilizational, analytical, global).

% Dispute the expansionist reading's core premise: that jihad includes offensive campaigns to establish governance where absent. They emphasize internal spiritual struggle (jihad al-nafs) and defend-only armed response, constrained by proportionality and non-combatant immunity. They remain within the Quranic corpus but interpret it differently; their exclusion is framing-based, not structural (they are Islamic scholars who reject this reading's legitimacy).
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, theological_defenders_of_defensive_spiritual_reading, excluded,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, state_caliph_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform rules for when and how expansion of Islamic governance is legitimate: requiring state authority (imam), prior invitation to Islam, assessment of proportionality and readiness, and tributary or conversion pathways for non-Muslims. Prevents ad-hoc or individual declarations of holy war; channels expansion through jurisprudential governance.
% TRANSFER_FUNCTION: Moves military obligation (risk, labor, life) from the state authority to combatants and populations; moves legitimacy, territorial control, and tributary revenue to the declaring state/caliph; offers salvation-credit and possible material gain (booty, status) to combatants in exchange for participation. Non-Muslims move from unconquered status to conquered/converted/tributary status.
% ABSENT_VOICES: Defenders of the defensive-spiritual reading (who remain within Islamic jurisprudence but reject expansionist obligation) and non-Muslim populations in prospective target territories (who would object to the obligation but are addressed only as objects of invitation, not as participants in the framework).
% DISAPPEARANCE_RATIONALE: If this specific reading's obligation to expand governance systematically disappeared, Islamic states would lose the jurisprudential warrant for offensive campaigns against non-Muslim territories; expansion would require different legitimation or would cease. Historically-actual geopolitical outcomes would have been different — the territorial extent of Islamic governance, tribute streams, and the framing of conquest would reorganize. The obligation's absence would not dissolve Islam or create theological void; it would shift the permissible scope of jihad to defensive-only models or individually-obligatory spiritual models. Geopolitical reorganization is certain; scale depends on how many states anchor expansion-authority to this reading vs. alternatives.
% FOUNDING_PROBLEM: How should Islamic governance propagate and extend where it is absent? Should Muslims bear an obligation to invite and incorporate non-Muslim populations under Islamic law? On what conditions, and under whose authority?
% FOUNDING_PROBLEM_CORROBORATION: Islamic jurisprudential tradition across multiple schools (Hanafi, Maliki, Shafi'i, Hanbali) attests the founding problem as live: classical jurists compiled and debated conditions for expansion jihad. Contemporary scholars attest differently: expansionist scholars (e.g., certain takfiri theorists, state-authorized jurists in expansion-era states) treat the obligation as binding; defensive and humanitarian-Islam scholars (e.g., M. Abdulaziz Sachedina, Abdullah bin Bayyah) attest the founding problem as superseded by modern international law and non-combatant immunity, or reinterpreted as spiritual obligation only. Testimony from outside the jurisprudential beneficiary set (international law scholars, non-Muslim academic observers) attests the reading persists in certain contexts and has been operationalized by state actors and armed groups, regardless of contemporary Islamic scholarly consensus.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at interval end) because the constraint moves military obligation and territorial risk from centralized authority to distributed combatants and populations, while centralizing governance legitimacy and tribute revenue with the caliph. The beneficiary (state authority) does not bear the cost; the payers (combatants, subject populations, conquered non-Muslims) do not set the terms. Suppression is high (0.72) because maintaining the obligation requires suppressing competing interpretations (defensive-only readings) and enforcing the state monopoly on declaration against individual takfiri actors or rival authorities. Theater is moderate (0.41): the jurisprudential structure is substantively real (invitation protocol, proportionality assessment, imam authority are not pure fabrication), but enforcement increasingly serves the monopoly claim rather than the humanitarian constraints embedded in the reading. The measurement series shows extractiveness and suppression rising through t=24 (a period of active expansion claims and state consolidation), then plateauing by t=40 (suggesting the constraint has reached a stable operational regime). Theater rises throughout, indicating increasing performative maintenance of the humanitarian constraints (invitation phase becomes ritually abbreviated; proportionality assessment becomes formal rather than substantive).
 *
 * PERSPECTIVAL GAP:
 *   From the caliph's seat: coordination mechanism for unified expansion, prevents fragmentation, legitimates state authority. From the combatant's seat: fulfillment of religious obligation, but conscription and asymmetric risk. From the target-population's seat: expansion by external force with structured choice (convert/pay/fight) — legitimacy-framed coercion. From the excluded rival-authority seat: illegitimate monopoly claim on caliphate and interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The state/caliph authority is the structural beneficiary: it monopolizes declaration, collects tribute and territorial legitimacy, and offloads military cost. Directionality: d ≈ 0.05 (beneficiary, minimal exit pressure). Non-Muslim target populations are the primary targets: they face expansion obligation from a power they did not authorize, with constrained exit options (convert, pay tribute, or fight). Directionality: d ≈ 0.95 (full target). Muslim combatants sit between: they participate in fulfilling a shared religious obligation (low-ish d, ~0.45), but are conscripted by state authority and bear asymmetric risk. The identity-lock on exit (fighting the obligation is apostasy or shirking) pushes d toward 0.65. Rival Islamic authorities are structurally trapped: they remain within Islam and the Quranic corpus, but the reading's monopoly doctrine excludes them from legitimacy. Their d is high (~0.88) because they cannot exit without theological rupture, but their power is institutional, which modulates the effective extraction downward slightly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how should Islamic governance propagate; what obligation exists) was live in classical jurisprudence and remains contested in contemporary Islamic scholarship. Some readings (expansionist-legalist, revolutionary-vanguard) treat it as live; other readings (defensive-spiritual, humanitarian-Islam) treat the obligation as superseded or reinterpreted. Mandatrophy arises from: (1) historical fragmentation of caliphate authority, which dissolved the imam-monopoly condition the reading requires; (2) modern international law norms (territorial sovereignty, prohibition on aggressive war) which override the jurisprudential framework; (3) scholarly consensus drift in many Islamic schools toward defensive-only interpretation. The reading persists not through universal endorsement, but through operationalization by certain state actors and armed movements that reject the mandate-obsolescence claim. Classifying this as tangled_rope (not snare) requires showing genuine coordination function alongside asymmetric extraction: the jurisprudential structure genuinely coordinates state authority (prevents ad-hoc declarations) and offers humanitarian constraints (invitation, proportionality, tributary choice). Those functions are real; simultaneously, the asymmetry is real (beneficiary and payer are distinct, enforcement is active, extraction unmasks as suppression of competing readings). Tangled rope fits: coordination + active enforcement + asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imam_authority_monopoly_scope,
    'Does the reading''s requirement for imam/caliph authority permit delegation to regional governors, provisional authorities, or military commanders, or does it demand singular centralized caliphate declaration?',
    'Textual interpretation of classical jurisprudential sources (Mawardi, Ibn Qayyim al-Jawziyyah, Shafi''i school precedents) on the permissibility of delegated jihad authority. Historical examination of how Ottoman, Safavid, and Mughal authorities operationalized the reading.',
    'If delegation is permitted, the constraint''s monopoly-authority doctrine is weaker and multiple institutional seats could legitimately declare expansion, weakening the suppression requirement and opening competitive authorization paths. If singular centralized authority is required, the monopoly is stronger and rival authorities are more completely excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imam_authority_monopoly_scope, conceptual, 'Whether imam authority is monopoly-singular or delegable to regional authorities.').

omega_variable(
    invitation_protocol_enforceability,
    'Is the requirement to invite non-Muslims to Islam before combat a binding jurisprudential condition or a recommended (mustahabb) practice that does not invalidate jihad if omitted?',
    'Textual analysis of Quranic basis for the invitation requirement and classical jurisprudential sourcing. Examination of whether schools differ on the conditionality (some making it binding, others discretionary).',
    'If binding, the invitation phase is a structural gate that could prevent or delay campaigns against targets unaware of Islam, and failure to invite would be a jurisprudential violation that delegitimizes the campaign. If discretionary, the invitation phase is theater and can be abbreviated or omitted, increasing extractiveness and suppression of the humanitarian constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invitation_protocol_enforceability, empirical, 'Whether the invitation-to-Islam phase is a binding condition or discretionary practice.').

omega_variable(
    proportionality_assessment_criteria,
    'What criteria determine whether a proposed expansion campaign meets the proportionality requirement? Who assesses whether the expected benefits of expansion exceed the costs (lives lost, resources expended)?',
    'Examination of classical jurisprudential texts on proportionality standards. Investigation of how historical Islamic states and contemporary authorities operationalize the proportionality judgment (if at all).',
    'If proportionality criteria are clear and independently assessed, they function as a real constraint on expansion. If criteria are vague or assessment is delegated to the authority declaring jihad, proportionality becomes theater and the constraint provides little brake on expansion campaigns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_assessment_criteria, empirical, 'Whether proportionality assessment has operationalizable criteria or functions as unfalsifiable theater.').

omega_variable(
    tributary_status_permanence,
    'Is the dhimmi (tributary non-Muslim) status permanent, or is it a transitional state intended to eventually lead to conversion or incorporation into full Islamic governance?',
    'Classical jurisprudential texts on the rights and status of dhimmis, and on whether the covenant of protection is revocable. Historical documentation of dhimmi communities and their long-term trajectories.',
    'If tributary status is permanent, the constraint offers a stable exit from combat (non-Muslims can permanently opt out via tribute). If it is transitional, the tributary status is a temporary reprieve and expansion pressure continues, increasing extractiveness over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tributary_status_permanence, conceptual, 'Whether dhimmi tributary status is a permanent stable condition or a transitional stage.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the expansionist-legalist reading logically foreclose the defensive-spiritual reading, or can both coexist as live positions within Islamic jurisprudence?',
    'Examination of the core premises of each reading: expansionist-legalist asserts expansion is an obligation; defensive-spiritual asserts expansion is not obligatory and jihad is primarily spiritual. Can an actor hold both simultaneously, or does accepting one require rejecting the other?',
    'If foreclosure exists, one reading must win in any coherent Islamic framework, and the losing reading is logically impossible. If coexistence is possible, both readings remain live options, and the constraint''s authority rests on choosing this reading over the other through political power, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading logically forecloses the defensive-spiritual reading or coexists with it.').

omega_variable(
    takfiri_subversion_mechanism,
    'How does the expansionist-legalist reading account for the revolutionary-vanguard reading''s claim that takfir (declaring Muslims apostates) and individual obligation override the state authority monopoly?',
    'Jurisprudential textual analysis on the validity of takfir, the conditions for individual obligation (fard ''ayn), and whether non-state actors can legitimately declare jihad without imam authorization.',
    'If takfiri and individual-obligation claims are doctrinally indefensible, this reading''s state monopoly is robust. If takfiri claims have jurisprudential support in some schools or interpretations, the state monopoly is vulnerable to subversion by actors claiming emergency conditions or apostasy of ruling authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takfiri_subversion_mechanism, empirical, 'Whether the expansionist-legalist reading can defend against takfiri subversion of the state monopoly.').

omega_variable(
    humanitarian_constraint_internalization,
    'Are the humanitarian constraints (invitation, proportionality, non-combatant immunity within the tribute pathway) internalized norms that actors actually follow, or are they externally-enforced rules that actors follow only under observation?',
    'Historical and ethnographic examination of how expansion campaigns conducted under this reading have actually operationalized the constraints. Comparison of campaigns claiming to follow the reading vs. campaigns ignoring the constraints, and whether outcomes differ.',
    'If constraints are internalized, they represent genuine coordination function and the constraint is legitimately tangled-rope. If constraints are purely external theater, the suppression mechanism is more pervasive and extractiveness is higher (the constraint is closer to snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_constraint_internalization, empirical, 'Whether humanitarian constraints are internalized norms or external theater.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For Muslim combatants and populations under Islamic governance, how much of their suppression (inability to refuse the expansion obligation) is structural (fear of state punishment, economic dependence, geographic isolation) vs. internalized (identity fused with Islamic duty, belief that refusal is apostasy)?',
    'Post-exit suppression trajectory: if combatants or populations that leave the Islamic state-system maintain the internalized suppression, it is identity-locked; if suppression drops after exit, it was structural.',
    'If suppression is heavily internalized, exit is psychologically and socially difficult; the constraint''s effective suppression is higher than the structural measure suggests. If suppression is structural, exit is difficult but the suppression dissipates post-exit, enabling future re-negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether combatant suppression is structural or identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jiha_tr_t8, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(jiha_tr_t16, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(jiha_tr_t32, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(jiha_be_t8, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(jiha_be_t16, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(jiha_be_t32, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(jiha_su_t8, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(jiha_su_t16, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(jiha_su_t32, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_authority_monopoly_doctrine).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, islamic_governance_legitimacy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jihad_quranic_corpus kernel. Three constraint stories derive from this kernel, each instantiating a different reading of the Quranic corpus on jihad and Islamic expansion: (1) expansionist_legalist_reading (THIS STORY): obligation to systematize expansion, under jurisprudential conditions. (2) defensive_spiritual_reading: obligation to internal struggle and defensive-only response, no expansionist duty. (3) revolutionary_vanguard_reading: individual obligation overriding state authority via takfir. These readings are structurally distinct constraints with different ε values, beneficiary/victim structures, and types. They coexist in contemporary Islamic discourse, with different authorities, movements, and scholarly positions endorsing each. The kernel-reading frame decomposes what the natural-language concept 'jihad' conflates. Each reading has its own stability conditions, mandate status, and theater dynamics. The network edges link the stories so downstream analysis can trace how a shift in one reading's operational status (e.g., caliphate collapse delegitimizing expansionist-legalist authority monopoly) propagates to affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
