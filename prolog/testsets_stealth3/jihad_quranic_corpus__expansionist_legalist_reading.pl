% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Legalist Jihad Doctrine: Offensive Campaign as Standing Obligation (Expansionist Reading of the Qur'anic Corpus)
 *   domain: religious law/political theology/comparative jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the classical legalist reading of the jihad
 *   corpus: offensive campaign to establish Islamic governance where absent
 *   is a standing collective obligation, validly declared only by the imam,
 *   preceded by invitation to Islam, and conducted under juristic limits —
 *   with conquest, tribute, and the dhimma status regime legitimated inside
 *   the legal framework. The arrangement ran the caliphal war-fiscal order
 *   for roughly thirteen centuries (interval T=0 approximates the 630s CE
 *   conquests; T=1200 approximates 1924, the caliphate's abolition). It
 *   unified command, regularized spoils, and governed conquered populations;
 *   it also transferred territory, wealth, captives, and standing tax revenue
 *   from non-Muslim populations to the Muslim polity, and fixed millions of
 *   scriptuaries in a subordinate protected status. Over the interval the
 *   arrangement's enforcement machinery built up, plateaued, and decayed; its
 *   procedural conditions became increasingly perfunctory; and after its
 *   institutional bearer collapsed it persisted largely as inherited text and
 *   performance. The base_properties values describe the standing end-state
 *   arrangement and match the final points of the measurement grid; the
 *   classical-era values live in the series.
 *
 * KEY AGENTS:
 *   - caliphal_state: agenda-setter and primary beneficiary (institutional/identity_locked) — holds the declaration monopoly, collects treasury shares and standing tax revenue, its legitimacy fused with the expansion mandate
 *   - ulama_jurisprudential_class: secondary beneficiary (institutional/identity_locked) — administers the siyar jurisprudence, collects adjudication authority and endowed income
 *   - muslim_fighting_men: beneficiary (organized/constrained) — receive fixed spoils shares, stipends, and land grants; owe service under the same doctrine
 *   - conquered_non_muslim_populations: primary target (powerless/trapped) — face the convert-submit-or-fight triad at campaign, bear siege and spoliation
 *   - dhimmi_scriptuary_taxpayers: standing payers (moderate/constrained) — carry jizya and kharaj under the dhimma contract, exit via conversion or emigration at communal cost
 *   - war_captives: targets (powerless/trapped) — enter the classical captivity system of enslavement, ransom, or exchange
 *   - non_muslim_frontier_polities: excluded (powerful/constrained) — the invitation's addressees, classified by a jurisprudence in which they hold no seat
 *   - historians_of_islamic_law: analytical observer (analytical/analytical) — reconstruct the full structure from manuals, chronicles, treaties, and fiscal records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.55).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.3).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Legalist Jihad Doctrine: Offensive Campaign as Standing Obligation (Expansionist Reading of the Qur'anic Corpus)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious law/political theology/comparative jurisprudence").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'b6623af0-85bc-4407-bd6e-46a4f1e048e6').
narrative_ontology:cs_kernel_codification('b6623af0-85bc-4407-bd6e-46a4f1e048e6', fixed_text).
narrative_ontology:cs_authority_grounding('b6623af0-85bc-4407-bd6e-46a4f1e048e6', lineage).
narrative_ontology:cs_interpretation_layer_present('b6623af0-85bc-4407-bd6e-46a4f1e048e6').
narrative_ontology:cs_reading_relation('b6623af0-85bc-4407-bd6e-46a4f1e048e6', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6623af0-85bc-4407-bd6e-46a4f1e048e6', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('b6623af0-85bc-4407-bd6e-46a4f1e048e6', foundational, offensive_jihad_standing_obligation).
narrative_ontology:cs_axiom_status(offensive_jihad_standing_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b6623af0-85bc-4407-bd6e-46a4f1e048e6', offensive_jihad_standing_obligation, theological).
narrative_ontology:cs_axiom('b6623af0-85bc-4407-bd6e-46a4f1e048e6', foundational, imam_authority_constitutive).
narrative_ontology:cs_axiom_status(imam_authority_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('b6623af0-85bc-4407-bd6e-46a4f1e048e6', imam_authority_constitutive, conventional).
narrative_ontology:cs_axiom('b6623af0-85bc-4407-bd6e-46a4f1e048e6', secondary, invitation_precedes_hostilities).
narrative_ontology:cs_axiom_status(invitation_precedes_hostilities, holdable).
narrative_ontology:cs_axiom_grounding('b6623af0-85bc-4407-bd6e-46a4f1e048e6', invitation_precedes_hostilities, conventional).
narrative_ontology:cs_reference_frame('b6623af0-85bc-4407-bd6e-46a4f1e048e6', classical_siyar_caliphal_order).
narrative_ontology:cs_drift_state('b6623af0-85bc-4407-bd6e-46a4f1e048e6', post_caliphal_nation_state_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('b6623af0-85bc-4407-bd6e-46a4f1e048e6', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurisprudential_class).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_fighting_men).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_scriptuary_taxpayers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, war_captives).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, classical_siyar_juristic_framework).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, sword_verses_abrogation_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the monopoly on declaring offensive campaign and administers the doctrine as the constitution of its war-making: treasury shares of spoils and tribute, standing jizya and kharaj revenue, and the sultanic legitimacy that the expansion mandate confers. The office's claim to authority is fused with the obligation — a caliph who renounced it would dissolve the warrant of the office itself. Exit is unavailable to the institution without ceasing to be what it is; after 1924 the seat is vacant and the doctrine persists without it.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state, beneficiary).

% Administers the jurisprudence: the siyar manuals, fatwas on campaign legality, adjudication of spoils disputes, supervision of the invitation and treaty forms, and classification of territories and populations. Collects adjudication authority, social standing, and endowed income tied to the legal system. Their professional identity is constituted by madhhab transmission — abandoning the doctrine would dissolve the warrant of their own authority — which keeps them maintaining it long after enforcement capacity collapsed.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurisprudential_class, beneficiary,
    institutional, generational, identity_locked, continental).

% Receive fixed shares of spoils, stipends, and land grants under the classical distribution law, and owe military service as a duty under the same doctrine. Demobilization is possible in peacetime, but the obligation structure and the prospect of shares keep mobilization cheap for the state; individual refusal carries legal and communal sanction.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_fighting_men, beneficiary,
    organized, biographical, constrained, continental).

% Face the doctrine's triad at the moment of campaign — accept Islam, submit to treaty and tribute, or fight. Their cities bear siege, their movable property enters the spoils system under classical shares, their land passes to the treasury or grantees, and their captives enter the captivity system. Exit means flight or conversion; resistance meets the army the doctrine authorizes. They had no seat in the jurisprudence that assigned them these options.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations, payer,
    powerless, biographical, trapped, continental).

% Live under the dhimma contract after submission: standing head tax and land tax, legal subordination, and delimited communal autonomy with protected worship. Communities possessed legal personality and internal organization, and could petition, litigate within the system, and occasionally negotiate terms; individuals could exit the tax by conversion at the cost of communal rupture, or emigrate forfeiting property. The fiscal extraction is the standing, quotidian face of the arrangement across centuries.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_scriptuary_taxpayers, payer,
    moderate, generational, constrained, continental).

% Enter the classical captivity system upon capture: enslavement, ransom, or exchange as the jurists' rules and the commander's discretion direct. They hold no standing in the framework that classifies them; their disposition is itself one of the doctrine's regulated transfers, and their fate is decided entirely by seats they cannot address.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, war_captives, payer,
    powerless, immediate, trapped, local).

% Byzantine, Persian, and later frontier states are the invitation's addressees — the parties the jurisprudence classifies as the abode of war. They negotiated treaties, contested the taxonomy that assigned them conversion-tribute-or-war, and fought the campaigns; they would object to the framework itself if they had a seat, but the classification is performed entirely within the doctrine's own legal conversation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_frontier_polities, excluded,
    powerful, generational, constrained, continental).

% Reconstruct the doctrine's operation from siyar manuals, conquest chronicles, treaty corpora, and fiscal papyri. They can see the full structure — the coordination function, the extraction flows, the sincerity or perfunctoriness of the procedural conditions, and the drift of the doctrine after its enforcement bearer collapsed — without collecting or paying under it. Their seat is where the omega questions about invitation sincerity and proportionality enforcement are resolvable.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, historians_of_islamic_law, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies war-making under a single legitimate authority: the imam's monopoly on declaration replaces tribal raiding and private war; fixed revealed shares govern spoils distribution and prevent intra-community conflict; the invitation, treaty, and dhimma forms standardize the treatment of conquered populations; mobilization, truce, and succession rules coordinate the polity's war capacity across generations.
% TRANSFER_FUNCTION: Moves territory, movable wealth, captives, and standing head-and-land tax revenue from conquered non-Muslim populations to the Muslim polity — allocated by classical shares to fighters, the treasury, and designated recipients — and moves religious and legal status itself, converting populations or fixing them in subordinate protected status under the dhimma.
% ABSENT_VOICES: The conquered: non-Muslim polities and populations had no seat in the jurisprudence that classified them as invitation-recipients, combatants, or future dhimmis; their 'voice' appears only as the formal invitation delivered by the party that stood to gain from their refusal. Within the tradition, jurists emphasizing purely defensive constraint were a persistent minority voice; dhimmi communities appear in the record as petitioners and taxpayers, never as framers of the terms.
% DISAPPEARANCE_RATIONALE: The caliphal war-fiscal order was constituted by the doctrine: without the declaration monopoly, tribal and private war returns; without the spoils law, intra-Muslim distribution conflict over conquest income; without the invitation-treaty-dhimma framework, conquered populations are governed ad hoc. Treasury revenue, the frontier system, and the legal status of millions of scriptuary subjects all depended on the arrangement — its overnight disappearance would have forced a wholesale reorganization of war-making, taxation, and minority governance across the Islamic world.
% FOUNDING_PROBLEM: Regularizing warfare in the seventh-century conquest era: unifying command under a single legitimate authority, converting Arabian raiding into rule-bound campaign, defining treatment of non-combatants and conquered peoples, and distributing conquest income without intra-community conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law, working from the siyar manuals, conquest chronicles, and fiscal papyri, corroborate the founding function from outside the benefiting parties: the doctrine did regularize command, spoils, and fiscal extraction, and the institutional problem it solved was real. No corroborating source outside the benefiting parties attests that the founding problem remains live today — the liveness claim is carried by revivalist movements who are themselves prospective beneficiaries of the arrangement's restoration, while state-system jurists and academic historians attest that the institutional problem closed with the caliphate's abolition in 1924. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   The claimed type is tangled_rope from the structure: the doctrine solves genuine collective-action problems for the Muslim polity (monopoly on war declaration suppressed tribal raiding and private war; fixed spoils shares prevented intra-community distribution conflict; the dhimma contract regularized governance of conquered populations) while the same structure transfers wealth, labor, and status from non-Muslims to Muslims under active enforcement — beneficiaries, victims, and enforcement are all present and inseparable. The metrics are authored independently and describe the standing end-state: extraction 0.55 reflects the persistent fiscal and status extraction (jizya, kharaj, legal subordination) that outlived the conquest era; suppression 0.30 reflects enforcement decay after 1924 — the doctrine now coerces little actively and persists by inheritance; theater_ratio 0.72 reflects a doctrine maintained performatively (sermonic, titulary, jurisprudential rehearsal) after its enforcement bearer collapsed. The series runs on one shared time grid (all three metrics at all seven points): extraction declines slowly as conquest ceases while the fiscal regime persists; suppression rises through the classical enforcement buildup (peak at the Ottoman classical age) then falls sharply with institutional collapse; theater rises monotonically as the procedural conditions — especially the invitation — become perfunctory. Suppression is authored as a raw structural property and is not scaled by power or scope; only extraction is scaled downstream by directionality and scope. The trajectory is monotone drift, not cyclical: the arc is buildup, plateau, decay, and the end-state profile (high theater, low enforcement, persistent extraction, no concentrated maintainer) is piton-shaped — the claimed tangled_rope describes the arrangement as designed and operated through its classical life, and the divergence between claim and end-state metrics is the lifecycle finding this story exists to record.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute different types from the same structure. From the caliphal and ulama seats the arrangement is a legitimate rule-bound order: conditions honored, spoils distributed by revealed shares, protection contracts real, private war suppressed. From the dhimmi and conquered seats the same structure is a formalized choice set delivered under threat — convert, submit to tribute, or fight — with standing fiscal extraction and a legally fixed subordinate status. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. The excluded frontier polities occupy a third position: they are the objects of the framework's classification and would contest the taxonomy itself, not merely its application.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal_state sits nearest the beneficiary end (d near 0): it collects treasury shares, standing tax revenue, and the legitimacy premium, and it writes the rules — its identity-lock amplifies persistence rather than extraction. The ulama_jurisprudential_class derives low d as beneficiary, with identity-lock making them durable maintainers of the doctrine independent of material flow. muslim_fighting_men derive low-to-moderate d: spoils and land grants flow to them, offset partly by service obligation. The victim declarations drive the target side: conquered_non_muslim_populations and war_captives sit near the full-target end (d near 1), with trapped exit pushing them to the extreme; dhimmi_scriptuary_taxpayers sit near full-target with constrained exit — conversion or emigration available but costly. Spatial scope is continental for most seats, which amplifies effective extraction for targets by making verification of the doctrine's limiting conditions harder. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regularizing seventh-century warfare under unified legitimate authority, converting Arabian raiding into rule-bound campaign, and distributing conquest income without intra-community conflict — was genuinely solved for centuries: the doctrine did suppress private war, did standardize spoils and the treatment of conquered peoples. The institutional bearer of that solution (the caliphate administering the siyar) was abolished in 1924; what persists is the inherited doctrine without enforcement capacity, maintained performatively. The mandatrophy question is contested rather than resolved: revivalist movements claim the underlying obligation remains live and seek to restore the authority condition, while state-system jurists and academic historians treat the institutional problem as closed. The tangled_rope claim prevents mislabeling the classical arrangement as pure extraction (its coordination achievements were real and its conditions sometimes operative), while the victim declarations prevent mislabeling it as pure coordination (the same structure that suppressed private war extracted from the conquered). The end-state drift toward piton is carried by the measurement series, not by re-typing the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (expansionist_legalist_reading) of the jihad_quranic_corpus kernel: what changes structurally if a sibling reading is instantiated instead?',
    'Comparative instantiation of the sibling readings as separate constraint stories: the defensive_spiritual_reading would shrink the victim set to populations engaged in armed aggression only and remove the standing expansion mandate; the revolutionary_vanguard_reading would dissolve the imam-authority condition and relocate the agenda-setter seat from the state to a vanguard claiming takfir over existing rulers.',
    'Beneficiary/victim structure, the agenda-setter seat, and authored epsilon all shift with the reading; classification of the kernel is per-reading and never per-kernel, so cross-reading comparisons must run through the network edges rather than any single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates one of three documented readings of the jihad kernel; siblings are separate constraints.').

omega_variable(
    invitation_condition_sincerity,
    'Was the invitation-to-Islam precondition operative in actual campaigns, or perfunctory ritual delivered under conditions where refusal was foreordained?',
    'Campaign chronicles, treaty texts, and diplomatic correspondence compared against the siyar manuals'' stated procedure: if envoys preceded armies with genuine negotiation windows, the condition operated; if the invitation followed mobilization or served as formal preface to predetermined war, it was ritual.',
    'If perfunctory, a substantial share of the doctrine''s rule-bound character is theater, the effective extraction is less conditioned than the jurisprudence claims, and the arrangement sits closer to pure extraction; if operative, the coordination framing is strengthened and the measured extraction is partly the price of a real legal procedure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invitation_condition_sincerity, empirical, 'Whether the doctrine''s central procedural condition constrained conduct or merely decorated it.').

omega_variable(
    dhimma_protection_or_subordination,
    'Is the dhimma a protection-for-tribute contract as the jurisprudence frames it, or a subordination regime as the taxed communities'' own records describe it?',
    'Dhimmi communal records, petition archives, and comparative fiscal data on jizya and kharaj burdens relative to Muslim land tax and to contemporaneous non-Muslim polities'' treatment of religious minorities.',
    'A protection reading lowers the standing extraction attributable to the fiscal component and supports the tangled-rope coordination claim; a subordination reading raises it and pushes the fiscal regime toward snare-flavored classification, with the standing tax as extraction under threat of status loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimma_protection_or_subordination, conceptual, 'The reading''s own framing of the dhimma versus the taxed populations'' recorded experience.').

omega_variable(
    imam_necessity_post_caliphate,
    'Does the doctrine require a living imam for any valid offensive campaign (making it inoperative after the caliphate''s abolition), or does it persist as inherited law binding without the office?',
    'Post-1924 juristic debates across the madhhabs and reformist currents, and state practice of Muslim-majority states: positions range from suspension of the obligation pending the office''s restoration to reinterpretation of the authority condition for the nation-state era.',
    'If imam-necessary, the standing arrangement is an inert inheritance maintained performatively and the end-state classification drifts toward piton; if inheritable, extraction and obligation claims persist in modified form and the arrangement retains live enforcement potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imam_necessity_post_caliphate, empirical, 'Whether the doctrine''s authority condition blocks or merely suspends its operation after 1924.').

omega_variable(
    proportionality_enforcement_gap,
    'Did the doctrine''s proportionality and non-combatant rules constrain actual campaign conduct, or did they operate as hortatory ideals with enforcement only against co-religionists?',
    'Systematic comparison of siyar manual rules against campaign records: casualty patterns, treatment of besieged populations, disposition of captives, and internal disciplinary cases where fighters were sanctioned for violations against non-Muslims.',
    'A large enforcement gap means the rule-bound framing covers conduct the rules did not in fact govern, raising effective extraction and weakening the coordination half of the tangled-rope claim; close tracking supports genuine legal constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_enforcement_gap, empirical, 'Whether the doctrine''s limiting rules bound conduct or only the manuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(jiha_tr_t400, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(jiha_tr_t600, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 600, 0.25).
narrative_ontology:measurement(jiha_tr_t800, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement(jiha_tr_t1000, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1000, 0.55).
narrative_ontology:measurement(jiha_tr_t1200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1200, 0.72).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(jiha_be_t200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement(jiha_be_t400, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 400, 0.62).
narrative_ontology:measurement(jiha_be_t600, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(jiha_be_t800, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 800, 0.6).
narrative_ontology:measurement(jiha_be_t1000, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(jiha_be_t1200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1200, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jiha_su_t200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(jiha_su_t400, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(jiha_su_t600, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(jiha_su_t800, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(jiha_su_t1000, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(jiha_su_t1200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1200, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dhimma_fiscal_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'jihad' covers structurally distinct claims and decomposes into a constraint family per the epsilon-invariance principle. This file instantiates the expansionist_legalist_reading: a standing offensive obligation under an imam-authority condition, with conquest and the dhimma regime inside the legal framework — moderate-high epsilon over a referent that includes both the campaign apparatus and the standing fiscal/status regime. The defensive_spiritual_reading is a separate constraint with a different victim set (populations engaged in armed aggression only), no expansion mandate, and a different epsilon. The revolutionary_vanguard_reading is a separate constraint that dissolves the authority condition this reading makes constitutive. This reading is upstream of the dhimma_fiscal_regime (the standing tax and status arrangement that conquest produces and administers) and exerts structural pressure on the vanguard reading, whose takfir jurisprudence exists as an argument around this reading's authority axiom. Family members link through affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
