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
 *   human_readable: Expansionist Legalist Jihad as Obligation to Establish Islamic Governance
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint story models the expansionist legalist reading of the
 *   jihad Quranic corpus: the obligation to establish Islamic governance
 *   where it is absent, conditioned on prior invitation to Islam (da'wa),
 *   authorization by a legitimate imam/caliph, and proportionality in
 *   conduct, but structurally permitting offensive military campaigns to
 *   bring territories under Islamic rule. Non-Muslim polities occupy a
 *   liminal status — potential dhimmi (protected) subjects if they accept
 *   Islamic sovereignty and pay jizya, or combatants if they refuse the
 *   invitation. The caliph/imam holds a monopoly on declaration of offensive
 *   jihad. Conquest is legitimated within a legal framework that claims to
 *   regulate rather than unleash violence. The reading coordinates
 *   inter-civilizational relations through a rule-bound expansion logic but
 *   simultaneously extracts sovereignty, resources (jizya, kharaj, fai'), and
 *   demographic autonomy from non-Muslim populations.
 *
 * KEY AGENTS:
 *   - caliphal_imamate_authority: Primary agenda-setter (institutional/arbitrage) — declares jihad, sets conditions, collects spoils
 *   - jurist_class_ulema: Secondary beneficiary/agenda-setter (organized/constrained) — interprets conditions, legitimates campaigns, gains institutional authority
 *   - muslim_umma_collective: Beneficiary (organized/constrained) — receives spiritual merit, material shares of spoils, political order
 *   - non_muslim_polities_pre_invitation: Primary victim (powerless/trapped) — face conquest or subjugation after invitation
 *   - dhimmi_populations_under_rule: Victim/beneficiary dual (powerless/identity_locked) — protected but legally subordinate, pay jizya
 *   - non_muslim_combatants_post_refusal: Victim (powerless/trapped) — legitimate targets once invitation refused
 *   - defensive_spiritual_reading_adherents: Excluded observer (moderate/constrained) — contest the expansionist premise from within tradition
 *   - revolutionary_vanguard_reading_adherents: Excluded observer (moderate/trapped) — reject state monopoly on declaration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Expansionist Legalist Jihad as Obligation to Establish Islamic Governance").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '0c7814b4-9b0e-4f95-98fe-9903cab26e50').
narrative_ontology:cs_kernel_codification('0c7814b4-9b0e-4f95-98fe-9903cab26e50', fixed_text).
narrative_ontology:cs_authority_grounding('0c7814b4-9b0e-4f95-98fe-9903cab26e50', lineage).
narrative_ontology:cs_interpretation_layer_present('0c7814b4-9b0e-4f95-98fe-9903cab26e50').
narrative_ontology:cs_reading_relation('0c7814b4-9b0e-4f95-98fe-9903cab26e50', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c7814b4-9b0e-4f95-98fe-9903cab26e50', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('0c7814b4-9b0e-4f95-98fe-9903cab26e50', foundational, offensive_jihad_obligatory_until_global_islamic_rule).
narrative_ontology:cs_axiom_status(offensive_jihad_obligatory_until_global_islamic_rule, holdable).
narrative_ontology:cs_axiom_grounding('0c7814b4-9b0e-4f95-98fe-9903cab26e50', offensive_jihad_obligatory_until_global_islamic_rule, deontological).
narrative_ontology:cs_axiom('0c7814b4-9b0e-4f95-98fe-9903cab26e50', foundational, caliphal_monopoly_on_jihad_declaration).
narrative_ontology:cs_axiom_status(caliphal_monopoly_on_jihad_declaration, holdable).
narrative_ontology:cs_axiom_grounding('0c7814b4-9b0e-4f95-98fe-9903cab26e50', caliphal_monopoly_on_jihad_declaration, conventional).
narrative_ontology:cs_axiom('0c7814b4-9b0e-4f95-98fe-9903cab26e50', secondary, invitation_da_wa_precondition_for_conquest).
narrative_ontology:cs_axiom_status(invitation_da_wa_precondition_for_conquest, holdable).
narrative_ontology:cs_axiom_grounding('0c7814b4-9b0e-4f95-98fe-9903cab26e50', invitation_da_wa_precondition_for_conquest, conventional).
narrative_ontology:cs_axiom('0c7814b4-9b0e-4f95-98fe-9903cab26e50', secondary, dhimma_as_legitimate_permanent_status).
narrative_ontology:cs_axiom_status(dhimma_as_legitimate_permanent_status, holdable).
narrative_ontology:cs_axiom_grounding('0c7814b4-9b0e-4f95-98fe-9903cab26e50', dhimma_as_legitimate_permanent_status, conventional).
narrative_ontology:cs_reference_frame('0c7814b4-9b0e-4f95-98fe-9903cab26e50', classical_caliphal_expansionist_framework).
narrative_ontology:cs_drift_state('0c7814b4-9b0e-4f95-98fe-9903cab26e50', post_colonial_nation_state_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0c7814b4-9b0e-4f95-98fe-9903cab26e50', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_imamate_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma_collective).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, jurist_class_ulema).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities_pre_invitation).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_populations_under_rule).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_combatants_post_refusal).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_populations_under_rule).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma_collective).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the monopoly on declaring offensive jihad, sets the terms of invitation (da'wa), appoints commanders, distributes spoils (khums/fai'), and collects land tax (kharaj) and poll tax (jizya). The office's legitimacy depends on fulfilling the expansion obligation; failure to expand is doctrinal failure. Exit is arbitrage-grade — the caliph can negotiate truces, suspend campaigns, or reinterpret conditions via maslaha.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_imamate_authority, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_imamate_authority, beneficiary).

% Interpret the conditions of jihad (validity of invitation, proportionality, imam legitimacy), issue fatwas authorizing or limiting campaigns, legitimate the caliph's authority, and receive state patronage (madrasa funding, judicial appointments, waqf administration). Their interpretive authority is the constraint's coordination mechanism. Exit is constrained — dissenting jurists face marginalization or accusations of innovation (bid'a), but can migrate to rival courts or develop independent schools.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, jurist_class_ulema, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, jurist_class_ulema, agenda_setter).

% Receives spiritual merit (thawab) for participation or support, material shares of spoils (ghanima) for combatants, and the political order of Islamic governance. Bears the cost of military service, taxation (zakat, voluntary contributions), and social obligation to support jihad. Exit is constrained — apostasy is capital offense; migration (hijra) to non-Muslim lands is religiously discouraged; internal dissent is policed by community and state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma_collective, beneficiary,
    organized, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma_collective, payer).

% Sovereign non-Muslim political entities that receive the formal invitation to Islam (da'wa) before any military action. Their structural options: convert (lose sovereignty, gain full citizenship), accept dhimmi status (surrender sovereignty, pay jizya, retain communal autonomy), or refuse and face conquest. The invitation itself is a structural ultimatum — refusal legitimates total war. No exit from the dilemma; the constraint creates the liminal status.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities_pre_invitation, payer,
    powerless, immediate, trapped, regional).

% Non-Muslim communities living under Islamic rule with protected status (dhimma). They receive protection of life, property, and religious practice; communal legal autonomy (millet system); and exemption from military service. They pay jizya (poll tax), kharaj (land tax), accept legal disabilities (testimony restrictions, dress codes, building restrictions, no proselytizing Muslims), and face demographic pressure (conversion incentives, child levy in some periods). Exit from dhimmi status is conversion to Islam (irreversible, socially rewarded) or apostasy from Islam if born Muslim (capital) — identity-locked because communal identity is fused with the status.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_populations_under_rule, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_populations_under_rule, beneficiary).

% Combatants of polities that refused the invitation. Once refusal occurs, they become legitimate targets (mubah al-dam) — killing them incurs no penalty, their property becomes ghanima (spoils), their women and children may become sabi (captives/slaves). No exit from this status except surrender (which converts them to dhimmi candidates) or death. The constraint defines them as the enemy by structural necessity.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_combatants_post_refusal, payer,
    powerless, immediate, trapped, local).

% Scholars and communities (e.g., certain Sufi orders, modernist reformers, Quranist movements) who read jihad primarily as internal struggle (jihad al-nafs) and defensive warfare only. They contest the expansionist reading's offensive premise but lack institutional power to block caliphal declarations. Their exit is constrained — they remain within the tradition but are marginalized in authoritative discourse; some face heresy accusations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_reading_adherents, excluded,
    moderate, biographical, constrained, universal).

% Activist groups (e.g., Kharijite historical antecedents, modern jihadist movements) who reject the state monopoly on jihad declaration, declaring incumbent rulers apostate (takfir) and making jihad an immediate individual obligation (fard 'ayn). They are structurally excluded from the expansionist legalist framework — their premise forecloses the imam's authority. Exit is trapped — they cannot re-enter the legalist framework without abandoning their core commitment; state repression makes physical exit dangerous.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_reading_adherents, excluded,
    moderate, biographical, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_imamate_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-bound framework for inter-civilizational relations: defines how Islamic polity expands, how non-Muslim polities are incorporated (invitation → dhimma or conquest), how spoils are distributed, and how internal dissent is managed. Solves the coordination problem of legitimate violence authorization, resource mobilization, and pluralism management under a single sovereign claim.
% TRANSFER_FUNCTION: Moves sovereignty, land revenue (kharaj), poll tax (jizya), movable spoils (ghanima/fai'), and demographic control from non-Muslim polities and populations to the Islamic state (caliph, treasury, military, jurist class). The Muslim collective receives spiritual merit and political order; the jurist class receives interpretive authority and patronage.
% ABSENT_VOICES: Non-Muslim polities and populations subject to the invitation ultimatum — they have no voice in the juristic discourse that defines the conditions of their subjugation. Women and children captured as sabi — no standing in the legal framework. Revolutionary vanguard adherents — excluded by the state monopoly on declaration. The constraint's legal framework is produced entirely by Muslim male jurists serving the caliphal authority.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal framework authorizing offensive expansion, the dhimmi system, the spoils distribution rules, and the caliphal monopoly on war declaration would collapse. Non-Muslim polities would regain full sovereignty; dhimmi communities would become equal citizens or independent polities; the jurist class would lose its war-regulation authority; the caliphate would lose its primary legitimacy mandate. The inter-civilizational order would fundamentally reorganize.
% FOUNDING_PROBLEM: The Quranic mandate to establish God's governance (hukm) on earth, interpreted as requiring the political expansion of Islamic rule until 'religion is entirely for Allah' (Q. 8:39, 2:193). The early Muslim community faced existential threat and doctrinal imperative to expand the zone of Islamic sovereignty (dar al-Islam) as the only legitimate political order.
% FOUNDING_PROBLEM_CORROBORATION: The expansionist legalist reading (this constraint) attests the problem is live until global Islamic governance. The defensive_spiritual_reading attests the founding problem was defensive consolidation of the early community — corroborated by early Quranic chronology (Meccan non-violence, Medinan defensive permission) and the majority of early tafsir tradition. The revolutionary_vanguard_reading attests the problem is purging apostate rulers — corroborated by Kharijite and modern jihadist genealogies. No external (non-Muslim) corroboration exists for any reading's founding problem claim.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.72) reflects systematic transfer of sovereignty, land revenue (kharaj), poll tax (jizya), and movable spoils (ghanima/fai') from non-Muslim populations to the Islamic polity and its agents, modulated by legal conditions that reduce but do not eliminate extraction. Suppression (0.68) reflects the constraint's dependence on military enforcement and the structural closure of exit for targeted polities — once invitation is issued, refusal legitimates conquest; dhimmi status is hereditary and exit from it is apostasy (capital offense). Theater ratio (0.25) is moderate: the legal conditions (invitation, proportionality, imam authority) are real doctrinal commitments that genuinely shaped conduct in many cases, but a growing body of 'necessity' (darura) and 'interest' (maslaha) jurisprudence created exceptions that expanded over centuries. Accessibility collapse (0.45) is moderate — alternative arrangements (truce, tribute without submission, autonomous enclaves) existed but were structurally unstable. Resistance (0.55) is significant — non-Muslim polities resisted militarily, diplomatically, and through internal conversion dynamics; jurists debated conditions; revolutionary readings contested the state monopoly.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal authority sits at d ~ 0.1 (beneficiary: collects spoils, sets rules, controls declaration). The jurist class sits at d ~ 0.2 (beneficiary: institutional authority, interpretive monopoly, state patronage). The Muslim collective sits at d ~ 0.35 (near-symmetric: spiritual benefit and material shares vs. obligation to fight). Non-Muslim polities pre-invitation sit at d ~ 0.9 (full target: conquest or subjugation is the structural outcome). Dhimmi populations sit at d ~ 0.7 (high target: permanent subordinate status, jizya extraction, legal disabilities — but with protected personhood and communal autonomy). Combatants post-refusal sit at d ~ 1.0 (full target: legitimate killing). The defensive and revolutionary readings are excluded observers — they bear no direct extraction but their exclusion from the declarative monopoly is structurally significant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing divine governance on earth) remains contested as live — the expansionist legalist reading holds it is live until global Islamic governance; the defensive reading holds the founding problem was defensive consolidation; the revolutionary reading holds the founding problem is purging apostasy. The mandate has not atrophied because the expansionist reading's core claim (universal jurisdiction of Sharia) remains structurally expansionary — it cannot declare mission accomplished without self-negation. However, the legal conditions (invitation, proportionality) have become increasingly performative as the capacity for offensive jihad declined, creating a piton-like dynamic in the late premodern period: the constraint persisted as doctrine while its operational function atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is this constraint a genuine legal coordination mechanism or an extractive imperial framework disguised as law?',
    'Comparative analysis of historical application: whether non-Muslim polities consistently received invitation before conquest, whether proportionality was maintained, whether dhimmi protections were honored in practice, and whether the jurist class independently constrained rulers or legitimated expansion.',
    'If coordination dominates, the constraint functions as a regulated inter-civilizational order; if extraction dominates, the legal conditions are cover for systematic subjugation and resource extraction from non-Muslim populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the legal conditions (invitation, proportionality, imam authority) structurally coordinate or merely legitimate expansion').

omega_variable(
    invitation_proportionality_enforcement_gap,
    'How consistently were the juristic conditions (invitation to Islam first, proportionality in conduct, imam authorization) actually enforced against state actors across the historical record?',
    'Historical case study of major conquest campaigns (Ridda wars, early caliphate expansions, Ottoman campaigns) comparing juristic doctrine to recorded practice; analysis of fatwa literature authorizing exceptions.',
    'If conditions were systematically honored, the constraint is genuine tangled rope; if routinely suspended by ''necessity'' or ''interest of Islam'' doctrines, the conditions are performative and the constraint trends toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(invitation_proportionality_enforcement_gap, empirical, 'Gap between doctrinal conditions and historical practice in offensive campaigns').

omega_variable(
    dhimmi_status_extraction_mechanism,
    'Does the dhimmi framework function as a genuine protective coordination mechanism or as an institutionalized extraction system (jizya, legal disabilities, demographic management)?',
    'Economic and demographic analysis of dhimmi communities under long-term Islamic rule: tax burden relative to Muslim zakat, legal autonomy vs. disability, conversion rates and pressures, community survival trajectories.',
    'If dhimmi status is protective coordination, it stabilizes pluralism; if extractive, it is a snare component that makes the overall constraint snare-dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimmi_status_extraction_mechanism, empirical, 'Whether dhimmi protections constitute coordination or extraction in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t50, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t400, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_tr_t800, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 800, 0.25).

% Extraction over time
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t50, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t400, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 400, 0.7).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_be_t800, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 800, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t50, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t400, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(jihad_quranic_corpus__expansionist_legalist_reading_su_t800, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 800, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_status_framework).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, islamic_law_of_war_siyar).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_succession_mechanism).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. This expansionist_legalist_reading has ε=0.72 (substantial extraction via conquest, jizya, spoils). The defensive_spiritual_reading has ε≈0.15 (coordination of defense and spiritual discipline, minimal extraction). The revolutionary_vanguard_reading has ε≈0.85 (high extraction via takfir, property seizure, suspension of protections). They are linked because the expansionist reading's legal framework is cited by the revolutionary reading as the legitimate authority it claims to restore, and the defensive reading as the historical deviation it corrects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, institutional, 0.1).
constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, organized, 0.2).
constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
