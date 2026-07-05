% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Kurukshetra Discourse (Caste Duty and Righteous War)
 *   domain: religious/social/ethical
 *
 * SUMMARY:
 *   This story instantiates the orthodox literal reading of the Kurukshetra
 *   discourse as a distinct constraint: the text is read as mandating
 *   caste-fixed duty (svadharma tied to varna) and as legitimating violence
 *   performed by a duty-bound kshatriya in a righteous war, with the Brahmin
 *   class retaining exclusive interpretive authority over what dharma
 *   requires. This is one reading among a contested kernel; the allegorical
 *   reading (battlefield as internal spiritual struggle) and the universalist
 *   devotional reading (bhakti as caste-independent path) are separate
 *   constraints with their own beneficiary/victim structures and are not
 *   merged into this file's classification. Extraction rises over the
 *   interval as the doctrine is progressively invoked to underwrite specific
 *   caste-legal codifications and, later, nationalist militant appropriations
 *   of 'righteous war' rhetoric — a drift the text's narrative frame did not
 *   originally require.
 *
 * KEY AGENTS:
 *   - brahmin_interpretive_class: agenda_setter/beneficiary (institutional/arbitrage) — controls exegesis and collects deference
 *   - kshatriya_warrior_caste: beneficiary (powerful/constrained) — receives moral cover for violence performed as duty
 *   - ruling_dynasties: beneficiary (institutional/mobile) — naturalizes hierarchy as cosmic rather than political
 *   - shudra_and_lower_caste_communities: payer (powerless/trapped) — bears birth-fixed duty with no interpretive standing
 *   - war_dead_and_conscripted_soldiers: payer (powerless/trapped) — bears the literal cost of a doctrine that declares righteous killing metaphysically weightless
 *   - reformist_and_bhakti_movements: excluded (moderate/constrained) — contest the reading but lack institutional platform historically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.71).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Kurukshetra Discourse (Caste Duty and Righteous War)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/social/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'c1217ea7-60b8-480a-b0fa-7a4c849deb5b').
narrative_ontology:cs_kernel_codification('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', fixed_text).
narrative_ontology:cs_authority_grounding('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', lineage).
narrative_ontology:cs_interpretation_layer_present('c1217ea7-60b8-480a-b0fa-7a4c849deb5b').
narrative_ontology:cs_reading_relation('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', foundational, caste_duty_is_divinely_ordained_and_binding).
narrative_ontology:cs_axiom_status(caste_duty_is_divinely_ordained_and_binding, holdable).
narrative_ontology:cs_axiom_grounding('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', caste_duty_is_divinely_ordained_and_binding, theological).
narrative_ontology:cs_axiom('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', foundational, duty_bound_violence_in_righteous_war_incurs_no_true_sin).
narrative_ontology:cs_axiom_status(duty_bound_violence_in_righteous_war_incurs_no_true_sin, holdable).
narrative_ontology:cs_axiom_grounding('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', duty_bound_violence_in_righteous_war_incurs_no_true_sin, deontological).
narrative_ontology:cs_reference_frame('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', varna_ashrama_cosmological_order).
narrative_ontology:cs_drift_state('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c1217ea7-60b8-480a-b0fa-7a4c849deb5b', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_caste).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, ruling_dynasties).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_and_lower_caste_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_soldiers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_excluded_from_dharmic_agency).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varna_ashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_over_universal_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Sanskrit textual transmission, commentary tradition (bhashya), and ritual authority to pronounce on what svadharma requires of each caste. Sits atop the interpretive hierarchy the text itself ratifies, and collects deference, patronage, and ritual centrality as a direct consequence of being the caste positioned to explain everyone else's duty to them.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, beneficiary).

% Receives explicit textual sanction for violence performed in the discharge of caste duty — Krishna's counsel to Arjuna that killing in a righteous war incurs no true sin functions as a standing license. Rulers and warriors invoke this passage to legitimate war-making as dharma rather than choice, converting political violence into cosmic obligation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_caste, beneficiary,
    powerful, generational, constrained, regional).

% Uses the caste-duty framework to naturalize a social order in which taxation, land rights, and military conscription track birth rather than choice, and can cite the discourse to quiet dissent among subjects by framing hierarchy as divinely instituted rather than politically constructed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, ruling_dynasties, beneficiary,
    institutional, generational, mobile, regional).

% Assigned birth-fixed duties of service to higher castes and denied access to the text's interpretive authority and, in many orthodox readings, to certain modes of religious practice altogether. Exit from the assigned role is framed as adharma (violation of cosmic order) rather than a live choice, and enforcement runs through social, ritual, and sometimes legal sanction.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_and_lower_caste_communities, payer,
    powerless, civilizational, trapped, regional).

% Bears the literal cost of the doctrine's central claim — that killing performed as caste duty in a righteous war is not truly killing. Common soldiers and opposing combatants die inside a framework that has already declared their deaths metaphysically weightless if the war is dharmic, foreclosing the question of whether the war should have been fought at all.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_soldiers, payer,
    powerless, immediate, trapped, regional).

% Positioned within the same orthodox reading as bound to household and reproductive duty (stridharma) rather than independent religious or civic agency, with textual authority cited to foreclose alternative roles.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, women_excluded_from_dharmic_agency, payer,
    powerless, generational, trapped, regional).

% Devotional and reform traditions that read the same verses as caste-independent would contest the orthodox literal reading's monopoly on interpretation, but historically lacked the institutional platform (temple control, Sanskrit literacy, royal patronage) to displace the Brahmin interpretive class's reading in the dominant social order.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, reformist_and_bhakti_movements, excluded,
    moderate, generational, constrained, regional).

% Study the historical function of the orthodox literal reading as one of several live interpretive traditions, tracing how it interacted with colonial-era caste codification, legal systems, and nationalist reappropriation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cosmological framework that assigns social roles, resolves the ethical crisis of civil war through a doctrine of disinterested action (nishkama karma), and gives a warrior facing an agonizing choice a coherent account of duty that does not require him to invent his own ethics from scratch.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual deference, and material patronage toward the Brahmin class; moves compliance, service labor, and foreclosed mobility from lower castes toward the upper castes the hierarchy favors; moves the moral cost of war away from kshatriya decision-makers and onto the metaphysical ledger where killing-as-duty is declared not truly killing.
% ABSENT_VOICES: Lower-caste communities whose assigned duties the text ratifies are not positioned as interpreters of the text within the orthodox tradition — their objections surface centuries later through bhakti reformers, Buddhist and Jain counter-traditions, and eventually through Ambedkarite critique, none of which the orthodox literal reading's own interpretive chain treats as authoritative correction.
% DISAPPEARANCE_RATIONALE: Defenders of the orthodox literal reading hold that varna-ashrama-dharma reflects a real, if now largely defunct, social ordering whose disappearance would be a loss of tradition rather than a correction; critics and reformist traditions hold that its disappearance would simply remove textual cover for a caste hierarchy and war-legitimation doctrine that already operates through other social and legal mechanisms, so the world would not meaningfully rearrange for those mechanisms, only lose one of their justifications.
% FOUNDING_PROBLEM: The discourse was framed, within the epic narrative, to resolve Arjuna's paralysis before a war against kinsmen and teachers — an acute crisis of action under conflicting loyalties — and, at the level of the wider textual tradition, to stabilize a social order for a warrior-and-priestly aristocracy governing an agrarian, multi-caste society.
% FOUNDING_PROBLEM_CORROBORATION: Traditional orthodox commentators (Shankara, Ramanuja, and later dharmashastra-aligned exegetes) attest the founding problem — right action amid conflicting duty, and the maintenance of social order — as still live and correctly resolved by caste-duty. Independent historians of South Asian religion and law, along with Ambedkarite and reformist scholarship writing from outside the Brahminical interpretive tradition, attest that the caste-ordering function has lost normative legitimacy in most contemporary contexts even where the text retains devotional authority, and that the 'righteous war' doctrine has been repeatedly repurposed for political violence (including 20th-century nationalist and militant invocations) well past any original narrative context.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71) is authored high because the reading converts a social hierarchy and a decision to wage war into cosmological necessity, foreclosing the question of whether either arrangement is contestable on its own political or ethical merits — that conversion is the extraction mechanism itself. Suppression (0.78) is authored higher than extraction because maintaining the reading against competing devotional and reform traditions has historically required ritual exclusion (denial of textual access and ritual participation to lower castes), social sanction, and in some periods legal codification of caste status — suppression here is a raw structural property of enforcement machinery, not scaled by scope. Theater ratio (0.42) reflects a real coordination function (the text does address an authentic crisis of action) alongside a growing performative layer where invocation of 'duty and righteous war' in later centuries increasingly serves political legitimation rather than genuine resolution of the narrative's ethical dilemma. Accessibility collapse (0.62) is moderate rather than mountain-level: alternative readings (allegorical, devotional) persisted throughout history and were never fully extinguished, only marginalized from institutional power. Resistance (0.58) reflects sustained counter-traditions — Buddhist and Jain critique, bhakti reform, and modern Ambedkarite rejection — that never fully accepted the orthodox reading's caste ontology.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin interpretive class and kshatriya/ruling strata sit near the beneficiary end of directionality: they set the terms of duty, collect deference or war-legitimation, and face no structural cost from the doctrine's operation. Lower-caste communities and war casualties sit near the full-target end: duty is assigned to them without consent or interpretive voice, and its consequences (service obligation, death in war framed as dharma) are borne directly. Women within this reading are similarly locked into an assigned role (stridharma) with textual sanction cited against alternatives. Reformist movements are excluded rather than coordinated — their absence from the interpretive process is the mechanism by which the orthodox reading maintained dominance for centuries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving an acute crisis of action for a specific warrior facing civil war, within a specific social order — is treated by the orthodox tradition as permanently live and correctly resolved by caste-duty doctrine. But the tangled_rope classification captures that a genuine coordination function (an answer to moral paralysis, a coherent ethical framework for action) persists alongside asymmetric extraction (a caste hierarchy and a war-legitimation license that outlived, or were never limited to, the narrative crisis that occasioned them). Classifying this as pure snare would erase the coordination function real coordination-seeking traditions still find in the text's treatment of duty and non-attachment; classifying it as pure rope would erase the caste-locked victims and conscripted dead the same reading produces. The tangled_rope type holds both facts open simultaneously, which is the point of this reading as distinct from its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthodox_reading_as_one_among_several,
    'Is the orthodox literal reading the text''s original or primary intended meaning, or one historically dominant reading among several coexisting interpretive traditions (allegorical, devotional) that achieved institutional dominance because it served ruling and priestly interests rather than because of superior textual warrant?',
    'Comparative philological and historical analysis of commentarial traditions (Shankara, Ramanuja, Madhva, and non-Brahminical readings) tracing which readings had institutional patronage versus textual-critical support, and examination of pre-orthodox strata of the epic tradition.',
    'If the orthodox reading achieved dominance primarily through institutional patronage rather than textual necessity, its caste-hierarchy and war-license claims are better understood as constructed extraction dressed in cosmological necessity rather than a natural or inevitable reading of the source text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_reading_as_one_among_several, conceptual, 'Whether the orthodox literal reading''s dominance reflects textual warrant or institutional power.').

omega_variable(
    caste_beneficiary_naturalization,
    'Does the text''s ratification of caste-based duty describe a genuinely divinely-ordained cosmic order (as the tradition claims) or naturalize a historically contingent social hierarchy that benefits identifiable groups (Brahmins, kshatriyas, ruling dynasties)?',
    'Historical-sociological analysis of varna system formation, correlated with periods of textual codification and commentarial elaboration, to assess whether doctrinal elaboration tracked or preceded the material interests of the beneficiary castes.',
    'If naturalization, the coordination story (resolving Arjuna''s moral crisis) functions as cover for caste extraction that would otherwise require independent political justification; if genuine cosmic ordering as claimed by the tradition, the beneficiary structure is incidental rather than constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_beneficiary_naturalization, conceptual, 'Natural cosmic order versus constructed caste hierarchy naturalized through doctrine.').

omega_variable(
    righteous_war_doctrine_scope_creep,
    'Was the ''righteous war, no true sin'' doctrine bounded to the specific narrative circumstances (a war already deemed just by dharmic criteria, fought reluctantly) or has it been extended, in later invocation, to license violence more broadly regardless of those bounding conditions?',
    'Historical tracing of citations of the relevant verses in political and militant rhetoric across centuries, checking whether invocations preserve or discard the narrative''s bounding conditions (reluctance, exhaustion of alternatives, prior determination of dharmic cause).',
    'If scope has crept, later invocations constitute an extraction layered onto the original coordination function — using the text''s authority to license violence the original narrative logic would not have sanctioned, which would support the tangled_rope classification''s rising extraction trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(righteous_war_doctrine_scope_creep, empirical, 'Whether righteous-war doctrine has been extended beyond its original narrative bounding conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the gita_kurukshetra_discourse kernel. orthodox_literal_reading (this file) carries substantial extraction via caste-hierarchy beneficiaries and war-death/lower-caste victims, classified tangled_rope. gandhian_allegorical_reading dissolves the literal caste and war claims by reading the battlefield as internal struggle, structurally foreclosing this reading's core premise that the war and caste duties described are to be taken as literal external mandates — these two readings cannot coexist within a single interpretive framework held by one party. universalist_devotional_reading contests this reading's caste-gated interpretive monopoly by asserting bhakti access is path-independent of caste, which does not strictly foreclose this reading (different communities can and do hold each) but exerts strong downstream pressure against the Brahmin interpretive monopoly this reading depends on. All three share the same textual kernel but instantiate different ε, beneficiaries, and victims and must not be merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
