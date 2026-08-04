% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical-Continuity Standard for Hebrew's Living Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'hebrew_linguistic_life': the liturgical-preservation reading, which
 *   holds that Hebrew never died because its sacred texts were continuously
 *   recited, studied, and transmitted in an unbroken chain by scholars and
 *   congregations, regardless of the absence of native vernacular speakers
 *   for roughly seventeen centuries. Under this reading, the modern Hebrew
 *   revival associated with Eliezer Ben-Yehuda and the Zionist linguistic
 *   project is not resurrection of a dead language but rather a reorientation
 *   — critics within this reading's tradition would call it a desecration —
 *   of a living sacred tongue toward secular, mundane, vernacular purposes it
 *   was never meant to serve. This is a structurally distinct constraint from
 *   the sibling readings (native_generational_reading: Hebrew was dead until
 *   children acquired it as mother tongue in Ottoman/British Palestine;
 *   marketplace_pidgin_reading: Hebrew's life-status tracks its function as
 *   inter-communal trade/coordination medium). Each reading has a different
 *   victim set, a different beneficiary set, and a different ε — they are NOT
 *   the same constraint viewed from different angles; they are three
 *   constraints linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/identity_locked) — administer the liturgical-continuity criterion
 *   - yeshiva_institutions: beneficiary (organized/constrained) — reproduce the transmission chain
 *   - diaspora_religious_communities: beneficiary+payer (organized/constrained) — maintain liturgical practice, bear rigidity cost
 *   - sacred_liturgical_tradition: payer, non-agent (institutional/trapped) — the corpus mobilized as evidence
 *   - traditionalist_hebrew_scholars: payer (moderate/identity_locked) — career fused to the criterion
 *   - displaced_vernacular_registers: payer, non-agent (powerless/trapped) — erased from the vitality account
 *   - secular_revivalist_linguists: excluded (moderate/mobile) — hold the sibling reading, not consulted here
 *   - comparative_sociolinguists: observer (analytical/analytical) — evaluates cross-tradition validity of the criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.38).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical-Continuity Standard for Hebrew's Living Status").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'fce73150-3ff8-4d3b-980a-9d0e8badf30c').
narrative_ontology:cs_kernel_codification('fce73150-3ff8-4d3b-980a-9d0e8badf30c', distributed).
narrative_ontology:cs_authority_grounding('fce73150-3ff8-4d3b-980a-9d0e8badf30c', lineage).
narrative_ontology:cs_interpretation_layer_present('fce73150-3ff8-4d3b-980a-9d0e8badf30c').
narrative_ontology:cs_reading_relation('fce73150-3ff8-4d3b-980a-9d0e8badf30c', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_reading_relation('fce73150-3ff8-4d3b-980a-9d0e8badf30c', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('fce73150-3ff8-4d3b-980a-9d0e8badf30c', foundational, liturgical_recitation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_recitation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('fce73150-3ff8-4d3b-980a-9d0e8badf30c', liturgical_recitation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('fce73150-3ff8-4d3b-980a-9d0e8badf30c', secondary, vernacular_acquisition_is_not_necessary_for_life).
narrative_ontology:cs_axiom_status(vernacular_acquisition_is_not_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('fce73150-3ff8-4d3b-980a-9d0e8badf30c', vernacular_acquisition_is_not_necessary_for_life, conventional).
narrative_ontology:cs_reference_frame('fce73150-3ff8-4d3b-980a-9d0e8badf30c', unbroken_recitation_as_life_criterion).
narrative_ontology:cs_drift_state('fce73150-3ff8-4d3b-980a-9d0e8badf30c', post_zionist_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fce73150-3ff8-4d3b-980a-9d0e8badf30c', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_hebrew_scholars).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, displaced_vernacular_registers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, textual_continuity_as_life_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the standard by which Hebrew's liturgical use counts as proof of ongoing life: continuous recitation, study lineage, and textual transmission across generations of scholars and congregations. They set what counts as an unbroken chain and adjudicate disputes about textual fidelity. Their institutional authority depends on the liturgical-continuity criterion remaining the accepted measure of the language's vitality; a shift to a vernacular-acquisition standard would displace their gatekeeping role entirely.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Teach and reproduce the study-and-recitation chain that the standard treats as evidence of life. Their curricula, funding models, and institutional prestige are built around Hebrew's status as a living sacred tongue defined by continuous scholarly transmission rather than colloquial speech. They benefit from a definition of linguistic life that does not require them to compete with vernacular usage for legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Maintain synagogue liturgy, textual study circles, and prayer recitation in Hebrew regardless of daily vernacular (Yiddish, Ladino, Arabic, English, etc.). They benefit because their unbroken ritual practice is itself the proof-condition of the language's life, requiring no additional demonstration. They also pay a cost: the standard freezes expectations about correct transmission, making any adaptation or simplification of liturgical Hebrew read as decay rather than living change.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities, payer).

% The textual and ritual corpus itself is treated as the vessel whose unbroken recitation constitutes the language's life. Framed as a non-agent good, it nonetheless bears the cost of the standard: because vitality is defined by fidelity to transmission rather than by living generative use, the tradition is held static, defended against modernization, and mobilized as evidence in disputes it did not choose to enter.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition).

% Devote careers to the study and transmission chain that the standard treats as the sole legitimate proof of Hebrew's life. They bear the reputational and material cost of defending the liturgical-continuity criterion against both secular Hebrew revivalists and academic linguists who define linguistic life by vernacular acquisition. Their professional identity is fused to the claim that the chain, not the street, is what kept the language alive.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_hebrew_scholars, payer,
    moderate, biographical, identity_locked, regional).

% Historical spoken and written Hebrew registers used for commerce, correspondence, and informal composition across centuries are treated as irrelevant to the life-question under this reading, since only liturgical transmission counts. Their existence is erased from the vitality account even where they show generative, non-sacred use.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, displaced_vernacular_registers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, displaced_vernacular_registers).

% Would argue that the liturgical-continuity criterion mistakes preservation for life and that only vernacular acquisition by children counts as a language being truly alive — that Ben-Yehuda-style projects are properly termed revival rather than desecration. They are not part of the rabbinic-authority conversation that sets the liturgical standard and are treated by this reading's adherents as measuring a different, secular phenomenon.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_revivalist_linguists, excluded,
    moderate, generational, mobile, national).

% Study competing definitions of language death and life across cases (Coptic, Sanskrit, Latin, Old Church Slavonic) and can evaluate whether the liturgical-continuity criterion is coherent as a general linguistic standard or specific to sacred-text traditions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_sociolinguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, portable criterion for what counts as Hebrew's continued life that does not depend on any territorial concentration of vernacular speakers — allowing dispersed communities across centuries and continents to recognize themselves as participants in one continuous linguistic-religious lineage via shared recitation and study practice.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority toward rabbinic and yeshiva structures that administer the transmission chain, and away from vernacular speech communities and secular linguists whose criteria would relocate the locus of legitimacy to colloquial use; also transfers narrative credit for Hebrew's persistence away from any revival account and onto continuous religious practice.
% ABSENT_VOICES: Secular revivalist linguists and historians of the vernacular Hebrew registers (commercial, epistolary, poetic) used across the diaspora are not part of the liturgical-authority conversation and would object that the standard erases evidence of ordinary generative use in favor of ritual repetition alone.
% DISAPPEARANCE_RATIONALE: If the liturgical-continuity standard vanished, rabbinic and yeshiva institutions would lose their exclusive claim to defining Hebrew's vitality, and the modern revival narrative (native-generational reading) would gain uncontested field. Adherents of this reading dispute that anything would change in the language's actual status, since they hold Hebrew was never linguistically dead in the first place — only its vernacular use lapsed. The disagreement is precisely about what would rearrange: institutional authority certainly would; the language's ontological status is contested.
% FOUNDING_PROBLEM: How to establish and defend a criterion of linguistic life that credits sacred textual transmission — recitation, study, liturgical use — as sufficient proof of vitality, so that the absence of native vernacular speakers does not count as language death for a language embedded in unbroken religious practice.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and yeshiva institutions attest the founding problem remains fully live — that liturgical transmission is and always was the true life-criterion. Outside corroboration is mixed: historical linguists and scholars of language death (citing typologies distinguishing liturgical/learned languages like Latin and Sanskrit from vernacularly living ones) attest that the liturgical-continuity criterion describes a real and distinct phenomenon — sustained learned-language use — but dispute that it is the same phenomenon as ordinary linguistic life, suggesting the founding problem as stated conflates two different questions rather than solving one settled one.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).
:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low or high: the liturgical-continuity criterion does perform genuine coordination work (letting dispersed communities recognize a shared linguistic-religious lineage) but it also extracts interpretive authority away from vernacular speech evidence and toward rabbinic gatekeeping, and it extracts narrative credit away from any revival account. Suppression is moderate (0.38): the standard is defended by institutional authority and identity-fusion among scholars rather than by coercive apparatus, so it is lower than an enforcement-heavy snare but non-trivial because dissenting linguistic accounts are institutionally marginalized rather than engaged. Theater ratio is low-moderate (0.22) and rises slowly across the measured interval as the criterion increasingly functions to defend institutional turf against the encroaching prestige of the modern spoken-Hebrew revival narrative, rather than purely to describe liturgical practice.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic-authority seat, the criterion is simply a description of what has always been true: Hebrew's sacred use never stopped, so by definition it never died — no coordination/extraction tension is visible from inside. From the traditionalist-scholar and diaspora-community seats, genuine coordination value (shared identity across dispersion) exists alongside a cost: the standard requires defending textual fidelity against modernization pressure, freezing what would otherwise be living variation. From the excluded secular-revivalist seat, the criterion looks like institutional self-preservation dressed as linguistic description — a different structural read the engine would compute differently given the same underlying facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and yeshiva institutions sit near the beneficiary end: they administer and are institutionally validated by the criterion (low d). Diaspora religious communities are dual-positioned — real coordination benefit from shared liturgical identity, but constrained exit and a cost from frozen textual expectations (mid d). Traditionalist scholars and the non-agent sacred tradition and displaced vernacular registers sit near the target end: their exit is trapped or identity-locked, and the standard's persistence is what forecloses alternative accounts of their history (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure rope or pure mountain prevents two mislabeling errors: treating the criterion as purely natural/objective (which would hide the institutional interests served by defining vitality this way) and treating it as purely extractive cover (which would erase the genuine coordination function of shared liturgical identity across a two-thousand-year diaspora). Both a coordination function and an asymmetric extraction are present and required active maintenance — hence tangled_rope, not mountain or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    life_vs_preservation_conflation,
    'Does ''continuous liturgical use'' constitute the same phenomenon as ''linguistic life'' in the sense linguists use for spoken vernaculars, or does the liturgical-preservation reading conflate two distinct phenomena (sustained learned-language transmission vs. ordinary generative vernacular life) under one label?',
    'Comparative typological analysis against other liturgical/learned languages with continuous transmission but no native speakers (Latin in the Catholic Church, Sanskrit in Vedic recitation, Ge''ez in Ethiopian Orthodox liturgy) to determine whether sociolinguistics treats these as ''alive'' or as a distinct category (''liturgical language,'' ''learned language'') apart from living vernaculars.',
    'If the categories are genuinely distinct, the liturgical-preservation reading is not a competing claim about the SAME question the native-generational reading answers, but an answer to a different question wearing the same label — which would argue for permanently decomposed, non-competing constraints rather than a single contested kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(life_vs_preservation_conflation, conceptual, 'Whether liturgical continuity and vernacular life are the same explanandum or two different ones sharing a label.').

omega_variable(
    desecration_framing_beneficiary_check,
    'Is the ''desecration'' framing of Ben-Yehuda''s project a genuine theological/linguistic claim independent of institutional interest, or does it primarily serve to preserve rabbinic and yeshiva institutional authority over the definition of Hebrew''s vitality against the rival secular/national claim to have revived it?',
    'Examine historical rabbinic responsa and communal debate literature contemporaneous with the Ben-Yehuda project for arguments made independent of institutional turf concerns, and compare against cases where similar liturgical languages faced vernacularization (Aramaic, Church Slavonic) to see whether ''desecration'' language correlates with institutional stakes.',
    'If the desecration framing tracks institutional stakes closely, this strengthens the tangled_rope reading (coordination function real, but the specific claim about desecration is extraction-serving rhetoric); if it is largely independent of institutional interest, the beneficiary structure may be overstated and the constraint may sit closer to a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desecration_framing_beneficiary_check, empirical, 'Whether the desecration framing is institutionally self-serving or an independent theological claim.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of hebrew_linguistic_life (liturgical_preservation, native_generational, marketplace_pidgin) genuinely competing answers to one question, or incommensurable framings that cannot be adjudicated by any single evidentiary standard because each defines ''alive'' by a different observable?',
    'Attempt to construct a single operational definition of ''linguistic life'' that all three readings would accept as fair; if no such definition can command assent from all three traditions, the readings are incommensurable rather than merely disputed.',
    'If incommensurable, the kernel itself may not be a genuine single kernel with competing readings but rather three separate concepts sharing a label — which would argue for treating hebrew_linguistic_life as a decomposed family from the start rather than a contested kernel with resolvable disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are commensurable competing claims or incommensurable framings sharing a label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.21).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.41).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(hebr_su_t80, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.37).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the contested kernel hebrew_linguistic_life. liturgical_preservation_reading (this story) holds Hebrew never died — sacred textual transmission is sufficient proof of life, ε=0.42, tangled_rope, victim set centers the sacred tradition and its scholarly custodians. native_generational_reading holds Hebrew was linguistically dead until children acquired it as mother tongue in Mandate Palestine — a genuine revival, likely lower extraction and a rope/scaffold framing, victim set would center displaced or marginalized non-Hebrew vernaculars of the immigrant generation. marketplace_pidgin_reading holds vitality tracks inter-communal practical coordination function independent of sacred status — likely the lowest-extraction reading, closest to a rope, since it requires no institutional gatekeeper at all. Each reading is authored as its own ε-invariant constraint per the ε-invariance principle; they are linked here rather than merged because measuring 'is Hebrew alive' by liturgical continuity versus by mother-tongue acquisition versus by trade-medium function yields three different ε values, meaning they are three different constraints, not one constraint under three observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
