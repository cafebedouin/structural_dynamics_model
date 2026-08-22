% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical Preservation Reading of Hebrew Vitality (Sacred-Text Transmission Chain)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical-preservation reading of the
 *   contested kernel 'when is Hebrew alive.' On this reading, vitality is
 *   entirely a function of unbroken recitation, study, and transmission of
 *   the sacred textual corpus across generations, and is wholly independent
 *   of vernacular/spoken use. Under this reading, Hebrew never died in the
 *   first place — there was no gap to revive — so the 19th/20th century
 *   vernacular-revival project (Ben-Yehuda et al.) is not a resurrection but
 *   a repurposing/desecration of a register that was never dormant. The
 *   measured extraction rises over the interval as the reading's
 *   institutional guardians increasingly must defend the liturgical-only
 *   criterion against a rival vernacular-national criterion that has become
 *   socially dominant since 1948 (Israeli spoken Hebrew).
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/arbitrage) — certifies transmission legitimacy
 *   - yeshiva_institutions: beneficiary (organized/constrained) — institutional site of the chain
 *   - traditional_liturgical_communities: beneficiary/payer (moderate/identity_locked) — bears the identity cost of maintenance
 *   - sacred_textual_tradition: payer/non-agent (powerless/trapped/universal) — the entity whose sanctity is diluted by profane reuse
 *   - diaspora_vernacular_speakers: payer (powerless/constrained) — everyday Jewish vernaculars excluded from the vitality criterion
 *   - non_orthodox_hebrew_learners: payer (moderate/constrained) — engagement not recognized under this criterion
 *   - ben_yehuda_revivalist_project: excluded (organized/mobile) — the rival project this reading defines itself against
 *   - comparative_linguists: observer (analytical) — describes but does not adjudicate the criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical Preservation Reading of Hebrew Vitality (Sacred-Text Transmission Chain)").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '7754684d-df1e-4e32-afe9-53f7de44707a').
narrative_ontology:cs_kernel_codification('7754684d-df1e-4e32-afe9-53f7de44707a', fixed_text).
narrative_ontology:cs_authority_grounding('7754684d-df1e-4e32-afe9-53f7de44707a', lineage).
narrative_ontology:cs_interpretation_layer_present('7754684d-df1e-4e32-afe9-53f7de44707a').
narrative_ontology:cs_reading_relation('7754684d-df1e-4e32-afe9-53f7de44707a', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('7754684d-df1e-4e32-afe9-53f7de44707a', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('7754684d-df1e-4e32-afe9-53f7de44707a', foundational, liturgical_continuity_constitutes_life).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('7754684d-df1e-4e32-afe9-53f7de44707a', liturgical_continuity_constitutes_life, conventional).
narrative_ontology:cs_axiom('7754684d-df1e-4e32-afe9-53f7de44707a', foundational, vernacular_repurposing_is_desecration_not_revival).
narrative_ontology:cs_axiom_status(vernacular_repurposing_is_desecration_not_revival, holdable).
narrative_ontology:cs_axiom_grounding('7754684d-df1e-4e32-afe9-53f7de44707a', vernacular_repurposing_is_desecration_not_revival, deontological).
narrative_ontology:cs_reference_frame('7754684d-df1e-4e32-afe9-53f7de44707a', unbroken_diaspora_recitation_chain).
narrative_ontology:cs_drift_state('7754684d-df1e-4e32-afe9-53f7de44707a', post_1948_israeli_vernacular_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7754684d-df1e-4e32-afe9-53f7de44707a', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, non_orthodox_hebrew_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, traditional_liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_never_died_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_chain_of_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certify which recitation, pronunciation, and study practices count as authentic transmission of the sacred texts. Their authority to adjudicate what counts as 'living' Hebrew derives from this gatekeeping function, and they administer the institutions (yeshivot, batei din, textual academies) that reproduce it generation to generation. They can revise standards of legitimacy but have strong reasons not to, since the standard is the source of their authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Receive funding, students, and social legitimacy from being the institutional sites where the unbroken chain of recitation and study is performed and certified. Their continued relevance depends on liturgical Hebrew being recognized as the measure of the language's life, independent of whether anyone speaks it at the market.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Gain communal identity, continuity with ancestors, and religious meaning from participating in the recitation chain. They also bear the cost of maintaining exacting standards of textual fidelity across generations under conditions of dispersion and persecution, and their sense of self is fused with the practice such that abandoning it feels like abandoning peoplehood itself.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_liturgical_communities, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, traditional_liturgical_communities, payer).

% The corpus of text and practice itself — treated here as the entity whose integrity is put at risk. On this reading, every reappropriation of the sacred register for profane, nationalist, or commercial purposes (street signs, army commands, bureaucratic forms) erodes the boundary between holy and mundane speech that the chain of transmission was built to preserve. It has no voice of its own; its 'costs' are borne through the communities who defend its sanctity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition).

% Communities (Yiddish-, Ladino-, Judeo-Arabic-speaking) whose everyday Jewish vernaculars are demoted to secondary status under this reading, since the reading locates linguistic life exclusively in the liturgical register they do not control and often cannot fully read. Their vernaculars carry no standing in the vitality claim regardless of household use.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_vernacular_speakers, payer,
    powerless, biographical, constrained, regional).

% Secular and Reform/Conservative learners who study Hebrew for cultural, academic, or Zionist-nationalist reasons. Under this reading their engagement does not count toward the language's life-status, since only unbroken liturgical recitation does; they pay a legitimacy cost even while investing real effort in the language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, non_orthodox_hebrew_learners, payer,
    moderate, biographical, constrained, national).

% The 19th/20th-century movement to make Hebrew a spoken national vernacular. On this reading their premise (that Hebrew was dead and needed revival) is rejected outright — the project is recast as an act of desecration, converting a living sacred register into a profane national tool. They are not part of this reading's own account of vitality; they are the entity this reading argues against, and are not consulted as a source of legitimacy within it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_revivalist_project, excluded,
    organized, generational, mobile, national).

% Academic linguists who study language vitality using criteria such as intergenerational transmission, domain coverage, and native acquisition. They can describe what the liturgical-preservation criterion includes and excludes but do not adjudicate which criterion is correct; their descriptions are drawn on by all three rival readings of the kernel.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, stateless population across centuries and continents around a single fixed textual and liturgical register, so that Jewish communities separated by geography and vernacular retain a shared point of reference and mutual intelligibility in prayer and study.
% TRANSFER_FUNCTION: Moves interpretive authority, communal legitimacy, and institutional resources (yeshiva funding, rabbinic status, publishing of sacred texts) toward those certified as faithful transmitters of the liturgical chain, and away from vernacular speech communities and secular/nationalist Hebrew projects whose practices are not recognized as constituting the language's life.
% ABSENT_VOICES: Diaspora vernacular communities (Yiddish, Ladino, Judeo-Arabic speakers) and the Zionist vernacular-revival movement are structurally absent from this reading's own definition of vitality — they would object that everyday speech, not liturgical recitation, is what makes a language alive, but this reading's criterion is constructed precisely to exclude their evidence from counting.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation criterion disappeared overnight, rabbinic and yeshiva authority over what counts as 'living Hebrew' would lose its grounding, and the vernacular-revival and marketplace-pidgin readings would gain uncontested field; traditional communities would say something civilizationally essential had been lost even though daily spoken Hebrew (Israeli Hebrew) would be entirely unaffected — hence the dispute over whether the world rearranges is itself part of the kernel contest.
% FOUNDING_PROBLEM: In dispersion, without territory, common vernacular, or centralized political authority, Jewish communities needed some fixed, non-negotiable anchor of continuity across geography and time; unbroken liturgical recitation and study of sacred texts served as that anchor, keeping the textual tradition intelligible and authoritative regardless of what vernaculars communities spoke daily.
% FOUNDING_PROBLEM_CORROBORATION: Traditional rabbinic sources and yeshiva historiography attest the founding problem remains fully live (dispersion continues; the chain must still be actively maintained against assimilation). Outside corroboration is thinner and points the other way: historical linguists and Israeli sociolinguists (e.g. accounts of the Haskalah and the Ben-Yehuda revival) document that by the 19th century Hebrew's liturgical continuity was largely secure while its status as a spoken vernacular had genuinely lapsed for over a millennium — suggesting the founding problem this reading claims to still be solving (continuity of the SACRED register) is largely solved and stable, while the reading's practical effect is now mainly to withhold legitimacy from vernacular and secular Hebrew practice rather than to solve dispersion-era continuity.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) is moderate-to-substantial: the reading extracts legitimacy and institutional resources away from vernacular and secular Hebrew practice toward the certifying rabbinic/yeshiva structure, and this extraction has risen as the vernacular-revival criterion became sociolinguistically dominant in Israel, forcing the liturgical reading into an increasingly defensive, actively-maintained posture. Suppression (0.62) reflects real gatekeeping — non-Orthodox study and vernacular practice are denied standing as evidence of 'life' regardless of their scale. Theater ratio is comparatively low-moderate (0.28): the coordination function (a shared textual anchor across a dispersed population) is genuine and not merely performed, even though a growing share of the reading's energy now goes into boundary-defense against the rival vernacular reading rather than into the original dispersion-era coordination problem. Accessibility collapse is only moderate (0.4) because vernacular and academic alternatives to the liturgical criterion are robustly available and increasingly dominant — this reading has NOT foreclosed its rivals in practice, only within its own framework. Resistance is high (0.7): both the vernacular-revival tradition and diaspora vernacular communities actively contest the criterion's exclusivity.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/yeshiva seat, the arrangement is a genuine, ancient coordination solution — the only thing that has held a stateless, dispersed people together linguistically for two millennia. From the diaspora-vernacular and non-Orthodox learner seats, the same arrangement operates as an active denial of their linguistic reality's standing, propped up by institutions with a direct stake in the criterion's continued authority. The engine's per-seat computation should register this asymmetry directly from the declared power/exit/role data, independent of which framing the story's own claimed_type asserts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and yeshiva institutions sit near the full-beneficiary end: they administer the certifying function and derive their authority and resources directly from the criterion's persistence. Traditional liturgical communities are dual-positioned — real spiritual/communal benefit, but identity-locked exit means they also bear real cost defending an increasingly minority criterion. The sacred textual tradition itself (non-agent) is authored as the ultimate payer on this reading's own terms — its purity/sanctity is what is put at risk by profane reuse — even though it collects no rents and cannot act. Diaspora vernacular speakers and non-Orthodox learners are targets: their linguistic practice does not count toward vitality under this criterion, regardless of scale or sincerity, so their directionality sits near the full-target end. The Ben-Yehuda project is excluded rather than coordinated or targeted in the ordinary sense — it is the rival account this reading defines itself in opposition to, structurally outside this reading's legitimacy conversation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anchoring a dispersed people without territory or common vernacular) is largely solved and stable — the sacred corpus's continuity is not, on any serious account, currently at risk. What persists with rising intensity is the criterion's gatekeeping function against RIVAL readings of vitality, which is a different and newer function than the one that founded the practice. This is the mandatrophy signature: the arrangement's original coordination justification (dispersion-era continuity) has been substantially achieved, while an enforcement function (denying standing to vernacular/secular Hebrew) has grown to fill the space, riding on the original coordination story's legitimacy without needing to re-justify itself on its own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revival_vs_desecration_framing,
    'Is the 19th/20th century Hebrew vernacularization project properly described as a revival of a dead language, or as a repurposing/desecration of a liturgical register that was continuously alive?',
    'There is no neutral empirical fact that settles this — it depends on which vitality criterion (liturgical continuity vs. native daily acquisition) is taken as constitutive of ''life.'' Historical linguistic evidence (continuous liturgical recitation records vs. absence of native child acquisition for ~1700 years) is not in serious dispute; what is disputed is which fact is the relevant one.',
    'Under this reading, no revival was needed and Ben-Yehuda''s project is a category error at best and a desecration at worst; under the native_generational sibling reading, Hebrew was genuinely dead as a vernacular and the same project is a celebrated resurrection. The classification of Ben-Yehuda''s project as beneficial coordination vs. harmful extraction flips entirely depending on which reading is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revival_vs_desecration_framing, conceptual, 'Whether ''aliveness'' is defined by liturgical continuity or vernacular acquisition determines whether Hebrew ever died.').

omega_variable(
    sacred_tradition_as_victim_coherence,
    'Can a textual/liturgical tradition (a non-agent entity) coherently be authored as bearing costs, or is ''harm to the sacred tradition'' always a proxy for the interests of the specific human communities who claim to speak for it?',
    'Examine whether claims of ''desecration'' track any independently identifiable degradation (e.g., loss of specific liturgical practices, textual corruption, decline in recitation fidelity) versus tracking only the loss of certifying authority by specific rabbinic/institutional actors.',
    'If harm-to-tradition claims track only loss of institutional authority, the true victim set collapses to rabbinic/yeshiva institutional interests reframed as sacred harm, which would reclassify this reading closer to tangled_rope-with-thin-coordination or even snare; if independent degradation is identifiable, the non-agent victim framing is more defensible as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_as_victim_coherence, conceptual, 'Whether ''harm to sacred tradition'' is a real cost or a proxy for institutional interest.').

omega_variable(
    liturgical_criterion_naturalness,
    'Is the liturgical-preservation criterion a naturally emergent feature of unbroken religious practice, or a constructed/contested criterion that happens to confer authority on the rabbinic/yeshiva institutions that assert it?',
    'Comparative religious-studies analysis of whether other diaspora sacred-language traditions (e.g. Sanskrit in Hindu liturgy, Ge''ez in Ethiopian Orthodoxy, Church Slavonic) similarly ground their ''life'' claims in liturgical continuity, and whether that grounding correlates with institutional authority structures analogous to the rabbinic/yeshiva case.',
    'If the criterion recurs independently across traditions without correlating to institutional benefit, it supports a more naturalized (mountain-adjacent) reading of liturgical vitality claims generally; if the criterion is selectively deployed in ways that track institutional self-interest, it supports the tangled_rope classification authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liturgical_criterion_naturalness, conceptual, 'Whether the liturgical vitality criterion is cross-culturally natural or locally self-serving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t90, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t90, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t90, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement_basis(hebr_be_t90, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement_basis(hebr_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t90, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement_basis(hebr_su_t90, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.62).
narrative_ontology:measurement_basis(hebr_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kernel hebrew_linguistic_life. The native_generational_reading treats Hebrew as having genuinely lapsed as a spoken vernacular and Ben-Yehuda's project as a necessary, beneficial revival (victim set: suppressed diaspora vernaculars pre-revival; beneficiary set includes the revival project itself). The marketplace_pidgin_reading treats functional inter-communal use (independent of both liturgy and native acquisition) as the vitality criterion, recognizing medieval trade/administrative Hebrew as evidence of continuous life on different grounds than either sibling. This story (liturgical_preservation_reading) uniquely treats the sacred textual tradition as the victim of vernacular/secular reappropriation and treats Hebrew as never having died at all. Each reading carries a distinct epsilon, beneficiary/victim structure, and claimed_type; they are linked here rather than merged because merging would violate the epsilon-invariance principle — the three readings do not share a referent for what counts as evidence of 'life.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
