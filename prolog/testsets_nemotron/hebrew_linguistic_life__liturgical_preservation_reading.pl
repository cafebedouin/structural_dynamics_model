% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Hebrew Liturgical Continuity as Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This reading asserts that Hebrew never died — its life is the unbroken
 *   chain of sacred textual transmission (recitation, study, commentary,
 *   ordination) from Sinai to the present. Vernacular use is irrelevant; the
 *   language lives in the yeshiva, the synagogue, the bet midrash.
 *   Ben-Yehuda's secular revival is not resurrection but desecration: it
 *   severed the sacred lexicon from its halakhic moorings and produced a
 *   'pidgin' for marketplace coordination. The victim is the sacred tradition
 *   itself, which this reading sees as violated by the claim that a secular
 *   vernacular continues it. Beneficiaries are the institutions that embody
 *   the chain. The constraint presents as a mountain: the chain's continuity
 *   is a fact of history, not a human arrangement requiring enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.18).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.25).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Liturgical Continuity as Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '90a0babf-d7f5-4c37-94e3-ea982d715c7b').
narrative_ontology:cs_kernel_codification('90a0babf-d7f5-4c37-94e3-ea982d715c7b', fixed_text).
narrative_ontology:cs_authority_grounding('90a0babf-d7f5-4c37-94e3-ea982d715c7b', lineage).
narrative_ontology:cs_interpretation_layer_present('90a0babf-d7f5-4c37-94e3-ea982d715c7b').
narrative_ontology:cs_reading_relation('90a0babf-d7f5-4c37-94e3-ea982d715c7b', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('90a0babf-d7f5-4c37-94e3-ea982d715c7b', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('90a0babf-d7f5-4c37-94e3-ea982d715c7b', foundational, hebrew_never_ceased_transmission).
narrative_ontology:cs_axiom_status(hebrew_never_ceased_transmission, holdable).
narrative_ontology:cs_axiom_grounding('90a0babf-d7f5-4c37-94e3-ea982d715c7b', hebrew_never_ceased_transmission, conventional).
narrative_ontology:cs_axiom('90a0babf-d7f5-4c37-94e3-ea982d715c7b', foundational, vernacular_use_irrelevant_to_linguistic_life).
narrative_ontology:cs_axiom_status(vernacular_use_irrelevant_to_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('90a0babf-d7f5-4c37-94e3-ea982d715c7b', vernacular_use_irrelevant_to_linguistic_life, deontological).
narrative_ontology:cs_axiom('90a0babf-d7f5-4c37-94e3-ea982d715c7b', secondary, ben_yehuda_project_is_desecration).
narrative_ontology:cs_axiom_status(ben_yehuda_project_is_desecration, holdable).
narrative_ontology:cs_axiom_grounding('90a0babf-d7f5-4c37-94e3-ea982d715c7b', ben_yehuda_project_is_desecration, deontological).
narrative_ontology:cs_reference_frame('90a0babf-d7f5-4c37-94e3-ea982d715c7b', sinai_chain_unbroken).
narrative_ontology:cs_drift_state('90a0babf-d7f5-4c37-94e3-ea982d715c7b', post_ben_yehuda_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90a0babf-d7f5-4c37-94e3-ea982d715c7b', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_yeshiva_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authority_structures).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_language_immortality_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, oral_torah_unbroken_chain_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the unbroken chain of textual transmission through daily study cycles, ordination lineages, and institutional curricula. Their authority and resource flows depend on being recognized as the custodians of the living language. Exit would mean abandoning the self-concept that constitutes their institutional identity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_yeshiva_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Adjudicate what counts as valid transmission, authorize interpretive innovations, and define the boundaries of the sacred linguistic sphere. Their legitimacy derives from the claim that Hebrew never ceased to be alive — the chain is unbroken, so their authority is continuous. They cannot exit this role without dissolving the grounds of their authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authority_structures, agenda_setter,
    institutional, generational, identity_locked, global).

% Experience Hebrew as a living language through daily prayer, ritual, and communal study. They receive the coordination benefit of a shared sacred vocabulary across diaspora and generations. Exit is constrained by communal belonging — leaving the liturgical world means leaving the community.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities, beneficiary,
    organized, biographical, constrained, global).

% Speak Hebrew as a vernacular mother tongue in daily life, media, and governance. This reading classifies their speech as a separate phenomenon — a secular pidgin or desecration — not the continuation of the sacred language. They are not consulted in the definition of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_speakers, excluded,
    organized, biographical, mobile, national).

% Analyze the structural continuity between Biblical, Mishnaic, Medieval, and Modern Hebrew. They document the unbroken textual chain but apply different criteria for 'language death' and 'revival' than this reading's internal logic.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, trans-generational, trans-geographic semantic anchor for Jewish collective identity and religious practice through a textual tradition that never ceased transmission. The coordination problem solved: how to maintain a single sacred vocabulary and interpretive framework across millennia of diaspora without a shared vernacular.
% TRANSFER_FUNCTION: Moves interpretive authority and definitional power over the sacred lexicon from each generation's masters to the next through the unbroken chain of ordination and textual commentary. No material transfer; the flow is legitimacy and custodial responsibility.
% ABSENT_VOICES: Secular Hebrew speakers (native_generational_reading adherents) and Mizrahi/Sephardi communities whose liturgical pronunciation traditions differ from the Ashkenazi-dominant yeshiva standard — both would object to the claim that their Hebrew is 'not the living language' but are structurally excluded from the definitional authority.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the textual chain would continue unchanged — the yeshivas would still study, the communities would still pray, the rabbis would still ordain. The constraint describes what IS, not what MAKES it be. The world does not rearrange because the arrangement is descriptive of a natural continuity.
% FOUNDING_PROBLEM: How to preserve the integrity of divine revelation across exile, dispersion, and the loss of political sovereignty — ensuring that the words of Torah remain accessible, intelligible, and authoritatively interpreted without a Temple, a Sanhedrin, or a shared land.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of exile (Josephus, Talmudic accounts of Yavneh, medieval responsa literature) — sources outside the current beneficiary institutions. The claim that the problem remains live is corroborated by ongoing halakhic debates over new technologies, medical ethics, and political sovereignty that all presuppose the continuous applicability of the same textual tradition.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) — the constraint does not extract material resources from outsiders; its 'cost' is the exclusion of alternative definitions of linguistic life. Suppression is low-moderate (0.25) — the constraint does not actively prevent secular Hebrew speech, but it denies that speech the status of 'the living Hebrew language,' which has institutional consequences (e.g., for conversion, marriage, burial authority). Theater ratio is very low (0.12) — the textual transmission is genuine, not performative. Accessibility collapse is very high (0.88) — once one accepts the premise that sacred textual continuity defines linguistic life, no alternative definition is coherent within the framework. Resistance is near-zero (0.08) — the chain continues regardless of whether secular speakers accept the definition.
 *
 * PERSPECTIVAL GAP:
 *   From inside the yeshiva world, this is a mountain — the chain is a fact of reality. From the secular Israeli perspective, this is a snare — an institutional claim that delegitimizes their native language and extracts authority over Jewish identity. The engine will compute this divergence from the structural data: agenda_setter/beneficiary seats (institutional, identity_locked) will see mountain; excluded seat (secular_hebrew_speakers, mobile exit) will see snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional institutions are identity_locked beneficiaries — their institutional self-concept IS the unbroken chain. They cannot exit without ceasing to be what they are. Rabbinic authorities are agenda_setters who are also identity_locked — they define the chain's validity. Liturgical communities are constrained beneficiaries — they receive coordination value but exit is costly (communal belonging). Secular Hebrew speakers are excluded with mobile exit — they simply speak a different language by this reading's lights. Historical linguists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserve the textual chain) has not atrophied — the chain continues, the institutions function, the transmission is verified. The founding problem (exilic preservation) remains live because the halakhic system still requires authoritative interpretation of the same texts. No mandatrophy: the arrangement still does what it was built to do, and the beneficiaries are the ones who maintain it, not external extractors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_continuity,
    'Is the unbroken chain of textual transmission a genuine natural-historical fact (mountain) or a constructed institutional achievement that requires active maintenance (rope/scaffold)?',
    'Historical analysis of transmission gaps: were there periods when the chain was objectively broken (no yeshivas, no ordination, no study) and later reconstituted? If yes, the continuity is constructed; if no, it approaches natural law status.',
    'If constructed, the constraint is a rope or scaffold with beneficiaries (the maintaining institutions) — FSM candidate. If natural, it remains a mountain with beneficiaries as custodians, not creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_continuity, empirical, 'Whether the textual chain''s continuity is a brute fact or an institutional achievement.').

omega_variable(
    sacred_vs_vernacular_boundary,
    'Is the boundary between sacred Hebrew (liturgical/textual) and secular Hebrew (vernacular) structurally stable, or does the secular domain inevitably colonize the sacred lexicon through neologism and semantic shift?',
    'Linguistic analysis of Modern Hebrew''s vocabulary: what percentage of daily-use terms derive from sacred texts vs. modern coinages on sacred roots? If the sacred lexicon is the productive base for the vernacular, the boundary is porous.',
    'If porous, the ''desecration'' claim loses force — the sacred language feeds the vernacular, making the two continuous. The victim set (sacred tradition violated) would be undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_vs_vernacular_boundary, conceptual, 'Whether the sacred/vernacular distinction holds structurally or collapses under linguistic analysis.').

omega_variable(
    committer_frame_ambiguity,
    'This constraint is one reading of the contested kernel ''hebrew_linguistic_life'' — does the kernel structure admit a single authoritative reading, or is the contest itself constitutive of the kernel?',
    'Analyze whether the three readings (liturgical_preservation, native_generational, marketplace_pidgin) operate in distinct institutional domains with no need for a unified verdict, or whether they compete for the same authoritative space (state recognition, educational curriculum, conversion authority).',
    'If the contest is constitutive, no reading can claim mountain status — each is a coordination claim within its domain. If domains are separate, each reading can be a mountain within its domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the kernel''s contestedness is a bug or a feature of its structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t3000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 3000, 0.12).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t1000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(hebr_be_t3000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 3000, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebr_su_t1000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1000, 0.22).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(hebr_su_t3000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 3000, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the colloquial claim 'Hebrew is a living language' into three structurally distinct constraints with different ε values, beneficiary structures, and victim sets. The liturgical reading claims mountain status with near-zero extraction; the native_generational reading claims scaffold (transition to vernacular) with moderate extraction; the marketplace_pidgin reading claims rope (coordination) with low extraction. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
