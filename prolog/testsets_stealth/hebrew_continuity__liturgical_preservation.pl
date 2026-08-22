% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity through Liturgical Preservation — Ritual Recitation and Textual Transmission
 *   domain: sociolinguistic/religious/commitment-systems
 *
 * SUMMARY:
 *   This story instantiates the liturgical_preservation reading of the
 *   hebrew_continuity kernel: the claim that Hebrew lives through preserved
 *   ritual recitation and canonical textual transmission, with zero native
 *   speakers required. The arrangement under contest is the institutional
 *   apparatus that, after Hebrew ceased to be a natively spoken vernacular
 *   (c. 200-400 CE), kept the language in continuous communal use — mandatory
 *   liturgical recitation in Hebrew, a fixed canon transmitted through
 *   cheder, yeshiva, and academy, and communal discipline (excommunication,
 *   book licensing, educational gatekeeping) directed against vernacularizing
 *   and secularizing alternatives. Epsilon's referent is this standing
 *   preservation arrangement, assessed by the reading's own lights — not the
 *   native-revival arrangement the native_generative sibling would endorse,
 *   and not the diaspora contact medium of bridge_pidginized. The reading's
 *   declared victim set centers on secularizing forces: Maskilic writers and
 *   liturgical reformers, plus the structurally suppressed group the reading
 *   itself under-acknowledges, the women excluded from the Hebrew curriculum.
 *   The three readings are separate constraint files linked through
 *   network.affects_constraints; their epsilon values differ by construction.
 *   KEY AGENTS (by structural relationship): - rabbinic_interpretive_elite:
 *   Agenda-setter and principal beneficiary (institutional / identity_locked)
 *   — administers canon and discipline; the transmission chain constitutes
 *   its authority - diaspora_jewish_communities: Coordinated beneficiary and
 *   payer (organized / constrained) — receive the shared canonical medium;
 *   pay educational labor and communal taxes -
 *   women_excluded_from_text_study: Primary diffuse target (powerless /
 *   trapped) — bear the arrangement's costs without access to its literacy or
 *   standing - secularizing_maskilim: Suppressed challenger (moderate /
 *   constrained) — Hebrew-writing reformers whose medium the establishment
 *   treats as threat - reform_liturgical_reformers: Suppressed challenger
 *   with partial exit (organized / constrained) — vernacular liturgy at
 *   schism cost - karaite_scripturalists: Early schismatic target (organized
 *   / constrained) — scripture-alone alternative isolated by communal
 *   discipline - philological_scholars: Analytical observer (analytical /
 *   analytical) — studies the corpus from outside the communal discipline
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_elite: agenda-setter and principal beneficiary (institutional / identity_locked)
 *   - diaspora_jewish_communities: coordinated beneficiary-payer (organized / constrained)
 *   - women_excluded_from_text_study: primary diffuse target (powerless / trapped)
 *   - secularizing_maskilim: suppressed challenger (moderate / constrained)
 *   - reform_liturgical_reformers: suppressed challenger with partial exit (organized / constrained)
 *   - karaite_scripturalists: early schismatic target (organized / constrained)
 *   - philological_scholars: analytical observer (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity through Liturgical Preservation — Ritual Recitation and Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/religious/commitment-systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'd0bc9f21-d1cd-4baa-9735-c2c21a394c74').
narrative_ontology:cs_kernel_codification('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', fixed_text).
narrative_ontology:cs_authority_grounding('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', lineage).
narrative_ontology:cs_interpretation_layer_present('d0bc9f21-d1cd-4baa-9735-c2c21a394c74').
narrative_ontology:cs_reading_relation('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', foundational, lashon_hakodesh_sanctity_preservation_duty).
narrative_ontology:cs_axiom_status(lashon_hakodesh_sanctity_preservation_duty, holdable).
narrative_ontology:cs_axiom_grounding('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', lashon_hakodesh_sanctity_preservation_duty, theological).
narrative_ontology:cs_axiom('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', foundational, recitation_transmission_constitute_vitality).
narrative_ontology:cs_axiom_status(recitation_transmission_constitute_vitality, holdable).
narrative_ontology:cs_axiom_grounding('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', recitation_transmission_constitute_vitality, conventional).
narrative_ontology:cs_axiom('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', secondary, fixed_recitation_language_hebrew).
narrative_ontology:cs_axiom_status(fixed_recitation_language_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', fixed_recitation_language_hebrew, conventional).
narrative_ontology:cs_reference_frame('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', unbroken_masoretic_transmission_chain).
narrative_ontology:cs_drift_state('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', haskalah_revival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0bc9f21-d1cd-4baa-9735-c2c21a394c74', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, women_excluded_from_text_study).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_maskilim).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, reform_liturgical_reformers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, karaite_scripturalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, lashon_hakodesh_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, unbroken_masorah_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordains its successors, fixes the liturgical canon, licenses books, and rules on what the texts mean. Its standing, livelihood, and marriage networks are constituted by the transmission chain it administers; leaving the chain would dissolve the authority it holds. It maintains the arrangement through communal discipline, excommunication, and control of education and printing.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite, beneficiary).

% Receive a shared liturgy, a shared legal language, and a continuous textual identity across mutually unintelligible vernaculars and host societies. They pay for it in school years for their sons, communal taxes supporting academies and courts, and submission to communal discipline; exit — assimilation or conversion — carried the cost of kinship, standing, and burial rights.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, payer).

% Barred from the Hebrew curriculum and from liturgical roles that require it; their religious life is conducted in the vernacular through translated prayers and devotional literature. They carry the arrangement's labor costs — household support of study — without access to the literacy, standing, or interpretive voice it confers, and they have no exit from the communal role into which they are placed.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, women_excluded_from_text_study, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, women_excluded_from_text_study, excluded).

% Write and publish in Hebrew while arguing for its renewal as a secular literary medium; licensing and communal discipline treat their Hebrew as a threat to the tradition rather than a continuation of it. Their exit — into German, Russian, or the vernacular presses — costs them communal standing and audience.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_maskilim, payer,
    moderate, biographical, constrained, continental).

% Replace Hebrew prayer with the host vernacular, trim the canon's liturgical core, and build separate temples; they pay in excommunication threats, schism costs, and the hostility of the communities they leave. Their partial success marks both the price the discipline can impose and the point beyond which it cannot hold.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, reform_liturgical_reformers, payer,
    organized, generational, constrained, continental).

% Reject the oral-law interpretive layer and build practice on scripture alone with their own Hebrew usage; the rabbinic majority's discipline isolates them from marriage, commerce, and communal standing. They persist for a millennium as a separate community — the earliest large-scale cost of the discipline.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, karaite_scripturalists, payer,
    organized, generational, constrained, continental).

% Study the language and texts from outside the communal discipline — cataloguing manuscripts, reconstructing grammar, collating witnesses. They take no side in the communal disputes and depend on the corpus the arrangement preserved; their work feeds both the tradition's self-understanding and its challengers.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, philological_scholars, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, rabbinic_interpretive_elite).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single canonical language and text-base across diaspora communities whose daily vernaculars (Yiddish, Ladino, Judeo-Arabic, Persian Jewish dialects) are mutually unintelligible; synchronizes liturgy, law, and textual study across geography and generation; provides the boundary marker distinguishing the community from its host societies.
% TRANSFER_FUNCTION: Moves educational labor from households (cheder and yeshiva years for sons, communal taxation for academies and courts) into the transmission institutions, and moves interpretive authority and communal status from the laity at large to the rabbinic elite who control what the texts mean and who may teach them.
% ABSENT_VOICES: Women — excluded from Hebrew literacy and text study, religiously addressed in the vernacular — would contest the gendered division of linguistic labor; they are outside the beit midrash, in the vernacular sphere. Vernacular-preferring laity who recited without comprehension had no seat where the canon and curriculum were set. Secularizing writers were present in the communal conversation but their position was ruled out of order by licensing and discipline rather than argued against on its merits.
% DISAPPEARANCE_RATIONALE: If the preservation apparatus vanished overnight, the diaspora's communities — already vernacularly fragmented — would lose their last shared canonical medium within a generation or two; the rabbinic elite's authority base dissolves with the chain it administers; continuity with the canon becomes a scholarly specialty rather than a lived communal practice; and the eventual native revival would have had no preserved corpus to revive.
% FOUNDING_PROBLEM: After the destruction of the Second Temple and the cessation of Hebrew as a natively spoken vernacular (roughly 70-400 CE), a geographically dispersed people faced the problem of maintaining unbroken continuity with a canonical corpus in a language no child acquired natively — without which law, liturgy, and identity claims would fragment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Haskalah writers who fought the arrangement — Mendelssohn's Bi'ur project presupposed the continuity problem was live and urgent; Christian Hebraists and later academic philology (Wissenschaft des Judentums, Gesenius) attested both the reality of the continuity problem and the achievement of the transmission chain; the native-revival generation explicitly framed its project as completing what preservation had kept merely latent. No corroboration is claimed for the arrangement's specific extraction structure — only for the founding problem's reality and persistence through the interval.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (interval end): the arrangement delivered a real trans-diaspora canonical medium, but interpretive authority concentrated in the rabbinic elite while the communities' educational labor and the laity's comprehension gap grew — extraction rose steadily as the academies' interpretive service and the communities' needs drifted apart. Suppression (0.62) is authored as a raw structural property, unscaled by power or scope — only extractiveness is scaled by directionality and scope in the engine's computation; it reflects herem, licensing, curriculum gatekeeping, and gendered exclusion. Theater stays low (0.24): recitation remained functionally identity-coordinating throughout; the late rise tracks rote pedagogy and pilpul formalism, not a dead function kept alive for show. Accessibility_collapse (0.45) is mid-range: alternatives — Karaite scripturalism, vernacular liturgy, secular Hebrew — remained partly reachable and were taken, at real cost. Resistance (0.55): the arrangement met sustained schism and reform movements across its whole life. The claimed type, tangled_rope, is authored from the structure — a genuine coordination function AND asymmetric extraction operating through the same enforced apparatus — independently of these metric values. All three tracked series run on one shared eight-point grid (400-1880); the final values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently. From the rabbinic elite's position the arrangement is the sacred chain it embodies — its authority, marriage networks, and standing are constituted by the transmission it administers, so the same structure that costs others subsidizes it. From the suppressed seats — women excluded from the curriculum, Maskilic writers, liturgical reformers — the same apparatus operates as enforced extraction with the coordination story doing cover work for the interpretive monopoly. The communities sit between: genuine coordination benefit and real labor cost, with exit priced at kinship, standing, and burial. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The elite is declared beneficiary and sits near the beneficiary end: it collects interpretive authority and status, and its identity-lock means it cannot exit without dissolving itself. The communities are declared beneficiary with a payer secondary role and sit near symmetric: real coordination benefit, real labor cost, constrained exit. Women sit near the full-target end: they bear the arrangement's costs, are excluded from its prestige goods, and are trapped in the role. Maskilim, reformers, and Karaites are declared victims with high directionality: they pay in discipline and schism, and their exits are constrained by communal enforcement. The declarations map to real flows: recitation duty and study labor flow from communities and households into the institutions the elite staffs; meaning and legitimacy flow back through the elite's interpretive monopoly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining continuity with a canonical corpus in a language no child acquired natively — was live for the entire interval, so no mandatrophy is declared. The tangled_rope classification prevents two mislabelings: a pure-coordination reading would erase the arrangement's suppressed victim set (the secularizing forces the reading itself names, plus the gendered exclusion the reading under-acknowledges); a pure-extraction reading would erase the genuine trans-diaspora coordination that no rival arrangement delivered — the corpus preserved here is the corpus the later revival drew on. The R5 mismatch check (founding_problem_status=live x disappearance_verdict=world_rearranges) raises no zombie flag. Coalition note: the diffuse victims — women, vernacular laity — lacked any vehicle for coalition; their resistance ran through movements led by men (Hasidism's vernacular piety, Reform), which is part of why the enforcement held so long.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the hebrew_continuity kernel — the liturgical_preservation reading. How would classification change under the sibling readings, native_generative and bridge_pidginized, which instantiate different constraints from the same kernel?',
    'Comparative analysis across the three sibling story files: each reading authors its own epsilon, beneficiary/victim structure, and type for its own arrangement; divergence in computed types across the family is the measurement of the kernel contest.',
    'Under native_generative, the arrangement under contest becomes the native-speech requirement and the victim set shifts to the preservation apparatus that suppressed native speech; under bridge_pidginized the referent becomes the diaspora contact medium. Epsilon and type are not transferable across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the hebrew_continuity kernel; this file instantiates liturgical_preservation only.').

omega_variable(
    vitality_criterion_disagreement,
    'Where in the structure do the sibling readings disagree? On the criterion of linguistic vitality: whether unbroken recitation and transmission without native speakers constitutes a language living (this reading), whether only native generative competence does (native_generative), or whether contact-medium function does (bridge_pidginized).',
    'No empirical resolution is available inside the kernel — the criterion is a definitional and normative commitment. Resolution would require a conceptual ruling on what ''a language lives'' means, after which the sibling constraints are compared against that ruling.',
    'If the vitality criterion is fixed as native-generative, this reading''s core premise fails and the preservation arrangement reclassifies as the suppressor of a living language, inverting its victim set; if fixed as symbolic continuity, the native_generative reading loses its target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_criterion_disagreement, conceptual, 'The kernel contest is located in the criterion of linguistic vitality, not in any empirical disagreement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Of the measured suppression (0.62 at interval end), how much is structural (excommunication, book licensing, educational gatekeeping, gendered curriculum exclusion) and how much internalized (communal belief that Hebrew prayer is obligatory, fusion of sanctity with the language itself)?',
    'Post-exit trajectory analysis: communities that left the enforcement perimeter (Reform temples, secularized descendants) — if vernacular or secularized practice persisted without penalty, the suppressed component was structural; if communities restored Hebrew liturgy under no compulsion, as many did, a large component was internalized.',
    'If substantially internalized, the arrangement''s effective suppression outlasts its enforcement machinery and persists in weakened form where enforcement collapses; if structural, dismantling enforcement dissolves the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized components of liturgical suppression.').

omega_variable(
    communal_benefit_vs_capture,
    'Were the diaspora communities net beneficiaries of the arrangement (the shared canonical medium worth its labor and discipline costs) or captured payers whose coordination benefit was claimed by the elite?',
    'Comparative study of communities that reduced or exited the arrangement (Karaite communities, Reform Germany, western European secularization) versus those that maintained it: measure what coordination value was lost versus what extraction ceased.',
    'If communities were net beneficiaries, the arrangement''s extraction concentrates in the elite''s interpretive rents and the gendered exclusions; if captured payers, the coordination function was largely cover and the story trends toward the snare end of the tangled-rope range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_benefit_vs_capture, empirical, 'Whether coordinated communities gained more than they paid.').

omega_variable(
    comprehension_reallocation_theater_artifact,
    'Is the late-interval rise in theater_ratio genuine functional atrophy (rote recitation replacing comprehension) or a measurement artifact (comprehension relocating into vernacular glosses, translation pedagogy, and commentary rather than disappearing)?',
    'Pedagogical history: cheder and yeshiva curricula, the spread of Rashi and vernacular glosses, and comprehension indicators in communal records across the 16th-19th centuries.',
    'If artifact, the theater rise overstates decay and the arrangement remained more functional than the series suggests; if genuine, the late interval shows classic Goodhart drift with recitation persisting after its comprehension function thinned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(comprehension_reallocation_theater_artifact, empirical, 'Whether rising theater measures real atrophy or relocated comprehension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 400, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t400, hebrew_continuity__liturgical_preservation, theater_ratio, 400, 0.1).
narrative_ontology:measurement(hebr_tr_t620, hebrew_continuity__liturgical_preservation, theater_ratio, 620, 0.12).
narrative_ontology:measurement(hebr_tr_t840, hebrew_continuity__liturgical_preservation, theater_ratio, 840, 0.14).
narrative_ontology:measurement(hebr_tr_t1060, hebrew_continuity__liturgical_preservation, theater_ratio, 1060, 0.16).
narrative_ontology:measurement(hebr_tr_t1280, hebrew_continuity__liturgical_preservation, theater_ratio, 1280, 0.18).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_continuity__liturgical_preservation, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(hebr_tr_t1720, hebrew_continuity__liturgical_preservation, theater_ratio, 1720, 0.23).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_continuity__liturgical_preservation, theater_ratio, 1880, 0.24).

% Extraction over time
narrative_ontology:measurement(hebr_be_t400, hebrew_continuity__liturgical_preservation, base_extractiveness, 400, 0.3).
narrative_ontology:measurement(hebr_be_t620, hebrew_continuity__liturgical_preservation, base_extractiveness, 620, 0.33).
narrative_ontology:measurement(hebr_be_t840, hebrew_continuity__liturgical_preservation, base_extractiveness, 840, 0.37).
narrative_ontology:measurement(hebr_be_t1060, hebrew_continuity__liturgical_preservation, base_extractiveness, 1060, 0.41).
narrative_ontology:measurement(hebr_be_t1280, hebrew_continuity__liturgical_preservation, base_extractiveness, 1280, 0.45).
narrative_ontology:measurement(hebr_be_t1500, hebrew_continuity__liturgical_preservation, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(hebr_be_t1720, hebrew_continuity__liturgical_preservation, base_extractiveness, 1720, 0.55).
narrative_ontology:measurement(hebr_be_t1880, hebrew_continuity__liturgical_preservation, base_extractiveness, 1880, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t400, hebrew_continuity__liturgical_preservation, suppression_requirement, 400, 0.35).
narrative_ontology:measurement(hebr_su_t620, hebrew_continuity__liturgical_preservation, suppression_requirement, 620, 0.38).
narrative_ontology:measurement(hebr_su_t840, hebrew_continuity__liturgical_preservation, suppression_requirement, 840, 0.42).
narrative_ontology:measurement(hebr_su_t1060, hebrew_continuity__liturgical_preservation, suppression_requirement, 1060, 0.46).
narrative_ontology:measurement(hebr_su_t1280, hebrew_continuity__liturgical_preservation, suppression_requirement, 1280, 0.5).
narrative_ontology:measurement(hebr_su_t1500, hebrew_continuity__liturgical_preservation, suppression_requirement, 1500, 0.56).
narrative_ontology:measurement(hebr_su_t1720, hebrew_continuity__liturgical_preservation, suppression_requirement, 1720, 0.6).
narrative_ontology:measurement(hebr_su_t1880, hebrew_continuity__liturgical_preservation, suppression_requirement, 1880, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The natural-language claim 'Hebrew is a living language' decomposes into three structurally distinct constraints — one per reading of the hebrew_continuity kernel — because the epsilon of the claim changes with the criterion of vitality used to assess it (epsilon-invariance). This file carries the liturgical_preservation reading (epsilon authored for the preservation arrangement; victims = secularizing forces and the gender-excluded). hebrew_continuity__native_generative carries the revival reading (referent: the native-speech arrangement; the preservation apparatus appears there as suppressor). hebrew_continuity__bridge_pidginized carries the contact-medium reading. The upstream reading in the family is this one (highest empirical confidence: the transmission chain is the best-attested historical fact in the set) and it influences the downstream readings — the revival drew on the preserved corpus, and the contact medium rode the diaspora network the liturgy maintained. Family links run through every member's network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
