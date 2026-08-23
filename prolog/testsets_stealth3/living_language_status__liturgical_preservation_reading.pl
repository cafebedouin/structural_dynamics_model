% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Liturgical-Preservation Criterion for Language Vitality (Rabbinic Reading)
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: the claim that
 *   a language is living if its sacred texts are continuously recited,
 *   studied, and used in ritual — that preservation through liturgical
 *   transmission suffices. The referent arrangement is the standing
 *   liturgical-preservation order: a custodial seat administers recitation,
 *   study, and interpretation of a fixed sacred corpus for a dispersed
 *   population with no shared vernacular, and communal education, liturgy,
 *   and identity boundaries are organized around the sufficiency of that
 *   transmission. The anchor case is Hebrew between the Haskalah and the
 *   consolidation of spoken revival Hebrew, the period the measurement grid
 *   spans. Per the kernel-reading epsilon rule, epsilon is authored for this
 *   standing arrangement as the reading itself assesses it: the reading's
 *   lights register the transmission function as primary and genuine (it
 *   demonstrably maintained textual and communal continuity across a
 *   territory-less, vernacular-less diaspora), and register the subordination
 *   of secular usage openly — the desecration framing is published doctrine,
 *   not hidden machinery — which bounds rather than conceals the extraction.
 *   Claimed type and metrics are independently authored: the claimed type is
 *   tangled_rope; the metrics are low-moderate as descriptively believed,
 *   with the manifest's low epsilon bin refined to 0.40 on the strength of
 *   the named victim structure and the enforcement ratchet in the temporal
 *   record. Sibling readings of the same kernel are separate constraints,
 *   linked through network edges; see kernel_context and the committer omegas
 *   for the contest structure, which is deliberately not adjudicated inside
 *   this story.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: agenda-setter and primary beneficiary (institutional / identity_locked) — administers liturgy, curricula, and interpretation; custodial standing accrues to this seat
 *   - traditionalist_laity: coordinated population, beneficiary with diffuse costs (organized / identity_locked) — recites, studies, transmits; receives continuity and cohesion, bears the bounded vernacular cost
 *   - secular_speech_community: primary payer (moderate / constrained) — writers, revivalists, and secular users whose usage confers no vitality under the criterion and is framed as desecration
 *   - secularized_diaspora_jews: excluded voice (moderate / mobile) — neither recite nor study; would object that an unspoken language is declared alive; exit by departure rather than contest
 *   - sociolinguistic_researchers: analytical observer (analytical / analytical) — tracks the gap between the technical measures of vitality and the criterion's normative redefinition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.4).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.45).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical-Preservation Criterion for Language Vitality (Rabbinic Reading)").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '88cd3516-f436-4301-a679-08e9d463dd5b').
narrative_ontology:cs_kernel_codification('88cd3516-f436-4301-a679-08e9d463dd5b', formalized).
narrative_ontology:cs_authority_grounding('88cd3516-f436-4301-a679-08e9d463dd5b', lineage).
narrative_ontology:cs_interpretation_layer_present('88cd3516-f436-4301-a679-08e9d463dd5b').
narrative_ontology:cs_reading_relation('88cd3516-f436-4301-a679-08e9d463dd5b', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('88cd3516-f436-4301-a679-08e9d463dd5b', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('88cd3516-f436-4301-a679-08e9d463dd5b', foundational, liturgical_transmission_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('88cd3516-f436-4301-a679-08e9d463dd5b', liturgical_transmission_suffices_for_vitality, theological).
narrative_ontology:cs_axiom('88cd3516-f436-4301-a679-08e9d463dd5b', secondary, interpretive_authority_follows_custodial_transmission).
narrative_ontology:cs_axiom_status(interpretive_authority_follows_custodial_transmission, holdable).
narrative_ontology:cs_axiom_grounding('88cd3516-f436-4301-a679-08e9d463dd5b', interpretive_authority_follows_custodial_transmission, conventional).
narrative_ontology:cs_reference_frame('88cd3516-f436-4301-a679-08e9d463dd5b', custodial_liturgical_vitality).
narrative_ontology:cs_drift_state('88cd3516-f436-4301-a679-08e9d463dd5b', post_revival_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88cd3516-f436-4301-a679-08e9d463dd5b', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, traditionalist_laity).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, traditionalist_laity).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sanctity_of_lashon_hakodesh).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, diaspora_textual_continuity_without_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the criterion through control of liturgy, curricula, and interpretation: decides which texts are recited, how they are studied, and what usage of the language counts as legitimate. Custodial standing accrues to this seat — the language's claimed life runs through its courts, schools, and prayer halls, and interpretive questions route to its authorities. The criterion is constitutive of the seat's own role: conceding that liturgical transmission does not suffice would dissolve the basis of its authority, so exit from the arrangement is unavailable without self-dissolution.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Recites, studies, and transmits the fixed corpus across generations, educating children into recitation and text study before any vernacular. Receives continuity, communal cohesion, and a shared identity from the practice. Bears a bounded cost: the repertoire acquired is liturgical rather than conversational, and daily life runs in other languages. Participation is not experienced as optional — leaving the practice would mean leaving the community.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, traditionalist_laity, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, traditionalist_laity, payer).

% Uses the language for newspapers, literature, teaching, and — in the revival's later phase — daily speech. Under the criterion this usage confers no vitality: the language was never dead, so there is nothing to revive, and everyday use of the holy tongue is framed in traditionalist rulings as desecration. Builds parallel institutions — secular schools, presses, academies — but cannot escape the delegitimation wherever traditionalist authority reaches, and its claim is bound to this particular language, so exit would mean abandoning the project itself.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, generational, constrained, regional).

% Neither recite nor study the corpus and do not use the language; for them it is a heritage code encountered at life-cycle events. Would object that a language most of the community cannot converse in is being declared alive, but stands outside the conversation the criterion governs. The characteristic response has been departure rather than contest — exit from the traditionalist sphere rather than argument within it.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secularized_diaspora_jews, excluded,
    moderate, biographical, mobile, global).

% Studies language vitality comparatively and notes that the technical measures of a living language — native-speaker transmission, productive registers, acquisition by children — do not track liturgical use; by technical measures a recited-but-unspoken language is maintained rather than living. Takes no side in the communal contest; the seat's work is analysis of what the word 'living' is doing in each rival criterion.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguistic_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a dispersed population with no shared territory or vernacular a common corpus, a common practice for transmitting it, and a single interpretive authority for questions about it: recitation and study solve the continuity problem once, centrally, instead of leaving each community to reconstruct the language's textual life on its own.
% TRANSFER_FUNCTION: Moves interpretive authority and linguistic legitimacy to the custodial seat — whoever controls recitation and interpretation controls what the language's life is claimed to be — and moves standing away from secular users, whose usage confers no vitality under the criterion and is classed as desecration when it touches the holy tongue.
% ABSENT_VOICES: Holders of the rival criteria are answered polemically rather than seated — the native-generation and literary-continuity positions appear in this arrangement's record as objects of ruling, not as parties. Secularized community members who neither recite nor study are outside the conversation entirely. Within traditionalist communities, those historically excluded from text study encounter the criterion through recitation alone and had no seat in its administration.
% DISAPPEARANCE_RATIONALE: Rabbinical institutions would lose the definitional ground of their custodial role: the language's claimed life would no longer run through their courts and schools, and interpretive questions would route elsewhere. The secular speech community's project would lose the traditionalist claim it defines itself against — 'revival' presupposes the counter-claim that the language was never dead. Curricula, liturgy, and communal boundaries are organized around the sufficiency claim; overnight removal would not end the practice of recitation, but the contest over what the language's life is would rearrange around the remaining criteria.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's mother tongue (roughly the second-third century CE), a dispersed community with no shared spoken language faced the problem of how its sacred texts and its linguistic continuity would survive at all — who would maintain the corpus, teach it, and keep it in use across generations with no territory and no vernacular. The liturgical-preservation criterion is the traditional answer: recitation, study, and ritual use are the language's life support, and their sufficiency is the claim that this maintenance is enough.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociolinguistics corroborates the founding problem's reality and the arrangement's effectiveness from outside the benefiting parties: the post-vernacular diaspora did face a continuity problem, and liturgical transmission demonstrably kept the corpus in continuous use for some seventeen centuries. Holders of the rival readings — outside the beneficiary set — attest the status is transformed: the native-generation position holds the preservation problem obsolete now that the language has native speakers, and the literary-continuity position holds it answered by modern print. No party outside the traditionalist beneficiary set attests that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.40: the criterion transfers interpretive authority and linguistic legitimacy to the custodial seat and subordinates the secular speech community's project — their usage confers no vitality, and their everyday use of the holy tongue is framed as desecration. That is real extraction of status and authority, but no material seizure, and it rides on a coordination function that is load-bearing: liturgical transmission demonstrably maintained textual and communal continuity across a diaspora with no territory and no vernacular for some seventeen centuries. That genuine function bounds epsilon at low-moderate rather than high. Suppression 0.45 (a structural property, unscaled by power or scope in the engine's computation): enforcement is real — published rulings against secular usage, curricular exclusion of rival criteria, social sanction inside traditionalist institutions, and an enforcement ratchet visible in the suppression_requirement series as the rivals gained institutional power — but the rival readings were never physically suppressed; they built parallel institutions and stayed live, so suppression is boundary-policing rather than closure. Theater_ratio 0.18: recitation and study are functional within the arrangement's own terms — they ARE the transmission — and performative defense rises only under contest, as the series shows: polemics re-assert in words what practice already embodied. Accessibility_collapse 0.30: understanding the criterion does not collapse alternatives — the rival criteria remain fully articulable and institutionally pursued; alternatives close only inside traditionalist institutions. Resistance 0.60: organized rival positions plus a practical revival movement constitute sustained, institutionalized resistance across the whole interval. Claimed type tangled_rope: the arrangement coordinates a real collective-action problem AND concentrates its benefits (custodial authority) while distributing its costs (delegitimation) onto a constituency excluded from the coordination's governance, and it is actively defended against rival readings. Receipt surface: the arrangement's gains demonstrably accrue to the rabbinical_authority seat — custodial authority concentrates with every ruling — so gain_flow names that seat rather than 'diffuse'; fixing_cost is prohibitive because the seat that could revise the criterion is identity-constituted by it, so abandoning the sufficiency claim dissolves the custodial role itself, a cost exceeding any benefit of settlement with the rival constituency. All three metric series run on one shared six-point grid spanning roughly 1790-1910 on the anchored reading; the suppression series is authored because the story specifically tracks an enforcement ratchet, not incidental variation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the custodial seats should compute different types from the same structural data. From rabbinical_authority's position the arrangement is custodianship it performs and continuity it delivered — the criterion is not contingent but constitutive, and its identity_locked exit means no vantage point exists inside that seat from which the arrangement could be otherwise. From traditionalist_laity the arrangement is both inheritance received and inheritance owed to children — net benefit with a cost (the bounded repertoire) that the criterion itself renders invisible by defining the liturgical repertoire as the language's life; the identity-lock here is a fusion of self-concept and communal membership, so the cost is not experienced as imposed. From secular_speech_community the same structure operates as subordination: a definitional rule that converts their practice into desecration and their project into a category error. secularized_diaspora_jews, mobile and outside the conversation, register the arrangement mostly as an artifact they have already left. The engine computes this divergence from power, exit, and role; the divergence is sharpest where exit is identity_locked on one side and constrained on the other — neither seat can adopt the other's ledger.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (rabbinical_authority, traditionalist_laity) derive low directionality for those seats — the arrangement subsidizes them; the victim declaration (secular_speech_community) derives high directionality — the arrangement extracts from them, amplified by their constrained exit (they cannot leave the language without abandoning their project) and by the diaspora scale at which vitality claims are verified (hard to check, modestly amplified at the payer seat). traditionalist_laity's dual position (beneficiary with secondary payer costs) sits near symmetric: continuity received, repertoire bounded. secularized_diaspora_jews carry no beneficiary or victim declaration; their derived directionality falls to the power-atom fallback near symmetric with low stakes, which matches their situation — the arrangement costs them little because they have already exited. No directionality overrides are authored: the derivation from the structural declarations produces the right profile, and the two moderate-power seats are correctly differentiated by their presence in (or absence from) the victim declaration rather than by any override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabelings. Reading the arrangement as pure extraction ignores that its coordination function is real, prior, and demonstrably effective — the continuity problem it solved was genuine, and even the rival positions concede the transmission happened (they dispute what it signifies). Reading it as pure coordination ignores that the same structure concentrates custodial authority in one seat and converts a rival constituency's practice into desecration — the coordination and the subordination run through the same criterion, which is the tangled-rope signature. On mandatrophy: the founding problem (maintaining textual and communal continuity without territory or vernacular) remains live for the traditionalist constituency and is superseded for the revived-vernacular constituency — hence founding_problem_status contested rather than dead, and no mandatrophy resolution is declared. The mismatch consumer should note the coherent pairing: a live-for-some, superseded-for-others mandate with a world_rearranges disappearance verdict — not a dead-mandate zombie profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading — liturgical_preservation_reading — of the living_language_status kernel. What structurally changes if a sibling reading prevails, and where exactly is the disagreement located?',
    'The siblings are separate constraints (native_generation_reading, literary_continuity_reading) with their own epsilon and stakeholder structures; adoption of a sibling reassigns the seats this reading fixes — under the native-generation criterion the custodial seat loses standing and the secular speech community becomes the language''s legitimate heir; under the literary-continuity criterion the producers of new work gain standing. The disagreement is located in the vitality criterion itself: whether ''living'' is constituted by transmission practice (this reading), by native speakerhood, or by productive use.',
    'Classification of this constraint is stable under sibling adoption because each reading is its own epsilon-invariant constraint; the corpus-level verdict on the kernel shifts instead. If the readings are ultimately judged equivocal on ''living'' (two senses, one word), the family decomposes further and the foreclosure relation to the native-generation reading weakens to coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the living-language kernel; the contest''s locus is the vitality criterion itself.').

omega_variable(
    vitality_term_register_ambiguity,
    'Is the three-way contest a dispute about one concept (does liturgical transmission keep a language alive?) or about different concepts sharing a word (a religious register in which the holy tongue''s life is its ritual use, versus a sociolinguistic register in which vitality is native-speaker transmission and productivity)?',
    'Test whether any party would accept the rival criterion as a partial measure of the SAME property: if the traditionalist position concedes that its ''living'' is a distinct sense rather than the true one, the dispute is terminological and the family decomposes; if each party claims the single true sense and rules on the same cases (Hebrew circa 1800), it is one substantive contest.',
    'If terminological, the readings are homonyms rather than rivals — the secular speech community cannot be excluded from a contest it was never entered in, and the costs attributed to this criterion shrink toward the sanctity regime''s independent operation. If substantive, the current profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_term_register_ambiguity, conceptual, 'Whether ''living language'' is one contested concept or two registers sharing a word.').

omega_variable(
    custodianship_service_vs_rent,
    'How much of the custodial seat''s interpretive authority is the price of a service the community could not otherwise obtain — transmission, arbitration, education — versus authority collected from controlling the criterion itself?',
    'Compare communities and periods with competing interpretive authorities (multiple courts, movements, or academies interpreting the same corpus): if custodial perquisites persist where competition exists, the service component dominates; if authority and its perquisites concentrate precisely where the criterion is monopolized, the collected component dominates.',
    'A dominant service component supports the coordination-dominant reading and holds epsilon at the low-moderate level authored here; a dominant collected component pushes the payer seat''s classification toward the extractive end and would justify revising epsilon upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodianship_service_vs_rent, empirical, 'Service-versus-rent composition of the custodial seat''s authority.').

omega_variable(
    desecration_framing_scope,
    'Is the framing of secular usage as desecration an enforcement mechanism of this criterion — deployed and intensified to defend liturgical sufficiency against the revival — or an independent sanctity doctrine whose application to language predates the vitality contest and would persist without it?',
    'Trace the rulings historically: if rulings against secular usage intensify precisely when the revival challenges the criterion and relax where it does not, the framing is enforcement; if it holds constant across contexts and applies to usage patterns unrelated to any vitality claim, it is independent doctrine belonging to a separate constraint.',
    'If enforcement, the authored suppression understates the active machinery at the contest''s peak and the tangled_rope profile firms up. If independent doctrine, part of the measured extraction belongs to the sanctity regime and should be decomposed out of this story per the epsilon-invariance rule, lowering epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desecration_framing_scope, empirical, 'Whether the desecration framing is this criterion''s enforcement machinery or a prior, independent sanctity doctrine.').

omega_variable(
    own_lights_extraction_visibility,
    'The reading''s own lights treat custodianship as service and the subordination of secular usage as published, legitimate doctrine rather than hidden machinery; does assessing the standing arrangement by those lights understate the costs the secular speech community actually bears?',
    'Seat-resolved ledger comparison: measure the arrangement''s costs from the payer seat (delegitimation, the foreclosed linguistic project, desecration exposure) against the beneficiary seats'' own accounting of the same arrangement; divergence between the ledgers indicates costs the reading''s framework normalizes into invisibility.',
    'If the payer-seat ledger runs substantially higher than the own-lights ledger, epsilon should be revised upward and the payer seat''s computed classification moves toward the extractive end; if the ledgers converge, the authored low-moderate epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(own_lights_extraction_visibility, conceptual, 'Whether the reading''s own-lights assessment normalizes costs its framework openly imposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t24, living_language_status__liturgical_preservation_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(livi_tr_t24, observed).
narrative_ontology:measurement(livi_tr_t48, living_language_status__liturgical_preservation_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement_basis(livi_tr_t48, observed).
narrative_ontology:measurement(livi_tr_t72, living_language_status__liturgical_preservation_reading, theater_ratio, 72, 0.15).
narrative_ontology:measurement_basis(livi_tr_t72, observed).
narrative_ontology:measurement(livi_tr_t96, living_language_status__liturgical_preservation_reading, theater_ratio, 96, 0.16).
narrative_ontology:measurement_basis(livi_tr_t96, observed).
narrative_ontology:measurement(livi_tr_t120, living_language_status__liturgical_preservation_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement_basis(livi_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t24, living_language_status__liturgical_preservation_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(livi_be_t24, observed).
narrative_ontology:measurement(livi_be_t48, living_language_status__liturgical_preservation_reading, base_extractiveness, 48, 0.34).
narrative_ontology:measurement_basis(livi_be_t48, observed).
narrative_ontology:measurement(livi_be_t72, living_language_status__liturgical_preservation_reading, base_extractiveness, 72, 0.36).
narrative_ontology:measurement_basis(livi_be_t72, observed).
narrative_ontology:measurement(livi_be_t96, living_language_status__liturgical_preservation_reading, base_extractiveness, 96, 0.38).
narrative_ontology:measurement_basis(livi_be_t96, observed).
narrative_ontology:measurement(livi_be_t120, living_language_status__liturgical_preservation_reading, base_extractiveness, 120, 0.4).
narrative_ontology:measurement_basis(livi_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t24, living_language_status__liturgical_preservation_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement_basis(livi_su_t24, observed).
narrative_ontology:measurement(livi_su_t48, living_language_status__liturgical_preservation_reading, suppression_requirement, 48, 0.33).
narrative_ontology:measurement_basis(livi_su_t48, observed).
narrative_ontology:measurement(livi_su_t72, living_language_status__liturgical_preservation_reading, suppression_requirement, 72, 0.38).
narrative_ontology:measurement_basis(livi_su_t72, observed).
narrative_ontology:measurement(livi_su_t96, living_language_status__liturgical_preservation_reading, suppression_requirement, 96, 0.42).
narrative_ontology:measurement_basis(livi_su_t96, observed).
narrative_ontology:measurement(livi_su_t120, living_language_status__liturgical_preservation_reading, suppression_requirement, 120, 0.45).
narrative_ontology:measurement_basis(livi_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: living_language_status decomposes into three readings because the colloquial question 'is the language alive?' conflates three structurally distinct criteria, each with its own stable epsilon. This story authors epsilon for the standing liturgical-preservation arrangement as the liturgical reading assesses it (low-moderate, coordination-dominant). native_generation_reading evaluates the same historical arrangement under the native-speaker criterion, where liturgical maintenance reads as preservation of a corpse — its beneficiary/victim structure and epsilon will differ accordingly. literary_continuity_reading authors epsilon around productive literary use. This reading is upstream: the siblings define themselves against its criterion, and its enforcement (desecration rulings) is the pressure that shaped their emergence. Edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
