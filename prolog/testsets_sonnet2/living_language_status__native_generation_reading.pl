% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native Intergenerational Transmission as the Sole Criterion of Linguistic Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint is the native-generation reading of the contested kernel
 *   'living language status.' It holds that a language is genuinely alive
 *   only when native speakers acquire it as a mother tongue and use it in
 *   ordinary daily life — and that continued liturgical recitation, however
 *   unbroken, preserves a linguistic corpse rather than sustaining vitality.
 *   Historically this reading was mobilized by secular nationalist revival
 *   movements (the Hebrew revival is the paradigm case) to establish that
 *   their project — engineering a cradle-tongue population where none existed
 *   for centuries — was a genuine restoration rather than an artificial
 *   imposition, and correspondingly to devalue the millennia of liturgical
 *   and literary use that had kept the language present without native
 *   speakers. The reading requires real institutional infrastructure
 *   (schools, broadcasting, curricula) to make native transmission actually
 *   happen, which is why ε sits at a moderate level: this is not free-riding
 *   extraction, there is a genuine coordination achievement in raising a
 *   generation of native speakers, but that achievement is used to
 *   delegitimize rival modes of linguistic continuity that persist
 *   independently of it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.52).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.58).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native Intergenerational Transmission as the Sole Criterion of Linguistic Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '49bd1e49-8b1c-49bc-bffd-546b83bde4cb').
narrative_ontology:cs_kernel_codification('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', distributed).
narrative_ontology:cs_authority_grounding('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', distributed).
narrative_ontology:cs_reading_relation('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', foundational, daily_vernacular_use_is_necessary_for_vitality).
narrative_ontology:cs_axiom_status(daily_vernacular_use_is_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', daily_vernacular_use_is_necessary_for_vitality, conventional).
narrative_ontology:cs_axiom('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', foundational, ritual_only_use_constitutes_linguistic_death).
narrative_ontology:cs_axiom_status(ritual_only_use_constitutes_linguistic_death, holdable).
narrative_ontology:cs_axiom_grounding('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', ritual_only_use_constitutes_linguistic_death, empirically_contingent).
narrative_ontology:cs_reference_frame('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', vernacular_cradle_tongue_as_vitality_standard).
narrative_ontology:cs_drift_state('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', contemporary_heritage_language_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49bd1e49-8b1c-49bc-bffd-546b83bde4cb', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_revival_movement).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_language_academies).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, native_speaking_new_generation).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_ritual_custodians).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, literary_continuity_scholars).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, vernacularization_as_national_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built schools, media, and civil institutions to make the revived tongue a cradle language again, and actively campaigns for the native-transmission definition because it is the only standard under which their project — and their claim to speak for the nation — counts as success. They administer school curricula and public broadcasting that enforce the vernacular standard and marginalize liturgical registers as archaic.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_revival_movement, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_revival_movement, beneficiary).

% Certify curricula, publish the standardized grammar, and control what counts as 'proper' native usage. Their institutional budget and prestige depend on the native-transmission definition remaining the official metric of the language's survival; a liturgical-sufficiency standard would dissolve their gatekeeping function.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_academies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, state_language_academies, agenda_setter).

% Children raised with the revived language as a mother tongue gain full social, economic, and cultural standing in the national project — schooling, employment, and belonging are organized around their fluency. They did not choose the definitional fight but inherit its benefits.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, native_speaking_new_generation, beneficiary,
    moderate, biographical, mobile, national).

% Maintain the language through prayer, textual study, and ritual recitation across diaspora communities, often for centuries, without native cradle transmission. Under this reading, their entire mode of custodianship is redefined as tending a corpse rather than sustaining a living tongue — their religious and cultural authority over the language is delegitimized even though nothing about their practice changed. Exit from the identity that grounds this practice is not meaningfully available; leaving liturgical life means leaving a religious community, not switching languages.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    powerless, civilizational, identity_locked, global).

% Cantors, liturgical scholars, and community elders whose social status derives from mastery of the language-in-worship. The native-generation standard strips their expertise of the 'vitality' credential and redirects prestige, funding, and cultural authority toward the nation-state's vernacular institutions, without any change in their own practice or competence.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_ritual_custodians, payer,
    powerless, generational, trapped, global).

% Point to periods where the language flourished as a medium for new literature and thought without a native-speaking population — under this reading, that entire mode of vitality is dismissed as insufficient, and their scholarly framework is treated as a footnote to the 'real' revival rather than an alternative account of the same evidence.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, literary_continuity_scholars, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, literary_continuity_scholars, excluded).

% Study language vitality across many cases and note that the native-transmission criterion is one defensible operationalization among several, each tracking a different real phenomenon (intergenerational fluency vs. textual productivity vs. ritual continuity) rather than a single fact about whether 'the language is alive.'
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_revival_movement).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, institutionally verifiable standard for what counts as a living national language, allowing schools, courts, media, and census-takers to coordinate around one operational definition instead of adjudicating vitality case by case.
% TRANSFER_FUNCTION: Moves cultural legitimacy, state resources, curricular authority, and the socially recognized status of 'authentic heir to the language' from liturgical and diaspora custodians to the secular nationalist revival movement and the state institutions it built.
% ABSENT_VOICES: Liturgical-only communities and diaspora ritual custodians rarely sit on the language academies or curriculum boards that adjudicate the standard; their objection — that continuous sacred use is itself a form of transmission — is treated as sentimental rather than as a competing definition with its own coherence.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion were dropped as the sole test of vitality, liturgical and literary continuity would count as sufficient evidence of a living language on their own terms; funding, prestige, and legitimacy currently channeled exclusively to native-transmission institutions would have to be shared or reallocated, and the nationalist movement's exclusive claim to have 'revived' the language would lose its strongest evidentiary basis.
% FOUNDING_PROBLEM: In the absence of a native-speaking population, revivalists needed a criterion that would count their project as a genuine restoration of the nation's language rather than an artificial neologism — and needed it to disqualify rival claims (liturgical sufficiency, literary continuity) that would have made revival unnecessary or already accomplished.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguists working outside both the nationalist and religious institutions (comparative language-vitality researchers) attest that intergenerational native transmission is one legitimate operationalization of vitality but dispute that it is the only one; UNESCO's own endangerment framework treats multiple transmission modes as evidence, which the strict native-generation standard does not fully track. No liturgical or diaspora authority is on record accepting the corpse framing of their own practice.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint's coordination function — building the institutional apparatus of native transmission — is real and costly, not free-riding; but its classificatory move (declaring liturgical and literary continuity insufficient) transfers legitimacy and resources away from communities that did nothing to lose that legitimacy on their own practice's terms. Suppression (0.58) is substantial because the standard is embedded in state curricula, census categories, and academic classification systems that actively deny the 'living language' label to liturgical-only communities, not merely a passive difference of opinion. Theater ratio stays low-moderate (0.22) because the revival's core function — actually producing native speakers — is genuinely performed, not merely staged; the rising trend across the interval reflects growing self-congratulatory institutional messaging as the revival matures and becomes an object of national pride rather than urgent necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist revival movement and the state academies it built are structural beneficiaries: the native-generation standard is precisely the metric by which their historical project counts as successful, so they have organized institutional power to defend it. The new native-speaking generation benefits incidentally, inheriting full social standing without having fought the definitional battle. Liturgical-only communities and diaspora ritual custodians are structural targets: their mode of language maintenance is declared insufficient by a standard they had no hand in setting and cannot meet without abandoning what makes their practice liturgical in the first place — hence identity_locked rather than merely constrained exit. Literary continuity scholars sit as targets of a milder form, their evidentiary framework marginalized rather than their community harmed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing that a state-engineered native-speaking population constitutes genuine linguistic restoration, not artifice — is still partly live (revival languages remain more fragile than millennia-old vernaculars) but has also long since been achieved for the core case that motivated the standard. Continuing to apply the standard as a universal test of vitality for all languages, rather than as the specific historical argument it was built to win, is where the mandatrophy risk lies: the criterion outlives its founding argument and becomes a general tool for delegitimizing any non-native-transmission mode of linguistic survival, including ones that never needed to win the argument the standard was built to settle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_pluralism,
    'Is native intergenerational transmission the uniquely correct test of linguistic vitality, or one of several structurally distinct phenomena (native fluency, literary productivity, ritual continuity) that the single word ''living'' conflates?',
    'Comparative sociolinguistic analysis across many cases (compare this kernel''s three sibling readings'' predictions against documented outcomes for languages with each transmission profile) to determine whether a single criterion tracks what practitioners and speakers actually mean by ''alive.''',
    'If vitality is genuinely plural, imposing the native-generation criterion as sole arbiter is a category error that structurally advantages nationalist revival projects over liturgical and literary continuity claims regardless of the latter''s own coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_pluralism, conceptual, 'Whether ''living language'' names one phenomenon or several conflated ones — the root ambiguity behind the kernel''s three readings.').

omega_variable(
    reading_as_committer_choice,
    'This constraint instantiates the native_generation_reading of the living_language_status kernel. The liturgical_preservation_reading and literary_continuity_reading are separate constraints with different beneficiary/victim structures and different epsilon values. Which reading a given classification exercise selects is itself a normative choice, not a neutral empirical finding.',
    'No empirical resolution mechanism exists at the kernel level; each reading is internally coherent on its own terms. Resolution would require an explicit meta-level argument for why one operationalization of ''vitality'' should govern policy, funding, or census classification over the others.',
    'Treating this reading as the sole correct account of linguistic vitality (rather than one of three live readings) is precisely the mechanism by which the secular nationalist movement''s legitimacy claim gets naturalized as fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_committer_choice, conceptual, 'Documents that this story is one committer reading of a three-way contested kernel; the other two readings are separate constraint files linked via network.affects_constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(livi_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(livi_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(livi_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(livi_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(livi_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(livi_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(livi_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(livi_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(livi_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(livi_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(livi_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the living_language_status kernel. native_generation_reading, liturgical_preservation_reading, and literary_continuity_reading share the same underlying contested object (whether a given language counts as 'living') but authored as three separate constraints with distinct beneficiary/victim structures and distinct epsilon values, per the ε-invariance principle. This reading's epsilon (0.52, moderate) reflects genuine institutional coordination cost; the liturgical reading's epsilon is expected to be lower (preservation requires less infrastructure and produces less asymmetric extraction); the literary continuity reading's epsilon sits between, reflecting extraction against native-nationalist claims of exclusive legitimacy rather than against liturgical communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
