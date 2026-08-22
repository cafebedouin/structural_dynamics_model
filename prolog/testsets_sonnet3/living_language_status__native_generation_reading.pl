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
 *   human_readable: Native-Generational-Transmission Standard of Linguistic Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'living_language_status': the native-generation reading, which holds that
 *   a language is genuinely living only when native speakers transmit it as a
 *   mother tongue in ordinary daily life across generations, and that
 *   liturgical recitation without native transmission preserves a corpse
 *   rather than sustaining vitality. This reading was central to secular
 *   nationalist revival projects that treated manufactured cradle-to-grave
 *   native speech as the proof-condition for linguistic (and by extension
 *   political) sovereignty, displacing liturgical and diaspora continuity as
 *   insufficient. The standing arrangement under contest is the institutional
 *   apparatus (schools, academies, certification bodies) built around this
 *   criterion, and ε is authored for that arrangement as this reading's own
 *   lights assess it — not for whatever arrangement the reading would ideally
 *   put in place.
 *
 * KEY AGENTS:
 *   - secular_nationalist_revival_movement: primary agenda-setter and beneficiary (organized/arbitrage) — defines and enforces the vitality criterion
 *   - state_language_academies: institutional beneficiary (institutional/arbitrage) — administers certification and funding around the standard
 *   - liturgical_only_communities: primary target (moderate/constrained) — their mode of continuity is redescribed as death rather than life
 *   - diaspora_heritage_speakers: secondary target (powerless/trapped) — excluded from 'living speaker' status despite functional attachment
 *   - national_school_children: dual beneficiary/payer (powerless/constrained) — bear the manufactured-transmission project directly
 *   - linguistics_researchers: analytical observer — assesses whether the criterion tracks genuine vitality or legitimating function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.52).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.58).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generational-Transmission Standard of Linguistic Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '683a0bf3-747f-4d17-9151-bc8852a7205a').
narrative_ontology:cs_kernel_codification('683a0bf3-747f-4d17-9151-bc8852a7205a', distributed).
narrative_ontology:cs_authority_grounding('683a0bf3-747f-4d17-9151-bc8852a7205a', distributed).
narrative_ontology:cs_reading_relation('683a0bf3-747f-4d17-9151-bc8852a7205a', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('683a0bf3-747f-4d17-9151-bc8852a7205a', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('683a0bf3-747f-4d17-9151-bc8852a7205a', foundational, native_daily_transmission_is_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_daily_transmission_is_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('683a0bf3-747f-4d17-9151-bc8852a7205a', native_daily_transmission_is_necessary_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('683a0bf3-747f-4d17-9151-bc8852a7205a', foundational, liturgical_only_use_constitutes_linguistic_death).
narrative_ontology:cs_axiom_status(liturgical_only_use_constitutes_linguistic_death, holdable).
narrative_ontology:cs_axiom_grounding('683a0bf3-747f-4d17-9151-bc8852a7205a', liturgical_only_use_constitutes_linguistic_death, conventional).
narrative_ontology:cs_reference_frame('683a0bf3-747f-4d17-9151-bc8852a7205a', pre_revival_diaspora_liturgical_continuum).
narrative_ontology:cs_drift_state('683a0bf3-747f-4d17-9151-bc8852a7205a', contemporary_multilingual_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('683a0bf3-747f-4d17-9151-bc8852a7205a', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_revival_movement).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_language_academies).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_heritage_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, national_school_children).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, national_school_children).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, mother_tongue_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built schools, youth movements, and settlement institutions specifically to force the language back into cradle-to-grave daily use, treating native transmission as the sole legitimate proof of the nation's linguistic sovereignty. Collects political legitimacy and state resources by defining the standard this way; controls curricula and the definition of who counts as a genuine speaker.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_revival_movement, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_revival_movement, beneficiary).

% Administer the official register, certify curricula, and adjudicate what counts as 'living' usage for funding and immigration-integration programs. Their institutional mandate and budget depend on the native-transmission standard remaining the operative definition of vitality.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_academies, beneficiary,
    institutional, civilizational, arbitrage, national).

% Maintain unbroken recitation, study, and ritual use of the language across centuries without raising children as native daily speakers of it. Under this standard their entire mode of continuity is redescribed as preserving a corpse rather than sustaining life, which delegitimizes their claim to speak for the language's future and can exclude them from state cultural funding and status recognition.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, generational, constrained, regional).

% Grew up hearing or partially learning the language in mixed-language households or communal settings without it becoming their sole or primary mother tongue. The native-generation standard classifies them as non-transmitters regardless of their functional fluency or emotional attachment, cutting them out of the 'living speaker' category they identify with.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_heritage_speakers, payer,
    powerless, biographical, trapped, global).

% Are raised as native speakers through deliberate state-organized immersion, gaining a functioning mother tongue and national identity. They also bear the cost of an intensive, sometimes anxious project of manufactured native transmission that treats their childhood as the proving ground for the language's survival.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, national_school_children, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, national_school_children, payer).

% Study language vitality empirically — intergenerational transmission rates, domains of use, speaker attrition — and can document whether the native-generation standard tracks genuine sociolinguistic vitality or serves as a legitimating device for one revival project among several possible ones.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistics_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_revival_movement).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national revival project by giving institutions (schools, media, military, bureaucracy) a single, testable criterion — cradle transmission as mother tongue — around which curricula, certification, and cultural funding can be organized rather than left to diffuse or contested proof of vitality.
% TRANSFER_FUNCTION: Moves cultural legitimacy, state recognition, and funding from liturgical and diaspora heritage communities toward the institutions and populations enacting native transmission; moves the burden of proving the language's survival onto parents and schoolchildren tasked with manufacturing that transmission on a compressed timeline.
% ABSENT_VOICES: Liturgical scholars and diaspora heritage speakers who regard their mode of continuity as equally constitutive of the language's life are rarely seated on the academies or ministries that set the vitality standard; their objection — that a language kept alive in daily prayer, study, and communal reading has not died — goes unheard in the bodies certifying 'living' status.
% DISAPPEARANCE_RATIONALE: If the native-generation standard were dropped, liturgical and literary continuity would again count as sufficient proof of vitality, immediately re-legitimizing diaspora and liturgical communities' claims to the language, reallocating cultural funding and status recognition, and removing the pressure that currently makes native transmission the exclusive object of state educational policy.
% FOUNDING_PROBLEM: A dispersed people possessed a language sustained for centuries in prayer, study, and text but with no contemguous community of native daily speakers; nationalist reformers judged this insufficient to ground political sovereignty and needed a criterion that could justify — and measure the success of — a deliberate, state-scale project to make the language a living mother tongue again.
% FOUNDING_PROBLEM_CORROBORATION: The revival movement and its successor state institutions attest the standard remains necessary to secure the language against retreat into ceremonial use. Independent sociolinguists studying successful minority-language revitalization elsewhere, along with liturgical-tradition scholars, attest that intergenerational native transmission succeeded decades ago and that the standard now functions mainly to police who counts as an authentic national subject rather than to diagnose ongoing vitality risk.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.52) because the standard does real coordination work — it gave a genuinely dispersed, largely liturgical-only linguistic community a workable, measurable target for a state-building project, and native transmission was in fact substantially achieved. But it also extracts status and legitimacy from liturgical and diaspora communities by definitional fiat, converting a contestable claim about what counts as 'life' into an institutionally enforced boundary. Suppression starts high (0.70) during the active revival period when the definitional boundary was aggressively enforced against rival continuity claims, and eases modestly (0.58) as native transmission becomes an accomplished fact requiring less active suppression of alternatives — though it never approaches zero because the standard still actively excludes liturgical/diaspora claims from 'living' status. Theater ratio rises over time (0.10 to 0.28) as the standard's original urgent function (proving the language could survive as a mother tongue at all) is substantially achieved, leaving an increasing share of enforcement devoted to boundary maintenance and status gatekeeping rather than genuine vitality-building.
 *
 * PERSPECTIVAL GAP:
 *   From the revival movement and state academies' seat, this is a coordination triumph: a criterion that organized real institutional effort and produced a genuine living vernacular where none existed. From the liturgical and diaspora seats, the same criterion operates as an act of definitional extraction — their centuries of continuous transmission are recoded as death, stripping them of the legitimacy and resources that flow to the native-transmission-defined 'authentic' speaker community. The engine computes these as structurally different seat classifications from the same authored data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The revival movement and language academies are declared beneficiaries with arbitrage-level exit — they authored the standard and can adjust its application as institutional needs shift, so directionality sits near the beneficiary end. Liturgical-only communities and diaspora heritage speakers are declared victims: the former have constrained exit (their communal continuity persists but is delegitimized under this standard), the latter are trapped (their partial fluency cannot satisfy a native-mother-tongue bar no matter what they do). National schoolchildren occupy a genuine dual position — beneficiaries of successful native acquisition, but also bearers of the intensive, high-stakes project that treats their upbringing as the proof of national linguistic survival.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proving a dispersed liturgical language could become a genuine living vernacular — has been substantially solved: native transmission was achieved and sustained across multiple generations. Under a naive mandate-persistence read this would look like continued coordination. But because the standard's enforcement now functions primarily to police the boundary of 'authentic' speakerhood against liturgical and diaspora claims rather than to build vitality that no longer needs building, the tangled_rope classification (versus a pure rope) captures that the coordination function has partly ossified into status-extraction while retaining genuine residual coordination value in current pedagogy and certification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness_vs_construction,
    'Is the native-generational-transmission criterion a discovery about what linguistic ''life'' really consists in, or a constructed political standard selected because it happened to justify a particular nation-building project?',
    'Comparative sociolinguistic analysis of other language communities that sustain rich liturgical or literary continuity without native daily transmission (e.g. Sanskrit, Classical Arabic, Latin in certain communities) to assess whether ''life'' judgments track a stable cross-cultural criterion or vary with which community stands to benefit from a given definition.',
    'If the criterion is a genuine cross-linguistic discovery, the tangled_rope classification understates its coordination legitimacy; if it is a constructed standard selected for its political payoff, the classification is closer to a snare wearing coordination language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness_vs_construction, conceptual, 'Whether the native-transmission vitality standard is a linguistic discovery or a politically constructed boundary.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of living_language_status (native-generation, liturgical-preservation, literary-continuity) genuinely incommensurable value framings, or does one of them track the empirically correct sociolinguistic definition of language vitality that linguistics as a discipline could adjudicate?',
    'Survey of contemporary sociolinguistic consensus (if any) on defining ''living language'' status, and whether that consensus favors, rejects, or brackets the native-transmission criterion as decisive.',
    'If linguistics has a working consensus definition, one reading may be closer to descriptively correct and the others closer to interest-driven redefinition; if the field genuinely brackets the question as a matter of community self-understanding, the three readings remain properly coexisting rather than adjudicable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s readings are empirically adjudicable or genuinely incommensurable value framings.').

omega_variable(
    founding_problem_residual_urgency,
    'Given that native transmission has been substantially achieved across several generations, does the founding problem (proving the language could live as a mother tongue) retain any residual urgency that justifies continued strict enforcement of the standard against liturgical/diaspora claims?',
    'Longitudinal tracking of native-speaker retention rates and domain-of-use breadth; if retention is stable or growing without active boundary enforcement against non-native claimants, urgency has lapsed.',
    'If urgency has lapsed, continued enforcement is better read as status-gatekeeping (supporting a piton-adjacent reading of ongoing enforcement) rather than active coordination-building, sharpening rather than softening the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_residual_urgency, empirical, 'Whether the founding transmission-building problem remains live or has been substantially solved, leaving enforcement as residual boundary policing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(livi_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(livi_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(livi_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(livi_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(livi_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(livi_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(livi_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(livi_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(livi_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(livi_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(livi_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the living_language_status kernel. native_generation_reading claims moderate extraction (0.52) via a tangled_rope structure (genuine revival coordination plus status-extraction from liturgical/diaspora communities). liturgical_preservation_reading and literary_continuity_reading are authored as separate constraints with their own ε, beneficiary/victim sets, and claimed types, per the ε-invariance principle — each reading identifies a different population as living/dead and a different institutional apparatus as enforcing that boundary, so they cannot share a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
