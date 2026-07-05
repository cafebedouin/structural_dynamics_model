% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact/Bridge Language (Pidginized Reading)
 *   domain: sociolinguistics/religious_institutional
 *
 * SUMMARY:
 *   This story instantiates the 'bridge_pidginized' reading of the Hebrew
 *   continuity kernel: Hebrew is treated as neither a strictly liturgical
 *   artifact (recited but not generated) nor a fully native vernacular
 *   (intuited and generatively spoken from birth), but as a working contact
 *   language occupying diaspora communities' actual instrumental needs —
 *   high-register written correspondence among scholars and communal bodies,
 *   and a simplified marketplace pidgin among merchants and travelers
 *   crossing linguistic zones. This reading is deliberately distinct from the
 *   sibling readings 'liturgical_preservation' (continuity through fixed
 *   ritual recitation) and 'native_generative' (continuity only through daily
 *   generative native use), which are separate constraint stories with their
 *   own epsilon values, not alternate measurements of this one. Both
 *   siblings, from within their own frameworks, dismiss this register as 'not
 *   really Hebrew' — a degraded liturgical use on one side, an insufficiently
 *   generative pidgin on the other — but this reading holds that the register
 *   performed real, sustained coordination work across centuries of
 *   dispersion regardless of that dismissal.
 *
 * KEY AGENTS:
 *   - diaspora_communal_organizations: agenda_setter/beneficiary — set correspondence and record-keeping conventions
 *   - hebrew_correspondence_networks: beneficiary — scholars/rabbis whose portable credential is fluency in this register
 *   - cross_regional_jewish_merchants_and_travelers: beneficiary — use simplified pidgin for trade and travel
 *   - non_hebrew_literate_diaspora_members: payer — excluded from communal records and decisions conducted in this register
 *   - vernacular_dominant_youth: payer — constrained exit, bear the cost of a barrier to communal participation
 *   - liturgical_preservationists / native_generative_advocates: excluded — dismiss this register from outside its own framework
 *   - sociolinguistic_observers: observer — document the register's actual coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.38).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.42).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact/Bridge Language (Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/religious_institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'e26f57f0-9368-4921-9668-5f5bcb7555cf').
narrative_ontology:cs_kernel_codification('e26f57f0-9368-4921-9668-5f5bcb7555cf', distributed).
narrative_ontology:cs_authority_grounding('e26f57f0-9368-4921-9668-5f5bcb7555cf', practice).
narrative_ontology:cs_interpretation_layer_present('e26f57f0-9368-4921-9668-5f5bcb7555cf').
narrative_ontology:cs_reading_relation('e26f57f0-9368-4921-9668-5f5bcb7555cf', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('e26f57f0-9368-4921-9668-5f5bcb7555cf', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('e26f57f0-9368-4921-9668-5f5bcb7555cf', foundational, instrumental_use_constitutes_continuity).
narrative_ontology:cs_axiom_status(instrumental_use_constitutes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e26f57f0-9368-4921-9668-5f5bcb7555cf', instrumental_use_constitutes_continuity, conventional).
narrative_ontology:cs_axiom('e26f57f0-9368-4921-9668-5f5bcb7555cf', secondary, partial_register_sufficiency).
narrative_ontology:cs_axiom_status(partial_register_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('e26f57f0-9368-4921-9668-5f5bcb7555cf', partial_register_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('e26f57f0-9368-4921-9668-5f5bcb7555cf', diaspora_correspondence_continuity).
narrative_ontology:cs_drift_state('e26f57f0-9368-4921-9668-5f5bcb7555cf', contemporary_post_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e26f57f0-9368-4921-9668-5f5bcb7555cf', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_correspondence_networks).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, cross_regional_jewish_merchants_and_travelers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, non_hebrew_literate_diaspora_members).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, vernacular_dominant_youth).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, hebrew_as_living_instrumental_medium).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, diaspora_intercommunal_intelligibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain correspondence, contracts, and communal record-keeping in a shared high-register Hebrew because it is legible across communities with mutually unintelligible vernaculars (Yiddish, Ladino, Judeo-Arabic, local languages). They set the register and vocabulary conventions used in responsa, communal minutes, and inter-community letters, and benefit from the standing capacity to communicate without translation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations, beneficiary).

% Rabbis, scholars, and communal scribes who write and receive responsa, halakhic queries, and scholarly correspondence in Hebrew. Fluency in written contact-Hebrew is a portable credential that lets them participate in networks spanning multiple diaspora regions; they can relocate or correspond across communities precisely because this shared code exists.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_correspondence_networks, beneficiary,
    organized, generational, mobile, continental).

% Use a simplified, marketplace-register Hebrew pidgin to transact and coordinate with Jewish counterparts from other linguistic regions where no shared vernacular exists. The pidgin is functional rather than grammatically complete, but it lets business and travel proceed without a full shared native language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, cross_regional_jewish_merchants_and_travelers, beneficiary,
    moderate, biographical, mobile, continental).

% Community members without formal Hebrew literacy — often women, the poor, and those without access to religious education — are excluded from communal records, correspondence, and networked opportunities conducted in contact-Hebrew. They depend on intermediaries to access documents and decisions that affect them, paying a real cost in reduced communal standing and information access.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, non_hebrew_literate_diaspora_members, payer,
    powerless, biographical, trapped, regional).

% Younger diaspora members raised primarily in the local vernacular experience contact-Hebrew as a barrier to full participation in communal governance and inter-community networks, since fluency is acquired only through targeted religious or communal education not universally available. Their exit is constrained: they can assimilate into vernacular-only communal life at the cost of reduced standing, or invest scarce time acquiring a register with little use outside these networks.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, vernacular_dominant_youth, payer,
    powerless, biographical, constrained, regional).

% Rabbinic and liturgical authorities who hold that Hebrew's continuity is properly located in fixed ritual recitation and textual transmission consider the contact/pidgin register a degraded or illegitimate instantiation — 'not really Hebrew' — and are not represented within the bridge-language communities' own account of what keeps Hebrew alive. Their framework is not refuted by this reading; it simply does not recognize the marketplace and correspondence registers as continuity at all.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_preservationists, excluded,
    institutional, civilizational, analytical, continental).

% Revivalist linguists and later Israeli-Hebrew native speakers who hold that only generative, intuition-driven daily use constitutes a living language dismiss the diaspora contact register as pidginized and non-generative — insufficiently a language, sufficiently a code. They are not part of the diaspora correspondence networks and do not weigh in on how those networks sustain themselves.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_generative_advocates, excluded,
    institutional, civilizational, analytical, national).

% Historians and linguists studying diaspora language contact document how this register functioned as a genuine, if partial, continuity mechanism across centuries of dispersion, distinct from both liturgical fixity and native generativity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared written and spoken code that lets geographically and linguistically dispersed Jewish communities correspond, trade, adjudicate religious questions, and travel among each other without a common vernacular — solving an intercommunal intelligibility problem that no single diaspora vernacular could solve alone.
% TRANSFER_FUNCTION: Moves communicative and institutional access — participation in communal governance, correspondence networks, trade opportunities, and religious adjudication — toward those literate in contact-Hebrew (communal elites, scholars, merchants) and away from those without that literacy (the poor, women historically, vernacular-only youth), who must rely on intermediaries or are excluded from networked decision-making.
% ABSENT_VOICES: Non-literate community members and vernacular-dominant youth would object that access to communal records and networks is gated by a register acquired mainly through privileged education; liturgical preservationists and native-generative advocates would object on different grounds that this register is not authentically Hebrew at all — both groups are structurally outside the correspondence and merchant networks whose account of Hebrew's vitality this reading represents.
% DISAPPEARANCE_RATIONALE: If the contact/pidgin register vanished, diaspora communities would lose their primary channel for cross-regional correspondence, halakhic query networks, and intercommunal trade coordination; communities would fragment into vernacular-isolated units, responsa networks would need a replacement lingua franca (likely a colonial or trade language), and communal elites who currently derive standing from Hebrew literacy would lose a distinguishing credential.
% FOUNDING_PROBLEM: Diaspora dispersion left Jewish communities speaking mutually unintelligible vernaculars while needing to correspond on religious law, trade, marriage, and communal governance across regions; a shared code was needed that did not depend on any one region's native tongue.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic historians outside the correspondence networks and outside the beneficiary communities corroborate that this intercommunal intelligibility gap was real and that contact-Hebrew functioned as its solution across centuries; liturgical and native-generative authorities dispute whether the problem was ever properly 'Hebrew continuity' rather than a separate practical accommodation, and non-literate community members corroborate that the register's continued use also gates their access rather than purely solving a shared problem.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) and rises modestly over the interval: the register's coordination value is real but its acquisition cost is unevenly distributed, and over time its instrumental function increasingly doubles as a status marker among communal elites, layering a mild extractive residue onto the coordination core. Suppression is moderate (0.42) rather than low: there is no active enforcement machinery, but access to the education required to acquire the register is not universally available, functioning as a soft structural barrier rather than coercion. Theater ratio is modest and slowly rising (0.12 to 0.28) — most of the activity is genuinely functional correspondence and trade, but a growing share of high-register written production over time serves credentialing and status display rather than communication that could not otherwise occur. Accessibility collapse is moderate-low (0.35): vernaculars, translation intermediaries, and other lingua francas (Yiddish, Ladino, later colonial languages) remained real alternatives throughout, so the register never became the only path to intercommunal coordination. Resistance is moderate-high (0.55) primarily in the form of the two sibling readings actively contesting this register's legitimacy as 'real Hebrew,' plus periodic vernacular-community pushback against the education gate.
 *
 * PERSPECTIVAL GAP:
 *   From the correspondence-network and merchant seats, this register looks like straightforward, low-cost coordination — a rope. From the non-literate and vernacular-youth seats, the same register looks like a gatekeeping mechanism that converts an accident of birth or education access into differential communal standing — closer to a mild tangled coordination. The engine computes these per-seat divergences from the declared structural data; the claimed_type of rope reflects the story's own reading (contact-language coordination, no active enforcement) rather than an attempt to average the two experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal organizations and correspondence networks are structural beneficiaries: they set the conventions, hold the acquired literacy, and derive standing and access from the register's persistence — low d. Merchants and travelers are moderate beneficiaries with mobile exit — the register solves a real problem for them and they are not trapped by it. Non-literate members and vernacular-dominant youth are the structural targets: they bear the access cost without controlling the register's terms, and their exit options are trapped or constrained respectively (non-literate members generally cannot simply acquire elite communal standing by choice; youth face a real but costly acquisition path). This is not an override case — the derivation from beneficiary/victim declarations plus exit options tracks the actual asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — intercommunal unintelligibility across diaspora vernaculars — remains genuinely live in this reading's account: dispersed Jewish communities still lack a universally shared vernacular, and contact-Hebrew continues to perform real coordination work in correspondence and trade networks rather than persisting as pure institutional inertia. This distinguishes the constraint from mandatrophy: the coordination function has not detached from the problem it solves, even though its status-marking side effects have grown somewhat over time (rising theater_ratio). Framing it as pure extraction would miss that the register still enables coordination unavailable through any single vernacular; framing it as pure coordination would miss the real, unevenly distributed acquisition cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Is the contact/pidgin register a genuine third mode of Hebrew continuity, or is it better described as a degraded liturgical practice (per liturgical_preservation) or a failed/incomplete generative language (per native_generative) rather than an independent reading of the kernel?',
    'Comparative sociolinguistic analysis of whether the register exhibits internally consistent grammar and productive rule-generation distinct from both liturgical formula-recitation and native intuitive generativity; corpus analysis of correspondence registers across centuries and regions for internal consistency versus drift toward either pole.',
    'If the register collapses into one of the sibling readings under closer analysis, this constraint should be merged into that sibling rather than treated as independently instantiating the kernel; if it remains distinct, the three-way decomposition holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the bridge/pidgin reading is a genuinely independent kernel occupation or a hybrid mischaracterized as a third reading.').

omega_variable(
    beneficiary_versus_natural_continuity,
    'Do the declared beneficiaries (communal organizations, correspondence networks, merchants) actively construct and maintain this register for their own advantage, or does it emerge naturally from the diaspora dispersion problem regardless of who benefits?',
    'Historical analysis of whether communal elites actively restricted access to Hebrew literacy (e.g., gatekept education) versus whether literacy gaps arose from general resource scarcity unrelated to elite maintenance of the register.',
    'If elites actively restricted access to preserve their network advantage, extractiveness and suppression should be revised upward; if literacy gaps were incidental to general educational scarcity, the coordination reading is stronger and extraction should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_versus_natural_continuity, empirical, 'Whether beneficiary advantage is actively maintained or an incidental byproduct of the coordination function.').

omega_variable(
    dismissal_as_evidence_or_noise,
    'Does the fact that both sibling readings dismiss this register as ''not really Hebrew'' constitute evidence that the register is structurally marginal, or is the dismissal itself a status-competition byproduct of the sibling readings'' own institutional interests in gatekeeping authenticity claims?',
    'Examine whether liturgical and native-generative authorities have institutional stakes (educational, religious, national) in narrowing what counts as ''real'' Hebrew, versus whether the dismissal tracks genuine linguistic criteria independent of those stakes.',
    'If the dismissal is interest-driven, it should not be treated as corroborating evidence against this reading''s legitimacy; if it tracks genuine linguistic criteria, it strengthens the case that this reading captures a functionally distinct but linguistically marginal phenomenon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dismissal_as_evidence_or_noise, conceptual, 'Whether sibling readings'' dismissal is principled or self-interested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__bridge_pidginized, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__bridge_pidginized, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__bridge_pidginized, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__bridge_pidginized, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__bridge_pidginized, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__bridge_pidginized, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__bridge_pidginized, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__bridge_pidginized, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_continuity__bridge_pidginized, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_continuity kernel, decomposed per the ε-invariance principle: the natural-language label 'Hebrew continuity' conflates structurally distinct claims about what sustains the language across dispersion. 'liturgical_preservation' locates continuity in fixed ritual recitation and textual transmission (likely low extraction, high accessibility_collapse, mountain/rope-leaning). 'native_generative' locates continuity only in daily generative native use (a claim centered on modern revival contexts, with its own beneficiary/victim structure around revivalist institutions and non-native diaspora speakers). This story, 'bridge_pidginized', locates continuity in instrumental contact-language use — correspondence and marketplace registers — with moderate extraction and a distinct beneficiary/victim structure (communal literacy elites versus non-literate members). Each carries its own ε and stakeholder structure; they are linked here rather than merged because measuring 'Hebrew continuity' by ritual-fidelity, by native-generativity, or by intercommunal-coordination-utility yields three different extraction pictures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
