% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity Through Liturgical Preservation and Textual Transmission
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew persists as a living language through two mechanistically distinct
 *   pathways: liturgical recitation and textual study (controlled by
 *   religious institutional authority, reinforced daily in observant
 *   practice) and native-speaker generative use (arising in Israel, now the
 *   dominant transmission mechanism for most speakers). This constraint story
 *   instantiates the LITURGICAL PRESERVATION reading: the commitment to
 *   understanding Hebrew as a language whose continuity depends on textual
 *   fidelity and institutional guardianship of ritual correctness. This
 *   reading constrains what counts as legitimate Hebrew (texts, prescribed
 *   recitations, rabbinic interpretation) and deems non-liturgical innovation
 *   as corruption or degradation. The constraint extracts from secular Hebrew
 *   speakers and diaspora communities without active liturgical practice by
 *   rendering their usage inauthentic and their linguistic agency secondary
 *   to institutional authority. It benefits religious institutions and
 *   tradition-keeper specialists whose status depends on the continuation of
 *   textual gatekeeping. The sibling readings — native_generative and
 *   bridge_pidginized — instantiate a different understanding of Hebrew
 *   continuity, one that centers speaker intuition and innovation. This story
 *   tells the institutional-preservation account, not because it is true
 *   universally (empirically, native speakers have proven sufficient), but
 *   because this is the reading the historical religious establishment has
 *   enforced and continues to defend.
 *
 * KEY AGENTS:
 *   - religious_institutional_authority: Agenda-setter, institutional power — defines correct Hebrew, controls educational institutions, enforces liturgical standards
 *   - textual_tradition_keepers: Beneficiary + agenda-setter, organized power — professional class whose expertise and status depend on continuation of textual authority
 *   - observant_jewish_communities: Beneficiary with identity-lock, moderate power — participate in daily liturgical recitation, experience constraint as constitutive identity
 *   - secular_hebrew_speakers: Payer, moderate power — use Hebrew generatively, face institutional dismissal of non-liturgical innovation
 *   - diaspora_communities_without_liturgical_practice: Payer, powerless — unable to access Hebrew through constraint's primary transmission vehicle (daily ritual)
 *   - secular_israel_state: Beneficiary + observer, institutional power — benefits from both religious and secular legitimation narratives; dual position creates complex directionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity Through Liturgical Preservation and Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '82b53945-0ab0-4599-bf3e-9574f45fe020').
narrative_ontology:cs_kernel_codification('82b53945-0ab0-4599-bf3e-9574f45fe020', fixed_text).
narrative_ontology:cs_authority_grounding('82b53945-0ab0-4599-bf3e-9574f45fe020', extraction).
narrative_ontology:cs_interpretation_layer_present('82b53945-0ab0-4599-bf3e-9574f45fe020').
narrative_ontology:cs_reading_relation('82b53945-0ab0-4599-bf3e-9574f45fe020', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('82b53945-0ab0-4599-bf3e-9574f45fe020', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('82b53945-0ab0-4599-bf3e-9574f45fe020', foundational, textual_fidelity_necessary_for_continuity).
narrative_ontology:cs_axiom_status(textual_fidelity_necessary_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('82b53945-0ab0-4599-bf3e-9574f45fe020', textual_fidelity_necessary_for_continuity, empirically_contingent).
narrative_ontology:cs_axiom('82b53945-0ab0-4599-bf3e-9574f45fe020', foundational, institutional_authority_required_for_linguistic_stability).
narrative_ontology:cs_axiom_status(institutional_authority_required_for_linguistic_stability, holdable).
narrative_ontology:cs_axiom_grounding('82b53945-0ab0-4599-bf3e-9574f45fe020', institutional_authority_required_for_linguistic_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('82b53945-0ab0-4599-bf3e-9574f45fe020', institutional_textual_authority_framework).
narrative_ontology:cs_drift_state('82b53945-0ab0-4599-bf3e-9574f45fe020', contemporary_post_israeli_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('82b53945-0ab0-4599-bf3e-9574f45fe020', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_tradition_keepers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_communities_without_liturgical_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, observant_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, secular_israel_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the textual canons, liturgical protocols, and educational institutions that define normative Hebrew use. Sets the boundary between correct (liturgically sanctioned) and incorrect usage. Maintains yeshivas, rabbinic courts, and ritual standards that enforce continuity through prescribed recitation. Collects institutional authority and legitimacy from the role of custodian.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Scholars, cantors, scribes, and educators who specialize in precise transmission of textual tradition. Their professional identity, status, and sometimes material support depend on the continuation of liturgical recitation and textual preservation. They transmit the constraint by embodying and teaching it; their expertise is validated by the constraint's operation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_tradition_keepers, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, textual_tradition_keepers, agenda_setter).

% Participate in daily liturgical recitation, study of sacred texts, and ritual practice that reproduce the Hebrew constraint. They experience the constraint as access to tradition, spiritual continuity, and communal identity. Exiting means severing connection to a constitutive element of religious self-understanding. The constraint is lived as non-negotiable identity, not as imposed rule.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, observant_jewish_communities, beneficiary,
    moderate, generational, identity_locked, global).

% In Israel and diaspora communities, use Hebrew for daily communication, literature, and secular purposes. The liturgical preservation standard treats their generative innovations and spoken usage as degradation or corruption of the language. Their Hebrew usage is constrained by institutional dismissal of non-liturgical innovation. They bear the cost of being told their living language is inauthentic.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_hebrew_speakers, payer,
    moderate, biographical, constrained, national).

% Communities with declining or absent liturgical knowledge and practice (progressive congregations, secular enclaves, assimilated populations). For them, Hebrew is not available through daily ritual recitation. The constraint renders their Hebrew learning costly (requires formal education, textual study, ritual apprenticeship) and their usage unstable (no daily reinforcement mechanism). They are unable to access Hebrew through the constraint's primary transmission vehicle.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_communities_without_liturgical_practice, payer,
    powerless, biographical, trapped, regional).

% Study Hebrew revitalization and language change. They would advocate for recognizing generative native-speaker innovation as legitimate continuity, and for decoupling language transmission from liturgical practice. Excluded from the textual authority structure; their evidence about how languages naturally persist and evolve is dismissed by institutional authorities as irrelevant to the sacred-text framework.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguists_and_language_planners, excluded,
    organized, generational, constrained, global).

% Has established Hebrew as the official national language of Israel and promoted it as a shared secular identity marker. The liturgical preservation constraint provides historical depth and cultural continuity that legitimizes the state's reclamation of Hebrew as a revitalized national language. Benefits from both religious and secular narratives of Hebrew continuity; occupies a dual position of enforcing secular Hebrew education while depending on religious institutional authority for cultural-historical legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_israel_state, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, secular_israel_state, observer).

% Structural pressure toward assimilation, declining religious practice, and preference for dominant national languages in diaspora. Not an actor but a force the constraint names as its primary threat. The constraint's persistence depends on suppressing the conditions that would make liturgical recitation a declining or optional practice.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_social_forces, excluded,
    powerful, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_continuity__liturgical_preservation, secularizing_social_forces).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Hebrew through a standardized, non-generative mechanism: precise repetition of authorized texts in prescribed ritual contexts. Solves the problem of language continuity when native-speaker communities are absent or fragmented by anchoring the language in textual fixity and institutional guardianship. Coordinates diaspora Jewish communities across geography and time by ensuring mutual intelligibility through shared liturgical protocol.
% TRANSFER_FUNCTION: Moves authority over language definition from speakers (who innovate and generate) to institutional custodians (who preserve and enforce textual fidelity). Transfers the burden of language maintenance from the speaker community to specialists (rabbis, scholars, cantors) whose status and livelihood depend on the constraint's continuation. Transfers cultural legitimacy from generative innovation to historical fidelity.
% ABSENT_VOICES: Secular linguists, modern Hebrew speakers in Israel, diaspora communities without active ritual practice, and generative native speakers of revitalized Hebrew would argue that language lives through speaker intuition and innovation, not textual preservation alone. They are structurally excluded from defining what counts as legitimate Hebrew continuity; rabbinic authority, not linguistic evidence, adjudicates the matter.
% DISAPPEARANCE_RATIONALE: If the liturgical preservation constraint and its institutional enforcement disappeared overnight, Hebrew transmission would shift rapidly to generative daily use (as it has in Israel) and to bridge linguistic practices in diaspora (as happens in communities where ritual practice declines). The language would continue but would be reorganized around native-speaker intuition rather than textual fidelity. Institutional authority structures centered on textual guardianship would lose their primary legitimating function.
% FOUNDING_PROBLEM: After the Roman exile and dispersal of the Jewish people, Hebrew ceased being a primary spoken language but had to persist as a vehicle for religious practice, textual study, and collective identity across geographically separated communities with no shared native-speaker intuition to maintain it.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and textual scholarship attest the founding problem is still live: without institutional preservation of textual accuracy and liturgical protocol, the language would fragment into unintelligible dialects or disappear entirely. Secular linguists and revitalization scholars attest the founding problem was substantially solved by the 20th-century emergence of native Hebrew speakers in Israel, whose generative use has proven sufficient for full language continuity without dependence on textual preservation; they document that the constraint now persists as institutional authority maintenance, not as necessity.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint moves authority over language definition from speakers to specialists, and because secular speakers and non-practicing diaspora communities bear real costs (delegitimation, constrained linguistic agency, inability to access transmission). Suppression is substantial (0.58) because the constraint depends on active enforcement: institutional dismissal of non-liturgical innovation, gatekeeping of educational authority, framing of generative speakers as linguistic incompetents or cultural threats. The measurement series shows steady but modest increase over 25 time units — this reflects gradual rise in institutional defensiveness as secular Israeli Hebrew and diaspora bridge languages challenge the textual-preservation monopoly. Theater is moderate (0.41): the constraint has a real coordination function (diaspora communities did depend on liturgical transmission when generative use was impossible), but a growing share of enforcement activity defends institutional authority rather than serving that coordination function. The trajectory shows theater ratio rising faster than extractiveness, indicating institutionalization of performative defense.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-authority seat, the constraint appears as necessary coordination: diaspora communities needed a mechanism to preserve Hebrew when living native-speaker communities did not exist. From the observant-community seat, it appears as identity continuity: participating in textual transmission is intrinsically valuable, not extractive. From the secular-speaker seat, it appears as institutional suppression: linguistic innovation is treated as error, and generative competence is delegitimated. From the diaspora-without-practice seat, it appears as exclusion: the constraint's primary transmission mechanism is unavailable to them, and alternative access is costly and unstable. The engine should compute these as distinct classifications: the institutional authority seat sees coordination (rope-like); the observant seat sees beneficial coordination; the secular-speaker seat sees extraction; the excluded diaspora seat sees structural closure. The authored metrics describe the constraint from a position that recognizes both its historic coordination function and its current extractive operation — this is deliberately a third-party analytical seat, not one of the inside positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional authority occupies the agenda-setter seat with high institutional power and arbitrage-grade exit options (they define the rules and can reframe them; they are not trapped by their own constraint). Their directionality approaches the beneficiary end. Textual tradition keepers are organized beneficiaries with arbitrage exit, also beneficiary-end directionality. Observant communities present a complex case: they occupy the beneficiary role (coordination of identity, community, tradition), but their exit is identity-locked (leaving observance means severing a constitutive element of self-concept). The identity-lock increases effective extraction from their position even though they report the constraint as beneficial. Their directionality sits near symmetric or slightly toward target because the identity fusion constrains their ability to exit. Secular Hebrew speakers have moderate power but constrained exit (Hebrew literacy still carries value and status; linguistic innovation is possible but carries institutional dismissal cost). Their directionality approaches the target end. Diaspora communities without liturgical practice are powerless and trapped (Hebrew is inaccessible to them through the constraint's primary mechanism; they must bear the cost of alternative acquisition or linguistic loss). Their directionality is clearly target. The secular Israel state occupies a dual position: it benefits from the legitimacy the constraint confers (historical depth, cultural continuity), but it also enforces secular Hebrew education that undermines the constraint. This creates a beneficiary-observer split.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (maintaining Hebrew across exile and diaspora without native speakers) is contested as to current status. Religious authorities attest it is still live: without textual preservation and institutional gatekeeping, Hebrew would degrade or fragment. Secular linguists and revitalization scholars attest it is dead: the existence of native Hebrew speakers in Israel has proven that language continuity does not require textual gatekeeping, and diaspora bridge languages demonstrate that intelligibility persists through speaker innovation even without institutional authority. If the founding problem is dead and the constraint persists anyway, mandatrophy has occurred: the institutional preservation apparatus persists because it benefits institutional actors, not because it solves a live coordination problem. The theater_ratio trajectory (rising from 0.28 to 0.41) and the suppression_requirement trajectory (rising from 0.52 to 0.58, flat after year 15) suggest that institutional energy is increasingly devoted to defending authority rather than solving the founding problem — a pattern consistent with mandatrophic persistence. However, the measured extractiveness is not extreme (0.62, moderate-high rather than near-pure), which suggests the constraint retains some real coordination function in diaspora communities still organized around liturgical practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does Hebrew persist through liturgical-textual preservation (this reading), through native-speaker generative use (native_generative), or through both as a hybrid bridge language (bridge_pidginized)?',
    'Empirical test: observe which transmission mechanism is actually sufficient in communities where others fail. In secular Israel, generative native speakers maintain Hebrew without liturgical practice. In diaspora without strong secular community, liturgical preservation persists even as generative capacity declines. Bridge communities show both mechanisms operating simultaneously.',
    'If generative native-speaker use is sufficient, the liturgical preservation constraint is revealed as institutional authority maintenance rather than necessity — reclassifies as snare under the native_generative reading. If both mechanisms coexist as equally necessary, the bridge_pidginized reading dominates. If liturgical preservation alone suffices (vanishingly rare in observed communities), this reading''s necessity claim stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which transmission mechanism for Hebrew is structurally necessary vs. contingent institutional authority.').

omega_variable(
    suppression_mechanism_source,
    'Is the measured suppression of secular Hebrew innovation structural (textual authority truly required for mutual intelligibility) or internalized (speakers defer to authority out of cultural reverence even when innovation would be mutually intelligible)?',
    'Observe whether innovation and generative deviation persist in communities with weakened institutional authority (progressive diaspora congregations, secular Israeli Hebrew writers). If speakers innovate freely once authority is weakened, suppression was internalized. If intelligibility actually breaks down, suppression is structural.',
    'If internalized, the constraint is capturing agents'' linguistic intuition and choice, not maintaining a functional necessity — reframes effective suppression as higher than authored. If structural, the preservation function is more genuine. Evidence suggests majority internalized (secular Israeli speakers generate freely; diaspora speakers defer even where authority is weak).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression of innovation is structural necessity or internalized deference.').

omega_variable(
    beneficiary_identity_fusion,
    'Are observant communities benefiting from the constraint, or are they identity-locked subjects unable to exit? Is their participation a chosen benefit or a constitutive identity obligation?',
    'Survey self-reported experience: do observant community members experience liturgical Hebrew as empowering coordination and identity continuity, or as a non-negotiable expectation they could not violate without shattering their self-concept? Observe switching behavior: do individuals who leave observance report experiencing linguistic autonomy, or linguistic loss?',
    'If beneficiary (experienced as benefit), the constraint''s coordination function is real from their seat. If identity-locked (experienced as obligation), their role may more accurately be payer-with-benefits — they bear suppression of alternative linguistic paths as the price of belonging. This affects directionality calculation and seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Whether observant communities are beneficiaries or identity-locked subjects.').

omega_variable(
    native_generative_threat_authenticity,
    'Is the institutional framing of native Hebrew speakers in Israel as a ''threat'' to tradition genuine (native speech actually corrupts textual fidelity), or is it a cover story for institutional authority displacement (native speakers make textual gatekeeping obsolete)?',
    'Compare textual accuracy and intelligibility rates in communities organized around liturgical preservation vs. those organized around native-generative use. If accuracy and intelligibility are comparable, the threat framing is institutional-maintenance rhetoric, not linguistic necessity.',
    'If threat is genuine, validates the constraint''s enforcement. If threat is cover story, reveals the constraint as pure institutional protection against displacement — reframes extraction upward, theater higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_generative_threat_authenticity, empirical, 'Whether native Hebrew speakers represent a genuine linguistic threat or institutional authority displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hebr_tr_t5, hebrew_continuity__liturgical_preservation, theater_ratio, 5, 0.31).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__liturgical_preservation, theater_ratio, 15, 0.39).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hebr_tr_t25, hebrew_continuity__liturgical_preservation, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hebr_be_t5, hebrew_continuity__liturgical_preservation, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__liturgical_preservation, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(hebr_be_t25, hebrew_continuity__liturgical_preservation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(hebr_su_t5, hebrew_continuity__liturgical_preservation, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__liturgical_preservation, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__liturgical_preservation, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hebr_su_t25, hebrew_continuity__liturgical_preservation, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.14).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel decomposes into three structurally distinct constraints instantiated by different readings. The liturgical_preservation reading (this story) centers institutional textual authority and prescriptive recitation. The native_generative reading centers speaker intuition and innovative use. The bridge_pidginized reading centers hybridity and diaspora contact dynamics. Each reading carries a different epsilon (institutional-authority reading: higher extraction from speakers who innovate; generative reading: lower extraction, coordinated around natural language use; bridge reading: mixed). Each instantiates different beneficiary/victim structures. All three readings contest the same kernel (what does Hebrew continuity mean?), but each answers the question differently. The engine's cross-reading contamination analysis should track: (1) how institutional authority in the liturgical reading is displaced by native speakers in the generative reading; (2) how diaspora bridge languages influence both readings by demonstrating practical alternatives; (3) how the foundational problem (exile diaspora continuity) is solved differently under each reading — validating one reading does not invalidate others, it only shifts which communities are organized by which reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
