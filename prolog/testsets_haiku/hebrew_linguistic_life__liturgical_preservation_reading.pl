% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life via Liturgical Preservation Chain
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical preservation reading of the
 *   contested kernel 'hebrew_linguistic_life.' The reading claims: a language
 *   is alive when its sacred texts are continuously recited, studied, and
 *   transmitted in unbroken institutional chain, regardless of whether the
 *   language is used as a native vernacular. Under this reading, Modern
 *   Hebrew's development is not a resurrection but a desecration—an
 *   appropriation of the sacred language into mundane secular use. The victim
 *   set includes not only those who wish to speak Hebrew vernacularly
 *   (suppressed by the sacredness constraint) but also the sacred linguistic
 *   tradition itself, which is harmed by the diversion of Hebrew into profane
 *   domains. This reading coexists with two sibling readings—the marketplace
 *   reading (a language is alive when it functions for inter-communal
 *   coordination) and the generational reading (a language is alive only when
 *   children acquire it as mother tongue)—that have incompatible ε values and
 *   different beneficiary/victim structures. The liturgical reading posits
 *   high extraction and high suppression because the constraint persists by
 *   enforcing the institutional monopoly on legitimate Hebrew use; exit is
 *   identity-locked for those invested in the tradition, and constrained for
 *   those who want secular Hebrew.
 *
 * KEY AGENTS:
 *   - Yeshiva scholars: institutional agenda-setters, control interpretive authority, identity-locked to the scholarly lineage
 *   - Rabbinical hierarchy: institutional agenda-setters + beneficiaries, enforce the sacredness rule, adjudicate legitimacy, identity-locked to their institutional role
 *   - Tradition guardians: moderate-power beneficiaries, embody the unbroken chain through daily recitation and study, identity-locked through religious participation
 *   - Hebrew vernacular speakers: organized payers, constrained by the sacredness doctrine, wish to use Hebrew for secular purposes
 *   - Secular Hebrew movement: powerful payers, attempt to exit by creating Modern Hebrew as a national language, resisted by rabbinical suppression
 *   - Diaspora Jewish communities: excluded from the mechanism, do not have the fluency to participate in the yeshiva-centered constraint, their actual linguistic practice is marginalized
 *   - Jewish nationalist project: analytical observer, instrumentalizes the constraint for identity-building while pursuing secular state-building
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.71).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life via Liturgical Preservation Chain").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'e00a5747-b999-4bd7-825b-449889cd9dbd').
narrative_ontology:cs_kernel_codification('e00a5747-b999-4bd7-825b-449889cd9dbd', fixed_text).
narrative_ontology:cs_authority_grounding('e00a5747-b999-4bd7-825b-449889cd9dbd', lineage).
narrative_ontology:cs_interpretation_layer_present('e00a5747-b999-4bd7-825b-449889cd9dbd').
narrative_ontology:cs_reading_relation('e00a5747-b999-4bd7-825b-449889cd9dbd', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_reading_relation('e00a5747-b999-4bd7-825b-449889cd9dbd', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('e00a5747-b999-4bd7-825b-449889cd9dbd', foundational, sacred_text_continuity_constitutes_aliveness).
narrative_ontology:cs_axiom_status(sacred_text_continuity_constitutes_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('e00a5747-b999-4bd7-825b-449889cd9dbd', sacred_text_continuity_constitutes_aliveness, deontological).
narrative_ontology:cs_axiom('e00a5747-b999-4bd7-825b-449889cd9dbd', secondary, vernacular_use_violates_sacred_integrity).
narrative_ontology:cs_axiom_status(vernacular_use_violates_sacred_integrity, holdable).
narrative_ontology:cs_axiom_grounding('e00a5747-b999-4bd7-825b-449889cd9dbd', vernacular_use_violates_sacred_integrity, deontological).
narrative_ontology:cs_reference_frame('e00a5747-b999-4bd7-825b-449889cd9dbd', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('e00a5747-b999-4bd7-825b-449889cd9dbd', modern_hebrew_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e00a5747-b999-4bd7-825b-449889cd9dbd', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinical_hierarchy).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, tradition_guardians).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_linguistic_tradition).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_movement).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, torah_textual_sanctity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_chain_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Direct the curriculum, set the interpretive methods, and determine what counts as legitimate study. They control access to the sacred texts and the hermeneutic frames within which the texts can be read. Their professional identity and institutional status depend on the continuity of liturgical transmission; exit means abandoning the scholarly lineage they are embedded in.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_scholars, agenda_setter,
    institutional, generational, identity_locked, regional).

% Enforces the requirement that Hebrew sacred texts remain the authoritative medium of Jewish law and practice. They adjudicate which texts count, which interpretations are valid, and which uses of the language violate its sacred status. Exit from this role means the dissolution of their institutional authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinical_hierarchy, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, rabbinical_hierarchy, beneficiary).

% Perform the daily, weekly, and yearly cycles of recitation, study, and transmission. They embody the unbroken chain by participating in it. Their identity as Jews is constituted through participation in this linguistic-liturgical practice. Exit would mean denying the sacredness of the texts they have been raised to honor.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, tradition_guardians, beneficiary,
    moderate, biographical, identity_locked, local).

% Wish to use Hebrew for daily secular speech, commerce, and secular literature. They are constrained by the sacredness constraint: any use of the language outside liturgical study is framed as desecration or trivialization. The constraint extracts their capacity to develop Hebrew as a living vernacular by declaring vernacular use illegitimate. Ben-Yehuda and Hebrew revivalists represent this seat's attempt to exit.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers, payer,
    organized, biographical, constrained, national).

% Advocates for Hebrew as a secular national language for Jews who do not practice religious life. They are blocked by the sacredness constraint: rabbinical authorities refuse to legitimize secular use and attempt to suppress it as degradation of the language. Their exit option—creating a secular Hebrew canon—is contested as violating the sacred-text monopoly.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_movement, payer,
    powerful, biographical, constrained, national).

% Are largely shut out of the mechanism by geography and language barriers; most diaspora Jews do not learn liturgical Hebrew at the fluency level the constraint requires. They are excluded from the conversation about what counts as legitimate Hebrew use; their actual linguistic practice (Yiddish, Ladino, local vernaculars) is treated as secondary or fallen-away.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_communities, excluded,
    organized, biographical, constrained, global).

% The Hebrew language as a living system is treated as a victim of the constraint: its capacity for vernacular growth, innovation, and organic evolution is restricted by the enforced sacredness doctrine. The constraint removes Hebrew's freedom to become a living language; it preserves Hebrew as an object, not as a living practice. The language's interests (if we grant languages have them) are subordinated to the institutional interests in control.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_language_tradition, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_language_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinical_hierarchy).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the semantic and syntactic integrity of Hebrew through continuous liturgical use, study, and transmission in formal institutional settings. Solves the coordination problem of maintaining a stable language across generations when everyday speech has shifted to other languages (Aramaic in antiquity, Yiddish/Ladino in diaspora). By anchoring the language in fixed sacred texts and formal study, the constraint prevents semantic drift and keeps Hebrew available for religious practice, textual interpretation, and potential re-activation if political or religious conditions change.
% TRANSFER_FUNCTION: Moves the authority to define legitimate Hebrew use from speakers (who want vernacular speech) to institutions (yeshivas, rabbinical hierarchies) that restrict use to liturgical and textual domains. It transfers Hebrew's linguistic capacity away from everyday speech toward ceremonial/scholarly preservation. Those who want to use Hebrew for commerce, literature, or daily life must accept institutional approval of what counts as legitimate speech, or practice in defiance.
% ABSENT_VOICES: The Hebrew language itself—treated as a victim in this story—has no voice in the constraint's operation. Diaspora Jews are largely excluded: most do not learn liturgical Hebrew at fluency and cannot participate in the study mechanisms the constraint requires. The secular majority in any given era (whose actual linguistic practice is suppressed or marginalized) would object to the constraint's definition of linguistic legitimacy but are not in the room where rabbinical and yeshiva authorities make determinations. Modern Hebrew speakers and revivalists represent the excluded voice that was eventually powerful enough to contest the constraint, but only by working around it, not within it.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if the sacred-text monopoly on legitimate Hebrew use dissolved—the language would immediately enter vernacular competition. Modern Hebrew would develop along native-speaker lines (marketplace pidgin and intergenerational acquisition would become the legitimacy standard). The yeshiva system would collapse as an institutional authority without the enforceability of the sacredness rule. Hebrew's ontological status would shift from sacred-preservation vessel to living language. The historical record shows exactly this rearrangement occurred: Modern Hebrew developed despite the constraint because the secular movement was powerful enough to override it, proving that the world does indeed rearrange when the constraint is challenged.
% FOUNDING_PROBLEM: Hebrew as the vernacular language of ancient Judea fell out of everyday use during the Roman period and subsequent diaspora, with Aramaic becoming the dominant spoken language. The texts—Torah, Prophets, Mishnah, Talmud—became inaccessible if the language of those texts died entirely. The constraint was built to keep the language alive for religious practice and textual access by mandating continuous study and recitation in institutional settings (yeshivas, synagogues) regardless of whether Jews spoke Hebrew in daily life.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities and yeshiva scholars attest the founding problem remains live: without the constraint, Hebrew would disappear and Jews would lose direct access to their sacred texts, forced to rely on translation and interpretation by non-native speakers. However, secular Hebrew revivalists, modern linguists, and historical documentation from the 19th–20th centuries attest the founding problem is dead: Hebrew never died (the unbroken chain accomplished its function); Modern Hebrew's successful development as a living language proves the language is viable without the constraint's enforcement. The Linguistic Atlas of Hebrew, Ben-Yehuda's dictionary project, and the development of Modern Hebrew in Israel demonstrate that the preservation problem was solved and the constraint's primary function was accomplished by the late 19th century. What persists is institutional control, not preservation necessity.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).

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
 *   The extraction value of 0.62 reflects the constraint's dual character: it genuinely preserves the language (coordination function) but does so by extracting from Hebrew's capacity for vernacular growth and from those who want to use it secularly. The suppression value of 0.71 is higher because the constraint persists through active institutional enforcement (rabbinical review, yeshiva curriculum control, social stigma against secular Hebrew) and through the structural elimination of alternatives—once the sacredness doctrine is accepted, vernacular use becomes unthinkable within the tradition. Theater ratio starting low (0.22) and rising to 0.48 indicates that in the early period (time_point 0–30, roughly pre-1900s to 1970s) the constraint's functional purpose (keeping Hebrew alive for textual access) dominated. As Modern Hebrew became a living language despite suppression (time_point 60 onward), the constraint's functional purpose was increasingly accomplished but enforcement continued and intensified (suppression plateau at 0.71 from time_point 90 onward). The rising theater ratio reflects an increasing mismatch: the institutional machinery persists for control, not for the original preservation need. The constraint does not classify itself—it presents as rope (coordination, unbroken chain) while operating metrics show substantial extraction and suppression, making it tangled rope per the canonical gate (requires_active_enforcement: true, beneficiaries present, victims present). The measurement series shares one time grid to prevent misalignment drift.
 *
 * PERSPECTIVAL GAP:
 *   From the yeshiva/rabbinical seat, the constraint appears as rope: we preserve the language, maintain access to the texts, keep the tradition unbroken. From the secular Hebrew speaker's seat, it appears as snare: you want to use the language but are told it is sacred and profane use is prohibited, and this rule is enforced. From the modern nationalist seat (observer position), it appears as piton: the constraint's original function (prevent extinction) is accomplished, but the institutional machinery persists and increasingly performs theater—defending a problem already solved. The engine computes these divergences from the structural data. The authored claim (tangled_rope) sits between rope and snare, reflecting the genuine coordination function paired with genuine extraction; where the computed per-seat types diverge from this claim, that divergence is diagnostic of the constraint's true operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional agenda-setters (yeshiva, rabbinical) are beneficiaries with institutional power and identity-locked exit: they extract control and status from the constraint. The secular speakers are organized payers with constrained exit (they can use Hebrew in defiance but at high social cost). The tradition itself (listed as victim) has no exit: Hebrew's vernacular capacity is permanently restricted under the constraint's operation. Diaspora communities are excluded (trapped, no meaningful seat at the decision table). The beneficiary/victim declarations drive directionality: beneficiaries sit near d=0.0 (full subsidy from the constraint), victims sit near d=1.0 (full target). For institutional actors with organized power, the derivation may benefit from an override if regulatory capture or ideological lock-in changes their structural relationship, but in this case the data supports the derived d values—the rabbinic hierarchy genuinely benefits from controlling Hebrew's definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (prevent extinction of Hebrew when it fell out of vernacular use) was genuine in the Talmudic and medieval periods when the diaspora risked linguistic assimilation. By the 19th century, the problem was increasingly obsolete: the unbroken chain had succeeded—Hebrew survived through liturgy. Modern Hebrew's emergence despite rabbinical opposition reveals mandatrophy: the constraint persists in enforcing the sacredness rule even though the preservation function is accomplished. The institutional machinery (yeshiva curriculum, rabbinical authority, social stigma) continues not because Hebrew will die without it, but because the institutions benefit from controlling the language's status. The contest with the marketplace reading is precisely the mandatrophy boundary: the marketplace reading sees Modern Hebrew as proof that the language is alive through vernacular function and the constraint is now pure control; the liturgical reading insists that the sacredness doctrine must be maintained to prevent contamination. The theater ratio rising to 0.48 at the terminal interval indicates growing performative maintenance: the constraint defends what no longer needs defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_identity,
    'Is a language ''alive'' when it exists as a sacred-text transmission chain, or only when it functions as a living vernacular?',
    'This is the core irreducible ambiguity that spawns the three sibling readings. Resolution would require a definitional/conceptual choice about what ''alive'' means. No empirical measurement can settle it because the three readings have different operationalizations of aliveness.',
    'If aliveness is defined by liturgical preservation, this reading (tangled_rope, extractive but coordinating) is correct. If aliveness is defined by native acquisition or marketplace use, this reading''s victim set (the vernacular itself) is the true target. The constraint''s classification pivots on which definition is accepted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_identity, conceptual, 'Definitional contest over what makes a language alive.').

omega_variable(
    sacred_desecration_boundary,
    'Does secular use of Hebrew violate the sacred-text integrity the constraint exists to preserve, or does vernacular Hebrew amplify it by extending the language''s reach?',
    'This is an axiom-level disagreement: the constraint rests on the claim that sacred texts require protection from vernacular contamination. Revisionists argue that living vernacular use demonstrates the language''s vitality and makes the texts accessible to non-scholars. Historical analysis of how modern Hebrew developed could inform this, but the disagreement is fundamentally about what desecration means.',
    'If secular use desecrates, the rabbinical suppression of vernacular Hebrew is a necessary cost of coordination. If secular use amplifies, the suppression is pure extraction and the constraint should be reclassified toward snare. Ben-Yehuda''s successful creation of Modern Hebrew as a living language despite institutional opposition suggests the second reading; the rabbinical response of treating Modern Hebrew as a fallen or corrupted version supports the first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_desecration_boundary, conceptual, 'Whether vernacular use violates or amplifies the sacred-text preservation function.').

omega_variable(
    institutional_control_motivation,
    'Does the rabbinical/yeshiva enforcement of the constraint exist primarily to preserve the language, or primarily to maintain institutional control over Jewish identity and practice?',
    'Institutional history and comparative analysis: if rabbinical enforcement intensity tracks language-preservation needs (high when assimilation pressure is greatest), the coordination narrative holds. If enforcement intensity tracks institutional power struggles and the intensity of challenges to rabbinical authority, the extraction narrative dominates.',
    'The constraint could be reclassified from tangled_rope (coordination + extraction) toward snare (extraction only) if institutional power-maintenance is the primary driver. Evidence: the institutional response to Modern Hebrew was suppression, not integration. The response to vernacular Yiddish adoption was gradual acceptance, suggesting the constraint''s boundary is about control over Hebrew specifically (the prestige language linked to texts and authority) rather than about linguistic preservation per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_control_motivation, empirical, 'Whether enforcement is driven by preservation necessity or institutional control.').

omega_variable(
    extraction_from_tradition_itself,
    'Is the Hebrew language (as victim) being extracted from—is the constraint removing its capacity for vernacular life—or is the tradition being protected from the harm of vernacular contamination?',
    'This hinges on whether you accept that a language can have interests independent of its speakers. If yes, then restricting Hebrew to liturgical domains is an extraction from the language''s own capacity for growth and evolution. If no (languages are tools, not patients), then the constraint is protecting the tool from misuse.',
    'Classically, the victim-identification rule says victims are agents who bear costs. The constraint''s author lists ''sacred_linguistic_tradition'' as a victim, treating the tradition as an entity with interests. This is unusual and signals the reading''s conviction that the sacred texts and their language have been harmed by the constraint''s enforcement—the unbroken-chain doctrine is extracting from Hebrew''s own vitality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_from_tradition_itself, preference, 'Whether the Hebrew language itself can be a victim of institutional extraction.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the liturgical preservation reading logically foreclose the native-generational and marketplace readings, or do all three remain live positions across different parties?',
    'The core-premise test (Rules 4, reading_relations): the liturgical reading''s foundational axiom is that sacred-text continuity is THE legitimacy criterion for aliveness. The generational reading''s is that native acquisition is THE criterion. The marketplace reading''s is that functional inter-community use is THE criterion. Do these three claims logically rule each other out (one framework cannot hold all three) or do they coexist as different party commitments?',
    'If they foreclose: the readings are incompatible and one must win institutional adoption. If they coexist: the readings are different positions in an unresolved dispute, and the constraint operates in a contested environment where its own legitimacy is under challenge. Evidence: Modern Hebrew''s success despite rabbinical opposition suggests the readings coexist (different Jewish communities adopted different readings) rather than one logically eliminating the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the sibling readings are logically incompatible or coexisting positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t15, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t15, observed).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t30, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t90, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 90, 0.48).
narrative_ontology:measurement_basis(hebr_tr_t90, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.48).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).
narrative_ontology:measurement(hebr_tr_t150, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 150, 0.48).
narrative_ontology:measurement_basis(hebr_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t15, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(hebr_be_t15, observed).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(hebr_be_t30, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t90, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 90, 0.62).
narrative_ontology:measurement_basis(hebr_be_t90, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement_basis(hebr_be_t120, observed).
narrative_ontology:measurement(hebr_be_t150, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement_basis(hebr_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t15, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(hebr_su_t15, observed).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(hebr_su_t30, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t90, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 90, 0.71).
narrative_ontology:measurement_basis(hebr_su_t90, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.7).
narrative_ontology:measurement_basis(hebr_su_t120, observed).
narrative_ontology:measurement(hebr_su_t150, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 150, 0.71).
narrative_ontology:measurement_basis(hebr_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'hebrew_linguistic_life.' Sibling readings are hebrew_linguistic_life__native_generational_reading (aliveness via mother-tongue acquisition and daily use) and hebrew_linguistic_life__marketplace_pidgin_reading (aliveness via functional inter-communal use). The three readings have structurally different ε values, beneficiary sets, and time horizons because they operationalize 'linguistic aliveness' differently. The liturgical preservation reading (this file) models the constraint through institutional enforcement and sacred-text preservation; the generational reading would model it through native acquisition patterns and family transmission; the marketplace reading would model it through functional adoption and inter-group coordination. Each reading is a complete constraint story with its own stakeholders and metrics. The constraint family is linked via network.affects_constraints: the liturgical reading influences both sibling readings because the institutional enforcement affects what options are available for generational acquisition and marketplace adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
