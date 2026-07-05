% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation as Symbolic Boundary-Maintenance (Symbol-Survival Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the symbol_survival_reading of the
 *   catastrophe_memory_survival kernel: the claim that ritual practice
 *   preserves Jewish identity and boundary-norms through symbolic experience,
 *   and that communal survival IS continuity of practice itself — form as
 *   substance. Under this reading, exact ritual observance is not merely
 *   instrumentally useful for transmitting practical knowledge (the sibling
 *   competence_transmission_reading) but constitutes the survival good
 *   directly. This produces a distinctive structural profile: rabbinic and
 *   orthodox institutions that administer and certify correct practice become
 *   structurally indispensable to the doctrine's own truth, and those whose
 *   practice has thinned — secularized Jews, intermarried families,
 *   under-resourced diaspora youth — are cast as instances of survival
 *   failure regardless of what ethical, historical, or communal continuity
 *   they otherwise maintain. This is a distinct constraint from its siblings,
 *   not an alternative measurement of the same one: the
 *   competence_transmission_reading would find low extraction (ritual as
 *   neutral instructional technology, no interpretive gatekeeper required)
 *   and the hybrid_encoding_reading would sit between the two. Only this
 *   reading generates the identifiable beneficiary (rabbinic interpretive
 *   authority) and victim set (those excluded or diminished by the exactness
 *   standard) that make it substantially extractive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation as Symbolic Boundary-Maintenance (Symbol-Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'e10f69f9-4872-4aa1-8d34-03a853024ecf').
narrative_ontology:cs_kernel_codification('e10f69f9-4872-4aa1-8d34-03a853024ecf', distributed).
narrative_ontology:cs_authority_grounding('e10f69f9-4872-4aa1-8d34-03a853024ecf', lineage).
narrative_ontology:cs_interpretation_layer_present('e10f69f9-4872-4aa1-8d34-03a853024ecf').
narrative_ontology:cs_reading_relation('e10f69f9-4872-4aa1-8d34-03a853024ecf', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('e10f69f9-4872-4aa1-8d34-03a853024ecf', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('e10f69f9-4872-4aa1-8d34-03a853024ecf', foundational, practice_continuity_constitutes_survival).
narrative_ontology:cs_axiom_status(practice_continuity_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('e10f69f9-4872-4aa1-8d34-03a853024ecf', practice_continuity_constitutes_survival, conventional).
narrative_ontology:cs_axiom('e10f69f9-4872-4aa1-8d34-03a853024ecf', secondary, interpretive_authority_over_form_is_indispensable_to_identity).
narrative_ontology:cs_axiom_status(interpretive_authority_over_form_is_indispensable_to_identity, holdable).
narrative_ontology:cs_axiom_grounding('e10f69f9-4872-4aa1-8d34-03a853024ecf', interpretive_authority_over_form_is_indispensable_to_identity, conventional).
narrative_ontology:cs_reference_frame('e10f69f9-4872-4aa1-8d34-03a853024ecf', post_destruction_rabbinic_consolidation).
narrative_ontology:cs_drift_state('e10f69f9-4872-4aa1-8d34-03a853024ecf', contemporary_diaspora_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e10f69f9-4872-4aa1-8d34-03a853024ecf', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, orthodox_institutional_bodies).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_without_access).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, ritual_continuity_as_survival_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines correct ritual form, adjudicates what counts as authentic practice, and administers the institutions (seminaries, kashrut certification, conversion courts, communal recognition) through which ritual competence is credentialed. Frames continuity of exact practice as the mechanism of Jewish survival after catastrophe, which positions the authority's own interpretive gatekeeping as indispensable to that survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Synagogue federations, day schools, and communal organizations whose funding, membership, and legitimacy depend on the premise that precise ritual observance is the thing standing between the community and dissolution. Their institutional survival is coupled to the doctrine's persuasiveness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, orthodox_institutional_bodies, beneficiary,
    institutional, civilizational, arbitrage, global).

% Identify as Jewish by ancestry or culture but do not maintain full ritual observance. Under the symbol-survival reading, their identity is treated as attenuated or at-risk regardless of the substantive continuity of memory, ethics, or communal bonds they maintain by other means. They bear the cost of being read as failing the survival test even when no practical knowledge transmission has actually broken down.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, constrained, national).

% Face exclusion from ritual recognition (marriage validity, matrilineal descent rulings, conversion barriers) because the symbolic-boundary reading treats intermarriage as a breach of the practice-continuity chain, independent of whether the family transmits ethical or historical memory of catastrophe to children.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, intermarried_families, payer,
    moderate, generational, constrained, national).

% Grow up in communities with limited day-school infrastructure or affordable ritual education. Under this reading their thinned ritual competence is read as existential loss regardless of what they retain of the community's history, ethics, or resilience knowledge — they cannot buy their way into full recognition even where they want it, since access to certified ritual instruction is scarce and costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, diaspora_youth_without_access, payer,
    powerless, biographical, trapped, regional).

% Offer alternative accounts of Jewish continuity emphasizing ethical inheritance, historical memory, and community over exact ritual form. Their competing account is treated by orthodox institutions as a lesser or diluted survival strategy rather than as a legitimate alternative reading of the same catastrophe-memory kernel; they are rarely granted equal standing in the discourse about what 'survival' means.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, reform_and_reconstructionist_movements, excluded,
    organized, generational, constrained, national).

% Study ritual transmission across post-catastrophe communities comparatively, without institutional stake in any single account of what ritual accomplishes. They can assess whether practice-continuity independently predicts communal persistence, separate from the authority claims built on top of it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized ritual practice gives a dispersed, historically persecuted community a stable, recognizable, transmissible marker of shared identity that does not depend on any single geographic center or political sovereignty — a genuine coordination good for maintaining group continuity across generations and diasporas.
% TRANSFER_FUNCTION: Moves interpretive authority, communal legitimacy, and material resources (school funding, certification fees, communal recognition, marriage/conversion gatekeeping) toward institutions and figures who administer correct ritual form, and moves social standing and belonging away from those whose practice diverges from or falls short of the certified form — regardless of what those individuals retain of communal memory or ethical inheritance by other means.
% ABSENT_VOICES: Reform, Reconstructionist, and secular-cultural Jewish thinkers who hold that memory, ethics, and community persist without exact ritual continuity are structurally present in wider Jewish discourse but excluded from the specific institutional apparatus (rabbinic courts, certification bodies) that adjudicates 'authentic' survival under this reading.
% DISAPPEARANCE_RATIONALE: If the symbol-survival doctrine's institutional enforcement vanished overnight, rabbinic authorities and orthodox institutions would lose a primary claim to indispensability and a filtering mechanism for communal legitimacy; parties who hold the competence-transmission or hybrid readings would say the world barely changes, since the practical and ethical substance of continuity was never reducible to exact form in the first place. Whether the world 'rearranges' or stays the same depends entirely on which reading of the underlying kernel one holds — which is why this question is itself contested rather than settled.
% FOUNDING_PROBLEM: After catastrophic ruptures (destruction of the Temple, expulsions, the Holocaust), the community needed some mechanism to persist as a recognizable, bounded people despite the loss of land, institutions, and continuous transmission lines — ritual practice was built to hold identity together across the rupture.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and orthodox institutional leadership attest the founding problem remains fully live and that exact ritual continuity remains the operative solution. Outside the benefiting parties, comparative religion scholars and sociologists of American and Israeli Jewish life have documented (via demographic and identity-retention studies) that ethical, historical, and communal continuity persist substantially among Jews with attenuated ritual practice, and Reform/Reconstructionist theologians — who do not benefit from ritual-gatekeeping authority and in fact lose standing under it — corroborate that the founding problem has been at least partially resolved by alternative means. No fully disinterested third party outside all denominational stakes has adjudicated the matter; the corroboration available is cross-denominational rather than external to Judaism as a whole.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is authored high because the doctrine's coordination function (a genuine identity-continuity good) is bundled with an enforcement apparatus — certification, gatekeeping on marriage/conversion/kashrut — that channels legitimacy and resources toward those who administer correct form, at cost to those who diverge from it. Suppression (0.58) reflects real but partial structural force: exclusion from ritual recognition and communal standing, not physical coercion, and softened by the availability of alternative denominational readings (even though those readings are marginalized within the specific institutions this story tracks). Theater ratio rises over the interval (0.22 to 0.42) reflecting increasing performative emphasis on exact form as actual demographic engagement with full observance has declined — a Goodhart-style substitution of visible compliance markers for the underlying continuity the doctrine claims to secure. Accessibility collapse is moderate (0.5): alternative Jewish-identity frameworks (Reform, cultural, secular) genuinely exist and are practiced by millions, so alternatives have not collapsed, but within the specific institutions that adjudicate 'authentic' survival, exit from the form-standard is costly.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/institutional seat, the arrangement is experienced as the coordination mechanism that held a scattered people together across millennia of rupture — a rope, in the seat's own framing. From the payer seats, the same structure computes as tangled: real coordination benefit exists (shared identity, historical continuity) but riding alongside it is an enforcement layer that assigns survival-failure status to people whose lived continuity does not match the certified form. The engine should register this divergence structurally rather than resolve it toward either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and orthodox institutional bodies sit at the beneficiary end: they administer the standard, collect legitimacy and resources from applying it, and have durable exit options (their authority persists regardless of any individual case). Secularized Jews, intermarried families, and diaspora youth sit toward the target end: they bear the cost of being measured against an exactness standard that does not credit the continuity they do maintain, and their exit options are constrained (leaving the framework typically means leaving recognized communal status, not a costless alternative). Reform and Reconstructionist movements are marked excluded rather than beneficiary/payer because their status is about voice-suppression in the interpretive contest, not direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identity continuity across catastrophic rupture without land or sovereignty) is genuinely old and was genuinely load-bearing historically. Under this reading its status is contested rather than resolved: rabbinic authorities hold it fully live; outside corroboration (demographic and sociological study, cross-denominational testimony) suggests the practical work of the doctrine has been substantially replaced by other mechanisms — legal emancipation, diaspora communal infrastructure, historical consciousness independent of ritual exactness — while the exactness standard itself persists and hardens, which is the classic mandatrophy signature: the founding problem's original shape has shifted but the arrangement that grew up to solve it has not correspondingly relaxed its enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_vs_substance_survival_test,
    'Does Jewish communal survival actually depend on exact ritual form continuity, or does it depend on the transmission of ethical/historical/practical content that ritual form merely happens to carry in this reading''s framing?',
    'Comparative demographic and identity-retention studies across communities with varying ritual observance rates, tracking whether communal cohesion, ethical transmission, and group identity persistence correlate more strongly with exact form-continuity or with other transmission channels (education, historical memory, communal institutions).',
    'If content-transmission substantially explains persistence independent of exact form, the symbol_survival_reading''s central premise is undermined and the constraint would be better described by the competence_transmission_reading or hybrid_encoding_reading, which would substantially lower authored extractiveness by removing the interpretive-gatekeeping justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_vs_substance_survival_test, empirical, 'Whether exact ritual form is the actual survival mechanism or a proxy for content transmission.').

omega_variable(
    kernel_reading_selection_warrant,
    'Is the symbol_survival_reading the historically dominant or the institutionally self-serving reading of the catastrophe_memory_survival kernel among the three declared readings?',
    'Textual and historiographical analysis of how rabbinic authorities across different eras (post-Temple, post-expulsion, post-Holocaust) actually justified ritual continuity — whether appeals were predominantly symbolic/boundary-based or practical/competence-based — cross-referenced against which reading each era''s dominant institutions had material incentive to hold.',
    'If the symbol-survival framing is shown to be a comparatively recent institutional emphasis rather than the kernel''s original or most historically supported reading, this constraint''s claim to represent ''the'' authentic account weakens relative to its siblings, though the constraint itself remains valid as one reading among the declared set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_warrant, conceptual, 'Whether this reading''s dominance reflects historical accuracy or institutional self-interest in the reading contest.').

omega_variable(
    rabbinic_authority_natural_vs_constructed,
    'Is rabbinic interpretive authority over what counts as authentic ritual continuity a naturally emergent feature of a text-and-practice-based religious tradition, or a constructed gatekeeping structure that could be reorganized without loss to the tradition''s actual content?',
    'Comparative study of Jewish communities and eras with more distributed or contested rabbinic authority structures (e.g., pre-modern regional variation, contemporary pluralist communities) to see whether communal continuity was maintained without centralized interpretive gatekeeping.',
    'If authority is shown to be substantially constructed rather than naturally necessary, the beneficiary structure named in this story is better read as the primary driver of extraction rather than an incidental feature of a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_natural_vs_constructed, conceptual, 'Whether rabbinic gatekeeping authority is intrinsic to ritual coordination or a separable extractive layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(cata_tr_t45, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 45, 0.36).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(cata_be_t45, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cata_su_t45, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_survival kernel. competence_transmission_reading treats ritual as low-extraction practical-knowledge transmission with no interpretive gatekeeper required; hybrid_encoding_reading treats survival as dependent on both symbolic and practical registers simultaneously, producing intermediate extraction. This reading (symbol_survival_reading) is authored as the highest-ε member of the family because it is the only reading in which an identifiable authority (rabbinic interpretive authority) becomes structurally indispensable to the doctrine's own truth, generating a concentrated beneficiary and a diffuse victim set. All three should be linked via affects_constraints since institutional shifts toward or away from strict form-continuity in practice directly affect the plausibility and resourcing available to the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
