% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Authority Over Scriptural Interpretation (Tradition-Scripture Reading)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This story instantiates the tradition-scripture reading of the biblical
 *   authority kernel: scripture is held to require authoritative
 *   interpretation through tradition, with a magisterium (teaching office)
 *   guarding the deposit of faith against heterodox readings. This reading is
 *   structurally distinct from the sola scriptura reading (which holds
 *   scripture self-interpreting and denies the necessity of a mediating
 *   magisterium) and the conciliar reading (which locates authority in
 *   patristic consensus and ecumenical councils rather than an ongoing
 *   magisterial office). The tradition-scripture reading centralizes
 *   interpretive authority in an ordained hierarchy, ties sacramental grace
 *   to clerical mediation, and produces low doctrinal fragmentation at the
 *   cost of concentrated extraction from lay interpretive agency. Each
 *   reading is a separate constraint with its own epsilon; this file does not
 *   average across them.
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: institutional agenda-setter, sets and enforces doctrinal boundaries
 *   - ordained_clergy: organized beneficiary, sacramental gatekeeping at parish level
 *   - curial_theological_offices: institutional beneficiary, formalizes and defends interpretive boundaries
 *   - lay_interpretive_agency: powerless payer, forfeits direct scriptural access
 *   - vernacular_reform_movements: moderate-power payer, historically suppressed
 *   - dissenting_theologians: moderate-power payer, subject to censure
 *   - historians_of_doctrine: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.62).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.58).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Authority Over Scriptural Interpretation (Tradition-Scripture Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '0f802bc1-1689-47a6-9c07-65cd412b0143').
narrative_ontology:cs_kernel_codification('0f802bc1-1689-47a6-9c07-65cd412b0143', fixed_text).
narrative_ontology:cs_authority_grounding('0f802bc1-1689-47a6-9c07-65cd412b0143', lineage).
narrative_ontology:cs_interpretation_layer_present('0f802bc1-1689-47a6-9c07-65cd412b0143').
narrative_ontology:cs_reading_relation('0f802bc1-1689-47a6-9c07-65cd412b0143', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('0f802bc1-1689-47a6-9c07-65cd412b0143', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('0f802bc1-1689-47a6-9c07-65cd412b0143', foundational, scripture_requires_magisterial_mediation).
narrative_ontology:cs_axiom_status(scripture_requires_magisterial_mediation, holdable).
narrative_ontology:cs_axiom_grounding('0f802bc1-1689-47a6-9c07-65cd412b0143', scripture_requires_magisterial_mediation, conventional).
narrative_ontology:cs_axiom('0f802bc1-1689-47a6-9c07-65cd412b0143', foundational, sacraments_confer_grace_through_ordained_channels).
narrative_ontology:cs_axiom_status(sacraments_confer_grace_through_ordained_channels, holdable).
narrative_ontology:cs_axiom_grounding('0f802bc1-1689-47a6-9c07-65cd412b0143', sacraments_confer_grace_through_ordained_channels, theological).
narrative_ontology:cs_axiom('0f802bc1-1689-47a6-9c07-65cd412b0143', secondary, magisterium_possesses_unbroken_apostolic_succession).
narrative_ontology:cs_axiom_status(magisterium_possesses_unbroken_apostolic_succession, holdable).
narrative_ontology:cs_axiom_grounding('0f802bc1-1689-47a6-9c07-65cd412b0143', magisterium_possesses_unbroken_apostolic_succession, conventional).
narrative_ontology:cs_reference_frame('0f802bc1-1689-47a6-9c07-65cd412b0143', patristic_apostolic_deposit).
narrative_ontology:cs_drift_state('0f802bc1-1689-47a6-9c07-65cd412b0143', post_reformation_and_modern_biblical_criticism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0f802bc1-1689-47a6-9c07-65cd412b0143', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, ordained_clergy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, curial_theological_offices).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, vernacular_reform_movements).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, dissenting_theologians).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the teaching office (magisterium), determining which readings of scripture are authoritative and which are heretical. Convenes councils, issues definitive rulings, and controls the mechanisms (seminaries, censorship, excommunication) that enforce doctrinal boundaries. Its authority is self-perpetuating: it certifies its own successors.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administers sacraments held to be necessary channels of grace, positioning clergy as required intermediaries between laity and salvation. Local priests and confessors interpret doctrine for parishioners in daily practice, deriving social and material standing from this mediating role; leaving the clerical structure means losing both livelihood and sacramental authority.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ordained_clergy, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, ordained_clergy, agenda_setter).

% Staffs the doctrinal review bodies that examine texts, condemn errors, and formalize the boundaries of permissible interpretation. Their institutional survival depends on scripture remaining unintelligible without magisterial mediation; a self-interpreting text would dissolve their function.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, curial_theological_offices, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, curial_theological_offices, agenda_setter).

% Ordinary believers are told scripture cannot be safely read or interpreted apart from tradition and clerical guidance; vernacular translation and independent study are historically restricted or discouraged. They receive doctrine pre-interpreted and bear the cost of forfeited direct access to the text as a condition of remaining within the community of salvation.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, trapped, local).

% Groups pushing for vernacular scripture and lay access to interpretation are historically suppressed, marginalized, or forced into schism. Their exit is costly: leaving means excommunication, loss of sacramental standing, and social exclusion within Christendom.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, vernacular_reform_movements, payer,
    moderate, generational, constrained, regional).

% Scholars who propose readings at odds with magisterial teaching face censure, silencing, or condemnation. Their professional and spiritual standing depends on the same hierarchy they may be contesting, which limits genuine exit.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, regional).

% Study the historical formation of the magisterium-tradition-scripture relationship, comparing its claims of continuity against documentary evidence of doctrinal development and institutional consolidation over centuries.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, curial_theological_offices).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, centrally adjudicated reading of scripture across a vast and doctrinally diverse population, preventing the fragmentation that arises when many independent interpreters claim equal authority — genuinely useful for maintaining unity in belief and practice across languages, regions, and centuries.
% TRANSFER_FUNCTION: Moves interpretive authority and sacramental gatekeeping from individual believers to an ordained hierarchy; in material terms, moves tithes, sacramental fees, and institutional deference toward clergy and curial offices in exchange for mediated access to grace and doctrine.
% ABSENT_VOICES: Vernacular reform advocates, dissenting theologians, and lay readers who might argue scripture is sufficiently clear for direct engagement are structurally positioned outside the adjudicating body; their objections historically surface as heresy trials or schisms rather than as votes within the magisterium itself.
% DISAPPEARANCE_RATIONALE: If magisterial authority over interpretation vanished, lay access to vernacular scripture and independent theological reasoning would expand rapidly, sacramental mediation would lose its exclusive claim on grace, clerical economic and social structures dependent on interpretive gatekeeping would restructure substantially, and doctrinal plurality would likely increase sharply — much as occurred historically wherever this authority structure weakened or was rejected.
% FOUNDING_PROBLEM: Early Christian communities faced genuine interpretive chaos and competing claims to authentic apostolic teaching; a mechanism was needed to distinguish authentic transmission of apostolic faith from heterodox innovation and to preserve unity against schism.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy itself attests the problem remains live (ongoing risk of doctrinal error and fragmentation). Independent historians of early Christianity and comparative religion scholars, outside the beneficiary structure, corroborate that the founding problem (interpretive chaos, competing apostolic claims) was real in the patristic period, but many of the same scholars argue the mechanism that emerged progressively centralized power beyond what unity alone required, and that the deposit-of-faith framing itself developed historically rather than being given whole.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the genuine transfer of interpretive and sacramental authority away from the laity toward a hierarchy that derives material and social standing from being necessary. Suppression (0.58) is substantial but not maximal — it rests on excommunication, censure, and control of vernacular translation rather than direct physical coercion, and its intensity varies significantly by historical period (rising sharply through the medieval consolidation and softening somewhat as vernacular access and lay theology gained ground in later centuries, hence the suppression_requirement curve peaking near the historical high-water mark of magisterial control before easing). Theater ratio (0.28) is moderate-low because much of the interpretive apparatus performs a real coordination function (doctrinal consistency across a vast, linguistically diverse population) alongside its extractive overhead. Accessibility collapse (0.6) is high because once a believer accepts the deposit-of-faith framework, alternative interpretive paths are framed as spiritually dangerous, closing off exploration. Resistance (0.45) is moderate: reform movements and dissenting theologians persistently contest the arrangement, but centralized enforcement historically suppressed most organized alternatives within the same institutional framework.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal hierarchy's seat, this arrangement is coordination: a necessary bulwark against interpretive chaos and heresy, preserving unity of belief across a vast and diverse population. From the lay interpretive agency's seat, the same structure operates as an enforced transfer of interpretive agency and sacramental necessity, sustained by excommunication and vernacular restriction rather than by demonstrated interpretive incapacity on the laity's part. The engine computes these as different seat-level classifications from the same structural data; the claimed_type (tangled_rope) reflects the authoring judgment that both a genuine coordination function AND asymmetric extraction are present, which the divergent seat computations should confirm.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal hierarchy and curial offices are structural beneficiaries — they set the rules, administer enforcement, and collect deference, tithes, and institutional legitimacy from the arrangement; their exit options are effectively arbitrage-grade since they control the institution itself. Ordained clergy benefit from sacramental gatekeeping but are also somewhat bound within the hierarchy's own constraints (secondary agenda-setter role reflects genuine but subordinate authority). Lay interpretive agency sits at the target end: trapped exit options historically (leaving meant excommunication and social exclusion), bearing the cost of forfeited direct scriptural access. Vernacular reformers and dissenting theologians are targets whose moderate power let them organize resistance but whose exit remained costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine interpretive chaos and competing claims to apostolic authenticity in the early church — was real and is well-corroborated by historians outside the beneficiary structure. Whether that problem remains live in a form that requires the SAME concentrated magisterial solution centuries later, given that vernacular literacy, textual scholarship, and lay theological education have transformed the conditions under which the original problem existed, is exactly the contested status this story flags rather than resolves. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: treating the entire structure as pure extraction (ignoring the real, historically demonstrated coordination value of centralized doctrinal consistency) and treating it as natural/inevitable (ignoring the identifiable beneficiaries and victims and the active enforcement machinery required to sustain it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterium_necessity_vs_construction,
    'Is the magisterium a divinely instituted necessity for correctly transmitting apostolic teaching, or a historically constructed institution that consolidated interpretive authority progressively, using the genuine early interpretive chaos as justification for permanent centralization beyond what unity required?',
    'Historical-critical analysis of the documentary record of doctrinal development in the first several centuries, compared against the magisterium''s own claims of unbroken and complete continuity from apostolic times; comparison with how the conciliar and sola scriptura readings account for the same historical record.',
    'If constructed rather than necessary, the extraction attributed to lay interpretive agency is better characterized as institutional capture of a coordination problem rather than an unavoidable cost of achieving unity; if genuinely necessary, more of the measured extraction should be attributed to legitimate coordination overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterium_necessity_vs_construction, conceptual, 'Whether magisterial authority is theologically necessary or a historically contingent institutional consolidation.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the tradition-scripture reading''s core premise (scripture requires ongoing magisterial mediation) logically foreclose the sola scriptura reading''s core premise (scripture is self-sufficient), or can both be held as competing-but-coherent positions within a broader Christian tradition?',
    'Analysis of whether any single denominational framework has historically held both premises simultaneously without internal contradiction, versus whether the two premises have always sorted communities into mutually exclusive camps (Catholic/Orthodox vs. Protestant).',
    'If genuinely foreclosing, no single ecclesial body could coherently hold both readings, confirming these are structurally distinct constraints rather than points on a spectrum; if not foreclosing, the reading-relations declaration in cs_structure should be revised from forecloses to coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether tradition-scripture and sola scriptura readings are logically incompatible or merely competing emphases.').

omega_variable(
    lay_agency_forfeiture_measurement,
    'How much of the measured cost to lay interpretive agency reflects genuine loss of interpretive access (e.g., restricted vernacular translation, discouraged independent study) versus a benefit lay believers themselves valued (freedom from the burden of individually adjudicating complex theological questions)?',
    'Historical and sociological study of lay attitudes where documented (e.g., popular support for vernacular translation movements versus popular deference to clerical authority in different periods and regions).',
    'If largely a valued benefit rather than an imposed cost, the victim classification for lay_interpretive_agency should be softened toward a more symmetric directionality; if largely an imposed and resisted cost, the current victim classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_agency_forfeiture_measurement, empirical, 'Whether lay deference to magisterial interpretation was predominantly imposed or predominantly welcomed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__tradition_scripture_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__tradition_scripture_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__tradition_scripture_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__tradition_scripture_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__tradition_scripture_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__tradition_scripture_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__tradition_scripture_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__tradition_scripture_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__tradition_scripture_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__tradition_scripture_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__tradition_scripture_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__tradition_scripture_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the biblical_authority kernel: tradition_scripture_reading (this file, tangled_rope — high clerical extraction, low fragmentation), sola_scriptura_reading (higher doctrinal fragmentation, lower clerical mediation extraction, different beneficiary/victim structure), and conciliar_reading (authority in bounded historical councils and patristic consensus rather than an ongoing office, intermediate extraction profile). Each has its own epsilon and stakeholder structure per the epsilon-invariance principle; they are linked via network edges rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
