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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Liturgical Preservation Definition of Hebrew Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation_reading of the
 *   hebrew_linguistic_life kernel. It claims that Hebrew is alive precisely
 *   when its sacred texts are continuously recited, studied, and transmitted
 *   in an unbroken chain, regardless of vernacular use. Under this reading,
 *   Hebrew never died during the diaspora; Ben-Yehuda's modern revival
 *   project is not a resurrection but a desecration; and the victim of that
 *   desecration is the sacred tradition itself. The constraint is enforced by
 *   liturgical authorities who control textual education, ordination, and
 *   liturgical supervision, extracting deference and legitimacy from
 *   traditional diaspora communities while imposing costs on modern Hebrew
 *   speakers and the tradition they claim to protect. Two sibling readings
 *   exist: the native_generational_reading (mother-tongue daily use as the
 *   sole criterion) and the marketplace_pidgin_reading (inter-communal
 *   practical function as sufficient). Both are foreclosed by this reading's
 *   core premise that sacred transmission is the necessary and sufficient
 *   condition of linguistic life.
 *
 * KEY AGENTS:
 *   - liturgical_authorities: Agenda-setter (institutional/global) — controls textual transmission norms, education, and liturgical supervision.
 *   - traditional_diaspora_communities: Beneficiary (organized/global) — receives stable portable liturgical identity.
 *   - modern_hebrew_community: Payer (powerful/national) — daily vernacular use delegitimized under this definition.
 *   - sacred_tradition: Non-actor payer (powerless/global) — bears the cost of ossification and desecration framing.
 *   - zionist_linguists: Excluded (organized/national) — research and pedagogical frameworks structurally barred from legitimacy.
 *   - comparative_linguists: Observer (analytical/global) — external analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.72).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.8).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical Preservation Definition of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '94a02747-f9d4-48ab-98df-3aef1ed4b48e').
narrative_ontology:cs_kernel_codification('94a02747-f9d4-48ab-98df-3aef1ed4b48e', fixed_text).
narrative_ontology:cs_authority_grounding('94a02747-f9d4-48ab-98df-3aef1ed4b48e', lineage).
narrative_ontology:cs_interpretation_layer_present('94a02747-f9d4-48ab-98df-3aef1ed4b48e').
narrative_ontology:cs_reading_relation('94a02747-f9d4-48ab-98df-3aef1ed4b48e', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('94a02747-f9d4-48ab-98df-3aef1ed4b48e', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('94a02747-f9d4-48ab-98df-3aef1ed4b48e', foundational, liturgical_continuity_is_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_continuity_is_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('94a02747-f9d4-48ab-98df-3aef1ed4b48e', liturgical_continuity_is_linguistic_life, theological).
narrative_ontology:cs_axiom('94a02747-f9d4-48ab-98df-3aef1ed4b48e', foundational, vernacular_revival_is_desecration).
narrative_ontology:cs_axiom_status(vernacular_revival_is_desecration, holdable).
narrative_ontology:cs_axiom_grounding('94a02747-f9d4-48ab-98df-3aef1ed4b48e', vernacular_revival_is_desecration, theological).
narrative_ontology:cs_reference_frame('94a02747-f9d4-48ab-98df-3aef1ed4b48e', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('94a02747-f9d4-48ab-98df-3aef1ed4b48e', post_ben_yehuda_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('94a02747-f9d4-48ab-98df-3aef1ed4b48e', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_diaspora_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the norms of correct Hebrew textual transmission through yeshiva curricula, rabbinic ordination, and liturgical supervision. They define any Hebrew used outside sacred parameters as corrupt or illegitimate, and derive institutional authority from their role as guardians of the unbroken chain.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receive a stable, portable liturgical language that binds dispersed communities across geography and time. Their identity is fused with the maintenance of the textual chain; exit means assimilation and loss of communal coherence.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% Uses Hebrew for daily secular life, education, commerce, and statecraft. Under this constraint, their linguistic practice is classified as non-sacred and therefore irrelevant to whether Hebrew is alive; they bear the delegitimization of their vernacular as a desecration or at best a utilitarian tool.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_community, payer,
    powerful, biographical, constrained, national).

% The corpus of Hebrew scripture, liturgy, and legal texts, transmitted in an unbroken chain. It bears the cost of being treated as a static object rather than a living evolving tongue; its encounter with modernity is framed as desecration rather than organic growth, potentially ossifying its interpretive range.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

% Academic and pedagogical actors who treat modern Hebrew as a fully living language with native speakers. Their definition of linguistic life is structurally excluded from liturgical authority's framework; their research and curricula are treated as irrelevant or hostile to the sacred chain.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, zionist_linguists, excluded,
    organized, generational, mobile, national).

% External scholars who study language vitality using demographic and functional criteria. They observe the contest between liturgical and vernacular definitions without being bound to either, noting the structural extraction involved in monopolizing the definition of life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a globally unified liturgical Hebrew across dispersed Jewish communities by defining linguistic vitality as continuous sacred textual transmission, ensuring that ritual, legal, and scriptural competence remain interoperable regardless of geographic separation or vernacular shift.
% TRANSFER_FUNCTION: Moves authority to define legitimate language use from secular or generational users to liturgical gatekeepers; moves status and resources toward institutions that certify correct textual transmission and away from modernizing or vernacularizing projects.
% ABSENT_VOICES: Secular Zionist linguists, modern Hebrew-speaking parents, and marketplace practitioners who use Hebrew for non-sacred coordination are structurally excluded from the conversation about what counts as linguistic life; their practices are defined as irrelevant or desecrating by the constraint's framing.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation definition vanished, the global hierarchy of Hebrew legitimacy would shift: modern spoken Hebrew would be recognized as fully alive, Ben-Yehuda's project would be recast as continuity rather than desecration, and liturgical communities would lose their monopoly on defining linguistic vitality.
% FOUNDING_PROBLEM: Jewish diasporic dispersion threatened the loss of a shared sacred language capable of supporting ritual, legal, and scriptural continuity across communities separated by geography, vernacular, and political circumstance.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical authorities attest the problem remains live because vernacular assimilation constantly threatens sacred competence. Modern historians and sociolinguists outside the benefiting parties attest that the problem has transformed: the threat is no longer loss of Hebrew but the constraint's refusal to recognize modern Hebrew as legitimate continuity. Corroboration from secular Israeli academia and Zionist historiography supports the transformed-problem reading.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint monopolizes the definition of linguistic life, delegitimizing the daily speech of millions and channeling authority to a narrow institutional gatekeeper. Suppression (0.80) is higher still: the constraint actively suppresses rival definitions (native generational and marketplace) by framing them as desecration or irrelevance, and enforces this through control of education and communal boundary maintenance. Theater ratio (0.45) reflects the performative dimension of preserving a language solely for sacred recitation in communities where daily speech occurs in other languages; the maintenance is real but increasingly stylized. Accessibility collapse (0.70) is high because once the liturgical definition is accepted, modern Hebrew appears as a technical or secular tool rather than a living language. Resistance (0.60) reflects sustained pushback from Zionist educators, secular Israeli institutions, and linguists. The temporal series shows extraction and suppression ratcheting upward from 1880–2020 as the modern revival challenged liturgical authority, with theater accumulating as the defense of the unbroken chain became more performative.
 *
 * PERSPECTIVAL GAP:
 *   The liturgical authority seat experiences the constraint as necessary coordination preserving civilization-scale continuity; the modern Hebrew community seat experiences it as an atavistic extraction that denies their lived linguistic reality. The engine should compute these seats differently: the agenda-setter/beneficiary cluster derives low directionality and damped effective extraction, while the payer seats (modern community and sacred tradition) derive high directionality and amplified extraction. The excluded Zionist linguist seat would compute a contested absence — their resistance is present but structurally invisible to the liturgical framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical authorities are the concentrated beneficiaries of the constraint's operation: they collect status, institutional resources, and the power to define legitimacy, placing them near the full-beneficiary end (low d). Traditional diaspora communities are diffuse beneficiaries: they gain identity coherence but also pay in restricted linguistic range, placing them nearer symmetric but still beneficiary-side. Modern Hebrew speakers are the structural targets: the constraint extracts legitimacy from their vernacular practice and assigns it to sacred recitation, placing them near the full-target end (high d). Sacred tradition, though ostensibly protected, is coded here as a victim because the constraint's enforcement freezes the textual corpus and frames its encounter with modernity as desecration; as a non-actor it is excluded from directionality computation but is structurally coded as bearing cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling by preserving the genuine coordination function: the unbroken textual chain does solve a real diasporic coordination problem. However, the coordination is inseparable from extraction because the same structure that preserves texts across space also monopolizes the definition of life, actively suppresses modernizing alternatives, and names the tradition itself as victim. If the coordination were separable from the monopoly, it would be a Rope; if the coordination story were pure cover with no real transmission function, it would be a Snare. The Tangled Rope classification captures the hybrid accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacred_tradition_victimhood,
    'Is sacred tradition genuinely victimized by liturgical preservation, or is the true victim the modern Hebrew community whose vernacular is delegitimized?',
    'Comparative ethnography of communities that maintain liturgical Hebrew without suppressing modern Hebrew versus those that treat modern Hebrew as desecration; measure which group reports greater cultural harm.',
    'If sacred tradition is the primary victim, the constraint extracts from its own claimed beneficiary; if the modern community is the primary victim, the constraint operates as standard inter-group extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_victimhood, conceptual, 'Ambiguity about which seat bears the primary extraction cost.').

omega_variable(
    enforcement_mechanism_nature,
    'Is the suppression of modern Hebrew structural (institutional control of education and ordination) or internalized (communal identity-lock preventing acceptance of vernacular Hebrew)?',
    'Track acceptance rates of modern Hebrew in communities where institutional control is removed (e.g., online yeshivas outside rabbinic oversight); if rejection persists, suppression is partially internalized.',
    'Internalized suppression would raise effective extraction beyond the structural measure because the target carries the constraint after external barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the hebrew_linguistic_life kernel. How would classification shift under sibling readings?',
    'Generate sibling constraint stories and compare epsilon values and beneficiary/victim structures across the family.',
    'The native_generational reading would likely reclassify beneficiaries to secular Israeli parents and victims to liturgical authorities; the marketplace reading would distribute benefits to commercial mediators and costs to nationalist educators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Commitment-system decomposition of the linguistic-life kernel.').

omega_variable(
    revival_desecration_boundary,
    'Does Ben-Yehuda''s modern Hebrew project structurally desecrate the liturgical chain, or is the desecration narrative itself a defensive extraction mechanism?',
    'Historical analysis of liturgical authority reactions to modern Hebrew: whether the desecration framing precedes the revival (indicating independent theological commitment) or intensifies precisely as the revival threatens institutional authority (indicating defensive extraction).',
    'If the desecration framing is defensive, the constraint''s extractiveness is higher than its theological justification suggests; if independent, the extraction is partially the price of a genuine theological commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revival_desecration_boundary, empirical, 'Whether the desecration claim is theologically grounded or strategically defensive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_pres_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(heb_lit_pres_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(heb_lit_pres_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(heb_lit_pres_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(heb_lit_pres_tr_t80, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(heb_lit_pres_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(heb_lit_pres_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement(heb_lit_pres_tr_t140, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 140, 0.45).

% Extraction over time
narrative_ontology:measurement(heb_lit_pres_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(heb_lit_pres_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(heb_lit_pres_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(heb_lit_pres_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(heb_lit_pres_be_t80, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(heb_lit_pres_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.67).
narrative_ontology:measurement(heb_lit_pres_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.7).
narrative_ontology:measurement(heb_lit_pres_be_t140, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 140, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(heb_lit_pres_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(heb_lit_pres_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(heb_lit_pres_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(heb_lit_pres_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(heb_lit_pres_su_t80, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(heb_lit_pres_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(heb_lit_pres_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.78).
narrative_ontology:measurement(heb_lit_pres_su_t140, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 140, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_linguistic_life kernel. The kernel decomposes into three structurally distinct claims about what constitutes linguistic life, each with distinct beneficiary/victim structures and epsilon values. This reading treats sacred textual transmission as the sole criterion; siblings treat native generational acquisition or marketplace function as definitive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
