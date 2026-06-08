% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hybrid_reading
 *   human_readable: Hybrid Reading: Classical Latin via Medieval Transmission with Textual Correction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The hybrid reading of 'correct Latin' asserts that the Classical language
 *   is the form attested in surviving texts, recoverable through critical
 *   apparatus, but that medieval scribal transmission preserved the essential
 *   grammatical structure while introducing orthographic and lexical
 *   variations that are correctable through systematic comparison and
 *   emendation. This reading stakes a middle ground: rejecting both the pure
 *   continuity view (medieval forms ARE correct Latin, unbroken from
 *   antiquity) and the pure discontinuity view (medieval usage is so
 *   corrupted that only textual archaeology can recover the true form). The
 *   hybrid reading legitimates humanist textual scholarship as corrective
 *   work — not invention of new standards, but restoration of attested norms
 *   — while acknowledging that medieval practice preserved real linguistic
 *   continuity, not total degradation. This commitment to 'partial legitimacy
 *   with targeted reform' creates the tangled_rope structure: the medieval
 *   scribal tradition is simultaneously recognized as preserving continuous
 *   linguistic structure (coordination function) and devalued as containing
 *   errors requiring correction (extraction mechanism). The humanist reform
 *   authority both coordinates the recovery effort and consolidates
 *   gatekeeping power over what counts as correct. The extractiveness (0.38)
 *   reflects moderate asymmetry: medieval forms are not entirely
 *   delegitimized, but their authority is subordinated to textual evidence
 *   and humanist correction. The theater ratio (0.58) documents the rising
 *   performative content of the textual apparatus — as emendation practice
 *   matures, the elaborate justifications become increasingly theatrical,
 *   defending choices that are no longer empirically settled.
 *
 * KEY AGENTS:
 *   - Medieval Scribe and Living Transmission: Primary victim (powerless/trapped) — embodied linguistic practice is declared corrupt and subject to external correction with no mechanism for contestation
 *   - Humanist Reform Authority: Primary beneficiary (institutional/arbitrage) — controls the authority to define correctness, organize textual evidence, and legitimate emendations; captures prestige and gatekeeping power
 *   - University Grammarian: Secondary actor (moderate/constrained) — mediates between medieval and Classical norms, teaches both traditions, constrained by institutional dual loyalty but also benefits from mediator authority
 *   - Textual-Critical Apparatus: Organized coordination structure (organized/constrained) — the systematic machinery of collation, variant recording, and emendation justification; carries sunset logic (temporary scaffolding toward fixed canon)
 *   - Comparative-Historical Linguist (later): Observer with mobility (organized/mobile) — inherits textual apparatus but also has external evidence from Romance and Indo-European data; can bypass textual authority when comparative evidence conflicts
 *   - Medieval Transmission as Linguistic Practice: Not an agent but a victim status — the embodied rules, pronunciation, and written conventions passed through generations are revalued from authority to corrupted data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.38).
domain_priors:suppression_score(hybrid_reading, 0.42).
domain_priors:theater_ratio(hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_reading, "Hybrid Reading: Classical Latin via Medieval Transmission with Textual Correction").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_reading, '586edd24-518a-4cc6-a7ae-17b9bf29aa98').
narrative_ontology:cs_kernel_codification('586edd24-518a-4cc6-a7ae-17b9bf29aa98', fixed_text).
narrative_ontology:cs_authority_grounding('586edd24-518a-4cc6-a7ae-17b9bf29aa98', extraction).
narrative_ontology:cs_interpretation_layer_present('586edd24-518a-4cc6-a7ae-17b9bf29aa98').
narrative_ontology:cs_reading_relation('586edd24-518a-4cc6-a7ae-17b9bf29aa98', hybrid_reading__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('586edd24-518a-4cc6-a7ae-17b9bf29aa98', hybrid_reading__discontinuity_reading, influences).
narrative_ontology:cs_axiom('586edd24-518a-4cc6-a7ae-17b9bf29aa98', foundational, medieval_forms_partially_legitimate).
narrative_ontology:cs_axiom_status(medieval_forms_partially_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('586edd24-518a-4cc6-a7ae-17b9bf29aa98', medieval_forms_partially_legitimate, empirically_contingent).
narrative_ontology:cs_axiom('586edd24-518a-4cc6-a7ae-17b9bf29aa98', foundational, textual_evidence_decisive_for_correction).
narrative_ontology:cs_axiom_status(textual_evidence_decisive_for_correction, holdable).
narrative_ontology:cs_axiom_grounding('586edd24-518a-4cc6-a7ae-17b9bf29aa98', textual_evidence_decisive_for_correction, empirically_contingent).
narrative_ontology:cs_reference_frame('586edd24-518a-4cc6-a7ae-17b9bf29aa98', classical_norms_recovered_through_textual_apparatus).
narrative_ontology:cs_drift_state('586edd24-518a-4cc6-a7ae-17b9bf29aa98', mature_humanist_scholarship, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('586edd24-518a-4cc6-a7ae-17b9bf29aa98', '2025-01-15T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, humanist_reform_movement).
narrative_ontology:constraint_beneficiary(hybrid_reading, textual_scholarship_authority).
narrative_ontology:constraint_victim(hybrid_reading, medieval_scribal_tradition).
narrative_ontology:constraint_victim(hybrid_reading, living_usage_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBAL TRADITION (SNARE) — The scribe's embodied practice (morphological rules, orthographic conventions, pronunciation habits passed through monastery and cathedral school) is deemed corrupt and unreliable. No mechanism to contest the dismissal of living transmission. Exit is unavailable: the tradition either submits to correction or is declared invalid. Maximum extraction: the scribe's labor is valuated only insofar as manuscripts serve as evidence for Classical recovery, not as legitimate linguistic continuity.
constraint_indexing:constraint_classification(hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNIVERSITY GRAMMARIAN (TANGLED ROPE) — The grammarian coordinates the curriculum: teaching both the medieval rules (necessary for reading existing texts) and the corrected Classical forms (necessary for humanist prestige and reform authority). Constrained by institutional loyalty to both traditions but also benefits from the hybrid framework — they become mediator and arbiter of correction. Some agency through pedagogical control, but caught between two legitimacy claims.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANIST REFORM AUTHORITY (ROPE) — Experiences the constraint as coordination: the project of recovering Classical norms requires organizing textual evidence, assembling emendation apparatus, and distributing corrected forms through the learned community. Net beneficiary through authority over textual standards and pedagogical prestige. Arbitrage option: can choose which texts to privilege, which corrections to promote, which transmissions to treat as authoritative. The hybrid reading legitimates their corrective activity as restoration, not innovation.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TEXTUAL-CRITICAL APPARATUS (SCAFFOLD) — The organized practice of collating manuscripts, recording variant readings, and documenting emendation justifications is presented as transitional: a temporary scaffolding that will eventually yield to a stabilized, fully recovered Classical corpus. The apparatus itself carries a sunset clause — once the authoritative Classical text is established, the apparatus theoretically becomes dispensable. Constrained by the labor required for thorough collation, but sees itself as enabling a future state of settled knowledge.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL CANON AS INSTITUTIONAL PERFORMANCE (PITON) — The declared authority and immutability of the Classical text (as recovered and transmitted through the humanist apparatus) is largely performative by the time of mature textual scholarship. The canon is defended through elaborate apparatus justifications and scholarly ritual, but the foundational empirical claim — that there exists a recoverable, stable Classical norm — remains contested. The piton represents degraded function maintained theatrically: the canonical text persists as institutional authority precisely because alternatives haven't fully replaced it, not because the recovery is epistemically settled.
constraint_indexing:constraint_classification(hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPARATIVE-HISTORICAL LINGUIST (TANGLED ROPE) — The later comparative philologist (18th-19th centuries onward) both inherits and subverts the hybrid reading. They benefit from the textual apparatus and the organized recovery of Classical forms as data for comparative reconstruction. But they also have epistemic mobility: comparative evidence from Romance and other language families provides alternative ways to reconstruct Latin structure, bypassing dependence on any single 'correct' manuscript tradition. This perspective sees some coordination (organizing evidence) but also asymmetric extraction: the humanist authority's claimed monopoly on correctness is undermined by external data.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - NATURALIZATION OF TEXT (MOUNTAIN) — From a civilizational, universal analytical perspective, the hybrid reading risks naturalizing the contingent decision to privilege written textual evidence over living transmission as if it were an inevitable feature of how language works. The 'corrupted' medieval forms are treated as deviations from a natural, correct Classical state. But this naturalizes a choice: treating the attested text as the ground truth and living speech as corruption, rather than treating living speech (medieval usage) as the primary data and text as an artifact of transmission. The engine's false-summit detector flags this as a naturalization of a reading-specific commitment.
constraint_indexing:constraint_classification(hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The hybrid reading permits moderate extraction because it formally acknowledges medieval transmission while substantively subordinating it. The medieval scribe's work is praised as 'preserving continuity' but devalued as 'introducing errors.' This is extraction masked as recognition: the tradition is credited with maintaining structure but not entrusted with authority. The value increases over time (0.28 → 0.42) as humanist textual scholarship matures and the burden of proof shifts — early on, the hybrid reading must justify why medieval forms should be corrected; later, the apparatus simply asserts correction as obvious. Suppression (0.42): Moderate. The mechanisms suppressing medieval transmission authority include: institutional gatekeeping (only humanist-trained scholars control canonical texts), publication control (corrected texts displace manuscripts), pedagogical subordination (medieval rules taught as archaic variants of Classical truth), and authority concentration (textual scholarship licenses decision-making power). But suppression is not total — medieval texts remain available, some schools continue traditional teaching, and the hybrid reading does not entirely deny medieval legitimacy. Theater ratio (0.58): Moderate-high and rising. The earliest humanist emendations (15th century) are driven by genuine manuscript comparison and real corrections (0.35). But as the practice matures, the textual apparatus becomes increasingly performative — elaborate justifications defend choices that are no longer empirically determined by evidence alone. By the mature scholarship of the 16th-17th centuries, the theater rises to 0.58 as the apparatus defends an increasingly settled canon through ritualized scholarly debate rather than fundamental evidentiary advances.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals how the same structural data produces radically different classifications depending on observer position. The medieval scribe experiences snare (extraction with no exit). The humanist sees rope (coordination and legitimate authority). The university grammarian navigates tangled rope (caught between two legitimacy claims). The comparative linguist later sees tangled rope with mobility (the apparatus is useful but not authoritative because external evidence provides alternatives). The analytical observer risks seeing mountain (naturalizing the textual authority as inherent to how language works) until the engine's false-summit detection reveals this as a reading-specific commitment. The perspectival gap centers on whether medieval transmission is legitimate linguistic continuity (medieval scribe's view) or corruption requiring correction (humanist view) — the data does not adjudicate between these frames; the reading chooses which evidence to privilege.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is computed from their structural relationship to the extraction flow. The medieval scribe starts with low structural power (trapped, regional scope) and is the target of correction: high d. The humanist reform authority has institutional power and arbitrage options (can choose which texts to privilege): low d. The university grammarian is moderate power with constrained options and mixed beneficiary/victim status: middle d. Comparative linguists later have organized power and mobile exit options: low-to-moderate d. The engine applies the sigmoid f(d) to produce experienced extractiveness; high-d agents (medieval scribes) experience high chi; low-d agents (humanist authority) experience negative chi (subsidy).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves the mandatrophy by claiming that both correction and continuity are structurally real: medieval forms preserve genuine linguistic structure (mandated by continuity logic) while also containing errors correctable against textual evidence (mandated by reform logic). The constraint's function is simultaneously coordination (organizing recovery of Classical forms, enabling pedagogical curriculum) and extraction (consolidating textual-scholarship authority, displacing medieval gatekeeping). This dual function is not unstable — it is exactly what tangled rope is. The potential mandatrophy emerges if one side of the dual function atrophies: if textual evidence proves insufficient to support the corrections (extraction without genuine coordination) or if comparative evidence entirely bypasses the textual apparatus (coordination without the extraction). The measurements show theater_ratio rising, suggesting the apparatus is becoming more performative; this is consistent with incipient function atrophy but not yet critical. The hybrid reading can persist as long as the apparatus is perceived as coordinating a real recovery, even if that coordination increasingly relies on ritual rather than evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_evidence_sufficiency,
    'Is textual evidence sufficient to recover a stable Classical norm, or does variant evidence from multiple transmission lines indicate irreducible plurality in the Classical source?',
    'Comparative analysis of manuscript families and variant readings; examination of whether emendation produces convergence to a single form or reveals systematic bifurcation',
    'If sufficient: hybrid reading is vindicated — Medieval forms are correctable against a recoverable standard. If insufficient: plural readings coexist and the hybrid reading naturalizes one choice among many.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_evidence_sufficiency, empirical, 'Whether textual evidence supports recovery of a single Classical norm').

omega_variable(
    medieval_transmission_legitimacy,
    'Do medieval scribal practices represent a degradation of Classical norms, or legitimate linguistic evolution and adaptation that preserves core morphosyntactic structure while modernizing orthography and vocabulary?',
    'Structural analysis of medieval forms using modern linguistic methods; assessment of whether morphosyntactic rules are preserved while surface forms diverge; examination of whether medieval changes follow predictable patterns of language change rather than random corruption',
    'If degradation: hybrid reading stands — correction removes corruption. If legitimate evolution: medieval forms are authoritative linguistic data, not corruption to be corrected; the asymmetry of correction becomes contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_transmission_legitimacy, empirical, 'Status of medieval scribal practice relative to Classical norms').

omega_variable(
    reform_direction_vs_discovery,
    'Is humanist textual correction a discovery of pre-existing Classical forms, or a creative choice about which variant forms to privilege as authoritative, dressed in the language of recovery?',
    'Historical analysis of humanist emendation choices; comparison with parallel transmission lines and variant traditions; examination of whether corrections are deterministic from evidence or involve discretionary selection among legitimate alternatives',
    'If discovery: the hybrid reading''s asymmetry is justified — Medieval forms are indeed corruptions being corrected to truth. If creative choice: the extraction is masked as restoration; the medieval tradition becomes a victim of authority assertion rather than empirical correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_direction_vs_discovery, conceptual, 'Whether humanist correction discovers or constructs the Classical norm').

omega_variable(
    kernel_contest_live,
    'What is ''correct Latin''? Is it: (a) the forms attested in surviving Classical texts, recoverable through critical apparatus; (b) the living usage transmitted through medieval monastery and cathedral school, corrupted but continuous; or (c) some reconstructed ideal form that may have never been uniformly realized?',
    'This is the organizing question of the kernel. Resolution does not come from empirical evidence alone but from a choice about which evidence to privilege and how to weight textual authority against transmission continuity.',
    'The hybrid reading answers: ''(a), corrected against (b)'' — Classical forms from texts, refined through comparison with medieval attestation. But sibling readings prioritize (b) or (c) differently. The three readings do not empirically refute each other; they prioritize different sources of authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_live, conceptual, 'Fundamental contest over the definition of correct Latin').

omega_variable(
    extraction_mechanism_masked,
    'Does the hybrid reading''s claim to ''correction'' mask the extraction mechanism: the erasure of medieval transmission authority and the consolidation of textual-scholarship power under humanist control?',
    'Historical examination of who controlled textual resources pre- and post-reform; tracking of institutional power shifts as textual scholarship authority displaced monastic gatekeeping; analysis of whose interests were served by shifting authority from living transmission to recovered texts',
    'If largely masked: the tangled_rope classification holds — genuine coordination (organizing textual evidence) coexists with real extraction (consolidating authority). If fully exposed: the constraint reclassifies toward snare for medieval actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_masked, empirical, 'Degree to which correction-language masks power consolidation').

omega_variable(
    comparative_linguistics_revision,
    'Do later comparative-historical methods (Romance language evidence, Indo-European reconstruction) revise the humanist recovery, showing it to have been incomplete or misdirected?',
    'Comparison of humanist-recovered Classical forms with forms reconstructed by comparative methods; identification of cases where textual apparatus was mistaken and external evidence shows the error',
    'If substantial revision: the textual apparatus''s authority is retrospectively undermined; the hybrid reading''s claim to finality is falsified. If minor refinement: the apparatus is vindicated as fundamentally sound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comparative_linguistics_revision, empirical, 'Extent of later revision to humanist Classical recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_tr_t0, hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hybrid_tr_t50, hybrid_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(hybrid_tr_t100, hybrid_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement(hybrid_tr_t150, hybrid_reading, theater_ratio, 150, 0.62).
narrative_ontology:measurement(hybrid_tr_t200, hybrid_reading, theater_ratio, 200, 0.65).

% Extraction over time
narrative_ontology:measurement(hybrid_be_t0, hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hybrid_be_t50, hybrid_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(hybrid_be_t100, hybrid_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(hybrid_be_t150, hybrid_reading, base_extractiveness, 150, 0.4).
narrative_ontology:measurement(hybrid_be_t200, hybrid_reading, base_extractiveness, 200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_su_t0, hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hybrid_su_t100, hybrid_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement(hybrid_su_t200, hybrid_reading, suppression_requirement, 200, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, information_standard).
narrative_ontology:affects_constraint(hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, discontinuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, textual_apparatus_authority).
narrative_ontology:affects_constraint(hybrid_reading, medieval_grammar_legitimacy).

% DUAL FORMULATION NOTE:
% The hybrid reading is the central member of the correct_latin constraint family. It is constrained by and constrains both the continuity_reading (which denies the need for correction) and the discontinuity_reading (which denies the legitimacy of medieval transmission). The hybrid reading also affects constraints in the network of textual-apparatus authority and medieval-grammar legitimacy, which inherit the hybrid reading's commitment that medieval forms are partially legitimate but subject to correction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
