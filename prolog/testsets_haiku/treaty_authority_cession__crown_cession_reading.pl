% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty Cession of Sovereignty and Land (Crown Reading)
 *   domain: constitutional/colonial/indigenous
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the Treaty of
 *   Waitangi—the Crown cession reading, adopted in English law and Crown
 *   doctrine since 1840. In this reading, the English text controls
 *   interpretation; 'kāwanatanga' (governance/chieftainship in the Māori
 *   text) is translated and read as 'sovereignty'—a complete and permanent
 *   transfer of all legislative and executive authority to the British Crown.
 *   The Treaty is thus a completed legal act of cession, after which Māori
 *   iwi hold no independent authority; their land rights are subject to Crown
 *   alienation mechanisms; their people are subject to Crown law. This
 *   reading has been the official doctrine of the New Zealand Crown since
 *   1840 and remains embedded in courts, legislation, and institutional
 *   practice. However, this reading competes with alternative readings (the
 *   Māori-text-controlled rangatiratanga retention reading, and the
 *   retrospective snare-exposure reading that treats the textual divergence
 *   itself as an extraction mechanism). This story ONLY models the Crown
 *   cession reading as a clean, ε-invariant constraint; it does not arbitrate
 *   the contest. The alternative readings are separate constraint stories
 *   linked via the network.
 *
 * KEY AGENTS:
 *   - british_crown: institutional agenda-setter claiming absolute sovereignty over the territory and its peoples; collects political authority and institutional rents; maintains the reading through courts and precedent
 *   - settler_colonists: institutional beneficiary; acquire land, status, and wealth through Crown-validated property regimes; compounds over generations; never exit
 *   - maori_iwi_chiefs: powerful but trapped payers; signed the document believing in a different arrangement; lost political authority and faced land alienation; exit options limited to resistance or advocacy within Crown institutions
 *   - maori_iwi_populations: powerless trapped payers; carry multi-generational land loss and institutional exclusion; never parties to the original negotiation; exit via migration or cultural persistence outside Crown authority
 *   - rival_sovereignty_claimants: excluded but organized; legal scholars and Māori advocates arguing the Māori text means rangatiratanga was retained; their reading is systematically barred from official doctrine
 *   - crown_courts_and_judges: institutional agenda-setter with secondary observer role; interpret the Treaty through Crown legal framework; constrained from overturning the Crown's foundational reading; latterly acknowledge ambiguity but maintain jurisdictional supremacy
 *   - international_arbiters: analytical observers; UN human rights bodies and global legal scholars question the reading but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.79).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.71).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, mountain).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty Cession of Sovereignty and Land (Crown Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/colonial/indigenous").

domain_priors:emerges_naturally(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '6ddc4d93-50d9-4197-805c-5ed0aa0b4da9').
narrative_ontology:cs_kernel_codification('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', fixed_text).
narrative_ontology:cs_authority_grounding('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', extraction).
narrative_ontology:cs_interpretation_layer_present('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9').
narrative_ontology:cs_reading_relation('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', foundational, english_text_controls_treaty_meaning).
narrative_ontology:cs_axiom_status(english_text_controls_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', english_text_controls_treaty_meaning, conventional).
narrative_ontology:cs_axiom('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', foundational, kawanatanga_equals_absolute_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', kawanatanga_equals_absolute_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', secondary, crown_cession_completes_legal_transfer).
narrative_ontology:cs_axiom_status(crown_cession_completes_legal_transfer, holdable).
narrative_ontology:cs_axiom_grounding('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', crown_cession_completes_legal_transfer, conventional).
narrative_ontology:cs_reference_frame('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', crown_absolute_sovereignty).
narrative_ontology:cs_drift_state('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', contemporary_biculturalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ddc4d93-50d9-4197-805c-5ed0aa0b4da9', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, british_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_colonists).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_chiefs).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, ExtMetricName, E),
    domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(treaty_authority_cession__crown_cession_reading),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction score (0.79) reflects that the constraint transfers enormous value—political authority, land, legislative power, taxation revenue—from Māori to Crown/settlers, decoupled from any service cost that justifies the transfer. The suppression score (0.71) reflects sustained enforcement machinery: land alienation laws (Native Land Court, Crown purchase mechanisms), exclusion of rival readings from courts, legislative override of Māori interests, and the constraint's embedding in institutional practice across 186 years. Theater ratio (0.42) rises over time as the original justifications (preventing French colonization, preventing intertribal warfare) become moot, yet the Crown increasingly invokes symbolic recognition and consultation (the Treaty Settlements process, the Waitangi Tribunal) while maintaining the foundational cession claim—the machinery of recognition performs respect while enforcement persists. Accessibility collapse (0.68) is moderate: the constraint was presented in 1840 as a treaty (suggesting mutual agreement and exit options), not unilateral conquest, yet the power imbalance was enormous and chiefs signing the Māori text did not understand they were signing away sovereignty under the English reading. Resistance (0.58) reflects substantial historical and ongoing Māori resistance: armed uprisings (1860s-1870s), political advocacy, land-rights movements, and contemporary legal challenges—real resistance that the constraint must continuously suppress. The measurement series tracks 186 years: extractiveness rises as land alienation mechanisms intensify and legislative override becomes routine (1840-1990), stabilizing as contestation rises (1990-2026). Theater ratio rises sharply in the late 20th century as Crown recognition and consultation mechanisms proliferate alongside the foundational cession claim's remaining unchallenged.
 *
 * PERSPECTIVAL GAP:
 *   The Crown and settler seats experience this constraint as settled law, legitimate authority, and the foundation of prosperity and order. The Māori payer seats experience it as a reading that conflicts with what the Māori text was understood to mean, justified by an English-text-priority rule imposed unilaterally, and enforced through mechanisms (land alienation, legislative override, court exclusion of rival readings) that benefit one party continuously. The judges sit between: they interpret within the Crown's framework (cannot rule the Crown's authority void) but increasingly acknowledge the textual ambiguity. The Crown's framing as a natural law ('this is simply what the Treaty accomplished') derives from the long institutional embedding and the court system's structural inability to overturn it. The alternative readings remain excluded from official doctrine but rise in salience as Māori advocacy and international human rights scrutiny increase. The engine should compute very different effective extraction (χ) for the Crown (near-zero, as the constraint subsidizes Crown authority) versus Māori payers (very high, as the constraint extracts from them), driven by the directionality (d) computation from beneficiary/victim and exit-option asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown (institutional, arbitrage exit) is the structural beneficiary—the constraint transfers authority and rents to it; d near 0.0 (full beneficiary). Settler colonists (institutional, arbitrage exit) benefit continuously; d near 0.1 (strong beneficiary, though not the agenda-setter). Māori iwi chiefs (powerful but identity-locked to chiefly authority they thought they retained) are victims—they believed they were ceding governance but not sovereignty, and the Crown reading extinguishes their political role; d near 0.95 (strong target). Māori iwi populations (powerless, trapped by law and land dependency) are victims bearing multi-generational extraction; d near 1.0 (full target). Crown judges (institutional, constrained exit) sit near 0.5 (symmetric)—they defend the Crown reading but are constrained by the framework they operate within. International observers (analytical exit) sit at 0.5 (analytical-neutral). Rival sovereignty claimants (organized, trapped) are excluded but would be targets if admitted (d would be ~0.85, as they challenge the extraction claim itself). The directionality overrides should not be necessary if the beneficiary/victim declarations and exit options are correctly authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing French colonization, unifying legal systems, preventing intertribal conflict) is DEAD in 2026. French colonization was never a material threat post-1840; legal unification has been achieved; the constraint's justification has evaporated. Yet the constraint persists with high extractiveness (0.79) and increasing theater (0.42 in 2026 vs 0.08 in 1840). This is the classic mandatrophy signature: a constraint whose mandate has outlived its function, maintained by the beneficiary institutions through doctrine, precedent, and suppression of alternative readings. The Crown Settlements process (Treaty Settlements, Waitangi Tribunal) and symbolic recognition (Māori language revival support, consultation processes) are the theater—they perform respect for the Treaty while the foundational cession reading remains enforced and extraction continues. The constraint is NOT a piton (theater ratio is only 0.42, not the 0.6+ that would indicate mostly-performative maintenance), nor is it a rope (extraction is too high and asymmetric, beneficiaries are concentrated). It reads as TANGLED ROPE at risk of classification drift toward SNARE as contestation rises and the founding problem's death becomes undeniable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_translation_equivalence,
    'Did the Māori word ''kāwanatanga'' (as used by chiefs in the Māori text) carry the same meaning as the English word ''sovereignty'' (as used in the English text signed by the Crown)? Were they semantic equivalents in 1840 Māori and English legal thought?',
    'Historical analysis of contemporary Māori usage, missionary records, chiefly testimony (Hobson''s notes, letters from chiefs), and comparative linguistics. Analysis of what ''kāwanatanga'' meant in Māori political discourse (governance, chieftainship, authority to manage affairs) versus what ''sovereignty'' meant in English constitutional law (plenary, indivisible, permanent state authority).',
    'If NOT equivalent (kāwanatanga = limited governance, sovereignty = plenary cession), the Crown reading fails as a matter of contract interpretation (contra proferentem favors the Māori text''s narrower meaning, or mutual mistake doctrine applies). The entire property regime and legislative authority would become legally ambiguous. This omega routes directly to the rangatiratanga_retention_reading and the retrospective_snare_exposure readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_translation_equivalence, empirical, 'Whether ''kāwanatanga'' and ''sovereignty'' were semantically equivalent in 1840, or carried divergent meanings.').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the Crown cession reading a NATURAL FACT of what the Treaty says (emerges necessarily from the English text under standard interpretation rules), or is it a CONSTRUCTED READING (a particular interpretive choice, enforced by institutional power, that could be different)?',
    'Comparative constitutional law analysis: how other jurisdictions interpret treaties with text divergences (Canada, USA, Australia with indigenous treaties; EU/national conflicts); whether English-text priority is a rule of interpretation or a rule of power. Analysis of whether contra proferentem (ambiguity construed against the drafter) would produce a different reading. Whether courts have ever considered the Māori text as potentially controlling.',
    'If the reading is constructed (not natural), this constraint''s claimed_type (mountain) is false; it is actually a SNARE or TANGLED ROPE whose persistence depends on institutional enforcement (court doctrine, legislative override) rather than logical necessity. The FSM (false summit) detection and institutional reinforcement analysis would reclassify the constraint. If natural, the mountain claim holds and the extraction is the price of legal clarity and coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether Crown cession reading is a natural interpretation or an institutionally-enforced construction.').

omega_variable(
    founding_mandate_obsolescence_timing,
    'At what point did the founding problem (preventing French colonization, unifying legal systems, preventing intertribal conflict) become SOLVED or OBSOLETE? Is it live, dead, or contestable?',
    'Historical timeline: French colonization threat was minimal by ~1860; legal unification was substantially complete by 1900; intertribal warfare had been suppressed by 1870. The constraint persists with high extraction long after these problems are solved. Evidence of whether Crown institutions acknowledge the founding mandate as dead (they do not—courts still cite the original coordination justification).',
    'A dead founding mandate + high extractiveness + increasing theater ratio = strong mandatrophy signal. If the mandate is dead, the constraint should be reclassified as PITON (inertial, mostly theater) or SNARE (extraction decoupled from function, defended by beneficiary institutional power). The timing of mandate death is critical to the timeline of when the constraint shifted from coordination to rent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_mandate_obsolescence_timing, empirical, 'When the Treaty''s founding justifications became obsolete, and whether the constraint persists as mandatroph.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative readings (rangatiratanga_retention_reading, retrospective_snare_exposure) STRUCTURAL (legal doctrines and court rules exclude them) or INTERNALIZED (Māori have accepted the Crown reading as legitimate)? Or both?',
    'Observation of Māori political advocacy, court challenges, academic discourse, and polling data. If suppression is structural, it persists even when Māori and allies articulate the alternative reading loudly (courts still reject it). If internalized, Māori themselves defend the Crown reading. Mixed case: structural suppression is necessary because the alternative reading remains contested and would gain traction without active exclusion.',
    'If suppression is purely structural, removal of doctrinal barriers (courts accepting contra proferentem, legislative repeal of Crown-sovereignty-dependent laws) could shift the classification. If internalized, the suppression persists even without doctrinal enforcement. Evidence: contemporary Māori advocacy DOES articulate alternative readings; courts DO exclude them on doctrinal grounds—this suggests structural suppression is doing the work. The effective suppression is high because the structural barriers are strong.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of rival readings is structural or internalized in Māori acceptance.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the Crown cession reading LOGICALLY FORECLOSE the rangatiratanga retention reading within a single legal framework, or can both coexist as live contested positions?',
    'Analysis of whether accepting one reading requires rejecting the other. If the Māori text is genuinely ambiguous (as modern courts increasingly acknowledge), both readings can coexist in the sense that a court could adopt one without logical contradiction. If the readings are mutually exclusive (one claims full cession, the other claims partial authority retention), they cannot coexist in a single legal determination of the Treaty''s effect—though they CAN coexist as competing political positions held by different parties.',
    'If readings logically FORECLOSE each other, the Crown reading must eliminate the rangatiratanga reading through institutional power (courts, legislation) to maintain itself. If they coexist, the persistence of the Crown reading despite acknowledged ambiguity becomes a question of power, legitimacy, and institutional inertia rather than logical necessity. This omega determines the reading_relations classification in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether Crown and rangatiratanga readings logically foreclose each other or can coexist as contested positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__crown_cession_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__crown_cession_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__crown_cession_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(trea_tr_t1990, treaty_authority_cession__crown_cession_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__crown_cession_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.65).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1920, 0.76).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1960, 0.78).
narrative_ontology:measurement(trea_be_t1990, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2026, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.55).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1880, 0.63).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(trea_su_t1990, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1990, 0.71).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.18).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel divides into at least three structurally distinct constraint stories, each with its own ε and classification. This story (crown_cession_reading) reads the English text as controlling and the Treaty as completing a legal cession of sovereignty. The rangatiratanga_retention_reading reads the Māori text as controlling and the Treaty as establishing a partnership with Māori authority retained. The retrospective_snare_exposure reading treats the textual divergence itself as an extraction mechanism—the gap between the Māori text (which chiefs understood) and the English text (which the Crown asserted) becomes the machine of deception, and the constraint is reclassified as pure snare. Each reading produces different metrics, beneficiary/victim structures, and classifications. They are linked via network.affects_constraints because the adoption of one reading affects the credibility and institutional standing of the others. The Crown reading's persistence affects the availability of space for the rangatiratanga reading; the rangatiratanga reading's articulation threatens the Crown reading's naturalness; the retrospective_snare_exposure reading, if accepted, would collapse the Crown reading into an institutional illegitimacy claim. All three remain live constraint stories in the corpus; they are not perspectives on a single constraint but distinct instantiations of the same kernel under different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
