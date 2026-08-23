% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Feudal Obsolescence Reading
 *   domain: constitutional_history/legal_philosophy
 *
 * SUMMARY:
 *   The Magna Carta authority constraint, read from the feudal obsolescence
 *   seat, treats the charter as an atrophied thirteenth-century baronial
 *   compact whose feudal context has fully dissolved, leaving no binding
 *   authority over modern parliamentary or executive sovereignty. The
 *   constraint persists as constitutional theater: it is ritually invoked by
 *   courts, cited by parliamentarians, and revered by popular
 *   constitutionalists, but its functional restraint has atrophied to the
 *   point where modern executive discretion operates effectively unchecked by
 *   it. This reading instantiates one position in a three-way kernel dispute
 *   with living constitutionalism (which claims inherited binding force) and
 *   parliamentary sovereignty (which claims absorptive statutory continuity).
 *   The expected structural delta is realized: popular constitutionalism and
 *   juridical restraint enter the victim set because they invest normative
 *   energy in an empty symbol, while executive discretion is maximized by the
 *   absence of genuine constitutional limitation.
 *
 * KEY AGENTS:
 *   - uk_parliament: Agenda-setter (institutional/constrained) â administers the uncodified constitution and could repeal the remaining statutory clauses
 *   - popular_constitutionalists: Primary target (moderate/identity_locked) â bear the cost of misplaced faith in ancient liberty
 *   - common_law_judiciary: Secondary target (organized/constrained) â legitimate modern rulings through atrophied precedent
 *   - executive_government: Analytical observer (powerful/mobile) â benefits from the vacuum of effective restraint but does not capture the constraint's extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.45).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Feudal Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'cb8c4e70-80a7-4ae3-930e-0d299955d46f').
narrative_ontology:cs_kernel_codification('cb8c4e70-80a7-4ae3-930e-0d299955d46f', fixed_text).
narrative_ontology:cs_authority_grounding('cb8c4e70-80a7-4ae3-930e-0d299955d46f', lineage).
narrative_ontology:cs_interpretation_layer_present('cb8c4e70-80a7-4ae3-930e-0d299955d46f').
narrative_ontology:cs_reading_relation('cb8c4e70-80a7-4ae3-930e-0d299955d46f', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('cb8c4e70-80a7-4ae3-930e-0d299955d46f', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('cb8c4e70-80a7-4ae3-930e-0d299955d46f', foundational, feudal_compact_obsolescence).
narrative_ontology:cs_axiom_status(feudal_compact_obsolescence, holdable).
narrative_ontology:cs_axiom_grounding('cb8c4e70-80a7-4ae3-930e-0d299955d46f', feudal_compact_obsolescence, empirically_contingent).
narrative_ontology:cs_axiom('cb8c4e70-80a7-4ae3-930e-0d299955d46f', foundational, modern_sovereignty_incompatibility).
narrative_ontology:cs_axiom_status(modern_sovereignty_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('cb8c4e70-80a7-4ae3-930e-0d299955d46f', modern_sovereignty_incompatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('cb8c4e70-80a7-4ae3-930e-0d299955d46f', feudal_baronial_compact).
narrative_ontology:cs_drift_state('cb8c4e70-80a7-4ae3-930e-0d299955d46f', contemporary_constitutional_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cb8c4e70-80a7-4ae3-930e-0d299955d46f', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the uncodified constitutional order in which Magna Carta remains formally on the statute book and ritually invoked. Could repeal or codify the remaining clauses but incurs political cost for disturbing constitutional symbolism. Administers the constraint's persistence without directly extracting from it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, uk_parliament, agenda_setter,
    institutional, generational, constrained, national).

% Invoke Magna Carta as a living guarantee of liberty against modern state overreach. Bear the cost of misplaced constitutional confidence â political energy and legitimacy are directed toward an atrophied 13th-century symbol rather than enforceable modern restraints. Exit is identity-locked because their political self-concept is fused with ancient-liberty narratives.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    moderate, biographical, identity_locked, national).

% Cite Magna Carta in judgments and legal reasoning as a source of continuing due-process restraint. Bear the professional cost of legitimizing modern rulings through a feudal text whose original tenurial and military context does not map onto contemporary administrative sovereignty. Exit is constrained by professional tradition, precedent-based reasoning, and the interpretive lineage of the common law.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_judiciary, payer,
    organized, generational, constrained, national).

% Operates with maximized discretion because the ancient restraint no longer binds modern sovereignty structures. Neither pays into nor collects from the constraint's maintenance; benefits from the vacuum of effective limitation that the atrophied symbol fails to provide.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_government, observer,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In 1215, coordinated the feudal relationship between Crown and barons by establishing mutual limits on arbitrary taxation, imprisonment, and feudal dues.
% TRANSFER_FUNCTION: Originally transferred predictability and procedural restraint from the Crown to the baronial class; now transfers misplaced constitutional legitimacy and normative energy from the general public and judiciary to an atrophied historical symbol.
% ABSENT_VOICES: Explicit constitutional codification movements, republican reformers, and legal modernists who would argue that thirteenth-century feudal instruments cannot bind twenty-first-century democratic sovereignty; they are marginalized in the uncodified constitutional tradition.
% DISAPPEARANCE_RATIONALE: If the symbolic authority of Magna Carta vanished overnight, popular constitutionalists would lose their primary anchor for liberty claims, the judiciary would need to re-ground due-process reasoning in modern statutory or international human-rights frameworks, and the uncodified constitution would face pressure toward explicit codification.
% FOUNDING_PROBLEM: Thirteenth-century baronial insecurity against arbitrary royal exaction, imprisonment without process, and violation of feudal custom.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians and legal historians outside the popular constitutionalist tradition attest that the charter addressed specific 1215 feudal grievances; the feudal military and tenurial context that gave the charter its force dissolved centuries ago. No corroboration from within the beneficiary set exists because no concentrated beneficiary is identified.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the atrophied constraint still exacts a real cost: constitutional energy, judicial reasoning capacity, and popular legitimacy are directed toward a thirteenth-century feudal instrument that cannot bind modern sovereignty. Theater_ratio (0.78) is high because the vast majority of modern Magna Carta invocation is performative â ceremonial citation, tourism, and patriotic ritual â rather than functional restraint. Suppression (0.45) is moderate: alternative constitutional framings (codified rights, republican checks) are not actively crushed but are marginalized by the gravitational pull of the ancient-symbol narrative. Resistance (0.35) is modest because the constraint is diffuse and the harm is cognitive and misdirectional rather than direct extraction. Accessibility_collapse (0.4) reflects that while alternatives exist, they are crowded out of constitutional discourse by the weight of the lineage claim. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary agenda-setter seat, Magna Carta is manageable constitutional furniture â inconvenient to disturb but not a primary tool. From the popular constitutionalist seat, it is a sacred bulwark; from this seat, its atrophy is invisible or denied. From the judicial seat, it is professional obligation and interpretive resource. The executive seat experiences neither cost nor benefit from the constraint itself, only freedom from its absence. The engine will compute different directionalities: payers (high d) experience the constraint as extractive theater; the agenda-setter (moderate d) experiences inertia; the observer (low d) experiences ambient unconcern.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because the piton structure characteristically lacks a concentrated capturer. The victims â popular_constitutionalists and common_law_judiciary â bear the diffuse costs of maintaining an atrophied symbol. The agenda_setter (uk_parliament) could fix the constraint at political cost but does not, consistent with piton inertia. The executive is structurally outside the constraint's directionality: they neither subsidize nor are targeted by it; the vacuum it leaves is an externality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved: the founding problem (thirteenth-century feudal grievance) is dead, the arrangement persists by inertia and theatrical maintenance, and the computed classification should land on piton if the theater_ratio and lack of beneficiary are honored. The classification prevents mislabeling this as a rope (it does not coordinate modern actors effectively) or a snare (there is no active coercive maintenance or concentrated beneficiary extracting from its operation). It is a textbook piton: a former coordination mechanism whose function has atrophied but whose shell remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    piton_vs_snare_ambiguity,
    'Is this constraint better understood as a piton (atrophied inertial restraint) or a snare (obsolescence claimed to enable executive extraction)?',
    'Examine whether executive actors actively cultivate the Magna Carta myth to defeat modern constitutional alternatives, or merely benefit passively from its atrophy.',
    'If active cultivation is found, reclassify as snare with executive as beneficiary; if passive inertia, remain piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_ambiguity, conceptual, 'Whether the atrophy is inertial or instrumentalized for extraction').

omega_variable(
    historical_obsolescence_empirics,
    'Does the historical evidence support the claim that Magna Carta was exclusively a feudal baronial compact with no trans-feudal intent?',
    'Archival and historiographical review of the 1215 context, subsequent reissues, and the charter''s reception in non-feudal eras.',
    'If the charter contained broader due-process language or was rapidly reissued with wider claims, the feudal obsolescence framing weakens and the constraint shifts toward living constitutionalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_obsolescence_empirics, empirical, 'Historical basis of the feudal-exclusive framing').

omega_variable(
    reading_sibling_foreclosure,
    'Does the feudal obsolescence reading foreclose living constitutionalism entirely, or do they coexist as incommensurable frameworks?',
    'Analyze whether any single legal framework can simultaneously treat Magna Carta as a dead feudal contract and as a binding source of inherited due process.',
    'If genuinely foreclosed, the engine should register strong axiom contradiction between the sibling readings; if coexisting, the contradiction is merely perspectival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Logical relationship between sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1350, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1350, 0.2).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(magn_tr_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1700, 0.5).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.65).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2025, 0.78).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1350, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1350, 0.25).
narrative_ontology:measurement(magn_be_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(magn_be_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1350, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1350, 0.25).
narrative_ontology:measurement(magn_su_t1500, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1500, 0.3).
narrative_ontology:measurement(magn_su_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1700, 0.35).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
