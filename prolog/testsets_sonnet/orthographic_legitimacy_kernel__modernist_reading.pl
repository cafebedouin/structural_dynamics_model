% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Latin Script Reform as Civilizational Rupture (Modernist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story instantiates the modernist reading of the orthographic
 *   legitimacy kernel: script reform is understood as constitutive of a
 *   civilizational rupture from an Ottoman/Islamic past and alignment with
 *   Western/European modernity, not merely a technical literacy-efficiency
 *   measure and not fundamentally about preserving textual continuity. Under
 *   this reading the extraction is high and specifically targeted: the
 *   Ottoman-trained literate class and religious scholars are disqualified
 *   not incidentally but because their disqualification IS the demonstration
 *   of rupture the reform exists to perform. The instrumentalist reading
 *   (efficiency-maximization) and the continuity reading
 *   (tradition-preservation) are separate constraints with their own ε values
 *   and stakeholder structures — they are not blended into this one.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — decrees and enforces the rupture, collects legitimacy dividend
 *   - ottoman_literate_class: payer (moderate/trapped) — professional capital erased overnight
 *   - religious_scholars_ulema: payer (moderate/identity_locked) — authority recoded as backward by the rupture framing itself
 *   - older_generation_arabic_script_literates: payer (powerless/trapped) — quietly reclassified as illiterate
 *   - future_national_citizens: beneficiary (moderate/analytical) — inherit the new script cost-free
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.72).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.68).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Latin Script Reform as Civilizational Rupture (Modernist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, 'e76f43cf-c6aa-443b-b32c-7a903e2e3ef4').
narrative_ontology:cs_kernel_codification('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', formalized).
narrative_ontology:cs_authority_grounding('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', extraction).
narrative_ontology:cs_interpretation_layer_present('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4').
narrative_ontology:cs_reading_relation('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', foundational, script_change_as_constitutive_rupture).
narrative_ontology:cs_axiom_status(script_change_as_constitutive_rupture, holdable).
narrative_ontology:cs_axiom_grounding('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', script_change_as_constitutive_rupture, conventional).
narrative_ontology:cs_axiom('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', foundational, western_alignment_as_civilizational_progress).
narrative_ontology:cs_axiom_status(western_alignment_as_civilizational_progress, holdable).
narrative_ontology:cs_axiom_grounding('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', western_alignment_as_civilizational_progress, conventional).
narrative_ontology:cs_axiom('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', secondary, ottoman_islamic_tradition_as_backwardness_marker).
narrative_ontology:cs_axiom_status(ottoman_islamic_tradition_as_backwardness_marker, holdable).
narrative_ontology:cs_axiom_grounding('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', ottoman_islamic_tradition_as_backwardness_marker, conventional).
narrative_ontology:cs_reference_frame('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', ottoman_islamic_textual_continuity).
narrative_ontology:cs_drift_state('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', post_reform_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e76f43cf-c6aa-443b-b32c-7a903e2e3ef4', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, new_literate_bureaucratic_class).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_educational_reformers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, older_generation_arabic_script_literates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, future_national_citizens).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, civilizational_rupture_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, westward_orientation_as_progress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the new Latin-based alphabet by decree, criminalizes continued use of the old script in official and eventually most public contexts, and builds the entire apparatus of legitimacy — schools, print, law, civil service exams — around the new orthography. Frames the change as rupture from an Ottoman/Islamic past and alignment with a Western future; collects the legitimacy dividend of appearing modern to European powers and domestic secular constituencies.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, beneficiary).

% Young, often urban cohort who learn the new script first and fastest, capturing state jobs, teaching positions, and publishing opportunities that open specifically because the old literate class is disqualified overnight. Their advantage is not merit accumulated over decades but a generational accident of timing relative to the decree.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, new_literate_bureaucratic_class, beneficiary,
    organized, biographical, mobile, national).

% Intellectuals and reform-minded officials who advocated the change as vindication of a broader secularizing, Europeanizing program. Their prestige and institutional standing rise directly with the reform's success; they administer new curricula and control what counts as legitimate scholarship going forward.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_educational_reformers, beneficiary,
    powerful, generational, mobile, national).

% Clerks, notaries, and administrators whose entire professional capital was fluency in the old script. Overnight they become functionally illiterate in the eyes of the new state; re-training is offered unevenly and often too late in their careers to matter. Exit means leaving the profession entirely, not switching employers.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    moderate, biographical, trapped, national).

% Their authority rested on interpreting texts written in the old script and rooted in an Ottoman-Islamic textual tradition. The reform does not merely inconvenience them — it recodes their entire vocation as backward by design, since the new orthography's legitimacy is explicitly built on rupture from what they represent. They cannot simply retrain without abandoning the tradition that constitutes their role.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, payer,
    moderate, civilizational, identity_locked, national).

% Ordinary literate adults — merchants, small landholders, letter-writers — who become unable to read street signs, newspapers, or their own children's schoolbooks within a few years. Too old for the state's crash literacy campaigns to reach effectively, they are quietly reclassified as illiterate by a system that used to count them as literate.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, older_generation_arabic_script_literates, payer,
    powerless, biographical, trapped, national).

% Generations born after the transition inherit a script that is, for them, simply the alphabet — no rupture cost is paid because there is no prior literacy to lose. They benefit from whatever administrative and pedagogical gains the new orthography produces without bearing any of the transition's extraction.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, future_national_citizens, beneficiary,
    moderate, civilizational, analytical, national).

% Watch the reform as a signal of the state's civilizational alignment, feeding it into judgments about trade terms, diplomatic recognition, and cultural affinity. Their approval functions as an external validation the state apparatus actively courts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, european_diplomatic_observers, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single national script does solve real coordination problems — one printing standard, one curriculum, one civil-service exam alphabet — that a genuinely mixed-script society would find costly to sustain indefinitely.
% TRANSFER_FUNCTION: Moves professional standing, state employment, and interpretive authority from the Ottoman-trained literate and religious-scholarly class to a new cohort trained from the outset in the Latin-based script and the secular-modernist curriculum built around it.
% ABSENT_VOICES: The Ottoman literate class and the ulema are the ones with the deepest structural knowledge of what is being discarded, but they are constitutively excluded from legitimacy under this reading precisely because their expertise is what the rupture is defined against — objecting from that position is treated as evidence of the backwardness the reform is correcting.
% DISAPPEARANCE_RATIONALE: If the mandated rupture-orientation of the reform disappeared — if the state instead framed script reform as continuity-preserving or purely administrative — the entire legitimacy architecture built on top of it (school curricula, civil-service qualification, the prestige hierarchy of the new intelligentsia) would need to be rejustified on different grounds, and the disqualification of the old literate class would lose its civilizational cover story.
% FOUNDING_PROBLEM: A state elite sought to demonstrate decisive civilizational realignment away from an Ottoman-Islamic imperial past and toward a Western-modern future, using script change as the most visible, irreversible marker of that realignment.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and its allied intellectuals attest the rupture was necessary and largely completed. Independent historians of the period, and descendants of the displaced literate class documented in oral-history collections, attest that administrative efficiency gains could have been achieved without the rupture framing, and that the rupture framing itself was the extractive, not merely the instrumental, element — no source outside the beneficiary coalition corroborates rupture-as-necessity on its own terms.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the years immediately following the decree (0.35 to 0.72 by year 10) as the disqualification of the old literate class and ulema is operationalized through exams, publishing gatekeeping, and civil-service requirements, then gradually falls as the transition generation dies off or retrains and the extraction becomes moot for a population that never knew the old script. Suppression follows a similar hump: heaviest during the enforcement window (banning the old script in print and official use, criminalizing its continued institutional use) and receding once compliance is no longer contested because there is no one left contesting it. Theater ratio is moderate throughout — some of the rupture performance (renaming, monument-building, curriculum theater) is genuinely disconnected from literacy outcomes, but a substantial coordination function (single national print standard) is real, which keeps theater from dominating.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's seat this is coordination: one alphabet, one curriculum, one legitimate print culture, aligned with a chosen civilizational reference point. From the ulema's seat and the Ottoman literate class's seat the identical structure is enforced extraction of professional and interpretive standing, dressed in the vocabulary of progress. Both seats are looking at the same enforcement machinery; the engine's per-seat computation is expected to diverge sharply between the agenda_setter/beneficiary seats and the payer seats — that divergence is the modernist reading's central empirical claim, not an artifact to be smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state apparatus, new bureaucratic class, reformers, future citizens) get low d because the reform either directly transfers standing to them or costs them nothing (having no prior literacy to lose). Victims (Ottoman literate class, ulema, older literates) get high d because their exit options are trapped or identity_locked — the ulema in particular cannot simply retrain, since their authority is constituted by the very tradition being ruptured, which is why identity_locked rather than merely trapped or constrained is the correct exit-option atom for that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (demonstrating civilizational realignment) is contested as still live: the state apparatus and its intellectual allies maintain that the rupture accomplished something durable and necessary; independent historical accounts suggest the marker function was front-loaded into the initial decades and the ongoing maintenance of the rupture-orientation past that window is closer to institutional inertia than to solving an active problem. This keeps the story from being mislabeled either as pure atemporal extraction (it did solve a real, if narrow, coordination problem — one script, one curriculum) or as pure benign coordination (the disqualification of a specific incumbent class was not incidental collateral but the constitutive act the legitimacy claim rests on).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_efficiency_separability,
    'Was the civilizational-rupture framing structurally necessary to achieve the literacy and administrative gains the reform produced, or was rupture-framing a separable extractive overlay on an efficiency-achievable reform?',
    'Comparative case analysis against script or orthography reforms elsewhere that achieved similar literacy/administrative gains without an explicit civilizational-rupture framing (e.g., orthography simplifications not tied to regime-change legitimacy claims); if comparable gains were achieved without rupture framing, rupture was not necessary to the coordination function.',
    'If separable, the modernist reading''s high ε is substantially rupture-framing extraction riding on top of a much smaller instrumentalist coordination core; if inseparable, the rupture framing was itself doing coordination work (rapid, unambiguous legitimacy transfer) that a purely efficiency framing could not have accomplished as fast.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_efficiency_separability, conceptual, 'Whether rupture-framing is separable from the reform''s efficiency gains.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence in the historical record supports selecting the modernist reading over the continuity or instrumentalist readings as the operative legitimacy claim actually used by the reforming state, as opposed to a post-hoc gloss?',
    'Textual analysis of contemporaneous state rhetoric, parliamentary/decree language, and educational curricula for explicit rupture-vs-continuity-vs-efficiency framing; triangulate against which framing dominated official justification at the time of enactment versus retrospective historiography.',
    'If contemporaneous sources overwhelmingly used efficiency language, this story''s reading may be a retrospective historiographical imposition rather than the operative kernel reading at the time — shifting weight toward the instrumentalist_reading as the historically dominant constraint and this modernist_reading as an interpretive layer added later.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether the modernist reading reflects the historically operative justification or a later interpretive gloss.').

omega_variable(
    generational_extraction_decay_mechanism,
    'Does the declining extractiveness after the transition generation reflect genuine resolution (the extraction was transitional, born by one cohort) or does it mask a durable but less visible ongoing extraction (e.g., continued marginalization of communities that retained ties to the old script/tradition)?',
    'Longitudinal study of descendant communities'' educational and civil-service outcomes relative to the new-script-native population, controlling for other factors, across multiple generations post-reform.',
    'If extraction persists intergenerationally through inherited disadvantage, the measured decline in base_extractiveness understates the constraint''s true long-run cost and the tangled_rope classification may undercount durable structural harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_extraction_decay_mechanism, empirical, 'Whether declining measured extraction reflects real resolution or intergenerational persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 40, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the orthographic_legitimacy_kernel. The instrumentalist_reading measures the same script reform through a literacy/efficiency lens and should show substantially lower ε and a narrower victim set. The continuity_reading inverts beneficiary/victim assignment relative to this story, treating the ulema and traditional literate class as the legitimate interest the kernel should have protected. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways — each carries its own stable ε and its own stakeholder structure, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
