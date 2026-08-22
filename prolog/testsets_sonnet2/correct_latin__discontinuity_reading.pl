% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Classical Text as Sole Authority, Medieval Usage as Corruption
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint is the discontinuity reading of the contested 'correct
 *   Latin' kernel: the claim that Classical Latin, preserved in ancient
 *   texts, is the sole legitimate form of the language, and that medieval
 *   Latin represents a corrupted deviation that must be corrected by
 *   reconstruction from textual sources rather than treated as a legitimate
 *   evolved stage. This reading crystallized with Renaissance humanism
 *   (Petrarch, Valla, and successors) as a polemical rejection of 'monkish'
 *   Latin and became institutionalized in humanist pedagogy, critical editing
 *   practice, and eventually modern classical philology. The ε value here
 *   (0.62) reflects the standing arrangement AS THIS READING SEES IT — a
 *   hierarchy that relocates linguistic authority from continuous practice to
 *   reconstructed text, subordinating medieval scholarship and the Church's
 *   own liturgical continuity to a Classical standard whose recovery only
 *   credentialed philologists can certify.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda-setters who define and enforce the Classical standard (institutional/arbitrage) — chief beneficiaries
 *   - humanist_pedagogy_institutions: beneficiary institutions whose curricula depend on the Classical/medieval rupture
 *   - medieval_latin_scholars: payers — subordinated field, chronically under-resourced relative to Classics
 *   - vernacular_latin_speaking_communities_historical: powerless historical payers, judged after the fact with no voice
 *   - church_liturgical_latin_tradition: institutional payer/beneficiary whose living continuity is treated as needing correction
 *   - modern_classical_reception_scholars: analytical observers tracing the historical construction of the rupture claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Classical Text as Sole Authority, Medieval Usage as Corruption").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '294f457c-914d-427c-83e5-9b3673b79b0a').
narrative_ontology:cs_kernel_codification('294f457c-914d-427c-83e5-9b3673b79b0a', fixed_text).
narrative_ontology:cs_authority_grounding('294f457c-914d-427c-83e5-9b3673b79b0a', expertise).
narrative_ontology:cs_interpretation_layer_present('294f457c-914d-427c-83e5-9b3673b79b0a').
narrative_ontology:cs_reading_relation('294f457c-914d-427c-83e5-9b3673b79b0a', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('294f457c-914d-427c-83e5-9b3673b79b0a', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('294f457c-914d-427c-83e5-9b3673b79b0a', foundational, classical_text_is_sole_legitimate_referent).
narrative_ontology:cs_axiom_status(classical_text_is_sole_legitimate_referent, holdable).
narrative_ontology:cs_axiom_grounding('294f457c-914d-427c-83e5-9b3673b79b0a', classical_text_is_sole_legitimate_referent, conventional).
narrative_ontology:cs_axiom('294f457c-914d-427c-83e5-9b3673b79b0a', foundational, medieval_usage_constitutes_corruption_not_change).
narrative_ontology:cs_axiom_status(medieval_usage_constitutes_corruption_not_change, holdable).
narrative_ontology:cs_axiom_grounding('294f457c-914d-427c-83e5-9b3673b79b0a', medieval_usage_constitutes_corruption_not_change, empirically_contingent).
narrative_ontology:cs_reference_frame('294f457c-914d-427c-83e5-9b3673b79b0a', classical_textual_purity_standard).
narrative_ontology:cs_drift_state('294f457c-914d-427c-83e5-9b3673b79b0a', post_romance_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('294f457c-914d-427c-83e5-9b3673b79b0a', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_pedagogy_institutions).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, critical_edition_publishers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, vernacular_latin_speaking_communities_historical).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, church_liturgical_latin_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, church_liturgical_latin_tradition).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_priority_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_purity_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the standard of correctness by editing critical texts, training students in Classical grammar, and adjudicating what counts as a legitimate Latin form. Collects prestige, academic positions, and curricular control from the discontinuity framing, which makes their reconstructive philological expertise the sole gatekeeper of correctness.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, classical_philologists, beneficiary).

% Universities and grammar schools built curricula on the premise that medieval Latin was decadent and Classical Latin must be recovered and taught as the true standard. This gave humanist institutions a monopoly on defining literate correctness against monastic and scholastic Latin traditions they displaced.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_pedagogy_institutions, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Produce and sell critical editions premised on stripping away medieval 'corruptions' to recover an authentic Classical original. Their commercial and scholarly value depends on the discontinuity being real and requiring specialist reconstruction rather than being continuous with a living tradition anyone could read.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, critical_edition_publishers, beneficiary,
    organized, generational, mobile, global).

% Study a millennium of Latin texts that the discontinuity reading treats as a degraded interval, not a legitimate stage of the language. Their field is chronically under-resourced relative to Classical philology, their subject matter is taught as 'what went wrong,' and their scholarly authority is subordinated to Classicists who set the correctness standard for a language medievalists actually work in daily.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_scholars, payer,
    moderate, biographical, constrained, continental).

% The historical speakers and writers of medieval Latin — clerks, monks, notaries, everyday users across centuries — whose living linguistic practice is retroactively categorized as error against a text-based standard they never used as their reference point. They cannot contest the verdict; it is rendered on their language after the fact by a later reconstruction project.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, vernacular_latin_speaking_communities_historical, payer,
    powerless, civilizational, trapped, continental).

% Maintains a continuous liturgical Latin usage descended directly from medieval forms. Under the discontinuity reading this living tradition is treated as needing correction toward Classical norms (as happened repeatedly in liturgical reform movements), even though the tradition's own continuous practice is what kept Latin alive as a spoken and written register at all.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, church_liturgical_latin_tradition, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, church_liturgical_latin_tradition, beneficiary).

% Study how the Classical/medieval boundary was constructed by Renaissance humanists and later philologists as a polemical and pedagogical project rather than a neutral linguistic finding. They can trace the history of the discontinuity claim without being required to adjudicate its truth.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, modern_classical_reception_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, citable textual standard against which any Latin usage can be checked — critical editions, grammars, and dictionaries all converge on a single reconstructed norm, which genuinely coordinates scholarly communication about 'what Classical Latin was.'
% TRANSFER_FUNCTION: Moves prestige, curricular authority, and interpretive control from communities of continuous Latin practice (medieval scribes, the Church, vernacular users) to the specialists who can read and reconstruct ancient texts — correctness is relocated from living usage to philological expertise applied to a closed textual corpus.
% ABSENT_VOICES: The historical speakers of medieval Latin cannot testify to their own usage's legitimacy; they are represented only through the texts philologists judge, never as parties to the judgment. Contemporary Church Latinists and living Latin-speaking communities (e.g. active Latinity movements) are rarely consulted by the discontinuity reading's institutional apparatus, which treats them as downstream of the correct form rather than as evidence of continuity.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading vanished, critical editions would lose their premise of 'purifying' texts of medieval interpolation, medieval Latin scholarship would gain parity with Classical philology in curricula and funding, and the Church's own liturgical continuity would stand as legitimate Latin in its own right rather than as deviation awaiting correction — the disciplinary hierarchy of philology would reorganize substantially.
% FOUNDING_PROBLEM: Renaissance humanists faced a genuine textual problem: manuscripts had accumulated centuries of scribal error, glosses, and reinterpretation, and recovering what ancient authors actually wrote required systematic comparison and correction of corrupted transmission.
% FOUNDING_PROBLEM_CORROBORATION: Textual critics and manuscript historians (a source largely overlapping with the beneficiary group, but methodologically independent) attest that genuine scribal corruption is a real and ongoing problem in transmission. However, historical linguists working outside classical philology (e.g. Romance linguistics, sociolinguistics of historical registers) attest from outside the beneficiary set that the further claim — that medieval usage itself, as opposed to specific manuscript errors, constitutes linguistic corruption — is a category conflation with no independent evidentiary support; it treats normal diachronic change as textual error.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the ongoing transfer of interpretive authority and institutional prestige from medieval Latin scholarship and continuous liturgical practice to Classical philology, sustained across centuries as humanist pedagogy became institutionalized in universities. Suppression (0.58) is substantial but not maximal — medieval Latin scholarship persists as a field, and the Church continued its own liturgical Latin practice regardless of philological censure, so the discontinuity reading has never fully suppressed alternatives, only subordinated them. Theater ratio (0.44) is moderate-to-high: much of the reconstructive apparatus (declaring medieval forms 'barbarisms,' 'purifying' texts of interpolations) is more performative boundary-maintenance than a response to genuine textual corruption, since actual scribal error and legitimate diachronic change are frequently conflated under the same corrective label. Accessibility collapse (0.62) is moderate: the reading has made it institutionally difficult to treat medieval Latin as simply 'Latin' without qualification, but has not eliminated alternative framings — hence resistance (0.55) from medievalists, Church Latinists, and Romance linguists who dispute the corruption framing on evidentiary grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and humanist pedagogical institutions sit near the beneficiary end: they set the standard, administer the gatekeeping apparatus (critical editions, curricula, credentialing), and collect prestige and institutional resources from the discontinuity framing. Medieval Latin scholars and the historical speakers of medieval Latin sit near the target end: their subject matter is defined as deviation, their scholarly and (for historical speakers) linguistic legitimacy is judged by an external Classical standard they never used as their own reference. The Church's liturgical tradition is dual-positioned — it benefits from continuous institutional survival of Latin as a living register, but pays when its own usage is repeatedly targeted for 'correction' toward Classical norms in periodic reform movements (e.g. the humanist revisions of liturgical texts).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine manuscript corruption requiring critical textual method — is real and remains partly live (scribal transmission errors are a genuine ongoing concern for editors of any period). But the discontinuity reading extends this legitimate textual-critical function into a much larger claim: that an entire millennium of living linguistic usage constitutes 'corruption' rather than normal language change. This is where mandatrophy applies — the narrow, still-live founding problem (fixing transmission errors) has been used to justify a much broader and more contestable verdict (delegitimizing medieval usage as such) that persists institutionally well past any narrow textual-critical necessity. Classifying this as tangled_rope rather than snare captures that there IS a genuine coordination function (a stable textual reference standard is useful) riding alongside the asymmetric extraction (subordinating medieval philology and the Church's continuous practice) — collapsing it to pure extraction would miss the real editorial value; collapsing it to pure rope would launder the institutional hierarchy it also produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_change_conflation,
    'Does the discontinuity reading''s ''corruption'' verdict track genuine scribal/textual transmission error, or does it conflate that narrow phenomenon with ordinary diachronic linguistic change (which is not corruption in any technical linguistic sense)?',
    'Systematic comparative analysis distinguishing manuscript-transmission errors (miscopying, interpolation) from attested, patterned morphological and syntactic changes across the medieval corpus using historical-linguistic methodology rather than philological value judgment.',
    'If the two are conflated, a substantial share of the discontinuity reading''s authority rests on a category error, and the ε attributable to genuine coordination (fixing real corruption) should be much lower than the ε attributable to institutional hierarchy-maintenance (delegitimizing medieval usage as such).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_change_conflation, conceptual, 'Whether ''corruption'' in the discontinuity reading conflates real transmission error with ordinary language change.').

omega_variable(
    discontinuity_construction_motive,
    'Was the Classical/medieval rupture originally a linguistic finding or a polemical humanist project aimed at displacing scholastic and monastic intellectual authority?',
    'Historical analysis of Renaissance humanist writings (Petrarch, Valla, Poliziano) for explicit statements of motive; comparison against contemporaneous non-humanist Latin usage records to establish whether a linguistic discontinuity was independently detectable prior to the polemic.',
    'If primarily polemical/institutional in origin, this strengthens the tangled_rope classification (genuine minor coordination function riding on a larger extraction project) over an innocent-rope reading; it would also bear on whether ''emerges_naturally'' framings of the standard are false-summit candidates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_construction_motive, empirical, 'Historical motive behind the construction of the Classical/medieval rupture claim.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''correct Latin'' better modeled as a single kernel with three readings (as done here), or does the underlying dispute actually decompose into orthogonal sub-claims — e.g., a narrower textual-transmission-fidelity claim (uncontested, near-Mountain) bundled with a broader legitimacy-of-usage claim (highly contested)?',
    'Attempt a further decomposition following the ε-invariance principle: separate ''is this specific manuscript reading a transmission error'' (likely near-uncontested, low ε) from ''is medieval usage as a category illegitimate'' (the actually contested claim, carrying the bulk of this story''s ε).',
    'If decomposition is warranted, this story''s ε may itself be an amalgam of a near-Mountain textual-criticism claim and a much more extractive legitimacy claim, suggesting a future finer-grained split within the discontinuity reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the discontinuity reading itself bundles a narrow uncontested claim with a broader contested one, inviting further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t80, correct_latin__discontinuity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(corr_tr_t80, observed).
narrative_ontology:measurement(corr_tr_t150, correct_latin__discontinuity_reading, theater_ratio, 150, 0.34).
narrative_ontology:measurement_basis(corr_tr_t150, observed).
narrative_ontology:measurement(corr_tr_t250, correct_latin__discontinuity_reading, theater_ratio, 250, 0.38).
narrative_ontology:measurement_basis(corr_tr_t250, observed).
narrative_ontology:measurement(corr_tr_t380, correct_latin__discontinuity_reading, theater_ratio, 380, 0.41).
narrative_ontology:measurement_basis(corr_tr_t380, observed).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.44).
narrative_ontology:measurement_basis(corr_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t80, correct_latin__discontinuity_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement_basis(corr_be_t80, observed).
narrative_ontology:measurement(corr_be_t150, correct_latin__discontinuity_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement_basis(corr_be_t150, observed).
narrative_ontology:measurement(corr_be_t250, correct_latin__discontinuity_reading, base_extractiveness, 250, 0.58).
narrative_ontology:measurement_basis(corr_be_t250, observed).
narrative_ontology:measurement(corr_be_t380, correct_latin__discontinuity_reading, base_extractiveness, 380, 0.6).
narrative_ontology:measurement_basis(corr_be_t380, observed).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.62).
narrative_ontology:measurement_basis(corr_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t80, correct_latin__discontinuity_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(corr_su_t80, observed).
narrative_ontology:measurement(corr_su_t150, correct_latin__discontinuity_reading, suppression_requirement, 150, 0.53).
narrative_ontology:measurement_basis(corr_su_t150, observed).
narrative_ontology:measurement(corr_su_t250, correct_latin__discontinuity_reading, suppression_requirement, 250, 0.55).
narrative_ontology:measurement_basis(corr_su_t250, observed).
narrative_ontology:measurement(corr_su_t380, correct_latin__discontinuity_reading, suppression_requirement, 380, 0.57).
narrative_ontology:measurement_basis(corr_su_t380, observed).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.58).
narrative_ontology:measurement_basis(corr_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the 'correct_latin' kernel. continuity_reading treats medieval Latin as legitimate evolved Classical Latin transmitted through unbroken practice (much lower ε — a rope-leaning reading with minimal institutional extraction). hybrid_reading treats Classical form as transmitted through medieval practice but correctable via textual evidence (intermediate ε). This discontinuity_reading carries the highest ε of the three because it authorizes the most complete institutional delegitimization of medieval usage and the most extensive gatekeeping apparatus (critical editions, humanist pedagogy, credentialing). All three share the same kernel (what makes Latin 'correct') but instantiate structurally distinct constraints with distinct beneficiary/victim sets, per the ε-invariance principle — they are not the same constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
