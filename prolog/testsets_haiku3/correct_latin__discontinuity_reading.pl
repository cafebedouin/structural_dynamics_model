% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_discontinuity_reading, []).

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
 *   human_readable: Classical Latin Purity and Medieval Deviation (Discontinuity Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The discontinuity reading of correct Latin asserts that authentic Latin
 *   is the Classical form preserved in ancient texts (Cicero, Virgil, etc.),
 *   and that medieval Latin represents linguistic corruption and decay
 *   requiring external reconstruction through philological emendation. This
 *   reading grounds humanist authority: the scholar's task is to recover the
 *   pure Classical standard from corrupted medieval manuscripts and to defend
 *   it against further degradation. Medieval Latin practitioners, scribal
 *   copyists, and vernacular literacy advocates are positioned as either
 *   carriers of corruption (scribes) or incoherent alternatives (vernacular
 *   advocates). The constraint operates as a gate: medieval forms are
 *   excluded from the legitimate usage set, and their exclusion is enforced
 *   through institutional control over education, textual authority, and
 *   scholarly prestige. The reading is one of three competing framings of the
 *   same kernel (correct Latin); the others are continuity_reading (medieval
 *   Latin is legitimate evolved Classical) and hybrid_reading (Classical with
 *   medieval transmission and targeted reform).
 *
 * KEY AGENTS:
 *   - humanist_scholars: institutional agenda-setters controlling textual recovery, defining correctness through emendation; beneficiaries of authority to declare forms corrupt
 *   - medieval_practitioners: identity-locked payers bearing the cost of retroactive linguistic judgment; their own productions classified as error
 *   - textual_reconstructors: institutional beneficiaries whose expertise is indispensable for recovery; depend on discontinuity framing for professional status
 *   - ecclesiastical_authorities: observers administering the constraint as correct doctrine without defending it; benefit from centralized textual authority
 *   - vernacular_literacy_advocates: powerless payers constrained by the prestige penalty on alternatives; their literacy claims delegitimized as abandonment of true standard
 *   - linguistic_continuity_researchers: excluded; their evidence for language-internal evolution is framed as misguided modern method application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin Purity and Medieval Deviation (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '264c11ef-81ce-47e7-b99a-107a554dae7d').
narrative_ontology:cs_kernel_codification('264c11ef-81ce-47e7-b99a-107a554dae7d', fixed_text).
narrative_ontology:cs_authority_grounding('264c11ef-81ce-47e7-b99a-107a554dae7d', extraction).
narrative_ontology:cs_interpretation_layer_present('264c11ef-81ce-47e7-b99a-107a554dae7d').
narrative_ontology:cs_reading_relation('264c11ef-81ce-47e7-b99a-107a554dae7d', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('264c11ef-81ce-47e7-b99a-107a554dae7d', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('264c11ef-81ce-47e7-b99a-107a554dae7d', foundational, classical_linguistic_rupture_exists).
narrative_ontology:cs_axiom_status(classical_linguistic_rupture_exists, holdable).
narrative_ontology:cs_axiom_grounding('264c11ef-81ce-47e7-b99a-107a554dae7d', classical_linguistic_rupture_exists, empirically_contingent).
narrative_ontology:cs_axiom('264c11ef-81ce-47e7-b99a-107a554dae7d', foundational, textual_authority_primacy_over_transmission).
narrative_ontology:cs_axiom_status(textual_authority_primacy_over_transmission, holdable).
narrative_ontology:cs_axiom_grounding('264c11ef-81ce-47e7-b99a-107a554dae7d', textual_authority_primacy_over_transmission, conventional).
narrative_ontology:cs_reference_frame('264c11ef-81ce-47e7-b99a-107a554dae7d', classical_pure_form_textual_preservation).
narrative_ontology:cs_drift_state('264c11ef-81ce-47e7-b99a-107a554dae7d', early_modern_humanist_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('264c11ef-81ce-47e7-b99a-107a554dae7d', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_authority_defenders).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_practitioners).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, vernacular_literacy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_reconstructors).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, scribal_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control textual recovery and emendation practices; define the canon of acceptable Classical Latin forms through manuscript work and philological commentary. They argue that medieval Latin deviations are corruptions introduced by scribal error, linguistic decay, and monastic ignorance, requiring expert restoration to the true Classical standard. They benefit from the authority to determine correctness and from the intellectual prestige of humanistic recovery work.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).

% Medieval clerics, scholars, and administrators who composed and used Latin for administrative, theological, and scholarly purposes. Their forms—inflectional simplifications, vocabulary innovations, syntactic accommodations to vernacular substrates—are classified as errors and corruptions under the discontinuity reading. They are locked in their medieval identity; exit would require adopting the Classical standard retroactively to their own historical productions, a logical impossibility. Their contemporary inheritors (Medieval Latin specialists) face professional marginalization.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Church authorities, manuscript custodians, and institutional guardians of textual tradition who benefit from the framing that correct Latin is preserved in authoritative ancient texts. This framing consolidates textual authority: the ancient text IS the standard, and institutional control over textual access and interpretation is justified by the need to protect the pure form from corruption. They benefit indirectly from the suppression of alternative standards.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_authority_defenders, beneficiary,
    institutional, generational, mobile, continental).

% Emerging writers and readers in vernacular languages who argue that Latin should adapt to living use or yield to mother-tongue literacy. Under the discontinuity reading, their argument is linguistically incoherent—Latin is the preserved Classical form, not a living language—which discredits vernacular literacy claims as abandonment of the true standard. They are constrained by the institutional prestige of the Classical form and the suppression of vernacular alternatives.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, vernacular_literacy_advocates, payer,
    powerless, biographical, constrained, local).

% Medieval and early-modern scribes who copy texts, adapting language to their own era's norms and comprehension. Their work is classified as error-prone corruption; the discontinuity reading treats scribal change as failure rather than natural linguistic adaptation. They bear the cost of being positioned as bearers of corruption and have no exit from this assessment retroactively.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, scribal_copyists, payer,
    powerless, immediate, trapped, local).

% Philologists and editors whose authority derives from the technical practice of reconstructing the Classical text from corrupted manuscripts. They benefit from the discontinuity framework because their expertise is indispensable: only through rigorous emendation can the true Classical form be recovered from medieval corruption. Their professional identity and status depend on this reconstructive mission.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_reconstructors, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, textual_reconstructors, agenda_setter).

% Modern linguists and historical grammarians who trace continuous sound change and morphological evolution from Classical to medieval to Romance forms. They are excluded from the discontinuity reading's legitimate reference set: their evidence for continuity is framed as misguided application of modern linguistic methods to historical phenomena. They would argue for a language-internal evolutionary reading, but are kept outside the canonical authority structure.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, linguistic_continuity_researchers, excluded,
    moderate, biographical, constrained, continental).

% Church hierarchy and monastic leadership who benefit from centralized textual authority and standardized liturgical Latin. The discontinuity reading supports their interests (a single correct form reinforces institutional control), but they do not actively defend it; they are seats from which the constraint is administered as correct doctrine rather than contested.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, humanist_scholars).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative standard for what counts as correct Latin, enabling scholars across regions and generations to reference a single authoritative form preserved in canonical texts, rather than accepting divergent medieval regional practices as equally legitimate.
% TRANSFER_FUNCTION: Transfers authority over linguistic correctness from living medieval practitioners to textual antiquarians and humanist reconstructors; transfers prestige and institutional standing from medieval scholarship (theology, canon law) to humanistic recovery and emendation work; transfers legitimacy away from vernacular and medieval-Latin-based literacy practices toward Classical-form-centered education.
% ABSENT_VOICES: Medieval Latin practitioners as living voices are entirely absent (their period is past); medieval linguistic theory and intentional grammatical adaptation are excluded from the legitimate reference set. Modern historical linguists studying continuity are structurally excluded. Vernacular literacy advocates are kept outside institutional scholarly discourse.
% DISAPPEARANCE_RATIONALE: Humanist scholars and textual authority defenders argue that without the discontinuity reading, Latin becomes merely one historical form among many with no claim to correctness, and textual scholarship loses its organizing principle. Continuity researchers argue that if the discontinuity framework vanished, Latin would be studied as it actually evolved—continuous transmission with medieval and Romance successors—and the prestige penalties on medieval forms would lift. The contest is over whether linguistic correctness requires historical rupture.
% FOUNDING_PROBLEM: Late Medieval and Renaissance scholars lacked systematic manuscript access and emendation methods; texts were corrupted through scribal error, and scholars needed a way to distinguish authoritative forms from manuscript accidents in order to recover the 'true' ancient standard.
% FOUNDING_PROBLEM_CORROBORATION: Modern textual criticism confirms that Medieval manuscripts are corrupt; however, philologists outside the humanist tradition (historical linguists, Medieval Latin specialists) argue the founding problem was a METHODOLOGY problem (manuscripts are messy), not a LANGUAGE problem (medieval Latin is corrupt). The problem was solved by statistical reconstruction and stemmatology, not by declaring medieval forms incorrect. Humanist scholars and institutional guardians attest the problem remains because the standard requires perpetual defense against corruption; continuity researchers attest it is superseded by evolutionary linguistic analysis.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.62) reflects the constraint's core operation: it transfers authority from medieval practitioners to humanist scholars and textual reconstructors, and it suppresses vernacular alternatives by positioning them as linguistically incoherent. The transfer is not purely coordinating (medieval Latin practitioners could have continued evolving; the constraint forcibly exits them from legitimacy), so extraction is substantial. Suppression (0.71) is high because the constraint's persistence depends on actively excluding medieval forms from the legitimate usage set and defending textual authority against degradation. Theater (0.48) represents the balance: there is real textual corruption in medieval manuscripts (the coordination function is authentic), but an increasing share of suppression activity after 1450 is defending the purity standard itself rather than repairing actual manuscript errors. Accessibility_collapse (0.78) is high: once the discontinuity frame is adopted, medieval forms are categorically excluded from legitimate use; alternatives have nowhere to exist within the framework. Resistance (0.42) is moderate: medieval practitioners are now past voices; modern resistance comes from historical linguists and Medieval Latin specialists, but they are institutionally marginal. The measurement series shows extractiveness and theater rising through the 15th century as humanist institutions solidify, then stabilizing in the 16th—the constraint has fully matured by 1500.
 *
 * PERSPECTIVAL GAP:
 *   The humanist agenda-setter seat experiences the constraint as genuine coordination: a necessary defense of the true standard against scribal corruption and linguistic decay. The medieval practitioner seat (now a historical voice) and the continuity researcher seat experience it as forced exclusion: the standard is not self-evident but rather imposed by institutional power, treating evolution as corruption. From the ecclesiastical observer seat, the constraint is neutral machinery for maintaining textual authority; the reading's truth is not their concern. The engine computes these per-seat divergences from the structural data: different power levels, different exit options (textual reconstructors have arbitrage exit via Renaissance humanist networks; medieval practitioners have identity-locked exit from their own era), and different beneficiary/victim positions drive different computed types across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars benefit (d → 0.0) by controlling correctness; medieval practitioners and vernacular advocates pay (d → 1.0) by exclusion and prestige suppression; ecclesiastical authorities are observers (d → 0.5) who administer rather than defend. No overrides needed; the structural derivation from beneficiary/victim + exit + power is sound.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows mandatrophy: the founding problem (corrupt medieval manuscripts) is substantially solved by 1500 through developed stemmatology and manuscript collation methods. The constraint persists not because the founding function remains live, but because institutional prestige and career incentives now ride on the discontinuity frame itself. By 1650, the theater_ratio is still 0.48, indicating that nearly half the enforcement activity is maintaining the purity standard as a matter of doctrine rather than repairing actual manuscript errors. The founding_problem_status is declared 'dead' because textual criticism solved the technical problem; the constraint persists as performance and institutional identity, not coordination necessity. This is not a snare (too much real coordination function remains) nor a piton (enough extraction remains to motivate institutional defense), but a tangled_rope in advanced decay: real coordination in origins, substantial extraction in operation, theatrical maintenance visible in the measurement rise and plateau.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_linguistic_evolution_vs_corruption,
    'Are medieval Latin changes natural linguistic evolution (sound change, morphological simplification, borrowing from substrate languages) or actual errors/corruptions introduced by scribal incompetence and decay?',
    'Historical linguistic analysis comparing medieval Latin to Classical Latin sound changes against known Romance language evolution; comparison to other language communities'' natural drift patterns; examination of whether medieval forms are systematically used (intentional grammar) or sporadic (error).',
    'If natural evolution, the discontinuity frame is imposed classification (extraction), and medieval Latin becomes linguistically legitimate. If actual corruption, the coordinate function (defense of the true form) is real and extraction is minimal. This determines whether the constraint is tangled_rope (mixed) or rope (pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_linguistic_evolution_vs_corruption, empirical, 'Whether medieval Latin differences are evolutionary or corrupted').

omega_variable(
    authority_source_textual_vs_evolutionary,
    'Should authority over correct Latin reside in ancient preserved texts (the discontinuity framing) or in the evolutionary transmission of the language itself, which includes medieval and Romance forms as legitimate descendants?',
    'Comparative method from historical linguistics: test whether medieval and Romance forms can be derived from Classical Latin through regular sound changes and grammatical rules, or whether they require positing ad-hoc corruption. If derivable, authority rests in evolutionary linguistics; if not, authority rests in textual preservation.',
    'If evolutionary authority is legitimate, the discontinuity frame is a choice to suppress one axis of evidence (living language change) in favor of another (written texts). This reclassifies the constraint as snare-like (suppression of alternatives). If textual authority is the only legitimate source, the discontinuity frame is coordinating (defending the one true standard).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_source_textual_vs_evolutionary, conceptual, 'Legitimacy source: preserved texts versus evolutionary transmission').

omega_variable(
    reading_instability_committer_frame,
    'Does the discontinuity reading remain the same constraint when adopted by different institutional parties (humanist scholars, ecclesiastical authorities, post-Enlightenment grammarians) or does its meaning shift with the justificatory framework each seat provides?',
    'Track the grounds offered for the discontinuity claim across seats and periods: Classical purity (humanist), doctrinal authority (ecclesiastical), empirical corruption (early modern philology), linguistic law (19th-century comparative method). Test whether the constraint''s enforcement pattern and beneficiary structure remain stable across these justification shifts.',
    'If the constraint''s identity is reading-dependent (meaning changes with justification), then this JSON instantiates only the discontinuity_reading held by humanist scholars with textual authority grounding. Other seats'' instantiations would be separate constraints linked via affects_constraints. If the constraint''s identity is stable across readings, then this single JSON captures a structure that persists despite shifting justifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_instability_committer_frame, conceptual, 'Instability of the discontinuity reading across institutional seats and epistemic frameworks').

omega_variable(
    suppression_mechanism_internal_vs_structural,
    'Is medieval Latin suppression structural (institutional barriers to medieval Latin publishing, teaching, and canonization) or internalized (medieval specialists come to believe their own forms are inferior, adopt Classical standards as the unquestionable norm)?',
    'Post-suppression trajectory analysis: if medieval Latin scholarship were removed from prestige penalties and institutional constraints, would Medieval Latin specialists spontaneously continue to treat medieval forms as corrupt, or would they re-legitimize medieval standards? Examine the adoption of Classical norms by medieval scholars exposed to humanist training.',
    'If internalized, the suppression survives institutional removal and medieval Latin remains self-subordinating even in contexts of scholarly autonomy. If structural, lifting institutional penalties would restore medieval Latin''s perceived legitimacy. This affects the long-term stability of the constraint and the tractability of reversing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internal_vs_structural, empirical, 'Suppression mechanism: structural constraint versus internalized norm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1350, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_disc_tr_t1350, correct_latin__discontinuity_reading, theater_ratio, 1350, 0.22).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1350, projected).
narrative_ontology:measurement(correct_latin_disc_tr_t1400, correct_latin__discontinuity_reading, theater_ratio, 1400, 0.32).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1400, observed).
narrative_ontology:measurement(correct_latin_disc_tr_t1450, correct_latin__discontinuity_reading, theater_ratio, 1450, 0.39).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1450, observed).
narrative_ontology:measurement(correct_latin_disc_tr_t1500, correct_latin__discontinuity_reading, theater_ratio, 1500, 0.45).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1500, observed).
narrative_ontology:measurement(correct_latin_disc_tr_t1575, correct_latin__discontinuity_reading, theater_ratio, 1575, 0.49).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1575, observed).
narrative_ontology:measurement(correct_latin_disc_tr_t1650, correct_latin__discontinuity_reading, theater_ratio, 1650, 0.48).
narrative_ontology:measurement_basis(correct_latin_disc_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(correct_latin_disc_be_t1350, correct_latin__discontinuity_reading, base_extractiveness, 1350, 0.38).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1350, projected).
narrative_ontology:measurement(correct_latin_disc_be_t1400, correct_latin__discontinuity_reading, base_extractiveness, 1400, 0.48).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1400, observed).
narrative_ontology:measurement(correct_latin_disc_be_t1450, correct_latin__discontinuity_reading, base_extractiveness, 1450, 0.56).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1450, observed).
narrative_ontology:measurement(correct_latin_disc_be_t1500, correct_latin__discontinuity_reading, base_extractiveness, 1500, 0.61).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1500, observed).
narrative_ontology:measurement(correct_latin_disc_be_t1575, correct_latin__discontinuity_reading, base_extractiveness, 1575, 0.63).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1575, observed).
narrative_ontology:measurement(correct_latin_disc_be_t1650, correct_latin__discontinuity_reading, base_extractiveness, 1650, 0.62).
narrative_ontology:measurement_basis(correct_latin_disc_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_disc_su_t1350, correct_latin__discontinuity_reading, suppression_requirement, 1350, 0.42).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1350, projected).
narrative_ontology:measurement(correct_latin_disc_su_t1400, correct_latin__discontinuity_reading, suppression_requirement, 1400, 0.54).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1400, observed).
narrative_ontology:measurement(correct_latin_disc_su_t1450, correct_latin__discontinuity_reading, suppression_requirement, 1450, 0.62).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1450, observed).
narrative_ontology:measurement(correct_latin_disc_su_t1500, correct_latin__discontinuity_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1500, observed).
narrative_ontology:measurement(correct_latin_disc_su_t1575, correct_latin__discontinuity_reading, suppression_requirement, 1575, 0.71).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1575, observed).
narrative_ontology:measurement(correct_latin_disc_su_t1650, correct_latin__discontinuity_reading, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement_basis(correct_latin_disc_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, humanist_authority_and_textual_recovery).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, medieval_latin_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'correct_latin' alongside continuity_reading and hybrid_reading. The three constraints share the same object (Latin language form selection) but diverge in beneficiary structure and suppression mechanism. The discontinuity reading privileges textual authority and historical rupture; the continuity reading privileges living transmission and evolutionary legitimacy; the hybrid reading privileges textual primacy with evolutionary transmission correction. All three are active in contemporary humanities discourse; institutional prestige concentrates on the discontinuity reading, which explains its enforcement strength despite the founding problem's resolution. The three constraints form a constraint family linked by affects_constraints; they are not alternative observations of one constraint but genuinely distinct constraints grounded in different kernel readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
