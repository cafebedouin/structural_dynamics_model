% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Correct Latin Standard — Hybrid Reading (Layered Recovery)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the correct-Latin kernel:
 *   the standing arrangement under contest is the humanist adjudication
 *   regime (c. 1400-1550) in which core morphology is judged by the
 *   continuous school tradition — medieval inflectional forms are declared
 *   legitimate survivals — while syntax and lexicon are judged against
 *   recovered classical texts, so that medieval syntactic and lexical usage
 *   is classified as corruption requiring correction. Reconstruction is
 *   therefore layered: partial reoccupation of the language from texts,
 *   grafted onto a tradition that never broke. CONSTRAINT FAMILY NOTE: the
 *   colloquial label 'correct Latin' decomposes into three structurally
 *   distinct constraints linked by network.affects_constraints. The
 *   continuity reading (all medieval usage natural evolution; internal
 *   correction suffices) carries low epsilon — no condemnation extraction
 *   operates. The discontinuity reading (Classical and Medieval are distinct
 *   systems; total textual reoccupation) carries high epsilon — every layer
 *   of medieval usage falls under condemnation. This hybrid reading sits
 *   between: it grants legitimacy where continuity is cheapest to verify
 *   (morphology, checked against the unbroken Donatus-Priscian school
 *   transmission) and assigns corruption where recovery expertise gains
 *   jurisdiction (syntax, lexicon), yielding intermediate epsilon with a
 *   distinctive selective-extraction structure. The upstream continuity claim
 *   supplies the morphology premise this reading inherits; the downstream
 *   discontinuity reading cites the same textual evidence for a stronger
 *   conclusion.
 *
 * KEY AGENTS:
 *   - humanist_philologists: Agenda-setter (organized/mobile) — sets the standard through recoveries, grammars, and polemics; collects adjudicative authority directly
 *   - classical_text_editors: Primary beneficiary (institutional/mobile) — collects revenue and standing from the recovery apparatus the standard mandates
 *   - latin_grammar_teachers: Secondary beneficiary (moderate/constrained) — their inflectional curriculum is validated as continuously legitimate
 *   - ecclesiastical_latin_users: Dual beneficiary/payer (institutional/identity_locked) — morphology vindicated, syntax and lexicon condemned, exit structurally unthinkable
 *   - scholastic_university_masters: Primary target (institutional/constrained) — bears the condemnation in their living practice and careers
 *   - medieval_scholastic_authors: Retroactive target (powerless/trapped) — posthumously emended and stigmatized; cannot answer
 *   - vernacular_writers: Excluded voice (moderate/mobile) — objects from outside the Latin conversation
 *   - comparative_linguists: Analytical observer (institutional/analytical) — assesses the standard's factual claims from outside its enforcement history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.52).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Correct Latin Standard — Hybrid Reading (Layered Recovery)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, 'b3791005-65be-4d43-b9b0-87ff3d53f476').
narrative_ontology:cs_kernel_codification('b3791005-65be-4d43-b9b0-87ff3d53f476', fixed_text).
narrative_ontology:cs_authority_grounding('b3791005-65be-4d43-b9b0-87ff3d53f476', expertise).
narrative_ontology:cs_interpretation_layer_present('b3791005-65be-4d43-b9b0-87ff3d53f476').
narrative_ontology:cs_reading_relation('b3791005-65be-4d43-b9b0-87ff3d53f476', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3791005-65be-4d43-b9b0-87ff3d53f476', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('b3791005-65be-4d43-b9b0-87ff3d53f476', foundational, morphological_core_transmitted_continuously).
narrative_ontology:cs_axiom_status(morphological_core_transmitted_continuously, holdable).
narrative_ontology:cs_axiom_grounding('b3791005-65be-4d43-b9b0-87ff3d53f476', morphological_core_transmitted_continuously, empirically_contingent).
narrative_ontology:cs_axiom('b3791005-65be-4d43-b9b0-87ff3d53f476', foundational, syntactic_lexical_drift_beyond_internal_correction).
narrative_ontology:cs_axiom_status(syntactic_lexical_drift_beyond_internal_correction, holdable).
narrative_ontology:cs_axiom_grounding('b3791005-65be-4d43-b9b0-87ff3d53f476', syntactic_lexical_drift_beyond_internal_correction, empirically_contingent).
narrative_ontology:cs_axiom('b3791005-65be-4d43-b9b0-87ff3d53f476', secondary, reconstruction_is_layered_partial_reoccupation).
narrative_ontology:cs_axiom_status(reconstruction_is_layered_partial_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('b3791005-65be-4d43-b9b0-87ff3d53f476', reconstruction_is_layered_partial_reoccupation, instrumental).
narrative_ontology:cs_reference_frame('b3791005-65be-4d43-b9b0-87ff3d53f476', layered_recovery_canon).
narrative_ontology:cs_drift_state('b3791005-65be-4d43-b9b0-87ff3d53f476', contemporary_descriptive_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b3791005-65be-4d43-b9b0-87ff3d53f476', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_text_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, latin_grammar_teachers).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, ecclesiastical_latin_users).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, scholastic_university_masters).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_scholastic_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, ecclesiastical_latin_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move between courts, chanceries, papal offices, and universities; recover and emend classical manuscripts; write the grammars, polemics, and model letters that define which usages count as correct. Their authority rests on demonstrated command of recovered texts, and their livelihoods — secretaryships, professorships, dedications — follow the demand for that command.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, humanist_philologists, agenda_setter,
    organized, generational, mobile, continental).

% Produce and sell corrected editions of classical authors through the new print houses. The standard's insistence that syntax and lexicon be judged from texts is their market: every condemnation of medieval usage increases demand for authoritative editions. They collect revenue and scholarly standing from the recovery apparatus without originating its rules.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_text_editors, beneficiary,
    institutional, generational, mobile, continental).

% Teach the Donatus- and Priscian-derived inflectional curriculum in town and cathedral schools. The standard validates their material as continuously legitimate — the declension tables they drill are declared unbroken survivals. Newer humanist grammars press on their methods, but their core stock of instruction survives the upheaval intact.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, latin_grammar_teachers, beneficiary,
    moderate, biographical, constrained, national).

% Pray, legislate, and administer in a Latin whose inflectional system descends uninterruptedly from the school tradition. The standard leaves their morphology legitimate while condemning much of their syntax and vocabulary — curial formulas, liturgical phrasing, patristic lexical habits. They cannot leave Latin without dissolving the institution itself, so they absorb correction selectively: liturgy keeps its medieval forms in practice while official rhetoric adopts recovered elegance.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, ecclesiastical_latin_users, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, ecclesiastical_latin_users, payer).

% Teach and write in the scholastic register — technical vocabulary, quod-clauses for reported speech, prepositional genitives, abstract nominalizations like quidditas. Find their usage ridiculed as barbarous, their books emended by editors, their students drawn away by humanist curricula. Leaving the register means abandoning the disputation forms their faculties run on; staying means wearing the label of corruption.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholastic_university_masters, payer,
    institutional, biographical, constrained, continental).

% Wrote the scholastic corpus — the glossators, the quaestio literature, the summae. Their syntax and coinages are retroactively classified as corruption: their works are emended in edition after edition, their vocabulary replaced in educated usage, their prose cited as the very image of barbarism. They cannot answer, and the judgment compounds with each generation of editors.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_scholastic_authors, payer,
    powerless, biographical, trapped, continental).

% Write in Italian, French, and English and argue for the dignity of the vernaculars from outside the Latin conversation. Their objection — that the authority of a dead language is an inherited habit rather than a necessity, and that the correction economy is a guild boundary — is heard only at the margins of the philological enterprise that defines the terms of debate.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_writers, excluded,
    moderate, generational, mobile, regional).

% Working centuries later with Romance corpora, manuscript stemmata, and the full medieval record, they can see which medieval forms descend continuously from the classical inflectional system and which were system-internal innovations. They assess the hybrid standard's factual accuracy from outside its enforcement history and its status economy.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, comparative_linguists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authoritative written register usable across regions and generations: the continuous morphological tradition lets every school transmit the same inflectional system without rupture, while the textually recovered syntactic and lexical norms give law, diplomacy, liturgy, and scholarship a common precision instrument.
% TRANSFER_FUNCTION: Moves linguistic authority from living practitioner communities to the textual apparatus and its custodians: compositional legitimacy is granted or withheld by reference to recovered classical usage; prestige, employment, and publication access flow toward those controlling manuscripts, editions, and grammars; correction labor flows onto every writer who composes in Latin.
% ABSENT_VOICES: The medieval authors whose usage is adjudicated are dead and cannot defend their practice. Vernacular writers and the unschooled — including all women, excluded from Latin schooling entirely — stand outside the conversation. Practicing scholastic masters objected, but enter the record chiefly as objects of ridicule rather than as participants in setting the standard.
% DISAPPEARANCE_RATIONALE: Without the layered standard, the republic of letters loses its common instrument: correspondence, editions, and university instruction fragment along regional and facultary lines. The humanist recovery program loses both its object and its claim to authority, collapsing with the standard it polices. Scholastic usage persists but no longer as a condemned alternative — the entire status economy organized around Latinity rearranges.
% FOUNDING_PROBLEM: After Roman administrative unity collapsed, written Latin diverged regionally and by use; scholars and chanceries needed a stable supraregional medium and a criterion for which usage was authoritative. The Carolingian correctio made the first repair; the humanist textual recovery was the second, layered on top of the first.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Charlemagne's Epistola de litteris colendis predates the humanists and documents both the divergence problem and the perceived need for repair; medieval charters and glosses independently record regional drift; modern historical linguistics confirms both the morphological continuity and the syntactic divergence. No source outside the humanist party, however, attests that the specific syntax-and-lexicon condemnations were necessary rather than fashionable — that attestation gap is itself signal.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but bounded (0.58 at interval end): the standard delivers a real coordination good — a common learned medium across a fragmented continent — while transferring adjudicative authority asymmetrically to the textual apparatus. Suppression (0.52) is reputational and institutional rather than violent: ridicule, emendation, exclusion from publication, and curriculum competition; alternatives persist but are stigmatized. Theater (0.28) reflects genuine philological labor (manuscript collation, stemmatic editing) mixed with performative Ciceronianism and rhetorical inflation of 'barbarism.' Accessibility collapse (0.48) is partial: scholastic usage survived inside the faculties and liturgy, and vernacular exits were opening throughout the interval. Resistance (0.55) is real: faculty defense of the disputation register, Erasmus's Ciceronianus against slavish imitation, Du Bellay's vernacular manifesto. The measurement series run on one shared time grid (1400/1430/1460/1490/1520/1550) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: it tracks enforcement-capacity maturation, not extraction shift — from scattered humanist polemic (informal mockery, 1400) through print-standardized grammars and school statutes to codified curricular sequencing approaching the Jesuit Ratio Studiorum — a rising trajectory modeling hardening enforcement infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist seat the arrangement is a rescue: a decayed instrument repaired by those with the learning to repair it, and the morphology concession proves their fairness. From the scholastic master's seat the same structure is usurpation: men holding the identical grammatical doctrine (Donatus, Priscian) relabel living competence as barbarism because aspiration, not knowledge, differs. The ecclesiastical seat experiences a split verdict — its inflections honored, its idioms condemned — which only an identity-locked participant would tolerate. The dead authors' seat experiences pure verdict without process. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: classical_text_editors and latin_grammar_teachers sit near the subsidized end (the standard manufactures their market and validates their curriculum); humanist_philologists sit nearest the beneficiary pole as the agenda-setting collectors of adjudicative authority. Victim declarations drive high directionality: scholastic_university_masters are constrained targets (careers fused to the condemned register); medieval_scholastic_authors are trapped targets bearing retroactive judgment at effectively full-target exposure. Ecclesiastical_latin_users are declared on both sides — genuine morphology subsidy, real syntactic cost — placing them mid-range, with identity lock amplifying their exposure on the payer side since exit is structurally unavailable. No directionality overrides are needed: each seat's beneficiary/victim status is declared distinctly enough for the structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. The coordination function is genuine — a supraregional learned medium that no fifteenth-century alternative delivered — which blocks a snare reading despite identifiable victims. The extraction is asymmetric and enforced — authority flows to the textual apparatus while living practitioners bear condemnation — which blocks a rope reading despite real beneficiaries. At interval end the founding problem (a stable supraregional medium) is still live: Latin remains the scholarly instrument into the eighteenth century, so no mandatrophy declaration is authored; the R5 mismatch consumer sees contested-status paired with world_rearranges, producing no zombie flag. The theater ratio stays well below piton range throughout: the philology is real even where the rhetoric inflates it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This constraint is one reading (hybrid_reading) of the correct_latin_kernel; what structural delta would the sibling readings produce in the per-seat classifications?',
    'Generate the sibling stories and compare computed seat classifications: continuity_reading should erase the victim class entirely (no corruption declared, no condemnation extraction); discontinuity_reading should extend condemnation to morphology, widening victims to include the grammar-teaching estate and the liturgical seat.',
    'Epsilon and per-seat types shift materially across readings; the cross-reading comparison is the measurement the family exists to take, not noise to be averaged away.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer structure: this story is the hybrid member of a three-reading kernel family.').

omega_variable(
    corruption_classification_validity,
    'Is ''corruption'' in medieval syntax and lexicon a linguistic fact or a jurisdictional classification serving the recovery apparatus''s authority?',
    'Neutral corpus comparison of medieval syntactic systems (quod-clauses, prepositional genitives, abstract nominalizations) against Late Latin baselines, evaluated by descriptive rather than normative criteria.',
    'If the innovations are system-internal development, the condemnation layer is authority extraction riding on genuine morphological continuity, pushing payer-seat readings toward snare flavor; if they are genuine deviation from the classical normative target, recovery was warranted repair and much of the measured extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_classification_validity, conceptual, 'Whether the hybrid reading''s central condemnation premise describes decay or difference.').

omega_variable(
    doctrinal_continuity_vs_usage_drift,
    'Grammatical doctrine transmitting classical syntax (Donatus, Priscian) never broke — so what exactly did textual recovery supply that the schools lacked?',
    'Compare what medieval grammarians codified against what writers actually practiced; measure the gap between doctrinal knowledge and usage across genres and centuries.',
    'If doctrine was available and unused, ''recovery'' was re-authoritation of known norms — the necessity claim weakens and measured extraction rises; if transmission itself corrupted or truncated the doctrine, recovery supplied missing content and the necessity claim strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_usage_drift, empirical, 'The Priscian paradox: continuous doctrine versus discontinuous usage as the basis of the recovery-necessity claim.').

omega_variable(
    morphology_syntax_boundary_location,
    'Where does ''core morphology'' end — do medieval innovations inside the inflectional system (reduced declensions, analogical plurals, new demonstrative paradigms) count as continuous core or as condemnable drift?',
    'Corpus census of medieval inflectional innovation against attested classical paradigms, coded form by form.',
    'A wider legitimate zone shrinks the condemned set and lowers effective extraction across payer seats; a narrower zone widens humanist jurisdiction and raises it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_syntax_boundary_location, empirical, 'The legitimacy boundary this reading draws is itself contestable in location, not just in principle.').

omega_variable(
    ecclesiastical_enforcement_selectivity,
    'Liturgical and curial Latin retained condemned medieval features in practice while the standard spread through schools and print — principled exemption or selective enforcement?',
    'Track correction practices across genres: papal bulls versus humanist correspondence versus university disputations, comparing which registers got emended and which were left alone.',
    'Systematic exemption by audience prestige would show enforcement tracking power rather than linguistic criteria, supporting capture-flavored readings of the agenda-setter seat; uniform application would support a good-faith standard.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_enforcement_selectivity, empirical, 'Whether the standard''s bite was uniform or calibrated to the target''s standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 1400, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin_kernel__hybrid_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(corr_tr_t1430, correct_latin_kernel__hybrid_reading, theater_ratio, 1430, 0.17).
narrative_ontology:measurement(corr_tr_t1460, correct_latin_kernel__hybrid_reading, theater_ratio, 1460, 0.2).
narrative_ontology:measurement(corr_tr_t1490, correct_latin_kernel__hybrid_reading, theater_ratio, 1490, 0.23).
narrative_ontology:measurement(corr_tr_t1520, correct_latin_kernel__hybrid_reading, theater_ratio, 1520, 0.26).
narrative_ontology:measurement(corr_tr_t1550, correct_latin_kernel__hybrid_reading, theater_ratio, 1550, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin_kernel__hybrid_reading, base_extractiveness, 1400, 0.4).
narrative_ontology:measurement(corr_be_t1430, correct_latin_kernel__hybrid_reading, base_extractiveness, 1430, 0.44).
narrative_ontology:measurement(corr_be_t1460, correct_latin_kernel__hybrid_reading, base_extractiveness, 1460, 0.48).
narrative_ontology:measurement(corr_be_t1490, correct_latin_kernel__hybrid_reading, base_extractiveness, 1490, 0.52).
narrative_ontology:measurement(corr_be_t1520, correct_latin_kernel__hybrid_reading, base_extractiveness, 1520, 0.55).
narrative_ontology:measurement(corr_be_t1550, correct_latin_kernel__hybrid_reading, base_extractiveness, 1550, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin_kernel__hybrid_reading, suppression_requirement, 1400, 0.35).
narrative_ontology:measurement(corr_su_t1430, correct_latin_kernel__hybrid_reading, suppression_requirement, 1430, 0.4).
narrative_ontology:measurement(corr_su_t1460, correct_latin_kernel__hybrid_reading, suppression_requirement, 1460, 0.44).
narrative_ontology:measurement(corr_su_t1490, correct_latin_kernel__hybrid_reading, suppression_requirement, 1490, 0.48).
narrative_ontology:measurement(corr_su_t1520, correct_latin_kernel__hybrid_reading, suppression_requirement, 1520, 0.51).
narrative_ontology:measurement(corr_su_t1550, correct_latin_kernel__hybrid_reading, suppression_requirement, 1550, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' conflates three structurally distinct constraints (epsilon-invariance decomposition). continuity_reading: no condemnation layer, epsilon near coordination floor. discontinuity_reading: total condemnation layer, epsilon high across all medieval usage. hybrid_reading (this file): selective condemnation confined to syntax and lexicon, epsilon intermediate, with the distinctive structure that legitimacy is granted exactly where continuity is cheapest to verify and corruption assigned exactly where recovery expertise gains jurisdiction. The continuity story is upstream (its morphology premise is inherited here); the discontinuity story is downstream (it cites the same textual evidence for a stronger conclusion). Each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
