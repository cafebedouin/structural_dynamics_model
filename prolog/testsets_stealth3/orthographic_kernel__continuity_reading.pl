% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Post-1928 Latin-Script Settlement (Continuity Reading)
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   On 1 November 1928 the Grand National Assembly adopted the Law on the
 *   Introduction of the Turkish Alphabet, replacing Arabic script with a
 *   Latin-based alphabet for Turkish public life. Schools, presses,
 *   registries, and street signs converted within months; printing in Arabic
 *   script for Turkish-language publication was restricted from 1929. A
 *   century later the settlement stands: Turkish literacy is near-universal
 *   in Latin script, while the Ottoman textual inheritance - state archives,
 *   endowed libraries, religious commentary, three generations of family
 *   papers - is legible only to specialists and elective students. This story
 *   authors that standing settlement as the continuity reading sees it: an
 *   arrangement that solved a real literacy and standardization problem while
 *   transferring the official textual medium away from the class that had
 *   embodied it, sealing the inherited corpus behind a script barrier that
 *   renews itself with every schooled cohort. KEY AGENTS (by structural
 *   relationship): - ottoman_literate_class: Primary target
 *   (organized/identity_locked) - bore the severance of vocation, corpus, and
 *   public voice - islamic_scholarly_establishment: Secondary target
 *   (organized/identity_locked) - lost the public carrier of its transmission
 *   chain - republican_state_apparatus: Agenda-setter
 *   (institutional/arbitrage) - drafted, enacted, and administers the
 *   settlement - kemalist_modernizing_coalition: Principal beneficiary
 *   (powerful/mobile) - collected the political payoff of the break -
 *   latin_typeset_print_industry: Commercial beneficiary (moderate/mobile) -
 *   captured the conversion market - post_reform_school_generation:
 *   Dual-positioned (organized/constrained) - gained cheap literacy, inherits
 *   the access cost - ottoman_dual_script_reformers: Excluded voice
 *   (moderate/constrained) - six decades of gradualist proposals never put to
 *   a vote - linguistic_policy_historians: Analytical observer
 *   (analytical/analytical) - compiles the record both camps cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.78).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.2).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Post-1928 Latin-Script Settlement (Continuity Reading)").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political linguistics / state formation / commitment systems").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '92221657-1e89-48b4-aa64-ffecf93d8826').
narrative_ontology:cs_kernel_codification('92221657-1e89-48b4-aa64-ffecf93d8826', fixed_text).
narrative_ontology:cs_authority_grounding('92221657-1e89-48b4-aa64-ffecf93d8826', lineage).
narrative_ontology:cs_interpretation_layer_present('92221657-1e89-48b4-aa64-ffecf93d8826').
narrative_ontology:cs_reading_relation('92221657-1e89-48b4-aa64-ffecf93d8826', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('92221657-1e89-48b4-aa64-ffecf93d8826', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('92221657-1e89-48b4-aa64-ffecf93d8826', foundational, script_constitutes_transmission_continuity).
narrative_ontology:cs_axiom_status(script_constitutes_transmission_continuity, holdable).
narrative_ontology:cs_axiom_grounding('92221657-1e89-48b4-aa64-ffecf93d8826', script_constitutes_transmission_continuity, deontological).
narrative_ontology:cs_axiom('92221657-1e89-48b4-aa64-ffecf93d8826', secondary, legitimate_reform_preserves_corpus_access).
narrative_ontology:cs_axiom_status(legitimate_reform_preserves_corpus_access, holdable).
narrative_ontology:cs_axiom_grounding('92221657-1e89-48b4-aa64-ffecf93d8826', legitimate_reform_preserves_corpus_access, instrumental).
narrative_ontology:cs_reference_frame('92221657-1e89-48b4-aa64-ffecf93d8826', ottoman_islamic_textual_continuum).
narrative_ontology:cs_drift_state('92221657-1e89-48b4-aa64-ffecf93d8826', contemporary_heritage_politics_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('92221657-1e89-48b4-aa64-ffecf93d8826', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, republican_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, kemalist_modernizing_coalition).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, latin_typeset_print_industry).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, post_reform_school_generation).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, islamic_scholarly_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, post_reform_school_generation).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, state_script_standardization_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, phonetic_alphabet_mass_literacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in medrese, scribal, and military schools, this class held its reading, writing, and vocational capital in Arabic script. After November 1928 its members could no longer publish officially, hold clerical posts, or address the state in the only script they commanded; retraining meant starting over mid-career, and their libraries, correspondence, and professional networks aged into privacy. Older members withdrew from public textual life; younger ones retrained under duress. Leaving the field meant abandoning the vocation and the textual world that constituted it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class, payer,
    organized, biographical, identity_locked, national).

% The medrese network, endowed libraries, and preaching hierarchy transmitted Qur'an, hadith, jurisprudence, and Ottoman commentary through Arabic-script pedagogy. The 1924 closure of the medreses preceded the script change; the 1928 mandate then severed its graduates from official literacy entirely. Sacred texts remained readable in mosques and homes, but the establishment's chain of transmission lost its public carrier: religious knowledge production moved into informal channels, and each ordained cohort since has had to acquire the old script privately, if at all.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_scholarly_establishment, payer,
    organized, generational, identity_locked, national).

% Drafted and enacted the 1928 alphabet law, converted schools, presses, and registries on a fixed timetable, and regulated printing so that Turkish-language publication migrated to Latin type. It gained administrative legibility, school throughput, and typographic compatibility with imported technology, and it sets and revises the terms on which the old script may appear in museums, elective courses, and archival access rules. Its exit is rule-making itself.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, republican_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The officer, bureaucrat, and intellectual coalition around the republic's founding leadership. It campaigned for the alphabet change as the visible edge of a broader civilizational reorientation and collected the political payoff: a marker of break with the imperial past, a mobilizing symbol for the new schools, and a constituency of Latin-literate citizens bound to the new state. Its members moved freely between ministries, presses, and universities as the settlement matured.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, kemalist_modernizing_coalition, beneficiary,
    powerful, generational, mobile, national).

% Imported linotype machines, cast Latin type, and printed the new textbooks, newspapers, and official forms. The mandate created its market overnight, and firms that retooled early captured the conversion contracts. Its exposure is commercial rather than existential: equipment, composition skills, and credit transfer across scripts and markets.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, latin_typeset_print_industry, beneficiary,
    moderate, biographical, mobile, national).

% Schooled wholly in Latin script from 1929 onward, this cohort acquired literacy in months instead of years and entered a public sphere keyed to the new alphabet. The same schooling left it unable to read the letters, ledgers, gravestones, and books of its own grandparents except through translation or specialist training - an access cost that recurs with every cohort and surfaces whenever family papers, endowed-library holdings, or the Ottoman state archive come into view.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, post_reform_school_generation, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, post_reform_school_generation, payer).

% A current running from the 1860s Tanzimat press debates through the 1908 constitutional-era proposals and the wartime phonetic-reform committees: separate letters for Turkish vowels, simplified ligatures, or a staged dual-script transition. Its proposals were before the state for six decades; the 1928 decision was prepared by a narrow circle and adopted by parliament in weeks, and the gradualists' middle path - reforming the script's pedagogy without breaking the corpus - was never put to a vote.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_dual_script_reformers, excluded,
    moderate, biographical, constrained, national).

% Comparative historians of literacy and script reform study the Turkish case alongside Soviet Latinization, Chinese simplification, and Japanese script policy; they compile the literacy statistics, printing records, and archival-access studies that both camps in the Turkish dispute cite. Their seat is retrospective and comparative; they hold no stake in either script's fortunes.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, linguistic_policy_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, republican_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a single phonetic alphabet for Turkish across schooling, printing, and administration, cutting character-acquisition time for new readers from years to months and connecting Turkish text to global typographic, telegraphic, and later digital infrastructure.
% TRANSFER_FUNCTION: Moves official textual competence and cultural capital: from the Arabic-script literate class and scholarly establishment to the state-school system and its Latin-literate graduates; renders the Ottoman corpus - state archives, endowed libraries, religious commentary, family papers - accessible only through specialist mediation; concentrates script authority in republican educational and printing institutions.
% ABSENT_VOICES: The medrese faculty, the calligraphers' guilds, and the gradualist script-reform current were absent from the 1928 deliberation. The ulema's institutional voice had been dismantled in 1924-25, before the alphabet question reached the assembly, and the dual-script proposals compiled since the 1860s were never tabled. Had they been present, they would have demanded a transition preserving corpus access; their absence is what made the all-or-nothing vote possible.
% DISAPPEARANCE_RATIONALE: Schools, publishing, administration, signage, and everyday literacy are organized around the Latin alphabet; overnight removal would force retooling of the entire textual infrastructure and strand the Latin-literate majority. The settlement's removal would rearrange far more than its imposition did.
% FOUNDING_PROBLEM: As the reform's builders stated it: Turkish literacy was confined to a small minority (roughly ten percent) because Arabic script demanded years of instruction ill-suited to mass schooling, and Ottoman typography was incompatible with modern printing and telegraphic technology; the republic needed a script a village child could learn in months.
% FOUNDING_PROBLEM_CORROBORATION: Comparative literacy historiography outside the beneficiary set corroborates the stated problem's reality - pre-reform literacy estimates, contemporary foreign observers, and cross-country schooling data all attest low Ottoman-era literacy - while disputing the script's share of the cause, since school finance, class policy, and war disruption are rival explanations. Ottomanist archivists and social historians, likewise outside the benefiting parties, corroborate the cost side: the corpus severance is documented in archive-access records and the fate of endowed libraries. No party outside the dispute denies both halves.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All series share one time grid (t = years since the 1 November 1928 alphabet law; t=0 is 1928, t=90 is 2018) and every tracked metric is authored at every point; all points are historical observations. Base extractiveness is authored from the continuity reading's own lights over the standing settlement: the severance cost recurs with every schooled cohort rather than being paid once, so the series opens high (0.82 at enactment), dips mid-century (0.72, c. 1963) when the settlement looked complete and the old corpus safely archived, and rises again (0.78 by 2018) as heritage politics, archive-access crises, and family-paper encounters made the alienation salient; the end-state scalar matches the series end. Suppression_requirement is authored because this story's dynamic IS enforcement-capacity change: active bans on Arabic-script Turkish printing and school-regime enforcement (0.85 at t=0) decayed into normalized hegemony (0.20 by t=90) - a falling trajectory recording enforcement decay, not relaxation of the underlying prohibition, which remains structurally in force in official spheres. Theater_ratio rises slowly (0.15 to 0.23) as maintenance activity shifts from functional enforcement toward commemorative and curricular ritual. Accessibility_collapse (0.68) reflects near-total foreclosure of Arabic-script public life with surviving niches (private worship, academic paleography, diaspora publishing); resistance (0.55) records the real but swiftly disarmed opposition - parliamentary dissent, petitions, passive noncompliance by the older cohort. Claim and metrics are independent: tangled_rope is claimed from the structure (a genuine literacy-and-standardization coordination delivered through the same machinery that dispossessed the script's former bearers); the metrics describe operation as the continuity reading assesses it. Receipt surface: the settlement's gains demonstrably accrue to the state apparatus (legibility, throughput, enforcement authority), so gain_flow names that seat rather than asserting diffuseness; fixing is prohibitive for whoever could fix it - a century of Latin-script infrastructure and a fully schooled population stand on the settlement. Coalition structure: the two payer seats shared interests and were each organized, but their coalition window closed before the alphabet vote - the caliphate's abolition and the medrese closures (1924) and the independence tribunals (1925-26) removed their institutional footholds, so the resistance the arrangement met was real but structurally disarmed at the moment of decision.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same facts: from the republican_state_apparatus seat the settlement is infrastructure it drafted, administers, and commemorates; from the identity_locked payer seats the same statute is the event that ended their public textual existence. Same-level divergence separates ottoman_literate_class from latin_typeset_print_industry - similarly positioned non-state actors facing the identical mandate whose power diverges on exit structure alone: the printers' capital was script-portable (equipment, composition skills, markets), the literate class's capital was script-constituted (vocation, library, sacred philology), so one exits by retooling and the other only by self-erasure. The reading-level perspectival gap - why this file authors high epsilon over a referent a sibling file authors low - is recorded in kernel_context and the kernel_reading_position omega, not reconciled here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the state apparatus sits nearest the beneficiary pole (it collects legibility, throughput, and enforcement authority and writes the rules); the Kemalist coalition and the print industry sit low-d as collectors without rule-setting; the school generation sits mid-low, lifted above pure beneficiary by its secondary payer position, since each cohort re-pays the corpus-access cost. The victim declarations drive the target pole: ottoman_literate_class and islamic_scholarly_establishment are identity_locked - professional identity (a career embodied in script mastery), relational identity (a textual community constituted in the script), and ideological identity (the sacred text's graphic body) fuse, so exit means abandoning the vocation and the inheritance together; locked targets sit near the full-target end and their effective extraction is amplified accordingly. Suppression is authored as a raw structural property and is not scaled: the 0.20 end-state scalar is the residual coercive force, while extractiveness alone is scaled by directionality and national scope - script compliance verifies easily locally, but the corpus-severance externality operates at national scale, modestly amplifying effective extraction for the targets. Suppression here is structural (legal bans, school regime, printing regulation), not internalized: the payer seats never accepted the settlement's valuation of their inheritance, and their compliance tracks enforcement decay and cohort attrition, not conviction. Identity-lock counterfactual: had the literate class been mobile - willing retrainers - the severance would price as transitional cost, d would fall, and the profile would drift toward ordinary coordination; the classification turns on the lock, not on the statute's text.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Coding the settlement as pure extraction erases the coordination even this reading concedes - mass literacy acquired in months, a single standard across schooling and printing, typographic entry into global infrastructure. Coding it as pure coordination erases the asymmetric, identity-locked severance that the coordination did not require if the continuity-preserving alternatives were viable (see omega continuity_preserving_alternative_viability). The R5 interview locates the arrangement's present justification precisely: the founding problem as stated (mass illiteracy) is solved, but its status is contested because the continuity side holds the diagnosis was wrong - class power and school finance, not script, kept literacy at ten percent - so the arrangement persists on achieved facts rather than on its stated warrant without tipping into zombie flagging. mandatrophy_resolved is not declared: the settlement's operative function (the standardized Latin public sphere) is live, not atrophied; what atrophied is its enforcement apparatus, which the suppression series tracks separately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the continuity_reading of the orthographic_kernel; what would the sibling readings (modernization_reading, rupture_reading) change structurally?',
    'Compile the sibling stories and compare victim sets, epsilon, and computed types over the same referent (the post-1928 settlement).',
    'modernization_reading would author low epsilon over the same referent and likely compute a coordination-dominant profile; rupture_reading would author high epsilon with intent-attributed victim sets and possibly an extraction-dominant profile. Divergence across the triplet is the measurement the kernel exists to take, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    continuity_preserving_alternative_viability,
    'Could a continuity-preserving reform (phonetic reform of Arabic script, or a staged dual-script transition) plausibly have delivered comparable literacy, or was the Latin break the only workable path?',
    'Comparative evidence: Soviet Latinization outcomes, Persian and Tajik script politics, digraphia stability cases, Chinese simplification results; plus assessment of whether enriched vowel notation could have carried Turkish phonology in mass schooling.',
    'If viable alternatives existed, the severance cost was avoidable and the extraction component of the settlement is larger; if not, part of the cost is irreducible coordination price and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_preserving_alternative_viability, empirical, 'Whether the settlement''s severance cost was necessary for its literacy gains.').

omega_variable(
    cohort_attrition_vs_structural_severance,
    'Is the persisting extraction borne by living cohorts (decaying as they age out) or by the standing relation between the Turkish-reading public and its Ottoman corpus (recurring per generation)?',
    'Heritage-script literacy rates, archive usage statistics, and intergenerational textual-access surveys compared across cohorts.',
    'Cohort-borne extraction decays toward inertial residue; structurally recurring extraction holds the hybrid coordination-extraction profile indefinitely. Determines the drift direction of the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohort_attrition_vs_structural_severance, empirical, 'Whether the settlement''s continuing cost is a fading cohort effect or a standing structural condition.').

omega_variable(
    enforcement_decay_vs_capacity_attrition,
    'Does the falling suppression trajectory reflect the settlement becoming self-sustaining (normalized consent) or enforcement capacity simply aging out (revivable coercion)?',
    'Test responsiveness: legislative proposals to widen Ottoman-script teaching, court treatment of Arabic-script publication, and ministry behavior when heritage funding shifts.',
    'Self-sustaining normalization predicts stable low suppression; capacity attrition predicts suppression spikes under favorable political coalitions, changing the drift forecast.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_vs_capacity_attrition, empirical, 'Whether the enforcement decay is consolidation or exhaustion.').

omega_variable(
    foreclosed_middle_path_stability,
    'Was the dual-script middle path genuinely viable, or do script bifurcations structurally resolve toward a single system regardless of policy?',
    'Digraphia literature: stability conditions of sustained multi-script regimes versus documented collapse cases, applied to the Turkish demographic and institutional setting.',
    'If the middle path was unstable, the mandate''s foreclosure of it was selection rather than suppression and the accessibility_collapse score overstates agency; if stable, the foreclosure was a choice and the extraction component grows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosed_middle_path_stability, conceptual, 'Whether the blocked accommodation path was a live option or a structural impossibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t2, orthographic_kernel__continuity_reading, theater_ratio, 2, 0.14).
narrative_ontology:measurement_basis(orth_tr_t2, observed).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__continuity_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__continuity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t35, orthographic_kernel__continuity_reading, theater_ratio, 35, 0.13).
narrative_ontology:measurement_basis(orth_tr_t35, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_kernel__continuity_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(orth_tr_t50, observed).
narrative_ontology:measurement(orth_tr_t65, orthographic_kernel__continuity_reading, theater_ratio, 65, 0.18).
narrative_ontology:measurement_basis(orth_tr_t65, observed).
narrative_ontology:measurement(orth_tr_t80, orthographic_kernel__continuity_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(orth_tr_t80, observed).
narrative_ontology:measurement(orth_tr_t90, orthographic_kernel__continuity_reading, theater_ratio, 90, 0.23).
narrative_ontology:measurement_basis(orth_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t2, orthographic_kernel__continuity_reading, base_extractiveness, 2, 0.8).
narrative_ontology:measurement_basis(orth_be_t2, observed).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__continuity_reading, base_extractiveness, 5, 0.79).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__continuity_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t35, orthographic_kernel__continuity_reading, base_extractiveness, 35, 0.72).
narrative_ontology:measurement_basis(orth_be_t35, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_kernel__continuity_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement_basis(orth_be_t50, observed).
narrative_ontology:measurement(orth_be_t65, orthographic_kernel__continuity_reading, base_extractiveness, 65, 0.75).
narrative_ontology:measurement_basis(orth_be_t65, observed).
narrative_ontology:measurement(orth_be_t80, orthographic_kernel__continuity_reading, base_extractiveness, 80, 0.77).
narrative_ontology:measurement_basis(orth_be_t80, observed).
narrative_ontology:measurement(orth_be_t90, orthographic_kernel__continuity_reading, base_extractiveness, 90, 0.78).
narrative_ontology:measurement_basis(orth_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t2, orthographic_kernel__continuity_reading, suppression_requirement, 2, 0.8).
narrative_ontology:measurement_basis(orth_su_t2, observed).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__continuity_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__continuity_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t35, orthographic_kernel__continuity_reading, suppression_requirement, 35, 0.36).
narrative_ontology:measurement_basis(orth_su_t35, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_kernel__continuity_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(orth_su_t50, observed).
narrative_ontology:measurement(orth_su_t65, orthographic_kernel__continuity_reading, suppression_requirement, 65, 0.24).
narrative_ontology:measurement_basis(orth_su_t65, observed).
narrative_ontology:measurement(orth_su_t80, orthographic_kernel__continuity_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(orth_su_t80, observed).
narrative_ontology:measurement(orth_su_t90, orthographic_kernel__continuity_reading, suppression_requirement, 90, 0.2).
narrative_ontology:measurement_basis(orth_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Turkish script reform' covers three structurally distinct commitments sharing one referent (the post-1928 Latin-script settlement) and diverging in epsilon by reading: this continuity_reading file authors high epsilon (severance of the Ottoman-Islamic textual continuum, dispossession of its bearers); the modernization_reading file authors low epsilon (delivered literacy and technical compatibility); the rupture_reading file authors low-to-negative epsilon (the severance was the aim). Per the epsilon-invariance principle these are three files, not one story with a measurement parameter; each carries its own claimed_type, metrics, and stakeholders, and the family is linked through affects_constraints edges in all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
