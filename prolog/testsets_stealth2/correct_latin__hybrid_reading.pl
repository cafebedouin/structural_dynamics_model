% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Hybrid Standard for Correct Latin: Transmitted Core with Textual Correction
 *   domain: historical linguistics/philology/intellectual history
 *
 * SUMMARY:
 *   Between roughly 1400 and 1700 (interval units are decades: T0
 *   approximates 1400, T30 approximates 1700) the learned world rebuilt the
 *   standard for written Latin. Medieval practice had transmitted the
 *   classical grammatical core with high fidelity while accumulating
 *   divergent orthography, vocabulary, and idiom; Renaissance philology —
 *   Valla's Elegantiae, Poliziano's conjectural method, the Aldine and Froben
 *   presses, Erasmus' editions — made ancient textual evidence an instrument
 *   for correcting the transmitted surface without displacing the transmitted
 *   core. This story instantiates the HYBRID READING of the correct_latin
 *   kernel: correct Latin is the classical form as carried by living
 *   transmission, correctable where the manuscripts show the transmission
 *   slipped. Per the kernel-reading epsilon rule, the epsilon referent is the
 *   standing hybrid-governed arrangement itself, assessed by the reading's
 *   own lights: the reading concedes the transmitted core's legitimacy (so it
 *   does not extract the way a full-reconstruction regime would) while
 *   imposing real correction burdens and concentrating adjudicative authority
 *   in the philologically trained (so it does not sit at a coordination floor
 *   either). Claim and metrics are authored independently: the claim is
 *   tangled_rope; the metrics describe the regime's actual operation, and the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - humanist_philologists: Agenda-setting beneficiary (organized/constrained) — sets what counts as correct, collects adjudicative authority and patronage
 *   - - scholastic_authors: Primary payer (institutional/trapped) — bears correction and stigma on inherited practice
 *   - - monastic_copying_traditions: Payer (moderate/identity_locked) — transmission practice emended from outside
 *   - - latin_students: Payer-beneficiary (powerless/constrained) — pays double mastery, receives gated access
 *   - - printing_publishers: Commercial beneficiary (powerful/arbitrage)
 *   - - latin_schoolmasters: Beneficiary (organized/constrained) — collects fees and standing
 *   - - ecclesiastical_chancery: Institutional beneficiary (institutional/constrained) — consumes and propagates the standard
 *   - - vernacular_authors: Excluded voice (moderate/mobile)
 *   - - modern_classical_scholars: Analytical observer — sees the full arc from outside the enforcement era
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.58).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.45).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Hybrid Standard for Correct Latin: Transmitted Core with Textual Correction").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical linguistics/philology/intellectual history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'db0e08f7-367c-4c63-888c-53fdede3c07e').
narrative_ontology:cs_kernel_codification('db0e08f7-367c-4c63-888c-53fdede3c07e', fixed_text).
narrative_ontology:cs_authority_grounding('db0e08f7-367c-4c63-888c-53fdede3c07e', expertise).
narrative_ontology:cs_interpretation_layer_present('db0e08f7-367c-4c63-888c-53fdede3c07e').
narrative_ontology:cs_reading_relation('db0e08f7-367c-4c63-888c-53fdede3c07e', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('db0e08f7-367c-4c63-888c-53fdede3c07e', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('db0e08f7-367c-4c63-888c-53fdede3c07e', foundational, transmitted_deviation_textually_correctable).
narrative_ontology:cs_axiom_status(transmitted_deviation_textually_correctable, holdable).
narrative_ontology:cs_axiom_grounding('db0e08f7-367c-4c63-888c-53fdede3c07e', transmitted_deviation_textually_correctable, empirically_contingent).
narrative_ontology:cs_axiom('db0e08f7-367c-4c63-888c-53fdede3c07e', foundational, grammatical_core_transmission_legitimate).
narrative_ontology:cs_axiom_status(grammatical_core_transmission_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('db0e08f7-367c-4c63-888c-53fdede3c07e', grammatical_core_transmission_legitimate, conventional).
narrative_ontology:cs_reference_frame('db0e08f7-367c-4c63-888c-53fdede3c07e', classical_core_via_living_transmission).
narrative_ontology:cs_drift_state('db0e08f7-367c-4c63-888c-53fdede3c07e', contemporary_learned_usage, gap(stable, minor, true)).
narrative_ontology:cs_created_at('db0e08f7-367c-4c63-888c-53fdede3c07e', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, printing_publishers).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, latin_schoolmasters).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, ecclesiastical_chancery).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, scholastic_authors).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, monastic_copying_traditions).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, latin_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, latin_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collate manuscripts, publish corrected editions and grammars, and teach the corrected standard through patronage networks and the presses. Their professional standing, patronage income, and sense of vocation depend on the correction enterprise remaining necessary; stepping outside it means abandoning the philological identity that organizes their careers.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_philologists, agenda_setter,
    organized, generational, constrained, continental).

% University masters writing commentaries, disputations, and summae in the received technical Latin their genres were built on. Their terminological habits predate the corrective fashion; adopting humanist style would cost them precision and standing in their own faculties, while refusing it marks their prose as barbarous in the print market and before patrons. Their entire output economy runs through the medium being corrected.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, scholastic_authors, payer,
    institutional, biographical, trapped, continental).

% Religious communities whose liturgical and textual life runs on forms handed down through their own copying practice. Correction arrives from outside as emendation of their manuscripts and criticism of their habits. Leaving the transmitted practice would dissolve the communal rhythm and identity the practice sustains, so they absorb the corrections as indignity rather than exit.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, monastic_copying_traditions, payer,
    moderate, biographical, identity_locked, regional).

% Spend years mastering grammar and then further labor conforming to corrected usage demanded by examiners, patrons, and editors. In exchange they receive access to university, law, diplomacy, and church office. Individually they cannot negotiate the standard's demands; opting out forfeits the paths the standard gates.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, latin_students, payer,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, latin_students, beneficiary).

% Profit from authoritative corrected editions, grammars, and schoolbooks whose imprimatur depends on the correction apparatus. They are commercially committed to selling whatever the learned market certifies as correct rather than to any particular form of the standard, and can shift inventory as tastes move.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, printing_publishers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Teach the corrected standard in town schools and colleges. Their authority, fees, and employment depend on the standard remaining demanding enough to require professional instruction. They collect from the arrangement without setting its agenda, which is fixed by the philologists and the presses.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, latin_schoolmasters, beneficiary,
    organized, biographical, constrained, continental).

% Administers a pan-European institution that needs one uniform language for liturgy, canon law, and correspondence. The hybrid standard serves it well: a stable transmitted core for the liturgy, correctable registers for legal instruments. It adopts the standard, staffs schools, and enforces usage in its documents, but does not run the correction agenda.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, ecclesiastical_chancery, beneficiary,
    institutional, generational, constrained, continental).

% Write in Tuscan, French, and other vernaculars outside the Latin conversation, while the prestige hierarchy channels patronage, office, and honor through demonstrated Latin competence. They would argue that resources and standing should flow to vernacular letters, but they hold no seat where the standard is debated.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_authors, excluded,
    moderate, biographical, mobile, continental).

% Inherit both the transmitted corpus and the critical apparatus the correction era built, and assess from outside the enforcement period what the standard preserved, what it distorted, and what its costs purchased.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, modern_classical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single written medium intelligible across regions and generations for law, liturgy, theology, diplomacy, and scholarship in a language with no native speech community. The hybrid form keeps the transmitted grammatical core so new texts stay interoperable with the accumulated corpus, while permitting evidence-based repair of divergent spellings, vocabulary, and idioms.
% TRANSFER_FUNCTION: Moves correction labor and deference from writers and students to the philologically trained; moves publication advantage, patronage, and office toward those who command the corrected style; moves authority over the language from customary practice to textual expertise.
% ABSENT_VOICES: Monastic copyists and unreformed university masters had no seat in the republic of letters that set the standard; vernacular authors stood outside the Latin conversation while its prestige hierarchy drained patronage from their languages; rank-and-file clergy and clerks who needed only functional Latin were never consulted about the correctness regime imposed on their schooling.
% DISAPPEARANCE_RATIONALE: Without a shared standard, learned writing fragments into regional usages and the pan-European circulation of law, science, and theology collapses; universities, chanceries, and international religious bodies would each have had to build separate vernacular or Latinate regimes, and the Republic of Letters as a single conversation would not have existed in the form it took.
% FOUNDING_PROBLEM: After the western empire's political collapse, Latin persisted only as a learned second language handed on through schools and copying. Regional practice drifted apart, threatening the mutual intelligibility on which law, liturgy, and scholarship depended. The arrangement was built to keep one authoritative form of a language nobody spoke natively.
% FOUNDING_PROBLEM_CORROBORATION: University statutes, chancery formula-books, and conciliar records attest the intelligibility problem from outside the humanist beneficiary set. The scholastic opponents of the correction apparatus conceded the need for a standard — their quarrel was over which standard — and the later vernacular language academies, which rebuilt the same function for French and Italian, corroborate that the underlying problem was real and persistent.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the correction apparatus decouples authority from service: adjudicating correctness requires credentials in textual method that most writers of Latin cannot hold, and the standard's demands (double mastery of transmission and evidence) exceed what bare communication among the learned requires. Suppression is moderate (0.45) and is authored as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater rises across the interval (0.14 to 0.30) as Ciceronian display culture grows: imitation contests, ornamental school exercises, and orthographic point-scoring are performative maintenance riding on a functional core of real manuscript collation. Accessibility_collapse is moderate (0.45): once the hybrid standard is understood, writing uncorrected transmitted forms carries social cost, but genuine alternatives persist — vernaculars, and pockets of unreformed scholastic usage inside the universities. Resistance is substantial (0.50): the scholastic counterattack, the Ciceronian controversy, and defenses of consuetudo all contest the standard, which is why active enforcement is required. The temporal series share one grid (T0, 6, 12, 18, 24, 30) with every tracked metric authored at every point. Extraction rises through the polemic phase (Valla to Erasmus to the Ciceronian wars), peaks around T18 as the standard reaches maximum contentious reach, then plateaus and eases slightly as enforcement shifts from polemic to curriculum. The suppression_requirement series traces enforcement capacity specifically: it builds through the school-and-index phase (rising to 0.55 at T12) and then relaxes (to 0.45) as the corrected habitus becomes self-enforcing through upbringing — a genuine enforcement-decay-after-consolidation dynamic, not merely extraction shifting.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the philologist's position the arrangement is a service: the manuscripts really do disagree, and someone must adjudicate. From the scholastic master's position the same apparatus is expropriation: a thousand-year practice's authority is transferred to men who call its leading practitioners barbarians, and his trapped exit (his whole output economy runs through the medium) converts disagreement into payment. From the student's position it is a tollbooth with a payoff: years of surplus labor exchanged for gated access to office. From the printer's position it is inventory. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it. Note also the coalition possibility latent in the powerless student seat: individually unable to bargain, students collectively did eventually move curricula — and the vernacular turn drew off the next generation's consent, which is visible in the post-peak flattening of the extraction series.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Humanist_philologists sit nearest the beneficiary end: the standard subsidizes their authority, and their constrained-but-real exit (general letters, secretaryship) keeps them from full capture-only position while their agenda-setting role anchors the low-d pole. Printing_publishers and latin_schoolmasters derive low d as beneficiaries, with the printers' arbitrage exit pushing them furthest toward the subsidy end. Ecclesiastical_chancery benefits as consumer of the standard despite bearing internal compliance costs — its declared beneficiary position reflects net structural relationship. Scholastic_authors derive high d: victims with trapped exit and institutional power locked inside the medium. Monastic_copying_traditions derive high d amplified by identity_locked exit — they carry the constraint with them; they cannot leave the practice without dissolving the community it constitutes. Latin_students are listed among victims but are genuinely dual-positioned: their derived d sits below the pure-target end because the standard also delivers the access they seek; the commentary notes this asymmetry rather than papering over it with an override, since the override surface keys on power atom and would misapply across the moderate-power seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping one authoritative form of a language nobody speaks natively — remains live throughout the interval and beyond it, so this is not a resolved mandate persisting as shell. The hybrid design is anti-mandatrophic by construction: targeted correction self-limits (it repairs where evidence shows slippage rather than reoccupying the language wholesale), which is precisely what prevents drift into either a pure extraction regime (full reconstruction, in which every writer owes fealty to the reconstructors) or an inertial relic (an uncorrectable fossilized standard maintained by performance alone). The two live dangers are routed to omegas: collapse of the hybrid position into one of its siblings (omega kernel_reading_stability) and spread of performative correctness from the display subculture into the standard's core (omega purist_theater_separability). On the R5 mismatch check, founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no zombie flag, consistent with the computed tangled_rope profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_stability,
    'Is the hybrid position a stable third reading of the correct_latin kernel, or a transitional compromise that collapses into continuity (wherever textual evidence is thin or contested) or into discontinuity (wherever philological method gains confidence)?',
    'Track adjudicated cases over time: if disputed forms are increasingly settled by manuscript evidence alone, the reading collapses toward discontinuity; if appeals to established usage override evidence, it collapses toward continuity.',
    'Collapse toward discontinuity raises effective extraction (full delegitimation of transmitted practice places a reconstruction burden on every writer); collapse toward continuity removes the correction warrant and drops extraction toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether partial-continuity-with-correction is a stable reading or transitional between its siblings.').

omega_variable(
    naturalness_of_standard,
    'Is the hybrid standard a discovered feature of Latin — a classical core that independent manuscript traditions converge on when corrected — or a constructed authority whose ''core'' is whatever the correcting class certifies?',
    'Test convergence: independent editorial traditions starting from different manuscripts arriving at the same forms indicates discovery; persistent divergence tracking school and national lines indicates construction.',
    'If constructed, the constraint reclassifies toward snare-flavored extraction with the philological class as capturing beneficiary; if discovered, extraction above the coordination floor is the price of accuracy and supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_standard, empirical, 'Discovered linguistic fact versus constructed philological authority.').

omega_variable(
    extraction_floor_attribution,
    'How much of the measured extraction is inherent to maintaining any standard for a non-native learned language (a coordination cost every regime must pay), and how much is rent specific to the correction apparatus?',
    'Compare institutions holding the same textual evidence under different gatekeeping intensities — Jesuit colleges, Italian academies, Protestant gymnasia; extraction invariant across gatekeeping levels is floor, and the variable remainder is apparatus rent.',
    'A large invariant component lowers attributable extraction and supports the coordination framing; a large variable component indicts the apparatus and generates reclassification pressure toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_floor_attribution, empirical, 'Separating the coordination floor from apparatus-specific rent.').

omega_variable(
    purist_theater_separability,
    'Is the rising theatricality — Ciceronian affectation, ornamental imitation, orthographic point-scoring — a property of the hybrid standard itself, or of a separable purist subculture riding on it?',
    'Compare the output of working philological editions (functional correction) against display oratory and school exercises (performative correctness) across the interval.',
    'If separable, the theater_ratio overstates the standard''s decay and the constraint stays close to functional coordination; if inseparable, performative maintenance is spreading through the standard and piton drift becomes a live terminal path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purist_theater_separability, conceptual, 'Whether performative correctness contaminates the whole standard or stays confined to a display subculture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(corr_tr_t6, correct_latin__hybrid_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(corr_tr_t12, correct_latin__hybrid_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(corr_tr_t18, correct_latin__hybrid_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement(corr_tr_t24, correct_latin__hybrid_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(corr_tr_t30, correct_latin__hybrid_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(corr_be_t6, correct_latin__hybrid_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(corr_be_t12, correct_latin__hybrid_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(corr_be_t18, correct_latin__hybrid_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(corr_be_t24, correct_latin__hybrid_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(corr_be_t30, correct_latin__hybrid_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(corr_su_t6, correct_latin__hybrid_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(corr_su_t12, correct_latin__hybrid_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(corr_su_t18, correct_latin__hybrid_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(corr_su_t24, correct_latin__hybrid_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(corr_su_t30, correct_latin__hybrid_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes into three structurally distinct constraints — one per reading of the kernel — with materially different epsilon values: the continuity reading imposes no correction burden (lowest extraction), the discontinuity reading fully delegitimizes transmitted practice and places a reconstruction burden on every writer (highest extraction), and the hybrid reading (this file) occupies the middle: dual mastery plus targeted correction, with adjudicative authority concentrated in the philologically trained. The upstream/downstream structure runs through the evidentiary apparatus: the hybrid reading builds the critical-edition machinery that the discontinuity reading later radicalizes, while preserving enough transmission-legitimacy to keep the continuity reading live in liturgical and administrative contexts. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
