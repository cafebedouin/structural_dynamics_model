% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Latin-Script Exclusivity Regime (Kemalist Rupture Reading)
 *   domain: linguistic/political/state-building
 *
 * SUMMARY:
 *   On 1 November 1928 the Grand National Assembly enacted exclusive
 *   Latin-script orthography for Turkish; within months Arabic-letter
 *   publication was barred, national schools mobilized every adult into
 *   literacy classes, and within a single generation Arabic-script Turkish
 *   literacy collapsed demographically. This file instantiates the KEMALIST
 *   RUPTURE READING of the script_as_identity kernel: the arrangement is
 *   assessed from the seat that holds textual severance as the enabling act
 *   of secular national modernity, with textual rupture as feature rather
 *   than bug and the state monopolizing the literacy apparatus to complete
 *   it. Per the epsilon-referent rule, the referent is the standing
 *   Latin-only enforcement regime — never the reading's endorsed
 *   counterfactual — and epsilon is reading-indexed: from this seat the
 *   displacement of the old learned class is the reform working as designed,
 *   so authored epsilon is moderate-low (0.36) despite a coercive operational
 *   record. Claim and metrics are independent authored facts: claimed_type
 *   tangled_rope reflects my structural assessment (genuine coordination
 *   function, real asymmetric extraction, active enforcement); the metrics
 *   describe the arrangement's actual operation. The sibling readings
 *   (ottoman_continuity, phonetic_instrumentalism) are separate files with
 *   their own epsilon over the same referent, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   kemalist_republican_state: Agenda setter (institutional/arbitrage) —
 *   legislates and enforces exclusivity; collects the literacy-certification
 *   monopoly - new_secular_intelligentsia: Primary beneficiary
 *   (powerful/mobile) — careers constituted by the new script economy -
 *   state_education_bureaucracy: Secondary beneficiary and administrator
 *   (institutional/constrained) — runs certification -
 *   ottoman_script_literati: Primary target (moderate/trapped) — professional
 *   capital rendered illegible - arabic_script_publishers: Target
 *   (moderate/constrained) — trade banned outright - pre_reform_citizenry:
 *   Mass target with partial benefit (powerless/trapped) — compulsory
 *   re-literacy, heritage access severed - religious_textual_establishment:
 *   Target (organized/trapped) — transmission chain broken -
 *   exiled_ottomanist_scholars: Excluded voice (moderate/mobile abroad) —
 *   objection recorded outside the polity -
 *   republican_historiography_analyst: Analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.36).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.55).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Latin-Script Exclusivity Regime (Kemalist Rupture Reading)").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "linguistic/political/state-building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'aa973dbb-54a8-485c-b075-4642d06e2aa0').
narrative_ontology:cs_kernel_codification('aa973dbb-54a8-485c-b075-4642d06e2aa0', formalized).
narrative_ontology:cs_authority_grounding('aa973dbb-54a8-485c-b075-4642d06e2aa0', extraction).
narrative_ontology:cs_interpretation_layer_present('aa973dbb-54a8-485c-b075-4642d06e2aa0').
narrative_ontology:cs_reading_relation('aa973dbb-54a8-485c-b075-4642d06e2aa0', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('aa973dbb-54a8-485c-b075-4642d06e2aa0', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('aa973dbb-54a8-485c-b075-4642d06e2aa0', foundational, textual_severance_enables_secular_modernization).
narrative_ontology:cs_axiom_status(textual_severance_enables_secular_modernization, holdable).
narrative_ontology:cs_axiom_grounding('aa973dbb-54a8-485c-b075-4642d06e2aa0', textual_severance_enables_secular_modernization, instrumental).
narrative_ontology:cs_axiom('aa973dbb-54a8-485c-b075-4642d06e2aa0', secondary, state_monopoly_on_literacy_certification).
narrative_ontology:cs_axiom_status(state_monopoly_on_literacy_certification, holdable).
narrative_ontology:cs_axiom_grounding('aa973dbb-54a8-485c-b075-4642d06e2aa0', state_monopoly_on_literacy_certification, conventional).
narrative_ontology:cs_reference_frame('aa973dbb-54a8-485c-b075-4642d06e2aa0', rupture_as_founding_normality).
narrative_ontology:cs_drift_state('aa973dbb-54a8-485c-b075-4642d06e2aa0', contemporary_neo_ottoman_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aa973dbb-54a8-485c-b075-4642d06e2aa0', '2026-08-05T09:15:00Z').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_republican_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, new_secular_intelligentsia).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_script_literati).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_publishers).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, pre_reform_citizenry).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_textual_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, pre_reform_citizenry).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secularization_through_institutional_rupture).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, legislative_supremacy_of_revolutionary_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1928 Alphabet Law making modified Latin letters the sole legal orthography for Turkish; funds the national schools that teach it; bars Arabic-letter publication; issues the literacy certificates that employment and office increasingly require. It defines the rules it administers, and its gatekeeping position over the literacy apparatus is what the exclusivity maintains.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_republican_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Journalists, teachers, lawyers, and officials whose schooling and careers are built entirely in the new script; they staff the republic's newspapers, courts, and ministries, and their professional standing depends on the old script staying retired.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, new_secular_intelligentsia, beneficiary,
    powerful, biographical, mobile, national).

% Runs the literacy campaigns, trains the teachers, administers the examinations; its budget and personnel expanded with the reform's enforcement; it executes the exclusivity rules day to day and certifies who counts as literate.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, state_education_bureaucracy, agenda_setter).

% Scribes, poets, calligraphers, and religious scholars whose entire professional capital — handwriting, textual memory, patronage networks — was denominated in the Arabic-letter Ottoman tradition. Within months of the law their skills had no legal market; retraining meant abandoning the identity their craft expressed. Many withdrew into private circles or poverty.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_script_literati, payer,
    moderate, biographical, trapped, national).

% Printers and booksellers holding Arabic-letter type, plates, and stock; the trade in their inventory was banned outright. Presses converted to Latin composition or idled; capital sunk in Arabic-letter setting became unsellable.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_publishers, payer,
    moderate, biographical, constrained, national).

% Adults compelled into night-school literacy classes on pain of fine; grandparents' letters, deeds, and gravestones became illegible to their grandchildren within a generation. They gained state-certified literacy in the new script while losing unmediated access to everything written before 1928.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, pre_reform_citizenry, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, pre_reform_citizenry, beneficiary).

% Qur'an schools, lodge libraries, and their teachers, whose chain of textual transmission ran through Arabic-letter Ottoman. With the medreses and tekkes already closed in 1924-25, the script reform severed their last reproductive link; surviving religious instruction retreated to liturgical Arabic memorization.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_textual_establishment, payer,
    organized, generational, trapped, national).

% Historians and men of letters who left the country or were pushed to its margins; they published the era's most sustained objections to the severance in exile journals and European presses — voices with standing in the old order, absent from the deliberations that retired it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, exiled_ottomanist_scholars, excluded,
    moderate, biographical, mobile, continental).

% Assesses the reform across the century: literacy curves, archive-access costs, generational rupture, and the political afterlives of the script question. Takes no part in the arrangement and collects nothing from it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, republican_historiography_analyst, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_republican_state).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the republic's schooling, administration, and publishing on a single national orthography, enabling mass literacy campaigns run through state schools and aligning the country's textual infrastructure with Western typographic and commercial systems. Stated without evaluation.
% TRANSFER_FUNCTION: Moves literacy-certification authority and cultural-legitimacy production from the Ottoman-Islamic learned class (scribes, ulema, calligraphers, Arabic-script printers) to the republican state and its new secular intelligentsia; moves adults' time and resources into compulsory re-literacy; renders the inherited textual corpus inaccessible to non-specialists.
% ABSENT_VOICES: The Ottoman-script literati and Arabic-script printers stood in the assembly's galleries but not its deliberations in any effective sense — the law passed in weeks. The illiterate peasantry, the reform's largest subject population, had no vote and no voice. Religious authorities had been institutionally decapitated in the preceding four years (caliphate abolished 1924, tekkes closed 1925). The sharpest contemporaneous objections survive in exile publications and private correspondence, not in the record the reform's beneficiaries curated.
% DISAPPEARANCE_RATIONALE: Overnight removal of the exclusive-Latin regime would immediately bifurcate Turkish publishing, schooling, and administration; the state's literacy-certification monopoly — the seat the gains accrue to — would dissolve; a dual-script market would re-emerge wherever Arabic-script competence survived; the new intelligentsia's gatekeeping position and the education bureaucracy's mandate would collapse. The arrangement is load-bearing for the republic's entire textual order.
% FOUNDING_PROBLEM: As this reading states it: a new nation-state inherited a textual apparatus anchored in a religiously constituted learned class and an Arabic-letter curriculum, and needed a nationally integrated, secularly governed citizenry with mass literacy; the founding problem was how to break the old apparatus's hold on literacy without breaking the state.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: international literacy statistics and foreign diplomatic reporting of the period corroborate the illiteracy crisis the reform addressed; comparative cases (Soviet Central Asia's parallel script ruptures; Japan's and Greece's non-rupture modernizations) bear on but do not settle the necessity claim; exiled Ottomanist historians and, later, Turkish archival scientists corroborate the continuity-loss side. No source outside the beneficiary set attests the strong claim that severance was necessary rather than merely sufficient — that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).
:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.36 is reading-indexed over the fixed referent: the rupture seat counts the literati's displacement as the reform succeeding, and concedes as residual cost only the compulsory re-literacy burden, coerced holdouts, and heritage-access loss. Suppression 0.55 is a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine): the legal foreclosure of Arabic-script Turkish publication persisted across the whole interval even as active policing decayed. Theater_ratio 0.28: the pedagogical function was real (literacy rose sharply), but a growing share of activity is commemorative — alphabet anniversaries, founder iconography — rising as active instruction completed. Accessibility_collapse 0.82: once the regime was understood, alternatives collapsed near-completely; dual-script literacy went extinct outside specialist archives within one generation. Resistance 0.42: parliamentary dissent, clandestine Arabic-script writing, and religious objection were real but decapitated — the medrese and tekke closures of 1924-25 removed the opposition's organizational spine before the reform passed, and single-party discipline held the assembly. Coalition note: the victim set was broad but coalition-incapable — literati scattered, publishers asset-frozen, citizenry atomized and fined individually — which is why 0.42 resistance held despite the affected population's size. Measurements run on one shared six-point grid (every tracked metric authored at every point, t=0..30 mapping 1928-1958); suppression_requirement is authored because enforcement capacity demonstrably changed — hardening through the early 1930s, then demographic normalization as resistant cohorts aged out — a decay trajectory, not a static picture. No cyclical oscillation is authored: the dynamic is monotonic decay plus slow commemorative accretion, and no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the agenda-setter seat (state, education bureaucracy) the arrangement is the founding act of national literacy — coordination it built, administers, and legitimately profits from administering. From the literati and publisher seats the same structure is overnight expropriation of irreplaceable, script-specific capital with no legal exit. From the citizen seat it is simultaneously a compulsory tax (re-learning under fine) and a genuine gift (access to schooling, administration, print). The authored claim does not adjudicate among these; the structural data — who pays, who collects, who can leave — drives the per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map cleanly onto the structural relationships: the state and its apparatus sit at the beneficiary pole (they collect certification authority and legitimacy production); the literati, publishers, and religious establishment sit at the target pole (trapped, script-specific capital destroyed); the mass citizenry is victim-primary but genuinely subsidized with literacy, hence dual-roled. Two overrides are declared where the automatic derivation would misplace d. Institutional -> 0.10: a naive derivation reading the agenda-setter's enforcement burden as cost-bearing would damp d toward symmetric, but the state bears trivial net cost and defines the rules it enforces — near-full beneficiary. Powerless -> 0.72: derivation from the victim declaration alone would push the citizenry toward ~0.95, but the literacy gains are real and broadly received, so the true position is somewhat short of full target. No override is needed for the moderate atom: the literati (trapped) and publishers (constrained) derive correctly to the high-d region from their victim declarations and exit positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabelings. Calling the arrangement a snare erases the real coordination function — mass literacy delivered through a single teachable orthography was genuinely achieved, and the benefits reached the powerless seat, which a pure extraction story cannot accommodate. Calling it a rope erases the expropriated class and the coerced severance — a coordination story with no victims cannot explain the destroyed profession, the banned trade, or the fines. Tangled_rope holds both halves: coordination and extraction ride the same structure, held together by active enforcement. Mandatrophy: the founding problem (secular mass-national integration unmediated by the Islamic learned class) was substantially accomplished by mid-century, and the acute enforcement machinery decayed into legal prohibition plus commemoration — the classic precursor signature. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the parties dispute whether the problem is done, but the arrangement remains load-bearing either way, so no zombie flag fires. Piton risk is real and tracked (rising theater_ratio, enforcement_vs_demographic_completion omega) but the function has not atrophied — the schools still teach, the presses still print, the certification still gates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame_dependence,
    'Does this classification hold only under the kemalist_rupture_reading''s severance-as-liberation frame, given that this story is one reading of the script_as_identity kernel?',
    'Cross-reading comparison over the identical referent (the standing Latin-only enforcement regime): the ottoman_continuity_reading authors epsilon over the same arrangement from the opposite axiology, and the phonetic_instrumentalism_reading authors it from a neutrality frame; classification divergence across the three files is the measurement, not noise.',
    'If the rupture frame breaks politically (neo-Ottoman rehabilitation becomes the governing frame), this reading''s low reading-indexed epsilon loses its warrant and the same arrangement recomputes toward extraction-dominant classifications; the sibling files carry the complementary deltas.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_dependence, conceptual, 'Committer structure: this constraint is the kemalist_rupture_reading instantiation of kernel script_as_identity; sibling readings are separate constraints.').

omega_variable(
    incumbent_displacement_accounting,
    'The reading''s structural delta asserts zero transition cost because there are ''no incumbents to displace'' — is the destruction of the Ottoman literati''s cultural capital a cost under this reading''s own axiology, or does the reading''s value frame render it invisible by counting their displacement as the reform succeeding?',
    'Welfare accounting run twice over the same transition: once excluding incumbent script-specific capital (the reading''s implicit frame) and once including it (professional income, patronage networks, irreplaceable textual access); compare resulting epsilon.',
    'If incumbent losses count, epsilon rises materially above the authored 0.36 and the tangled_rope balance shifts toward extraction-dominant; if they do not count, the reading''s zero-cost claim is internally coherent and the low epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_displacement_accounting, conceptual, 'Whether the reading''s zero-transition-cost delta is a structural fact or an artifact of its own beneficiary-weighted accounting.').

omega_variable(
    enforcement_vs_demographic_completion,
    'Does the exclusivity regime now persist by ongoing enforcement or by demographic completion — the Arabic-script-literate cohorts aging out faster than any enforcement could matter?',
    'Separate enforcement expenditure and prosecution records from demographic surveys of Arabic-script Turkish literacy across the interval; test whether the suppression_requirement decline tracks policy relaxation or cohort attrition.',
    'If persistence is now inertial-demographic, the arrangement drifts toward piton-like maintenance (commemoration without enforcement, rising theater_ratio); if enforcement remains load-bearing, tangled_rope dynamics with active suppression continue indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_demographic_completion, empirical, 'Enforcement-machinery versus cohort-attrition accounts of the regime''s persistence.').

omega_variable(
    severance_necessity_attribution,
    'Was severing the Ottoman-Islamic textual past causally necessary for secular mass modernization, or merely coincident with it — the attribution problem underneath the reading''s foundational axiom?',
    'Comparative analysis: Soviet Central Asian script ruptures under a different ideology, and modernizers that changed little or nothing typographically (Japan; Greece''s simplification rather than replacement); test whether literacy and secularization outcomes track script rupture or state capacity and schooling investment.',
    'If severance was not necessary, the foundational axiom loses its empirical grounding, the arrangement''s justification reduces to ordinary standard-setting, and the extraction the reading currently discounts becomes uncompensated cost; if necessary, the reading''s warrant strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(severance_necessity_attribution, empirical, 'Necessity versus sufficiency of textual rupture for the modernization outcome the reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(scri_tr_t0, observed).
narrative_ontology:measurement(scri_tr_t6, script_as_identity__kemalist_rupture_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(scri_tr_t6, observed).
narrative_ontology:measurement(scri_tr_t12, script_as_identity__kemalist_rupture_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(scri_tr_t12, observed).
narrative_ontology:measurement(scri_tr_t18, script_as_identity__kemalist_rupture_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(scri_tr_t18, observed).
narrative_ontology:measurement(scri_tr_t24, script_as_identity__kemalist_rupture_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(scri_tr_t24, observed).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__kemalist_rupture_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(scri_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(scri_be_t0, observed).
narrative_ontology:measurement(scri_be_t6, script_as_identity__kemalist_rupture_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(scri_be_t6, observed).
narrative_ontology:measurement(scri_be_t12, script_as_identity__kemalist_rupture_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(scri_be_t12, observed).
narrative_ontology:measurement(scri_be_t18, script_as_identity__kemalist_rupture_reading, base_extractiveness, 18, 0.43).
narrative_ontology:measurement_basis(scri_be_t18, observed).
narrative_ontology:measurement(scri_be_t24, script_as_identity__kemalist_rupture_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(scri_be_t24, observed).
narrative_ontology:measurement(scri_be_t30, script_as_identity__kemalist_rupture_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement_basis(scri_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(scri_su_t0, observed).
narrative_ontology:measurement(scri_su_t6, script_as_identity__kemalist_rupture_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement_basis(scri_su_t6, observed).
narrative_ontology:measurement(scri_su_t12, script_as_identity__kemalist_rupture_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(scri_su_t12, observed).
narrative_ontology:measurement(scri_su_t18, script_as_identity__kemalist_rupture_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(scri_su_t18, observed).
narrative_ontology:measurement(scri_su_t24, script_as_identity__kemalist_rupture_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(scri_su_t24, observed).
narrative_ontology:measurement(scri_su_t30, script_as_identity__kemalist_rupture_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(scri_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script reform' decomposes into three structurally distinct constraints — three readings of the script_as_identity kernel, each with its own epsilon over the same standing referent (the post-1928 Latin-only enforcement regime). This file instantiates the kemalist_rupture_reading (low reading-indexed epsilon: rupture as liberation). The ottoman_continuity_reading authors very high epsilon over the identical arrangement (constitutive identity expropriated); the phonetic_instrumentalism_reading authors low epsilon on neutrality grounds (superior phonetic technology adopted). The upstream political decision (this reading) created the institutional vehicle the instrumentalist reading later occupied; the continuity reading is the displaced incumbent's account. Linked via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, institutional, 0.1).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
