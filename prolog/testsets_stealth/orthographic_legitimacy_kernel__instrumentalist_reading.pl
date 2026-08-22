% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Reading of Orthographic Legitimacy (Script-as-Tool Doctrine)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic replaced the Arabic-derived Ottoman script
 *   with a Latin-based alphabet, presented purely as a
 *   literacy-and-efficiency measure: a phonemically transparent script
 *   teachable in months, cheap to print, administratively uniform. This story
 *   instantiates the INSTRUMENTALIST READING of the orthographic-legitimacy
 *   kernel — the claim that a writing system's legitimacy rests on measured
 *   literacy and administrative performance, with script treated as a neutral
 *   tool rather than an identity marker. The eps referent is the standing
 *   arrangement under contest: the mandated Latin orthography, the state-run
 *   literacy campaigns, and the public retirement of the Arabic script —
 *   assessed by this reading's own lights, which is why the elite's
 *   devaluation appears as an acknowledged cost justified by the statistics
 *   rather than as the reading's endorsed alternative. Sibling readings
 *   (modernist, continuity) are separate constraint files linked through
 *   network.affects_constraints; the kernel contest is routed to omega
 *   variables, not folded into this classification. Claim and metrics are
 *   authored independently: claimed_type records the structure I believe true
 *   (a genuine coordination function carrying real asymmetric extraction);
 *   the metrics record the arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - reformist_state_bureaucracy: agenda-setter and primary administrative beneficiary (institutional/arbitrage) — drafts the script law, runs the campaigns, converts the registries, collects the efficiency gains
 *   - newly_literate_masses: primary intended beneficiary (powerless/constrained) — acquires literacy through state courses and schools; cannot conduct official life in any other script
 *   - arabic_literate_ulema: primary target (organized/identity_locked) — judges, preachers, professors whose professional medium and self-concept are bound to the Arabic-script corpus
 *   - ottoman_script_craftsmen: secondary target (moderate/trapped) — calligraphers, typesetters, copyists whose craft capital is stranded by the switch to Latin letterforms
 *   - state_schoolteachers: dual-positioned implementer (moderate/constrained) — gains salaried employment and status; supplies the daily enforcement labor in classrooms
 *   - rural_women_outside_school_system: excluded voice (powerless/trapped) — named in campaign literature as chief beneficiaries, seated in none of the reform bodies, reached last by village schooling
 *   - international_literacy_assessors: analytical observer (institutional/analytical) — foreign educators and statistical agencies whose comparative measurements feed back into the reform's legitimacy claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.48).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.32).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Reading of Orthographic Legitimacy (Script-as-Tool Doctrine)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'd81f2ef9-7989-4307-811a-55bdbd3d87f5').
narrative_ontology:cs_kernel_codification('d81f2ef9-7989-4307-811a-55bdbd3d87f5', formalized).
narrative_ontology:cs_authority_grounding('d81f2ef9-7989-4307-811a-55bdbd3d87f5', expertise).
narrative_ontology:cs_interpretation_layer_present('d81f2ef9-7989-4307-811a-55bdbd3d87f5').
narrative_ontology:cs_reading_relation('d81f2ef9-7989-4307-811a-55bdbd3d87f5', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d81f2ef9-7989-4307-811a-55bdbd3d87f5', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_axiom('d81f2ef9-7989-4307-811a-55bdbd3d87f5', foundational, literacy_outcome_supreme_criterion).
narrative_ontology:cs_axiom_status(literacy_outcome_supreme_criterion, holdable).
narrative_ontology:cs_axiom_grounding('d81f2ef9-7989-4307-811a-55bdbd3d87f5', literacy_outcome_supreme_criterion, empirically_contingent).
narrative_ontology:cs_axiom('d81f2ef9-7989-4307-811a-55bdbd3d87f5', foundational, script_is_neutral_instrument).
narrative_ontology:cs_axiom_status(script_is_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('d81f2ef9-7989-4307-811a-55bdbd3d87f5', script_is_neutral_instrument, instrumental).
narrative_ontology:cs_reference_frame('d81f2ef9-7989-4307-811a-55bdbd3d87f5', measurable_outcome_standard).
narrative_ontology:cs_drift_state('d81f2ef9-7989-4307-811a-55bdbd3d87f5', comparative_literacy_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d81f2ef9-7989-4307-811a-55bdbd3d87f5', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_masses).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_schoolteachers).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, reformist_state_bureaucracy).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, ottoman_script_craftsmen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, state_schoolteachers).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, instrumentalist_literacy_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__instrumentalist_reading, phonemic_transparency_efficiency_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces the script law, runs the mass literacy campaigns and night schools, converts census and land registries to the new orthography, and staffs the standing spelling commissions. Gains a single uniform administrative medium, an expanded schooling apparatus, and independence from foreign typesetting. Can amend or abandon the arrangement at will and has repeatedly restructured adjacent language policy without structural obstacle.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, reformist_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Adults enrolled in the emergency literacy courses and children schooled entirely in the new alphabet acquire reading and writing in months rather than years. The gain is real and large, but it arrives on the state's terms: every official transaction, employment credential, and printed good assumes the mandated script, and conducting life in any other orthography is not available inside the country's institutions.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_masses, beneficiary,
    powerless, biographical, constrained, national).

% Judges, theology professors, sermon-givers, and jurisconsults whose entire professional formation ran through Arabic-script texts. Overnight their working medium becomes unusable in courts, schools, and official publication; preaching and religious printing are confined to narrow licensed channels. Their self-concept as custodians of an unbroken millennium-long textual chain makes retraining in the new alphabet feel like surrendering the office itself. Some retreat to private teaching, some emigrate eastward; ties across the wider Muslim world sustain a shadow career but not domestic standing.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_ulema, payer,
    organized, civilizational, identity_locked, continental).

% Calligraphers, lithographers, hand-copyists, and Arabic-type compositors with decades of apprenticeship invested in the old letterforms. Demand collapses as government printing, newspapers, and commercial signage switch to Latin type almost simultaneously. Mid-career retraining means beginning a new trade from the bottom; a minority pivot to museum conservation or teaching the decorative arts as heritage subjects.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, ottoman_script_craftsmen, payer,
    moderate, biographical, trapped, national).

% Recruited in large cohorts to staff the new village and town schools. They gain salaried employment, urban posting prospects, and public standing as agents of the new order. They must master the unfamiliar alphabet quickly, teach in it exclusively, and supply the daily enforcement labor — correcting old-script habits, reporting holdout instructors — and they absorb the community friction this produces in conservative districts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_schoolteachers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_schoolteachers, payer).

% Named in campaign literature as the chief intended beneficiaries of mass literacy. In practice they are reached last and least by village schooling, hold no seat in any reform commission, and appear in the record mainly as aggregate percentages in statistics compiled by others. Their literacy arrives decades late and is counted in figures they never see or contest.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, rural_women_outside_school_system, excluded,
    powerless, biographical, trapped, local).

% Foreign educators attached to League-era missions and later UN-system statisticians and comparative-education researchers. They measure enrollment, self-reported literacy, and administrative throughput, and compare script-reform countries against countries that retained the older script. Their published comparisons feed directly back into the reform's legitimacy claims and are cited by both defenders and critics.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, international_literacy_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, reformist_state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mass literacy acquisition and standardized administration: a phonemically transparent alphabet teachable in months replaces a script requiring years, unifies official record-keeping across a large territory, and makes printing cheap enough to be domestically controlled.
% TRANSFER_FUNCTION: Moves textual authority and literacy access from a small Arabic-script learned elite to the mass population and the state apparatus; simultaneously strips the old elite of the scarcity value of their skill and severs everyday public access to the pre-reform textual corpus.
% ABSENT_VOICES: Rural women outside the school system, non-Turkish-speaking Muslim communities whose own languages were being transliterated without consultation, and continuity-minded literati recast as reactionaries rather than interlocutors. They stand outside the reform commissions, which were staffed by statist modernizers; their objections survive only obliquely, in court records over old-script signage, private correspondence, and exile publications.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, official textuality would fork, schooling would split by script, a century of records would become inaccessible to the next cohort, and the print economy built on Latin typefaces would unwind — the administrative and educational arrangements of the country visibly depend on the standing orthography.
% FOUNDING_PROBLEM: Mass illiteracy (roughly nine in ten adults) under a script requiring years of study, an administrative bottleneck in record-keeping and personnel, and dependence on foreign shops for typesetting and printing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the League-of-Nations-era assessments and the subsequent UNESCO literacy series, and by comparative-historical demography establishing the pre-1928 plateau. No external source attests that the founding problem remains live; the perpetual-campaign rhetoric of continued vigilance circulates only within the state apparatus and its own literature.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (interval end): the arrangement solves a real collective-action problem — mass literacy acquisition and administrative standardization — while imposing a real asymmetric cost: immediate devaluation of the Arabic-literate elite's professional capital and a permanent barrier between the population and its pre-1928 textual corpus. Extraction was front-loaded (0.62 at enactment, when the confiscatory transition bit hardest), decayed as the directly harmed cohort aged out, then ticked back up as the archive barrier compounded across generations. Suppression (0.32 at interval end) is a raw structural property, unscaled by power or scope: the state retired the Arabic script from public use through law, schooling, and printing economics. The suppression_requirement series is included because this story specifically traces enforcement-capacity change — mobilizational peak (0.78) decaying to routine self-sustainment (0.32) — not merely shifting extraction. Theater peaks mid-interval (0.31 circa 1960) when self-reported literacy statistics became propaganda instruments, then settles at 0.25 as measurement discipline improved; the underlying teaching function stayed real throughout, which is why theater never approaches inertial-performance territory. Accessibility_collapse 0.5: within official domains alternatives collapsed completely once the instrumentalist premise was adopted; private and religious domains retained Arabic-script alternatives throughout. Resistance 0.45: organized clerical opposition was broken early by a strong state, leaving diffuse passive persistence rather than open resistance. All three series share one seven-point grid (1928-2024) so every metric is authored at every examined time point. Receipt surface: the gains demonstrably accrue to the reformist_state_bureaucracy seat — unified official textuality, an expanded apparatus, the promised efficiency; the masses receive access, which is benefit, but the extracted value (the elite's displaced function, the suppressed alternative) lands on the setter's desk. Fixing cost is prohibitive: reversal would strand a century of schooling, records, and type.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement computes as the coordination it built and still runs; from the newly literate seats it computes as a subsidy received; from the ulema and craftsman seats the same structure computes as expropriation of irreplaceable skill. The engine derives these divergent per-seat classifications from the declared positions and exits; nothing in the authored claim adjudicates between them. Note the failed-coalition structure: the two victim classes never combined — the ulema's status distance from manual-trade craftsmen made joint action unavailable — so fragmented targets faced an organized setter sequentially rather than in concert.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto the derivation: the bureaucracy (beneficiary, arbitrage exit) sits near the full-beneficiary end; the masses (declared beneficiaries, constrained exit) sit low-d but less extremely than the setter, since their benefit arrives on the state's terms; teachers (beneficiary with a real payer secondary role) land near symmetric; the ulema (victim, identity_locked) and craftsmen (victim, trapped) sit near the full-target end, with identity lock pushing the ulema further toward full target than exit constraint alone would. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct ordering, and the override mechanism is keyed by power atom — too coarse to improve on the derivation for this seat structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass illiteracy blocking the republican project — is dead by the reading's own metric: literacy is near-universal, corroborated externally. The arrangement persists anyway because reversal is prohibitively costly and because the administrative function it carries is live. The R5 mismatch (status=dead x verdict=world_rearranges) is therefore expected and honestly authored; what it detects here is not zombie rent collection but a completed transition that never carried a sunset clause — scaffold-shaped origins hardened into infrastructure. Against the inertial-performance reading: theater stays low, the coordination function is real, and a concentrated receiver exists — the cost-asymmetry (fixer's cost exceeds its stake) alone does not make this vestigial, because the function would still be performed under any redesign. The mandatrophy lens earns its keep by blocking two mislabels at once: the coordination is real enough to block a pure-extraction reading, and the extraction is real enough to block a pure-coordination reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexation,
    'This constraint is one reading of the orthographic_legitimacy_kernel; would instantiating the modernist or continuity reading instead produce a different victim set, epsilon, and classification for the same 1928 script regime?',
    'Generate the sibling reading stories (orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading) over the same referent and compare per-seat classifications and epsilon across the family.',
    'The continuity reading would widen the victim set to the whole literate public severed from its textual past and push epsilon upward; the modernist reading would re-center beneficiaries on Western-aligned elites and reframe the extraction as civilizational gatekeeping. Divergence would locate the disagreement in what counts as harm, not in any disputed fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexation, conceptual, 'Committer structure: reading-indexed classification over a shared kernel; sibling readings are separate constraints.').

omega_variable(
    literacy_gain_attribution,
    'How much of the post-1928 literacy increase is attributable to the script change itself versus the simultaneous expansion of state schooling, given that Arabic-script countries such as Egypt and Iran later reached mass literacy?',
    'Comparative econometrics: difference-in-differences on literacy growth across script-reform and script-retention countries, controlling for schooling expenditure, urbanization, and compulsory-education enforcement.',
    'If the script''s marginal contribution is small, the instrumentalist justification is substantially post-hoc, the coordination claim weakens, and effective extraction at the payer seats rises; if large, the reading''s core axiom is vindicated and the moderate-extraction profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Attribution of literacy gains to script change versus concurrent schooling investment.').

omega_variable(
    enforcement_decay_interpretation,
    'Does the falling suppression series reflect voluntary internalization of the new script, or atrophying enforcement capacity over a persistent private preference for the old script?',
    'Time-series of prosecutions and administrative sanctions for Arabic-script public use, alongside survey and market data on private demand (licensed religious publishing, calligraphy markets, diaspora printing).',
    'Persistent latent demand would mean the arrangement is coercively held at reduced intensity, raising effective suppression at the payer seats and pulling per-seat classifications toward extraction; genuine internalization would confirm stabilization at the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_interpretation, empirical, 'Whether low current suppression reflects consent or enforcement-capacity decay.').

omega_variable(
    generational_archive_barrier_status,
    'Are later generations unable to read pre-1928 texts without special training victims of the standing arrangement, or bearers of the ordinary cost of any script standardization?',
    'Conceptual analysis distinguishing imposed severance (a reachable alternative dismantled by fiat) from ordinary transmission loss, tested against cases of voluntary orthographic evolution where no comparable barrier arose.',
    'Counting the archive barrier as harm widens the victim class beyond the transition cohort and raises epsilon; treating it as ordinary change cost confines victims to the initial elite and craftsmen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_archive_barrier_status, conceptual, 'Scope of the victim class across generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ortho_instrumentalist_tr_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1928, 0.14).
narrative_ontology:measurement(ortho_instrumentalist_tr_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1935, 0.21).
narrative_ontology:measurement(ortho_instrumentalist_tr_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1945, 0.29).
narrative_ontology:measurement(ortho_instrumentalist_tr_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1960, 0.31).
narrative_ontology:measurement(ortho_instrumentalist_tr_t1980, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1980, 0.27).
narrative_ontology:measurement(ortho_instrumentalist_tr_t2000, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(ortho_instrumentalist_tr_t2024, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(ortho_instrumentalist_be_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1928, 0.62).
narrative_ontology:measurement(ortho_instrumentalist_be_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1935, 0.56).
narrative_ontology:measurement(ortho_instrumentalist_be_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(ortho_instrumentalist_be_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1960, 0.47).
narrative_ontology:measurement(ortho_instrumentalist_be_t1980, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement(ortho_instrumentalist_be_t2000, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(ortho_instrumentalist_be_t2024, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ortho_instrumentalist_su_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement(ortho_instrumentalist_su_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1935, 0.71).
narrative_ontology:measurement(ortho_instrumentalist_su_t1945, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1945, 0.61).
narrative_ontology:measurement(ortho_instrumentalist_su_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1960, 0.51).
narrative_ontology:measurement(ortho_instrumentalist_su_t1980, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1980, 0.41).
narrative_ontology:measurement(ortho_instrumentalist_su_t2000, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(ortho_instrumentalist_su_t2024, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, ottoman_turkish_lexical_purification).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the script reform debate' covers three structurally distinct claims about what makes an orthography legitimate. This file authors the instrumentalist reading (epsilon indexed to literacy/efficiency outcomes; victims confined to devalued skill-holders). The continuity reading authors epsilon over the same standing arrangement but counts the whole literate public's severance from its textual corpus as harm (higher epsilon, widened victim set); the modernist reading indexes epsilon to civilizational alignment and centers Western-facing elites. The upstream/downstream pressure runs from this reading outward: its statistical successes were cited as evidence by the modernist reading, and its enforcement machinery is what the continuity reading measures its losses against. All three files link each other via network.affects_constraints; the lexical purification movement (ottoman_turkish_lexical_purification) is a downstream dependent that this script regime enabled and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
