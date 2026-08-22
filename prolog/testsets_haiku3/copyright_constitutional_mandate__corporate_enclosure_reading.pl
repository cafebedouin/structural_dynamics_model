% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal Property Right with Extended Term (Corporate Enclosure Reading)
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the corporate enclosure reading of the U.S.
 *   Copyright Clause. The reading interprets 'limited times' as compatible
 *   with maximal extension (life + 70 years for works made for hire;
 *   potentially renewable indefinitely without explicit perpetuity language).
 *   It treats copyright as a property right requiring absolute protection,
 *   justifying DMCA circumvention bans, aggressive enforcement against
 *   derivative works, and the systematic foreclosure of fair use through
 *   technological locks. The reading's primary beneficiaries are corporate
 *   copyright holders (Disney, RIAA, MPAA) who collect ongoing licensing
 *   revenue and maintain gatekeeper control over cultural distribution. Its
 *   victims are derivative creators, educators, archivists, and independent
 *   artists, whose access to and remix of existing cultural works is
 *   restricted. This is one reading of a contested kernel; sibling readings
 *   (public_scaffold_reading, judicial_ambiguity_reading) interpret the same
 *   Constitutional text and copyright statute differently, with different
 *   beneficiary/victim structures and far lower extractiveness.
 *
 * KEY AGENTS:
 *   - corporate_copyright_holders: institutional beneficiaries; set and enforce the reading through lobbying and litigation
 *   - derivative_creators: moderate-power victims; pay licensing fees, face fair-use erosion
 *   - educators_and_archivists: organized victims; bear legal liability and licensing costs for preservation and teaching
 *   - independent_artists: powerless victims; trapped between licensing costs and inability to enforce against larger entities
 *   - public_domain_advocates: excluded powerful actors; would reverse the reading but are locked out of legislative machinery
 *   - Congress and enforcement authorities: agenda setters; author the reading through term extensions and enforcement priorities, facing concentrated beneficiary lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.72).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal Property Right with Extended Term (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '5c879015-e0a1-4f2c-be5b-1501ba21b4ef').
narrative_ontology:cs_kernel_codification('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', fixed_text).
narrative_ontology:cs_authority_grounding('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', extraction).
narrative_ontology:cs_interpretation_layer_present('5c879015-e0a1-4f2c-be5b-1501ba21b4ef').
narrative_ontology:cs_reading_relation('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', foundational, copyright_as_property_maximalism).
narrative_ontology:cs_axiom_status(copyright_as_property_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', copyright_as_property_maximalism, deontological).
narrative_ontology:cs_axiom('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', foundational, limited_times_as_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_as_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', limited_times_as_maximal_extension, conventional).
narrative_ontology:cs_reference_frame('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', copyright_as_perpetual_property).
narrative_ontology:cs_drift_state('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c879015-e0a1-4f2c-be5b-1501ba21b4ef', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_holders).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, music_industry_incumbents).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, film_studios).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, publishing_conglomerates).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, cultural_commons_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Disney, RIAA, MPAA, major publishing conglomerates: collect ongoing licensing fees and enforcement proceeds from extended copyright terms. Control entry to cultural distribution channels through licensing gates. Actively lobby for further term extension and technological protection measures. Their business model depends on treating copyright as a perpetual property right that maximizes extraction from derivative use, remake, and education.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_holders, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_holders, agenda_setter).

% Musicians sampling existing works, filmmakers incorporating copyrighted material, authors drawing on literary traditions: must negotiate licenses, pay fees, or self-censor. The extended copyright term (life + 70 years) extends the licensing gate across entire creative careers. Fair use protections have eroded through the reading's enforcement, leaving creators paying rather than claiming exemptions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Universities, libraries, teachers, digital archivists: bear licensing costs and legal liability for educational and preservation uses. The criminalization of circumvention (DMCA) makes even lawful preservation of digitally-protected works technically infringement. Their exit option is restricted access to cultural materials — they cannot easily exit the constraint without abandoning their institutional function.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_archivists, payer,
    organized, generational, constrained, global).

% Musicians, writers, visual artists without institutional backing: pay licensing fees or enforce copyright against larger entities who sample them, both economically infeasible. Trapped between the need to sample existing culture (forbidden at scale) and the impossibility of preventing corporate appropriation of their own work (enforcement is prohibitively expensive). The reading consolidates cultural production in hands of those who can afford licensing and enforcement.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, independent_artists, payer,
    powerless, biographical, trapped, global).

% Scholars, internet archive operators, open-culture advocates: argue that the reading misinterprets 'limited times' and that cultural works should enter the public domain on a reasonable schedule. Their position is structurally excluded from the negotiation — the corporate enclosure reading dominates legislative and enforcement machinery, and alternative readings are locked out of the dominant institutional channels.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    powerful, generational, constrained, global).

% U.S. Congress and the enforcement apparatus (DOJ, Copyright Office): author and administer the reading through repeated term extensions (Sonny Bono Act, etc.), DMCA enforcement, and interpretation of fair use doctrine. They have the formal power to revise the reading but face concentrated lobbying pressure from corporate beneficiaries and diffuse opposition from excluded parties.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress_and_enforcement_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Non-U.S. jurisdictions and treaty bodies (WIPO, WTO): observe and negotiate copyright harmonization. The U.S. corporate enclosure reading is exported as a global norm through trade agreements and pressure, affecting cultural policy globally.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, international_trading_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Copyright exists to incentivize creation by granting temporary monopoly on reproduction. Under this reading, perpetuating that monopoly as long as possible (without explicit perpetuity) maximizes the incentive signal and protects creators' long-term interests in their works.
% TRANSFER_FUNCTION: Moves licensing revenue, enforcement authority, and cultural gatekeeper status from derivative creators, educators, and independent artists to corporate copyright holders. The transfer is mediated through license fees, DMCA-backed technological locks, and the cost of fair-use litigation.
% ABSENT_VOICES: Public domain advocates, cultural commons users, open-access scholars, and international jurisdictions preferring shorter terms are excluded from legislative negotiation. They would argue that the reading mistakes property maximalism for constitutional fidelity and that cultural access is a competing value. Their exclusion is maintained by the concentrated lobbying resources of corporate beneficiaries.
% DISAPPEARANCE_RATIONALE: If this reading—the interpretation of 'limited times' as maximal extension, the enforcement machinery, and the corporate enclosure doctrine—disappeared overnight, derivative creators would create freely, educators would teach with copyrighted materials without licensing friction, and cultural works would enter the public domain on a reasonable timeline. The music and film industries would reorganize around shorter-term monopolies, licensing would reset to reflect actual marginal cost rather than scarcity rent, and derivative creativity would accelerate.
% FOUNDING_PROBLEM: Creators need economic incentive to produce original work; without exclusive rights, works would be copied and undersold, eliminating the incentive.
% FOUNDING_PROBLEM_CORROBORATION: Corporate copyright holders attest the founding problem is live and intensifying, citing the digital environment's ease of reproduction. Economists and public-domain advocates attest the problem was substantially solved by moderate-term protection (14–28 years) and is now superseded by the problem of cultural enclosure. Empirical evidence from jurisdictions with shorter terms shows no significant reduction in creative output; the problem-framing is contested by sources outside the corporate beneficiary set.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and rising over the 40-year interval because the corporate enclosure reading systematically broadens the scope of protected works (expanding copyright to cover derivative compilations, extending terms repeatedly, and restricting fair use). The reading concentrates the benefit of copyright (licensing monopoly) in the hands of corporate holders while dispersing and amplifying costs to creators and educators. Suppression is high (0.72) and rising because the reading's persistence depends on active enforcement: DMCA circumvention bans (which criminalize even lawful preservation), aggressive litigation against derivative creators, and the technical measures that replace transparent fair use with opaque licensing walls. Theater ratio is moderate and rising (0.22 to 0.48 over the interval) because the reading initially frames copyright as incentivizing creation (genuine coordination function) but increasingly functions as rent extraction without corresponding incentive effect—the ratio rises as extractiveness outpaces actual creation incentives. Accessibility collapse is moderate (0.68) because alternatives exist (open licensing, shorter-term regimes) but are systematically disfavored in legislative and judicial channels. Resistance is substantial (0.62) and sustained because public-domain advocates, educators, and derivative creators continuously contest the reading, though their power is diffuse.
 *
 * PERSPECTIVAL GAP:
 *   The corporate beneficiary and the educator-victim seats compute to different types from the same structural data. From the corporate agenda-setter seat, the reading is tangled_rope (real coordination—incentive to create—plus asymmetric extraction). From the educator and derivative-creator seats, it is closer to snare (the coordination function is atrophied; the constraint persists as pure extraction backed by enforcement machinery). The engine computes per-seat classifications from directionality (beneficiary vs. victim) and the structural data (high enforcement, low resistance to alternatives in beneficiary interest). The perspectival gap is the point—the reading itself is designed to collapse alternatives and concentrate authority in the beneficiary seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate copyright holders have d near 0.05–0.15 (full beneficiaries): they set the reading, collect licensing revenue, face minimal enforcement cost. Derivative creators and educators have d near 0.85–0.95 (full targets): they pay licensing fees, face legal liability, have constrained exit (they cannot exit their professions without abandoning their function). Independent artists have d near 0.95 (fully trapped target: they pay licensing fees and cannot enforce against larger entities). Public-domain advocates have d near 0.50–0.60 (symmetric): they benefit from cultural access but are excluded from gatekeeping authority and bear the cost of advocacy. Directionality is driven by beneficiary/victim declarations and exit options: beneficiaries have arbitrage mobility (they set terms); victims have constrained or trapped exit (they must operate within the reading's boundaries). Congress and enforcement authorities have d near 0.40–0.50 (moderately beneficiary-proximal) because they administer the reading and receive political support from beneficiaries, but face diffuse electoral pressure from excluded parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope (not snare) because a real coordination function—incentivizing creative production through temporary monopoly—is genuinely solved by the reading. However, the extractiveness metrics are high and rising while the actual incentive effect plateaus and declines (theater_ratio rises to 0.48, indicating growing disconnect between functional incentive and extractive rent). A snare reading is available: the constraint persists primarily through enforcement machinery and beneficiary lobbying, not through participant preference for the coordination it purports to enable. The mandatrophy case is: the reading was founded to solve a live problem (creation incentive); the problem is now substantially solved by any reasonable copyright term (14–28 years); the reading persists as tangled_rope because it continues the coordination function (authors are incentivized), but the degree of extraction has become decoupled from incentive necessity. The engine's per-seat computation will likely show the educator and independent-artist seats computing as snare (no coordination benefit, pure extraction); the corporate agenda-setter seat computing as rope-plus-benefit (genuine incentive function, plus concentrated rent). This intra-constraint type divergence is the signal that mandatrophy is present—the constraint's classification depends on which seat is speaking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_thresholdand_term_length,
    'What copyright term length is empirically necessary to incentivize creative production? Is the current term (life + 70 years) a necessary incentive, or would a much shorter term (14–28 years) produce equivalent creative output?',
    'Comparative analysis across jurisdictions with different copyright terms, econometric studies of creative output vs. term length, and controlled experiments where possible. Empirical evidence from countries with 25-year or 50-year terms vs. U.S. 70-year terms.',
    'If shorter terms produce equivalent output, the extended term is pure extraction unrelated to incentive necessity, and the constraint should reclassify toward snare across all seats. If the extended term does add measurable incentive, the coordination function is genuine but inefficiently large—suggesting a shorter mandatrophy-resolving term.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_thresholdand_term_length, empirical, 'Empirical necessity of extended copyright terms for creation incentives.').

omega_variable(
    limited_times_constitutional_meaning,
    'Does ''limited times'' in the Copyright Clause permit term extensions that approach perpetuity without explicit perpetual language, or does it require periodic renewal and genuine limitations proportional to incentive necessity?',
    'Constitutional interpretation by courts (unlikely under current doctrine, but possible via amendment or judicial reconsidering); originalist analysis of founding-era meaning; structural constitutional analysis of whether indefinite extension contradicts the word ''limited''.',
    'If ''limited times'' is interpreted as requiring genuine limitation, the corporate enclosure reading is constitutionally invalid, and the constraint would reclassify sharply downward in extractiveness. If ''limited times'' is interpreted as permitting indefinite renewal, the reading is constitutionally sound (from its own lights) and the classification remains high-extraction tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_times_constitutional_meaning, conceptual, 'Constitutionality of indefinite copyright extension under ''limited times'' language.').

omega_variable(
    fair_use_erosion_mechanism,
    'Is the decline of fair use (and its replacement with licensing walls) a structural consequence of the corporate enclosure reading, or a contingent side effect of technological change?',
    'Legal history tracing fair use doctrine decline (Harper & Row v. Nation; Sony v. Universal; DMCA; contract-based licensing walls); analysis of whether the reading explicitly forecloses fair use or whether fair use erosion is incidental to technological protection.',
    'If fair use erosion is intrinsic to the reading (intentionally replaced with licensing rents), the reading is unambiguously snare-flavored in its victim seats. If erosion is incidental, the reading remains tangled_rope in structure but with suppression mechanisms that exceed the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_erosion_mechanism, empirical, 'Whether fair use collapse is intrinsic to the corporate enclosure reading.').

omega_variable(
    reading_foreclosure_status,
    'Do the corporate enclosure reading and the public_scaffold reading logically foreclose each other, or do they coexist as genuinely different readings of the same text?',
    'Examine whether both readings can be held simultaneously by one party (can Congress legislate as if ''limited times'' requires public enrichment while extending terms indefinitely?). Test whether the axioms contradict or merely differ.',
    'If they foreclose (true contradiction), the relation is forecloses and one reading will eventually mathematically eliminate the other. If they coexist, the relation is coexists_with and both can persist as different parties'' frameworks. This determines long-term classification dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Logical foreclosure relationship between corporate_enclosure and public_scaffold readings.').

omega_variable(
    institutional_capture_of_congress,
    'Has Congress''s repeated adoption of the corporate enclosure reading (Sonny Bono Act, etc.) occurred because the reading best serves incentive purposes, or because corporate beneficiaries have concentrated lobbying power that dominates diffuse public interest?',
    'Campaign finance analysis, expert testimony from political economists, comparative analysis with jurisdictions where public-interest readings prevail, temporal analysis of Congressional voting correlated with lobbying expenditure.',
    'If capture is substantial, Congress''s endorsement of the reading is not reliable evidence of its merits; reclassify as the reading persisting via extraction machinery rather than genuine legislative consensus. This supports mandatrophy diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_congress, empirical, 'Degree of corporate regulatory capture in copyright legislative cycles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(copy_tr_t5, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(copy_tr_t15, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(copy_tr_t25, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(copy_tr_t40, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(copy_be_t5, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(copy_be_t15, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(copy_be_t25, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(copy_be_t40, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(copy_su_t5, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(copy_su_t15, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(copy_su_t25, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(copy_su_t40, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.18).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_circumvention_prohibition).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_erosion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the copyright_constitutional_mandate kernel. The public_scaffold_reading and judicial_ambiguity_reading are sibling readings of the same kernel with different epsilon values, beneficiary/victim structures, and classification outcomes. All three readings are linked by network.affects_constraints. The kernel itself is stable text (Copyright Clause; 17 U.S.C.); the readings are different instantiations of how that text is interpreted and enforced. Each reading has its own constraint_id, its own baselines, its own stakeholder structure, and its own classification trajectory. The corporate_enclosure_reading is the most extractive (ε ≈ 0.81); the public_scaffold_reading is least extractive (ε ≈ 0.35); the judicial_ambiguity_reading is intermediate and deferential (ε ≈ 0.50). Whichever reading dominates the legislative and judicial machinery at a given time determines the constraint's actual operation, but all three remain structurally valid instantiations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, powerless, 0.95).
constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
