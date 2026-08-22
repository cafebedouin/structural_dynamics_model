% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint story models the creator-centric reading of the fair use
 *   four-factor test (17 U.S.C. § 107) as a structurally extractive
 *   constraint on cultural production. The reading treats fair use as a
 *   narrow, defensive exception to the property right of copyright owners,
 *   with the fourth factor (market effect) functioning as a near-decisive
 *   veto on unlicensed uses that could serve as market substitutes. Over the
 *   interval 1976–2024, the constraint has accumulated extraction: licensing
 *   intermediaries have proliferated, statutory damages and DMCA takedowns
 *   have raised suppression, and the 'chilling effect' on transformative
 *   works has become a documented feature of cultural production. The claimed
 *   type is tangled_rope because the four-factor test genuinely coordinates
 *   dispute resolution (a real coordination function) while simultaneously
 *   extracting from transformative users and the public domain (asymmetric
 *   extraction). The engine will compute per-seat classifications from the
 *   structural data authored here.
 *
 * KEY AGENTS:
 *   - rights_holders: Primary beneficiary (organized/arbitrage) — collects licensing rents and controls derivative markets
 *   - creative_industries: Primary beneficiary (institutional/arbitrage) — shapes enforcement infrastructure
 *   - licensing_intermediaries: Secondary beneficiary (organized/mobile) — extracts fees from the permission gap
 *   - transformative_users: Primary payer (moderate/constrained) — bears licensing costs, legal risk, self-censorship
 *   - public_domain_beneficiaries: Primary payer (powerless/trapped) — loses cultural commons, no exit
 *   - educational_institutions: Secondary payer (organized/constrained) — pays clearance costs, mission-locked
 *   - documentarians_and_critics: Secondary payer (moderate/constrained) — faces insurance and litigation risk
 *   - courts_and_judiciary: Agenda setter (institutional/analytical) — administers and shapes the test
 *   - copyright_office_and_legislature: Agenda setter/observer (institutional/analytical) — statutory authority
 *   - legal_scholars_and_observers: Observer (analytical/analytical) — interpretive discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '5583fd79-b536-4d96-b2f8-a502b41be614').
narrative_ontology:cs_kernel_codification('5583fd79-b536-4d96-b2f8-a502b41be614', formalized).
narrative_ontology:cs_authority_grounding('5583fd79-b536-4d96-b2f8-a502b41be614', lineage).
narrative_ontology:cs_interpretation_layer_present('5583fd79-b536-4d96-b2f8-a502b41be614').
narrative_ontology:cs_reading_relation('5583fd79-b536-4d96-b2f8-a502b41be614', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('5583fd79-b536-4d96-b2f8-a502b41be614', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('5583fd79-b536-4d96-b2f8-a502b41be614', foundational, market_harm_factor_decisive).
narrative_ontology:cs_axiom_status(market_harm_factor_decisive, holdable).
narrative_ontology:cs_axiom_grounding('5583fd79-b536-4d96-b2f8-a502b41be614', market_harm_factor_decisive, conventional).
narrative_ontology:cs_axiom('5583fd79-b536-4d96-b2f8-a502b41be614', foundational, fair_use_as_equitable_defense_not_right).
narrative_ontology:cs_axiom_status(fair_use_as_equitable_defense_not_right, holdable).
narrative_ontology:cs_axiom_grounding('5583fd79-b536-4d96-b2f8-a502b41be614', fair_use_as_equitable_defense_not_right, conventional).
narrative_ontology:cs_axiom('5583fd79-b536-4d96-b2f8-a502b41be614', secondary, creator_incentive_requires_exclusive_derivative_control).
narrative_ontology:cs_axiom_status(creator_incentive_requires_exclusive_derivative_control, holdable).
narrative_ontology:cs_axiom_grounding('5583fd79-b536-4d96-b2f8-a502b41be614', creator_incentive_requires_exclusive_derivative_control, instrumental).
narrative_ontology:cs_reference_frame('5583fd79-b536-4d96-b2f8-a502b41be614', statutory_four_factor_balance_1976).
narrative_ontology:cs_drift_state('5583fd79-b536-4d96-b2f8-a502b41be614', post_campbell_google_oracle_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5583fd79-b536-4d96-b2f8-a502b41be614', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, creative_industries).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_beneficiaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, creator_incentive_justification).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, property_right_primacy_in_copyright).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, narrow_exception_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major copyright owners (studios, labels, publishers) who hold large catalogs. They benefit from the narrow fair use reading because it preserves licensing revenue streams and deters unlicensed uses that would otherwise compete with authorized derivatives. They can arbitrage across jurisdictions and contract terms; exit from the constraint is not a concern — they shape it.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holders, beneficiary,
    organized, generational, arbitrage, global).

% Trade associations (MPAA, RIAA, AAP) and collecting societies that administer the enforcement infrastructure. They benefit from the chilling effect the narrow reading produces — it funnels uses toward licensed channels. Their institutional power derives from the constraint's enforcement machinery, which they help design and lobby for.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, creative_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Stock agencies, clearance houses, sync licensing platforms, and collective management organizations. They extract fees from the gap between what users need and what fair use permits. If fair use widened, their business models would shrink; they have mobile exit (can pivot to adjacent services) but benefit strongly from the status quo.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Artists, remixers, meme creators, parody authors, and appropriation artists whose work adds new expression but faces legal uncertainty under the four-factor test. They bear the cost of licensing fees, legal risk, or self-censorship. Exit is constrained: they cannot easily leave the cultural sphere they operate in, and the constraint follows them across platforms.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, global).

% The diffuse public who lose access to works that would enter the public domain or be freely reusable under a broader fair use doctrine. The narrow reading extends effective copyright control beyond the statutory term by chilling uses that are technically non-infringing but legally risky. They are trapped — no individual can exit the copyright system, and collective action to reform it faces concentrated opposition.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_beneficiaries, payer,
    powerless, generational, trapped, universal).

% Universities, schools, libraries, and archives that need to copy, digitize, and share works for teaching and research. They pay through licensing fees, staff time for clearance, and forgone uses. Their exit is constrained by mission — they cannot stop educating — but they have some organizational capacity to push back (e.g., library copyright policies, fair use guidelines).
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Filmmakers, journalists, scholars, and critics who must incorporate copyrighted material to comment on it. They face clearance costs, insurance requirements, and the risk of litigation that deters publication. Exit is constrained: their work requires engaging with the cultural record as it exists, which is largely copyrighted.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics, payer,
    moderate, biographical, constrained, global).

% Federal courts that apply the four-factor test case by case. They administer the constraint's enforcement and shape its boundaries through precedent. Their role is not passive — judicial choices about factor weighting (especially the fourth factor, market effect) determine whether the narrow or broad reading prevails in practice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The U.S. Copyright Office and Congress, which maintain the statutory framework and can amend it. They set the agenda through rulemakings, reports, and legislation (e.g., DMCA, Music Modernization Act). They observe the constraint's operation but also have the power to restructure it.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_office_and_legislature, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, copyright_office_and_legislature, observer).

% Academics, public interest advocates, and policy analysts who study fair use doctrine. They do not directly pay or collect from the constraint but shape the interpretive discourse that courts and legislators draw on. Their exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legal_scholars_and_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, judge-administered framework for resolving disputes between copyright holders and users about unauthorized use, replacing ad hoc negotiation or total prohibition with a multi-factor balancing test.
% TRANSFER_FUNCTION: Moves expressive freedom and cultural participation from transformative users, educators, documentarians, and the public to rights holders and licensing intermediaries, in the form of foregone uses, licensing fees, legal risk, and chilled speech.
% ABSENT_VOICES: Future creators whose raw material is the existing culture (sampling artists, remix cultures, AI training data curators) — they are not yet organized and have no seat at the table. Also: users in the Global South whose access to knowledge is mediated by Western copyright holders and who cannot participate in U.S. doctrinal debates.
% DISAPPEARANCE_RATIONALE: If the creator-centric four-factor test vanished overnight, courts would default to broader fair use readings (transformative use or user-centric), licensing markets would shrink dramatically, transformative works would proliferate without clearance, and the economics of cultural production would shift toward open reuse models. Rights holders would lose a primary tool for controlling derivative markets.
% FOUNDING_PROBLEM: The 1976 Copyright Act codified fair use to resolve the tension between the constitutional mandate to promote progress and the property-like rights granted to authors. The creator-centric reading emerged from the legislative history emphasizing that fair use should not impair the market for the original work — the 'market harm' factor was intended as the decisive constraint on exceptions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (balancing incentives and access) is attested by the legislative history of the 1976 Act itself, which is outside any single beneficiary group. However, the status is contested: rights holder groups attest the problem remains live (piracy, AI training); transformative users and public interest scholars attest the problem has shifted — the constraint now protects incumbent revenue models more than creator incentives. The Supreme Court's Campbell v. Acuff-Rose (1994) and Google v. Oracle (2021) opinions, authored by justices outside the beneficiary set, corroborate that transformativeness has risen in doctrinal weight, challenging the narrow reading's dominance.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72 at 2024) reflects the large and growing gap between what the narrow reading permits and what transformative cultural production requires — the 'permission culture' documented by Lessig, Boyle, and others. Suppression (0.68) captures the active enforcement machinery: statutory damages up to $150k/work, DMCA takedown regimes, Content ID systems, and the litigation risk that deters even likely-fair uses. Theater ratio (0.35) acknowledges the test's genuine coordination function — it does resolve real disputes — but the rising trend shows increasing performative invocation of 'market harm' to block uses that do not actually substitute for the original. Accessibility collapse (0.55) and resistance (0.58) are moderate: alternatives (Creative Commons, open licensing, public domain) exist but are structurally marginal; resistance is organized but has not reversed the extraction trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The rights holder / creative industry seats experience the constraint as coordination (a predictable rule that protects their investment). The transformative user / public domain seats experience it as extraction (a veto on cultural participation). The engine will compute this divergence from the power/exit/beneficiary/victim structure. The courts sit in a genuine tension: their precedent-setting role means they can shift the constraint's effective type over time (as seen in the transformative use turn post-Campbell).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rights_holders, creative_industries, licensing_intermediaries) are declared because they collect rents from the constraint's operation — licensing revenue, control over derivatives, clearance fees. Victims (transformative_users, public_domain_beneficiaries, educational_institutions, documentarians_and_critics) are declared because they bear the costs: foregone expression, licensing fees, legal risk, chilled speech. The directionality derivation will assign low d to beneficiaries (χ damped) and high d to victims (χ amplified). Courts and the Copyright Office are agenda_setters — they administer the constraint but do not directly collect its extraction. Observers are analytical seats. Public_domain_beneficiaries are powerless/trapped — the highest effective extraction. Transformative_users are moderate/constrained — high extraction but some capacity to resist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing creator incentives with public access) remains live but has been redefined by the constraint's operation. The creator-centric reading was built to prevent market substitution; it now primarily prevents transformative addition. The constraint has not atrophied — it has intensified (rising extractiveness, suppression). But its mandate has drifted: the 'incentive' justification now serves incumbent revenue models more than living creators (many of whom are transformative users themselves). This is not mandatrophy in the sense of a dead function persisting; it is mandate capture — a live function redirected to serve the beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_framing_ambiguity,
    'Does the creator-centric reading foreclose the transformative-use reading within a single doctrinal framework, or do they coexist as competing interpretive weights that courts toggle between?',
    'Track Supreme Court and circuit court opinions for explicit rejection or absorption of the transformative use analysis. If a majority opinion holds that transformativeness cannot outweigh market harm, foreclosure is established; if transformativeness is treated as a sub-factor within factor one that can outweigh market harm, coexistence is confirmed.',
    'If forecloses: the kernel has a structural fault line — only one reading can be law at a time, making the constraint family a zero-sum contest. If coexists_with: the constraint family operates as a persistent doctrinal oscillation, with extraction levels varying by judicial composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Whether the creator-centric and transformative-use readings are logically incompatible or doctrinally coexistent.').

omega_variable(
    creator_vs_intermediary_benefit_divergence,
    'How much of the extracted value accrues to individual creators versus corporate rights holders and licensing intermediaries?',
    'Empirical studies of royalty distribution in music, film, and publishing; survey data on creator income sources; analysis of copyright termination rights exercise rates.',
    'If most extraction flows to intermediaries and corporate catalog owners, the ''creator incentive'' justification is structurally decoupled from the constraint''s actual beneficiary structure — strengthening the tangled_rope classification. If creators directly capture the majority, the coordination function is more genuinely aligned with the declared mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_vs_intermediary_benefit_divergence, empirical, 'Whether the constraint''s beneficiary structure matches its declared justification.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the creator-centric reading''s structural relationship to the fair_use_four_factor_test kernel differ from its siblings, and where is the disagreement located?',
    'Compare the three readings'' structural mappings: which factor each treats as decisive, which agents each names as beneficiaries/victims, and what each reading''s disappearance_verdict would be. The disagreement is located in the weighting of factor four (market effect) versus factor one (transformative purpose), and in the classification of transformative users as victims (this reading) versus rights-bearing participants (siblings).',
    'Confirms this is a distinct constraint with its own ε, not a parameter variation. Validates the kernel decomposition approach. If the sibling readings produce structurally identical classifications, the kernel is not genuinely contested at the constraint level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: this reading''s structural delta relative to sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_creator_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(fair_use_creator_tr_t1994, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement(fair_use_creator_tr_t1998, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(fair_use_creator_tr_t2005, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(fair_use_creator_tr_t2015, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(fair_use_creator_tr_t2024, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(fair_use_creator_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(fair_use_creator_be_t1994, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement(fair_use_creator_be_t1998, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(fair_use_creator_be_t2005, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(fair_use_creator_be_t2015, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(fair_use_creator_be_t2024, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_creator_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_use_creator_su_t1994, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1994, 0.5).
narrative_ontology:measurement(fair_use_creator_su_t1998, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(fair_use_creator_su_t2005, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(fair_use_creator_su_t2015, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(fair_use_creator_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, dmca_takedown_regime).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, statutory_damages_framework).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, orphan_works_problem).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, ai_training_fair_use).

% DUAL FORMULATION NOTE:
% This constraint (creator_centric_reading) is one member of the fair_use_four_factor_test constraint family. The siblings are user_centric_reading and transformative_use_reading. All three share the same statutory text (17 USC 107) but instantiate different constraints with different ε, beneficiary/victim structures, and effective types. The creator-centric reading has the highest extractiveness because it centers market-harm analysis; the transformative-use reading has lower extractiveness because transformativeness can outweigh market harm; the user-centric reading would have the lowest extractiveness of the three. They are linked by the kernel's statutory text and by the doctrinal oscillation between them in case law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, organized, 0.15).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, institutional, 0.1).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, powerless, 0.95).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
