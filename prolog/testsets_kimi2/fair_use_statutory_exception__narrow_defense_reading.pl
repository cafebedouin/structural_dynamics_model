% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-First Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint is the narrow_defense_reading of the
 *   fair_use_statutory_exception kernel. It treats copyright as property and
 *   fair use as a narrow affirmative defense construed to preserve market
 *   value for rights-holders. Sibling readings include
 *   transformative_right_reading (which weights transformativeness heavily to
 *   enable reuse) and market_licensing_reading (which denies fair use
 *   whenever a license is conceivable). The structural delta for this reading
 *   is high extraction, commercial nature as determinative,
 *   transformativeness underweighted, and burden on the defendant. The
 *   authored metrics are descriptively true of this reading's operation; the
 *   claimed type is tangled_rope because the constraint carries a genuine
 *   coordination function (property-based incentivization of creative
 *   production) while simultaneously extracting asymmetrically from users and
 *   follow-on creators through active judicial enforcement.
 *
 * KEY AGENTS:
 *   - Appellate courts: Primary agenda-setter (institutional/analytical) â construe the doctrine and bind lower courts.
 *   - Copyright holders: Primary beneficiary (powerful/mobile) â collect licensing revenue and control reuse.
 *   - Content industry: Secondary beneficiary (organized/mobile) â aggregates portfolios and lobbies for property-maximalist interpretation.
 *   - Unauthorized users: Primary target (moderate/constrained) â bear statutory damage exposure and defense burden.
 *   - Transformative creators: Primary target (moderate/constrained) â forced to license or abandon works.
 *   - Libraries and archives: Secondary target (institutional/constrained) â clearance costs limit preservation and access.
 *   - IP law scholars: Analytical observer (analytical/analytical) â document doctrinal drift and asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.82).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.75).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-First Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '0cc90e98-79d0-4949-8391-cd23968f7a25').
narrative_ontology:cs_kernel_codification('0cc90e98-79d0-4949-8391-cd23968f7a25', fixed_text).
narrative_ontology:cs_authority_grounding('0cc90e98-79d0-4949-8391-cd23968f7a25', lineage).
narrative_ontology:cs_interpretation_layer_present('0cc90e98-79d0-4949-8391-cd23968f7a25').
narrative_ontology:cs_reading_relation('0cc90e98-79d0-4949-8391-cd23968f7a25', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cc90e98-79d0-4949-8391-cd23968f7a25', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('0cc90e98-79d0-4949-8391-cd23968f7a25', foundational, copyright_as_property_maxim).
narrative_ontology:cs_axiom_status(copyright_as_property_maxim, holdable).
narrative_ontology:cs_axiom_grounding('0cc90e98-79d0-4949-8391-cd23968f7a25', copyright_as_property_maxim, conventional).
narrative_ontology:cs_axiom('0cc90e98-79d0-4949-8391-cd23968f7a25', foundational, fair_use_affirmative_defense_burden).
narrative_ontology:cs_axiom_status(fair_use_affirmative_defense_burden, holdable).
narrative_ontology:cs_axiom_grounding('0cc90e98-79d0-4949-8391-cd23968f7a25', fair_use_affirmative_defense_burden, conventional).
narrative_ontology:cs_reference_frame('0cc90e98-79d0-4949-8391-cd23968f7a25', property_maximalist_statutory_balance).
narrative_ontology:cs_drift_state('0cc90e98-79d0-4949-8391-cd23968f7a25', contemporary_circuit_split_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0cc90e98-79d0-4949-8391-cd23968f7a25', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_industry).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, unauthorized_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, libraries_and_archives).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Section 107 through a property-first lens, placing the burden of proving fair use on defendants and treating commercial nature as heavily disfavoring the defense; their precedents bind lower courts and narrow the statutory safety valve.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Control exclusive reproduction and distribution rights; benefit from judicial presumptions that unauthorized uses harm licensing markets, which expands the territory subject to fee-bearing permission.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, beneficiary,
    powerful, generational, mobile, global).

% Trade associations and major publishers that aggregate intellectual property portfolios; lobby for statutory and doctrinal interpretations that channel uses toward licensing revenue and restrict uncompensated reuse.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_industry, beneficiary,
    organized, generational, mobile, global).

% Individuals and small entities who reproduce or share copyrighted material without clearance; face statutory damages and injunctive relief if the use falls outside the narrowly construed defense.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, unauthorized_users, payer,
    moderate, biographical, constrained, national).

% Artists, remixers, and documentarians who build new expression atop existing works; their transformative purpose is systematically underweighted against market harm presumptions, forcing costly licensing or creative abandonment.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_creators, payer,
    moderate, biographical, constrained, national).

% Heritage and educational institutions that preserve and lend materials; narrow fair use limits mass digitization, interlibrary sharing, and accessibility adaptation, requiring permission for socially beneficial uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, libraries_and_archives, payer,
    institutional, generational, constrained, national).

% Analyze doctrinal evolution and empirical effects of copyright interpretation; document the asymmetry between rights-holder control and follow-on innovation, noting divergence from the constitutional progress clause.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, ip_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable exclusivity framework for expressive works, reducing transaction costs in licensing and purportedly incentivizing production by securing control against uncompensated use.
% TRANSFER_FUNCTION: Transfers freedom to use existing works without payment from unauthorized users, transformative creators, and heritage institutions to copyright holders and content industries, via judicial narrowing of the statutory safety valve.
% ABSENT_VOICES: Individual users and small creators lack resources to litigate fair use claims to final judgment; documentary filmmakers and remix communities are structurally excluded from the rulemaking that determines their legal risk; public-domain advocates are heard in amicus briefs but underrepresented in binding precedent.
% DISAPPEARANCE_RATIONALE: If the narrow defense reading vanished overnight, unauthorized uses currently enjoined or subject to damages would shift toward open transformative reuse; licensing leverage would compress for content industries; courts would need alternative doctrinal frameworks to adjudicate infringement; the information economy would restructure around broader fair use or statutory licensing.
% FOUNDING_PROBLEM: The Constitutional mandate to promote the progress of science and useful arts by securing exclusive rights for limited times, balanced against the public interest in access to knowledge and culture.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and the content industry assert the founding problem is live and requires strong property protection. Legal historians and library associations attest the original balance has shifted; the property-framed reading is contested by constitutional scholars and economists outside the benefiting parties, who argue the narrow defense undermines the progress clause's public purpose.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the narrow reading collapses most unauthorized uses into infringement and channels them toward licensing. Suppression (0.75) reflects statutory damages, injunctive relief, and the high cost of litigating an affirmative defense. Theater ratio is moderate (0.32): courts perform a four-factor balancing test, but the balancing is structurally tilted by market-harm presumptions so that a significant share of opinion-writing defends revenue capture rather than genuine public-interest calibration. Accessibility collapse (0.72) is high because alternatives to permission (transformative reuse, archival preservation) are foreclosed once the narrow reading is understood. Resistance (0.55) reflects ongoing litigation and amicus participation by libraries, creators, and technology firms. The temporal series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint is a necessary property framework that secures investment in creative production. From the payer seats, the same structure operates as an enforced extraction mechanism that taxes follow-on innovation, speech, and preservation. The engine computes this divergence from the structural data; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and the content industry are structural beneficiaries: they collect licensing rents and control reuse, situating them at the low-d end of the directionality spectrum. Unauthorized users, transformative creators, and libraries are structural targets: they bear the costs of the narrowed defense, situating them at the high-d end. Appellate courts administer the constraint with analytical exit options, producing a near-neutral directional position despite their agenda-setting role. The divergence between beneficiary and payer seats is driven by the property-framed statutory interpretation, which converts user freedom into licensable territory.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow defense reading prevents mislabeling by retaining a genuine coordination function: the property framework does reduce transaction costs in licensing and does incentivize some production. However, the coordination is hybridized with extraction because the defense is narrowed beyond what is necessary to preserve that incentive, converting statutory flexibility into rent-bearing territory. The founding problem (promoting progress) is contested, and the constraint's persistence depends on active judicial enforcement rather than spontaneous compliance, which blocks pure mountain or pure rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the fair use kernelânarrow defense, transformative right, or market licensingâcorrectly captures the structural function of Section 107?',
    'Comparative doctrinal analysis across jurisdictions and empirical measurement of creative output and follow-on innovation under varying fair use regimes.',
    'Resolution would determine whether the current high-extraction regime is a faithful interpretation of the statutory balance or a drift toward property maximalism that undermines the progress clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between competing readings of the same statutory kernel.').

omega_variable(
    market_harm_presumption_accuracy,
    'Does the presumption of market harm for unauthorized commercial uses accurately predict actual market substitution, or does it systematically overstate harm to copyright holders?',
    'Empirical economic studies measuring displacement of licensed uses by transformative or unauthorized uses; natural experiments from jurisdictions with broader fair use or open access regimes.',
    'If the presumption overstates harm, the extraction is largely rent-seeking justified by a false empirical premise; if accurate, the constraint''s extraction is closer to the necessary coordination cost of the property framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_presumption_accuracy, empirical, 'Empirical accuracy of the market harm presumption underlying narrow fair use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_narrow_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fair_use_narrow_tr_t8, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fair_use_narrow_tr_t16, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(fair_use_narrow_tr_t24, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(fair_use_narrow_tr_t32, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(fair_use_narrow_tr_t40, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(fair_use_narrow_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fair_use_narrow_be_t8, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(fair_use_narrow_be_t16, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(fair_use_narrow_be_t24, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(fair_use_narrow_be_t32, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(fair_use_narrow_be_t40, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_narrow_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fair_use_narrow_su_t8, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fair_use_narrow_su_t16, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(fair_use_narrow_su_t24, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(fair_use_narrow_su_t32, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(fair_use_narrow_su_t40, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 40, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
