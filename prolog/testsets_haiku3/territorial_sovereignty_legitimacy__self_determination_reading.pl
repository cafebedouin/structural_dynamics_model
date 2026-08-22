% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Self-Determination Reading)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel of
 *   territorial sovereignty legitimacy in the Israeli-Palestinian context.
 *   The self-determination reading grounds legitimacy in the modern principle
 *   of self-determination applied to the Arab population with demographic
 *   majority and continuous residence during the 19th-20th centuries. Under
 *   this reading, the 1948 partition is an unjust external imposition by
 *   colonial powers; the right of return restores the status quo ante; and
 *   the Israeli state is framed as a colonial project imposed against the
 *   majority's will. This reading does not claim uniqueness or finality —
 *   sibling readings (covenant continuity grounding legitimacy in ancient
 *   Jewish presence and international recognition; existential-matrix
 *   grounding it in survival preconditions regardless of legal claims) are
 *   incommensurable with it, held by different parties in an ongoing contest.
 *   The story models the self-determination reading AS ONE CONSTRAINT with a
 *   stable ε referent (the legitimacy claim under this reading's lights), not
 *   as the averaged or contested judgment across readings.
 *
 * KEY AGENTS:
 *   - Arab population with continuous modern residence: primary beneficiary under the reading's legitimacy calculus
 *   - Palestinian refugees and diaspora: bear the extraction cost of partition interpreted as violation of self-determination rights
 *   - Jewish population and Israeli state: structurally excluded from the reading's legitimacy frame (pre-modern presence dismissed, external sponsorship delegitimized)
 *   - Western colonial powers: framed as architects of unjust partition; agenda-setters of the illegitimate constraint
 *   - International self-determination doctrine: vindicated as the proper frame for resolving post-colonial territorial disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.78).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy (Self-Determination Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'e88a937c-ed52-49fb-80b4-f17a31683132').
narrative_ontology:cs_kernel_codification('e88a937c-ed52-49fb-80b4-f17a31683132', fixed_text).
narrative_ontology:cs_authority_grounding('e88a937c-ed52-49fb-80b4-f17a31683132', extraction).
narrative_ontology:cs_interpretation_layer_present('e88a937c-ed52-49fb-80b4-f17a31683132').
narrative_ontology:cs_reading_relation('e88a937c-ed52-49fb-80b4-f17a31683132', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e88a937c-ed52-49fb-80b4-f17a31683132', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('e88a937c-ed52-49fb-80b4-f17a31683132', foundational, modern_self_determination_temporal_frame).
narrative_ontology:cs_axiom_status(modern_self_determination_temporal_frame, holdable).
narrative_ontology:cs_axiom_grounding('e88a937c-ed52-49fb-80b4-f17a31683132', modern_self_determination_temporal_frame, conventional).
narrative_ontology:cs_axiom('e88a937c-ed52-49fb-80b4-f17a31683132', foundational, demographic_majority_as_legitimacy_ground).
narrative_ontology:cs_axiom_status(demographic_majority_as_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('e88a937c-ed52-49fb-80b4-f17a31683132', demographic_majority_as_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('e88a937c-ed52-49fb-80b4-f17a31683132', secondary, premodernism_irrelevance_to_sovereignty).
narrative_ontology:cs_axiom_status(premodernism_irrelevance_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e88a937c-ed52-49fb-80b4-f17a31683132', premodernism_irrelevance_to_sovereignty, conventional).
narrative_ontology:cs_reference_frame('e88a937c-ed52-49fb-80b4-f17a31683132', modern_self_determination_applied_to_demographic_majority).
narrative_ontology:cs_drift_state('e88a937c-ed52-49fb-80b4-f17a31683132', post_partition_non_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e88a937c-ed52-49fb-80b4-f17a31683132', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_modern_territorial_claim).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_post_partition).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reading's vindicated constituency: Arabs with continuous demographic presence and majority status in the territory during the 19th-20th centuries. This reading grants them the primary legitimacy claim to sovereignty via self-determination, framing partition as an external imposition that violated their rights. They benefit from the recognition this reading affords their claim; the constraint is the legal-normative framework that anchors their position.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_modern_territorial_claim, beneficiary,
    powerful, generational, constrained, national).

% Bear the direct cost of partition interpreted as a violation of self-determination: displacement, loss of property, exile. This reading frames their displacement as unjust because it negated the right of the demographic majority. They are trapped — exit from the territorial claim means accepting the partition; remaining within the claim means perpetual non-return. This reading offers them narrative justice (restoration-of-status-quo-ante framing) but not material remedy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_post_partition, payer,
    powerless, biographical, trapped, regional).

% Descendants of those displaced; carry the right-of-return claim that this reading enables. They are trapped by identity (the diaspora is constituted by the displacement narrative) and constrained by lack of standing in international forums dominated by other reading holders. The reading's vindication of self-determination on temporal grounds (continuous presence in 19th-20th centuries) excludes those born in diaspora, creating a two-generation exit lock.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora, payer,
    moderate, generational, constrained, global).

% This reading excludes or minimizes the Jewish legitimacy claim by restricting the relevant temporal frame to the modern period (19th-20th centuries) and emphasizing external sponsorship (Balfour Declaration) as colonial imposition rather than legitimate commitment. Jewish historical presence before the modern period and post-1948 settlement are not recognized as legitimacy sources under this reading. They are trapped because accepting this reading means surrendering their territorial claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_modern_territorial_claim, excluded,
    institutional, generational, trapped, national).

% The reading frames Western powers (Britain, France, international community) as the architects of partition and thereby the violators of self-determination. They 'set' the constraint by imposing the partition, but this reading renders them the antagonists, not beneficiaries. Their enforcement is the enforcement of an illegitimate division; their interest is in post-colonial order maintenance and strategic interest, not genuine self-determination.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, western_colonial_powers, agenda_setter,
    institutional, biographical, mobile, global).

% The doctrine itself — peoples' right to self-determination — is vindicated by this reading's application. It is not an agent but a proposition that the constraint operation instantiates and defends. Including it for narrative completeness: the reading is a claim ABOUT the doctrine's application, not a constraint that benefits from it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_self_determination_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__self_determination_reading, international_self_determination_doctrine).

% Holders of sibling readings (covenant_continuity, existential_matrix) who analyze this reading from the outside. They disagree on the temporal frame, the status of pre-modern presence, the legitimacy of the Balfour Declaration, and what self-determination entails in a zero-sum territorial dispute. They are analytical observers in the sense that they examine this reading's structure without being fully constituted by it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, reading_alternative_holders, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a normative principle (self-determination, demographic majority, continuous modern residence) for resolving competing territorial claims, offering a framework that coordinates international law around the criterion of popular sovereignty and majority rule applied to a bounded temporal window.
% TRANSFER_FUNCTION: Moves legitimacy and (potentially) territorial control from the Jewish/Zionist claim to the Arab/Palestinian claim by reweighting the evidentiary basis: pre-modern presence becomes irrelevant, modern demographic majority becomes dispositive, external sponsorship becomes delegitimizing. The beneficiary gains narrative recognition and legal standing; the payers lose territorial security and (in the partition reading) homeland access.
% ABSENT_VOICES: Jewish inhabitants arguing for ancient covenant legitimacy, existential security claims, and continuous minority presence throughout the period are structurally excluded from this reading's legitimacy calculus. They would object that restricting the temporal frame to the modern period erases their historical claim and that demographic majority should not override minority survival rights. This reading's framework makes their objection inaudible: pre-modern presence is dismissed as irrelevant to modern self-determination.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy frame vanished and were replaced by the existential or covenant readings, territorial arrangements, refugee law, international recognition, and the feasibility of right-of-return would all shift. The current stalemate is partly held by the tension between readings; collapse of this reading's framework would alter negotiating positions, international law interpretations, and the narratives that constitute party identity.
% FOUNDING_PROBLEM: Post-WWI mandate system and colonial fragmentation created a territorial dispute where two peoples claimed the same land based on different historical, religious, and modern grounds. Self-determination doctrine — applied to indigenous/colonized populations emerging from empire — was the international legitimacy framework for resolving such claims. The reading applies that framework to the Arab majority as the 'people' entitled to self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The reading is attested by Palestinian political movements, Arab states' positions, human-rights organizations emphasizing demographic self-determination, and scholarly work on post-colonial international law. It is contested by Israeli state positions, Jewish historical scholarship emphasizing ancient presence, Western powers that sponsored partition, and readings grounding legitimacy in existential survival or covenant. No unanimous corroboration exists; the corroboration is explicitly partisan — international law scholars from outside both parties largely acknowledge the reading as a coherent application of self-determination doctrine, even where they dispute its sufficiency or prefer sibling readings.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1880 (pre-modern period, reading not yet activated; 0.15) through partition (1948; 0.72), stabilizing at 0.78 by 1980. The rise reflects the reading's increasing activation as a normative frame and the growing cost borne by those (refugees, diaspora) who are redefined as the unjust losers under it. Suppression requirement runs slightly behind: the reading requires active enforcement (international law advocacy, delegitimization of competing claims, narrative suppression of pre-modern Jewish presence) to maintain its plausibility against alternative readings. Theater ratio accelerates through partition (0.35) and continues rising (0.42 by 1967), reflecting increasing performative work (commemoration of the Nakba, institutionalization of right-of-return discourse, legal activism) as the reading's material outcomes (refugee return, territorial restoration) remain unrealized. The metrics track the reading's operation as a normative constraint — what it extracts from whom, what suppression it requires, what percentage of its maintenance is performative vs. functionally restoring the status quo ante.
 *
 * PERSPECTIVAL GAP:
 *   The reading creates maximum divergence between seats: the beneficiary sees vindication and justice; the payers see erasure of their prior claim and perpetual exile; excluded parties see denial of their legitimate historical presence; observers see an incommensurable alternative reading held by adversaries. No seat perceives the same constraint the same way.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Arab majority with modern demographic presence) gain from the reading's application of self-determination: d ≈ 0.25 (beneficiaries sit at the low end). Payers (refugees, diaspora) lose territorial security and access: d ≈ 0.88 (high targets). Excluded parties (Jewish historical claimants) are trapped: d ≈ 0.92 (highest targets, but structurally excluded from negotiation). Western powers are complex: they enforce but are then blamed: directionality_override recommended to d ≈ 0.55 (symmetric/dual-edged, neither pure extraction nor pure coordination) because their structural role is both architect and antagonist under the reading's narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWI mandate fragmentation requiring a legitimacy principle for territorial resolution) was structurally live in 1920–1948. It remains contested whether the problem persists: the reading asserts it remains live (partition is ongoing violation), while sibling readings assert it is resolved (establishment of Israel, international recognition) or transcended (survival precedes legality). The reading's mandatrophy status is CONTESTED rather than RESOLVED. The theater_ratio rise (0.08 to 0.41) and stabilization post-1967 suggest growing performative activity without functional restoration of status quo ante, which is consistent with mandatrophy drift — the reading's founding coordination problem (how to legitimately resolve the territorial dispute) remains unsolved; the constraint persists partly by repeatedly invoking the principle without implementing it. This is NOT yet piton (the reading is actively enforced, not theatrically maintained), but the measurement trajectory suggests mandatrophy movement: the founding problem (resolution of competing territorial claims via self-determination) remains live in discourse while material outcomes (right of return, territorial restoration) remain unrealized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_frame_contestation,
    'Is the self-determination principle''s legitimacy grounded in a specific historical period (modern/19th-20th centuries) or in a principle that operates timelessly, making pre-modern claims equally valid?',
    'International law scholarship and state practice: does self-determination doctrine as actually applied prioritize recent continuous presence or does it recognize ancient claims? The post-colonial jurisprudence (e.g., UN decolonization resolutions) applied self-determination to immediate pre-independence populations; but does that temporally-bounded application establish a permanent principle or a contingent historical remedy?',
    'If self-determination requires modern continuity, the reading''s restriction of the relevant timeframe to 19th-20th centuries is a principled application of doctrine. If self-determination operates timelessly, the reading''s temporal restriction is an arbitrary limitation that excludes legitimate pre-modern claims. This directly affects the legitimacy of both the Arab majority claim and the Jewish covenant claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_frame_contestation, conceptual, 'Whether self-determination doctrine is temporally bounded or universal in scope.').

omega_variable(
    demographic_majority_as_legitimacy,
    'Does demographic majority constitute a sufficient legitimacy ground for sovereignty, or can it be overridden by other claims (historical, religious, existential security)?',
    'International law on plebiscites, partition principles, and minority rights: the majority-rule framing must be tested against cases where international law has accepted minority territorial claims or rejected majority voting (e.g., indigenous peoples'' rights, religious minority protections). Does the doctrine actually prioritize demographic majority as dispositive?',
    'If majority-rule is dispositive, the reading''s claim that Arab demographic majority grounds sovereignty is well-founded, and the partition''s division is unjust. If majority-rule is defeasible by stronger claims (minority rights, historical claim, security necessity), the reading''s framework is incomplete and the partition may have been justified despite majority opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_as_legitimacy, empirical, 'Whether demographic majority is a sufficient and overriding legitimacy criterion in international law.').

omega_variable(
    reading_incommensurability,
    'Are the three readings of the territorial_sovereignty_legitimacy kernel logically incommensurable (each forecloses the others in principle) or merely empirically competing (each could be true in different contexts)?',
    'Meta-analytical: does accepting the covenant_continuity reading (ancient Jewish claim) logically entail rejecting the self-determination reading, or can both claims coexist in a single framework? The answer determines whether the readings are alternative formulations of ONE normative order or expressions of a fundamental constitutional disagreement about what legitimacy means.',
    'If incommensurable, the three readings represent three different sovereignty kernels, and territorial resolution requires choosing one and rejecting others. If empirically competing but logically compatible, a mixed legitimacy framework (recognizing both ancient and modern claims) might be possible. The reading''s entire framing depends on this being a zero-sum choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the sibling readings are logically foreclosed by each other or merely empirically contested.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the suppression of pre-modern Jewish claims enforced by external institutional power (other states, international law bodies) or has it been internalized by parties who now believe pre-modern claims are irrelevant to modern self-determination?',
    'Post-reading-reversal observation: if a sibling reading (covenant_continuity) were to gain ascendance and pre-modern claims were re-legitimized, how quickly would parties abandon the modern-frame suppression? Internalized suppression would persist even after external enforcement ends; structural suppression would dissolve.',
    'If internalized, the reading has become self-perpetuating and more resistant to counter-claims. If structural, it is more fragile and subject to institutional change. The suppression metric (0.72) measures the raw requirement; the mechanism determines whether it reflects real cognitive capture or merely institutional constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression of alternative legitimacy claims is structural or internalized.').

omega_variable(
    right_of_return_implementation_gap,
    'Is the gap between the reading''s vindication of right-of-return (in principle) and its non-implementation (in practice) a sign of mandatrophy drift (the founding problem is unresolved, theater persists), or a sign that the reading was never meant to resolve the territorial dispute but only to establish a justice narrative?',
    'Historical analysis of how the reading''s proponents have discussed implementation: do they present right-of-return as a contingent political outcome (negotiable, subject to compromise) or as a normative entitlement (non-negotiable, foundational to the reading''s claim)? The answer determines whether non-implementation is a failure of the reading or evidence that it was never coordinated to produce territorial resolution.',
    'If the reading was coordinated to resolve the territorial dispute, non-implementation indicates mandatrophy — the founding problem persists, theater rises. If the reading was always meant to establish a justice claim rather than territorial resolution, non-implementation is success (the reading achieved what it was designed for: moral vindication). This affects the classification trajectory and whether mandatrophy_resolved is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_implementation_gap, preference, 'Whether the reading''s founding problem is unresolved territorial coordination or achieved justice narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1880, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1880, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(terr_tr_t1920, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement(terr_tr_t1945, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1945, 0.28).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.42).
narrative_ontology:measurement(terr_tr_t1980, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1980, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t1880, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(terr_be_t1920, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(terr_be_t1945, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1945, 0.62).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.76).
narrative_ontology:measurement(terr_be_t1980, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1980, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1880, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement(terr_su_t1920, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement(terr_su_t1945, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.73).
narrative_ontology:measurement(terr_su_t1980, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1980, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way contested kernel (territorial_sovereignty_legitimacy). The other readings are covenant_continuity_reading (legitimacy via ancient covenant + international recognition) and existential_matrix_reading (legitimacy via survival preconditions). Each reading instantiates a different constraint because each reweights historical evidence, the relevant temporal frame, and what counts as a legitimate people. The three constraints share the same referent (territorial sovereignty in the Israeli-Palestinian context) but have incommensurable ε values: covenant reading sees high legitimacy (low ε); existential reading sees legitimacy as prior to legality (high ε from legal dispute, low from survival logic); this reading sees partition as extraction (high ε). No single grand unified story could capture all three without collapsing the kernel into an averaged claim. The network link declares the incommensurability and enables analysis of how each reading affects the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
