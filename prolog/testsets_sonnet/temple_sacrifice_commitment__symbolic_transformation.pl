% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Sacrifice Commitment Reconstituted as Prayer/Study (Authorized Transformation Reading)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates the 'symbolic_transformation' reading of the
 *   temple_sacrifice_commitment kernel: the claim that, following the
 *   Temple's destruction, rabbinic authority did not merely suspend the
 *   sacrificial commitment pending restoration, but authoritatively
 *   transformed it — prayer and study are now the actual, complete
 *   instantiation of the divine command, not a placeholder for it. This is
 *   structurally distinct from the sibling readings (study_as_exercise treats
 *   study as itself performance without claiming full transformation of the
 *   underlying command; performance_only holds nothing but material sacrifice
 *   discharges the commitment and treats prayer as archival;
 *   hybrid_preparatory holds the commitment is suspended in an active
 *   preparatory state pending messianic restoration). The transformation
 *   reading is the one that most directly raises the extraction question
 *   named in the brief: if the authority to redefine what discharges a divine
 *   command was never actually granted, then the entire apparatus of
 *   institutions built atop 'prayer = fulfillment' constitutes unauthorized
 *   doctrinal drift dressed as continuity, extracting communal loyalty,
 *   resource allocation, and interpretive deference from those who hold the
 *   original material performance as non-negotiable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.58).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.52).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Sacrifice Commitment Reconstituted as Prayer/Study (Authorized Transformation Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '4db1c900-6f85-40f8-8733-9730d35d8a40').
narrative_ontology:cs_kernel_codification('4db1c900-6f85-40f8-8733-9730d35d8a40', fixed_text).
narrative_ontology:cs_authority_grounding('4db1c900-6f85-40f8-8733-9730d35d8a40', lineage).
narrative_ontology:cs_interpretation_layer_present('4db1c900-6f85-40f8-8733-9730d35d8a40').
narrative_ontology:cs_reading_relation('4db1c900-6f85-40f8-8733-9730d35d8a40', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('4db1c900-6f85-40f8-8733-9730d35d8a40', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('4db1c900-6f85-40f8-8733-9730d35d8a40', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('4db1c900-6f85-40f8-8733-9730d35d8a40', foundational, rabbinic_authority_can_redefine_commitment_instantiation).
narrative_ontology:cs_axiom_status(rabbinic_authority_can_redefine_commitment_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('4db1c900-6f85-40f8-8733-9730d35d8a40', rabbinic_authority_can_redefine_commitment_instantiation, conventional).
narrative_ontology:cs_axiom('4db1c900-6f85-40f8-8733-9730d35d8a40', foundational, prayer_study_are_complete_not_provisional_fulfillment).
narrative_ontology:cs_axiom_status(prayer_study_are_complete_not_provisional_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('4db1c900-6f85-40f8-8733-9730d35d8a40', prayer_study_are_complete_not_provisional_fulfillment, deontological).
narrative_ontology:cs_reference_frame('4db1c900-6f85-40f8-8733-9730d35d8a40', second_temple_material_sacrificial_practice).
narrative_ontology:cs_drift_state('4db1c900-6f85-40f8-8733-9730d35d8a40', post_destruction_rabbinic_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4db1c900-6f85-40f8-8733-9730d35d8a40', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_congregational_leadership).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, prayer_liturgy_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, restorationist_minority_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares, via post-Temple halakhic ruling (rooted in Hosea's 'let our lips substitute for bulls' and rabbinic exegesis), that prayer and study are not merely provisional stand-ins but the authorized new instantiation of the sacrificial commitment itself. This declaration is what makes ongoing communal prayer liturgy the fulfillment of divine command rather than a suspension of it. The authority to make this declaration is itself the asset being exercised — it lets the tradition continue functioning without the Temple, and it is the same authority structure that adjudicates competing readings, so it never faces an external check on whether the transformation claim is itself authorized.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Synagogues, prayer-book compilers, and liturgical scholarship built an entire institutional apparatus (fixed prayer times keyed to sacrificial hours, Amidah structured to mirror the Temple offerings) whose legitimacy rests entirely on the transformation reading being correct. If sacrifice is merely suspended rather than transformed, this apparatus is a stopgap rather than the fulfillment of the commitment, and its centrality diminishes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, prayer_liturgy_institutions, beneficiary,
    organized, civilizational, mobile, global).

% Local rabbinic leadership across the diaspora administers communal religious life entirely through prayer and study; their authority, their institutions, and their claim to be delivering full religious observance (not a diminished substitute) depend on the transformation reading holding. They actively teach and enforce this reading in congregational instruction.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_congregational_leadership, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, diaspora_congregational_leadership, agenda_setter).

% Hold that only material sacrificial performance discharges the original commitment and that prayer/study, however meritorious, is not the same obligation under a different name — the authority to redefine the commitment was never granted. They bear the cost of being treated as marginal or fringe within mainstream institutions, denied liturgical and institutional resources, and their reading is effectively suppressed in mainstream educational curricula because it destabilizes the settled communal consensus.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, restorationist_minority_communities, payer,
    moderate, generational, constrained, regional).

% Scholars who argue the transformation claim overreaches rabbinic authority — that no human court can redefine what discharges a divine command, only permit its temporary non-performance — find their position treated as a historical curiosity rather than a live halakhic option in most institutional settings. Publishing or teaching this view carries real professional and communal cost within mainstream institutions that have built identity around the transformation reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_scholars, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, literalist_halakhic_scholars, excluded).

% Communities and individuals actively preparing for or advocating literal Temple restoration and resumed sacrifice are structurally sidelined by a mainstream consensus that treats the commitment as already fully and permanently discharged through prayer. Their advocacy is read as fringe or even destabilizing to communal order, and they have essentially no institutional platform to contest the transformation reading at scale.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, temple_restoration_advocates, excluded,
    powerless, civilizational, trapped, regional).

% Study the textual and historical record of how the transformation claim was formulated and stabilized after 70 CE, including which rabbinic voices asserted it as settled versus contested. They can trace how much of the consensus reflects genuine authorized ruling versus retrospective institutional necessity, without themselves being bound by any of the readings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, comparative_halakhic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, universally teachable answer to 'what discharges the sacrificial commitment now' so that communal religious life can organize coherently around prayer and study without every congregation individually litigating the theological status of non-performance.
% TRANSFER_FUNCTION: Moves institutional legitimacy, resource allocation, and interpretive authority from any framework that would treat prayer/study as provisional or lesser toward the framework that treats it as the full and authorized fulfillment — and moves social and professional standing away from restorationist and literalist minorities toward mainstream rabbinic and congregational institutions.
% ABSENT_VOICES: Restorationist communities and literalist scholars who hold that only material sacrifice discharges the original obligation are structurally absent from mainstream halakhic curricula and communal decision-making; their objection — that no authority was actually granted to redefine, only to suspend — is treated as settled against them rather than live.
% DISAPPEARANCE_RATIONALE: Mainstream institutional voices would say nothing rearranges — the transformation is settled doctrine and its 'disappearance' is incoherent, since prayer already fully occupies the commitment. Restorationist and literalist voices would say the world rearranges substantially: absent the transformation claim, prayer reverts to being a stopgap, and communal urgency toward restoration and preparatory return would sharpen considerably. The verdict itself splits along the same fault line the constraint governs.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commitments commanded in Torah could no longer be materially performed; some authoritative response was needed to prevent the commitment from either lapsing entirely or leaving the community in permanent unresolved suspension.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream rabbinic authorities and the institutions built on their rulings attest the problem was resolved by authorized transformation and treat this as settled doctrine. Comparative halakhic historians, working from outside the benefiting institutional structure, document that the transformation claim was progressively stabilized over centuries of subsequent commentary rather than issued as a single clean authoritative act at the time of the destruction, and that dissenting minority and messianic-restorationist positions persisted continuously without ever being formally adjudicated against on textual grounds alone.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.52) are set at moderate-high, reflecting that the transformation claim is not merely descriptive but actively displaces a live rival reading (restorationism/literalism) from institutional standing — this is coercive in the soft but real sense of curriculum exclusion and professional cost, not physical coercion. Theater ratio (0.44) reflects that a meaningful share of the liturgical apparatus built around the transformation claim (fixed prayer times keyed to sacrificial hours, structural parallels in the Amidah) now functions partly as institutional self-justification for the claim's correctness rather than purely as devotional practice. Accessibility collapse (0.62) is moderately high: for most communities the transformation reading is presented as the only live option, though it has not achieved mountain-level inevitability since restorationist and literalist positions persist continuously in minority communities. Resistance (0.47) reflects that these minority readings, while marginalized, have not been eliminated and continue to be actively asserted.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority and congregational leadership seats, this looks like a rope: a genuine, hard-won solution to an existential problem, exercised through legitimate interpretive authority accumulated over centuries. From the restorationist and literalist seats, the identical structure looks like a snare: an unauthorized doctrinal move that entrenches itself through institutional capture of prayer books, curricula, and communal legitimacy, foreclosing the position that only material sacrifice discharges the commitment. The engine computes these as different seat-level classifications from the same structural data — the divergence is the finding, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority structure and the institutions it authorizes (prayer_liturgy_institutions, diaspora_congregational_leadership) sit at the beneficiary end: they hold the interpretive power to declare the transformation, and their institutional legitimacy is minted by that declaration being accepted as authoritative rather than as an unauthorized override. Restorationist minority communities and literalist halakhic scholars sit at the target end: constrained exit (leaving means either abandoning normative practice altogether or joining a marginalized minority position with little institutional support), and they bear the cost of having their reading treated as settled-against rather than live. Temple restoration advocates are excluded rather than merely constrained — they are outside the conversation that adjudicates the question at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is genealogical: was the founding problem (sacrificial commitment cannot be materially discharged) actually solved by the transformation claim, or has an unresolved suspension been relabeled as resolution to preserve institutional continuity? Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the arrangement as pure coordination (which would ignore the real cost imposed on dissenting minorities and the real interpretive power being exercised) and treating it as pure extraction (which would ignore that prayer-centered communal religious life genuinely does solve an urgent coordination problem — organizing observance without a Temple — for the overwhelming majority who accept the transformation reading). The R5 corroboration split (mainstream institutions say resolved; independent historians document a much slower, contested stabilization) is exactly the kind of evidence tangled_rope is built to hold without collapsing to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorized_vs_unauthorized_transformation,
    'Did the rabbinic authority structure actually possess the interpretive authority to declare prayer and study the full transformed instantiation of the sacrificial commitment, or does this exceed any authority actually granted (with the true status being permitted suspension, not authorized substitution)?',
    'Close textual-historical analysis of the earliest post-70 CE rabbinic sources (e.g., the framing in tractate Berakhot and related midrashic material) to determine whether the transformation claim was asserted as a novel authoritative ruling or as an extension of already-recognized principles of divine service; comparison with how other suspended commandments were classified in the same period.',
    'If the transformation claim exceeds granted authority, this constraint is better classified as a snare riding on genuine coordination need — the coordination function survives but the specific ''authorized transformation'' framing becomes illegitimate rent-extraction on interpretive trust. If the authority was genuinely exercised within recognized bounds, extraction drops substantially and the tangled_rope classification would trend toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorized_vs_unauthorized_transformation, conceptual, 'Whether the transformation claim is within or beyond the rabbinic authority''s actual interpretive mandate.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the four sibling readings of the temple_sacrifice_commitment kernel diverge — is it on WHETHER transformation occurred (symbolic_transformation vs. performance_only), or on WHAT KIND of occupation counts as fulfilling a suspended commitment (study_as_exercise vs. hybrid_preparatory), or both simultaneously?',
    'Map each reading''s core axiom against the others in a shared decision table: (a) is the commitment still materially binding, (b) has it been redefined or merely suspended, (c) does intellectual/liturgical engagement occupy or merely prepare for the commitment. This story asserts (a) not-currently-material, (b) redefined-not-merely-suspended, (c) occupies-fully.',
    'Clarifies that symbolic_transformation and hybrid_preparatory are not simply stronger/weaker versions of the same claim but differ on the redefinition question specifically — this affects which victim sets and extraction profiles are shared versus distinct across the sibling stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Precise location of structural disagreement among the four kernel readings.').

omega_variable(
    minority_reading_suppression_mechanism,
    'Is the marginalization of restorationist and literalist readings within mainstream institutions a matter of structural exclusion (curricula, funding, ordination gatekeeping) or internalized deference (minority adherents themselves treating the transformation reading as authoritative even while formally dissenting)?',
    'Survey minority-community religious leadership on whether their dissent is actively suppressed by mainstream institutional gatekeeping versus whether their own adherents have substantially internalized the transformation framing despite formal doctrinal disagreement.',
    'If suppression is primarily structural, targeted institutional reform (curricular inclusion, platform access) could meaningfully rebalance the constraint toward rope. If substantially internalized, the effective suppression is higher than the structural measure alone suggests, since minority adherents carry it even absent active institutional exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_reading_suppression_mechanism, empirical, 'Structural versus internalized suppression of dissenting kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 300, 0.28).
narrative_ontology:measurement_basis(temp_tr_t300, projected).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 700, 0.33).
narrative_ontology:measurement_basis(temp_tr_t700, projected).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1100, 0.37).
narrative_ontology:measurement_basis(temp_tr_t1100, projected).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.41).
narrative_ontology:measurement_basis(temp_tr_t1500, projected).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1900, 0.44).
narrative_ontology:measurement_basis(temp_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 300, 0.42).
narrative_ontology:measurement_basis(temp_be_t300, projected).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 700, 0.48).
narrative_ontology:measurement_basis(temp_be_t700, projected).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1100, 0.52).
narrative_ontology:measurement_basis(temp_be_t1100, projected).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.55).
narrative_ontology:measurement_basis(temp_be_t1500, projected).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement_basis(temp_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(temp_su_t0, projected).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 300, 0.36).
narrative_ontology:measurement_basis(temp_su_t300, projected).
narrative_ontology:measurement(temp_su_t700, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 700, 0.4).
narrative_ontology:measurement_basis(temp_su_t700, projected).
narrative_ontology:measurement(temp_su_t1100, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1100, 0.44).
narrative_ontology:measurement_basis(temp_su_t1100, projected).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement_basis(temp_su_t1500, projected).
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement_basis(temp_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the temple_sacrifice_commitment kernel, decomposed per the ε-invariance principle: measuring 'what discharges the sacrificial commitment now' yields structurally distinct answers depending on whether one asks about authorized redefinition (this story), mere exercise-through-study (study_as_exercise), strict material non-substitutability (performance_only), or active preparatory suspension (hybrid_preparatory). Each carries its own ε, victim set, and classification rather than being folded into a single averaged or hedged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
