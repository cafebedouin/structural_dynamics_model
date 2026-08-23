% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law (Endogamy & Priestly Authority)
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   Parsi Zoroastrian marriage law operates through the Parsi Marriage and
 *   Divorce Act (1936) and, more fundamentally, through priestly control of
 *   ritual validity. The core constraint is endogamy: a Parsi who marries a
 *   non-Parsi loses religious standing; their children are denied navjote
 *   (initiation) and access to fire temples and tower of silence. Priestly
 *   authority is the enforcement mechanism — mobeds/dasturs determine who may
 *   enter fire temples, receive last rites, and access community trust
 *   housing and schools. The community has shrunk from ~114k (1941) to ~57k
 *   (2011) in India; global estimates ~100k. Fertility is far below
 *   replacement. The endogamy rule, once a survival adaptation, now
 *   accelerates demographic collapse. Yet priestly authority resists reform,
 *   citing ritual purity and cosmic order. The constraint coordinates
 *   community preservation but extracts severely from intermarried members
 *   (especially women and children) and excludes converts entirely. Claimed
 *   as rope (community survival mechanism) but operates as tangled rope:
 *   genuine coordination function + asymmetric extraction enforced by
 *   priestly authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law (Endogamy & Priestly Authority)").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '5fbf9928-b999-460a-8ff7-6bbbd83b09df').
narrative_ontology:cs_kernel_codification('5fbf9928-b999-460a-8ff7-6bbbd83b09df', formalized).
narrative_ontology:cs_authority_grounding('5fbf9928-b999-460a-8ff7-6bbbd83b09df', lineage).
narrative_ontology:cs_interpretation_layer_present('5fbf9928-b999-460a-8ff7-6bbbd83b09df').
narrative_ontology:cs_reading_relation('5fbf9928-b999-460a-8ff7-6bbbd83b09df', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fbf9928-b999-460a-8ff7-6bbbd83b09df', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fbf9928-b999-460a-8ff7-6bbbd83b09df', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fbf9928-b999-460a-8ff7-6bbbd83b09df', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('5fbf9928-b999-460a-8ff7-6bbbd83b09df', foundational, endogamy_as_survival_imperative).
narrative_ontology:cs_axiom_status(endogamy_as_survival_imperative, holdable).
narrative_ontology:cs_axiom_grounding('5fbf9928-b999-460a-8ff7-6bbbd83b09df', endogamy_as_survival_imperative, deontological).
narrative_ontology:cs_axiom('5fbf9928-b999-460a-8ff7-6bbbd83b09df', foundational, priestly_ritual_authority_preserves_cosmic_order).
narrative_ontology:cs_axiom_status(priestly_ritual_authority_preserves_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('5fbf9928-b999-460a-8ff7-6bbbd83b09df', priestly_ritual_authority_preserves_cosmic_order, theological).
narrative_ontology:cs_axiom('5fbf9928-b999-460a-8ff7-6bbbd83b09df', secondary, female_lineage_exclusion_from_status_transmission).
narrative_ontology:cs_axiom_status(female_lineage_exclusion_from_status_transmission, holdable).
narrative_ontology:cs_axiom_grounding('5fbf9928-b999-460a-8ff7-6bbbd83b09df', female_lineage_exclusion_from_status_transmission, conventional).
narrative_ontology:cs_reference_frame('5fbf9928-b999-460a-8ff7-6bbbd83b09df', qissa_i_sanjan_founding_covenant).
narrative_ontology:cs_drift_state('5fbf9928-b999-460a-8ff7-6bbbd83b09df', contemporary_demographic_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fbf9928-b999-460a-8ff7-6bbbd83b09df', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_parsi_families).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_institutions).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, would_be_converts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobeds and dasturs control ritual validity of marriages, navjote ceremonies, and access to fire temples. Their authority derives from hereditary priestly lineage and textual mastery. They determine who counts as Parsi for religious purposes. Exit means abandoning priestly identity and community standing; no parallel religious structure exists.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, agenda_setter,
    institutional, generational, identity_locked, global).

% Families maintaining endogamous marriages retain full community membership, access to fire temples, tower of silence, trust housing, educational institutions, and marriage pool for children. They benefit from the boundary that excludes intermarried members. Exit would mean losing these material and social goods, but they could assimilate into broader Indian/Western society.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_parsi_families, beneficiary,
    organized, biographical, constrained, global).

% Parsis who marry non-Parsis lose access to fire temples, cannot have last rites at tower of silence, children denied navjote and community status. Women historically lose more (cannot pass status to children); men's children may be accepted if navjote performed. They bear the cost of the boundary. Exit options: accept exclusion, fight legal battles (some won in courts), or leave community entirely.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals, excluded).

% Born to one Parsi parent (usually father if navjote possible, mother if not). Denied navjote, fire temple entry, community institutions. No choice in parent's marriage decision. Structural exclusion from birth. Exit means complete assimilation; no pathway to inclusion without priestly approval which is systematically denied.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage, payer,
    powerless, biographical, trapped, global).

% Historically face severer exclusion: children cannot receive navjote, cannot be considered Parsi regardless of father's status. Recent court challenges (e.g., Goolrokh Gupta case) created limited pathways but priestly resistance persists. Bear gendered cost of endogamy rule. Exit options similar to intermarried men but with additional structural barrier for children.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out, payer,
    moderate, biographical, constrained, global).

% Non-Parsis wishing to join the community. Priestly consensus historically prohibits conversion; no ritual pathway exists. Community shrinkage makes this exclusion structurally significant. They are excluded not by their choice but by the community's boundary logic. No exit needed — they are never admitted.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, would_be_converts, excluded,
    powerless, immediate, trapped, global).

% Secular courts adjudicate disputes over trust properties, housing, and religious rights. Have ruled partly in favor of intermarried Parsis (e.g., allowing fire temple entry, navjote for children of Parsi mothers) but defer to religious authority on core ritual questions. Their interventions create pressure for reform but cannot override priestly ritual authority directly.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_state_courts, observer,
    institutional, generational, analytical, national).

% Community members advocating for gender equality in inheritance of status, acceptance of converts, or relaxation of endogamy. Marginalized within community institutions; labeled as threatening community survival. Their voices are structurally excluded from priestly decision-making. Exit options: persist internally, leave community, or seek secular legal remedies.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_reformist_voices, excluded,
    moderate, biographical, constrained, global).

% Track Parsi population decline (~100k globally, fertility ~0.8, aging). Document that endogamy rule accelerates demographic collapse. Provide evidence that boundary maintenance contradicts survival goal. No stake in outcome; analytical seat only.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, demographers_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a micro-minority ethno-religious community's distinct identity, ritual continuity, and material trust infrastructure across diaspora by maintaining a hard membership boundary.
% TRANSFER_FUNCTION: Transfers community membership, ritual access, trust benefits, and intergenerational status from intermarried individuals and their children to the endogamous core and priestly authority. The boundary itself is the mechanism: exclusion of the intermarried concentrates community goods among the endogamous.
% ABSENT_VOICES: Intermarried Parsis and their children (excluded from decision-making), would-be converts (never admitted), reformist Parsis (marginalized in community forums). Their absence is structural: the priestly authority that sets the agenda has no mechanism for their inclusion, and the endogamous families who benefit have no incentive to include them.
% DISAPPEARANCE_RATIONALE: If the endogamy rule and priestly gatekeeping vanished overnight, the community would likely open to converts and intermarried families, slowing demographic collapse but fundamentally altering what 'Parsi' means. Trust properties, fire temples, and tower of silence access would face contested governance. The community might survive as a voluntary association but would lose its current ethno-religious coherence.
% FOUNDING_PROBLEM: After the Islamic conquest of Persia (7th century), Zoroastrians who migrated to India needed a survival strategy that preserved religious identity, ritual purity, and community cohesion under foreign rule without a sovereign territory.
% FOUNDING_PROBLEM_CORROBORATION: Priestly tradition and community historians attest the survival imperative remains live (demographic crisis proves it). Reformists, demographers, and secular scholars attest the founding problem has mutated: the boundary that once ensured survival now drives extinction. Court judgments (e.g., Gujarat High Court on navjote for children of Parsi mothers) implicitly corroborate the shifted-function reading by recognizing exclusion's disproportionate harm.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high: the boundary transfers community goods from a growing excluded class (intermarried, their children, would-be converts) to a shrinking endogamous core and priesthood. Suppression (0.62) is moderate-high: exclusion is enforced through ritual denial (no navjote, no fire temple, no tower of silence) and trust governance, not state coercion, but the social and existential cost is severe. Theater ratio (0.41) is significant: priestly rhetoric emphasizes survival and cosmic order, but the demographic data shows the rule now threatens survival; the performance of 'preserving the community' increasingly masks the extraction of status from the excluded. Accessibility collapse (0.58): alternatives exist (civil marriage, assimilation, other faiths) but carry total community severance. Resistance (0.45): legal challenges, reformist advocacy, and demographic reality create pressure but priestly authority holds.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood seat, the constraint is genuine coordination: without the boundary, the community dissolves into assimilation; ritual purity is non-negotiable cosmic law. From intermarried and excluded seats, it is enforced extraction: the boundary concentrates dwindling community resources among a shrinking elite while expelling the very people who could sustain the community. The engine will compute this divergence from the structural data — the priesthood's identity_locked exit and institutional power versus the trapped/constrained exit of the excluded produces diametrically opposed effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Priesthood (agenda_setter, institutional, identity_locked) sits at beneficiary end (d ~0.15): they control the boundary, their authority depends on it. Endogamous families (beneficiary, organized, constrained) also beneficiary-end (d ~0.25): they collect trust goods, marriage pool, ritual access. Intermarried individuals (payer, moderate, constrained) and their children (payer, powerless, trapped) sit at target end (d ~0.85-0.95): they bear exclusion costs with minimal exit. Parsi women marrying out face additional gendered extraction (d ~0.9). Would-be converts (excluded, powerless, trapped) are structurally excluded — directionality conceptually applies but they never enter. State courts (observer, institutional, analytical) and demographers (observer, analytical, analytical) sit at analytical (d=0.5). Reformist voices (excluded, moderate, constrained) are payer-adjacent but with voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (survival in diaspora) is contested: priesthood says it's live; demographers say the solution now causes the problem. The constraint shows mandatrophy markers: original function (survival) inverted by current operation (accelerated extinction), but priestly authority prevents sunset. Theater ratio rising over time (0.15→0.41) tracks the decoupling of rhetoric from outcome. This is not a piton — priesthood actively benefits and enforces — but a tangled rope where coordination cover persists despite extraction dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a distinct reading of the family_law_authority kernel, or a parameterization of a shared constraint?',
    'Apply ε-invariance test: if evaluating Parsi marriage law via different observables (demographic trajectory vs. ritual compliance vs. trust property rights) yields different ε values, it confirms multiple constraints. Current analysis: ε is stable at ~0.68 across observables for this reading; sibling readings would yield different ε.',
    'Confirms this JSON correctly instantiates one kernel reading per DP-001. If ε varied by observable within this reading, decomposition would be required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Kernel/reading decomposition validity for this constraint story').

omega_variable(
    demographic_survival_vs_boundary_maintenance,
    'Does the endogamy rule still serve the community survival function it was founded for, or has it become a pure extraction mechanism that accelerates extinction?',
    'Counterfactual modeling: project population trajectories under (a) current endogamy, (b) gender-neutral status inheritance, (c) conversion openness. Compare community coherence metrics across scenarios.',
    'If (b) or (c) show survival with coherence, the constraint''s claimed coordination function is falsified — extraction is dominant. If all scenarios show dissolution, the coordination claim holds but the constraint is a failing scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_survival_vs_boundary_maintenance, empirical, 'Whether the constraint''s coordination function is empirically viable or a cover story').

omega_variable(
    priestly_authority_vs_community_autonomy,
    'Is priestly gatekeeping a genuine coordination mechanism for ritual validity, or does it concentrate power in a hereditary class that extracts status rents?',
    'Analyze whether ritual functions (navjote, last rites, fire temple access) could be administered by elected community bodies without doctrinal rupture. Compare with Protestant congregational models vs. Catholic hierarchical models.',
    'If ritual administration is separable from hereditary priesthood, the extraction component is isolable. If inseparable, the constraint is more deeply tangled — priestly authority is the coordination mechanism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_authority_vs_community_autonomy, conceptual, 'Structural separability of priestly authority from community coordination function').

omega_variable(
    gendered_extraction_asymmetry,
    'Is the disproportionate exclusion of Parsi women marrying out (and their children) a doctrinal necessity or a patriarchal extraction layer embedded in the endogamy rule?',
    'Textual-historical analysis: trace the gender asymmetry to specific textual sources vs. later customary accretion. Compare with Zoroastrian communities in Iran (where conversion/exclusion dynamics differ).',
    'If patriarchal accretion, the constraint contains a nested snare within the tangled rope — gendered extraction that could be severed without touching endogamy per se. If doctrinal, the gender asymmetry is structural to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_extraction_asymmetry, empirical, 'Origin and structural necessity of gendered exclusion in Parsi marriage law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1865, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flapzr_tr_t1865, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(flapzr_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(flapzr_tr_t1936, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(flapzr_tr_t1960, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(flapzr_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(flapzr_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(flapzr_tr_t2025, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(flapzr_be_t1865, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement(flapzr_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(flapzr_be_t1936, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1936, 0.48).
narrative_ontology:measurement(flapzr_be_t1960, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(flapzr_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(flapzr_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(flapzr_be_t2025, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(flapzr_su_t1865, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1865, 0.45).
narrative_ontology:measurement(flapzr_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(flapzr_su_t1936, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1936, 0.52).
narrative_ontology:measurement(flapzr_su_t1960, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(flapzr_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(flapzr_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(flapzr_su_t2025, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, parsi_trust_property_governance).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, parsi_fire_temple_access_rules).

% DUAL FORMULATION NOTE:
% This constraint is the parsi_zoroastrian_reading of the family_law_authority kernel. It decomposes the colloquial 'religious family law' label into a structurally precise claim: endogamy + priestly gatekeeping as community preservation mechanism. The ε differs substantially from sibling readings: hindu_dharmashastra (caste endogamy, ~0.55 extractive), muslim_shariat (contractual, ~0.35), christian_canonical (sacramental, ~0.45), secular_contractual (~0.15). This reading's high ε (0.68) reflects demographic crisis + priestly resistance to reform. All sibling readings are linked via affects_constraints in their respective stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, organized, 0.25).
constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, moderate, 0.85).
constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
