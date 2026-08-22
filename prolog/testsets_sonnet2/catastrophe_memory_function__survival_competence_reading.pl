% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover as Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint models one reading of a contested kernel: the function
 *   commemorative Passover practice serves in relation to historical
 *   catastrophe. Under the survival-competence reading (D5), the ritual's
 *   core function is not primarily grief-processing or boundary-maintenance
 *   but the transmission of a rehearsed, decentralized procedural competence
 *   — how to reconstitute meaningful communal practice with minimal
 *   institutional infrastructure after catastrophic institutional loss. This
 *   reading treats the household-portable, non-clergy-dependent structure of
 *   the ritual (any household can lead it, no temple or ordained priesthood
 *   required) as evidence that the practice's design encodes an adaptive
 *   lesson about surviving institutional collapse through decentralization,
 *   not merely as an accident of historical circumstance. Two sibling
 *   readings of the same kernel exist as separate constraints:
 *   mourning_practice_reading (emphasizing D1/D4 — memorial obligation and
 *   boundary-norm maintenance) and hybrid_transformation_reading (which holds
 *   both functions are simultaneously encoded). This story authors ONLY the
 *   D5 reading; ε, beneficiaries, and structural data here describe this
 *   reading's account of the standing arrangement, not a blend across
 *   readings.
 *
 * KEY AGENTS:
 *   - diaspora_communities: primary beneficiary (organized/constrained) — receive a decentralized continuity template
 *   - decentralized_household_practitioners: agenda-setting beneficiary (moderate/mobile) — perform and adapt the transmission each cycle
 *   - future_generations_facing_institutional_collapse: powerless/trapped beneficiary — inherit the competence without having shaped it
 *   - ritual_leaders_and_transmitters: agenda_setter (moderate/mobile) — decide the framing emphasis transmitted to the next cohort
 *   - institutional_historians_and_folklorists: analytical observer
 *   - mourning_focused_practitioners: excluded from this reading's frame — addressed by the sibling mourning_practice_reading instead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'a98733e1-60e6-434a-9192-b445679e8030').
narrative_ontology:cs_kernel_codification('a98733e1-60e6-434a-9192-b445679e8030', fixed_text).
narrative_ontology:cs_authority_grounding('a98733e1-60e6-434a-9192-b445679e8030', practice).
narrative_ontology:cs_interpretation_layer_present('a98733e1-60e6-434a-9192-b445679e8030').
narrative_ontology:cs_reading_relation('a98733e1-60e6-434a-9192-b445679e8030', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a98733e1-60e6-434a-9192-b445679e8030', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('a98733e1-60e6-434a-9192-b445679e8030', foundational, ritual_form_encodes_procedural_survival_knowledge).
narrative_ontology:cs_axiom_status(ritual_form_encodes_procedural_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('a98733e1-60e6-434a-9192-b445679e8030', ritual_form_encodes_procedural_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('a98733e1-60e6-434a-9192-b445679e8030', secondary, decentralized_leadership_capacity_is_the_transmitted_content).
narrative_ontology:cs_axiom_status(decentralized_leadership_capacity_is_the_transmitted_content, holdable).
narrative_ontology:cs_axiom_grounding('a98733e1-60e6-434a-9192-b445679e8030', decentralized_leadership_capacity_is_the_transmitted_content, empirically_contingent).
narrative_ontology:cs_reference_frame('a98733e1-60e6-434a-9192-b445679e8030', decentralized_household_transmission_baseline).
narrative_ontology:cs_drift_state('a98733e1-60e6-434a-9192-b445679e8030', contemporary_diaspora_context, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a98733e1-60e6-434a-9192-b445679e8030', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, decentralized_household_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations_facing_institutional_collapse).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_as_adaptive_knowledge_transmission).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_continuity_without_central_temple).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed communities without access to centralized institutional infrastructure (temple, unified priesthood, territorial polity) rely on household-scale, portable ritual practice to reconstitute communal function after institutional loss. The Seder is performed at home, requires no ordained clergy, and can be led by any household head, which is what let practice survive repeated displacement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Individual families and small groups who perform and adapt the ritual across geographically dispersed and institutionally discontinuous settings. They set the practice's concrete form each cycle (who leads, what is emphasized, what adaptations are made) and, in doing so, transmit a rehearsed procedural competence: how to reconstitute a functioning communal practice with minimal infrastructure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, decentralized_household_practitioners, beneficiary,
    moderate, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, decentralized_household_practitioners, agenda_setter).

% Not yet born, and structurally unable to consent to or shape the practice they will inherit. They benefit from an embodied procedural template for surviving institutional discontinuity, but have no voice in whether the ritual is maintained, altered, or discarded before the moment they might need it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, future_generations_facing_institutional_collapse, beneficiary,
    powerless, civilizational, trapped, global).

% Parents, elders, and lay leaders who transmit the practice within households and small communities. They decide how the ritual is taught to the next cohort — emphasis on the exodus narrative as instruction in what to do when institutions fail, rather than solely as loss commemoration. Their exit option is mobile: they can modify emphasis, drop elements, or transmit selectively without external enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_leaders_and_transmitters, agenda_setter,
    moderate, generational, mobile, local).

% Study the documented resilience of decentralized ritual practice across historical episodes of institutional destruction (temple loss, expulsion, persecution) and can compare the survival-competence framing against alternative readings of the same textual and performative record.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, institutional_historians_and_folklorists, observer,
    analytical, civilizational, analytical, global).

% Practitioners and interpreters who center the ritual's grief and boundary-maintenance functions rather than its adaptive-competence function. They are not silenced, but this reading's framing does not surface their emphasis — they are a live alternative constituency within the same textual tradition, addressed by the sibling mourning_practice_reading, not by this constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, mourning_focused_practitioners, excluded,
    moderate, generational, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual coordinates the transmission, across generations and geographically scattered households, of a rehearsed procedural template for reconstituting communal identity and practice when centralized institutions (temple, unified political authority) are destroyed or unavailable — a genuine solution to the problem of maintaining group continuity without dependence on fragile centralized infrastructure.
% TRANSFER_FUNCTION: The arrangement transfers procedural knowledge and adaptive competence from each generation of practitioners to the next: how to lead, adapt, and reconstitute the practice under resource-poor or institutionally disrupted conditions. No resource, money, or status is extracted from a payer class under this reading — the primary movement is knowledge and capacity, not rent.
% ABSENT_VOICES: Practitioners and interpretive traditions centered on mourning and boundary-maintenance (the sibling mourning_practice_reading) are not represented within this reading's framing; they would object that reducing the ritual to survival-competence undersells its grief and identity-boundary functions. They are present in the broader tradition but excluded from this specific reading's emphasis.
% DISAPPEARANCE_RATIONALE: Proponents of this reading would argue that if the ritual's survival-competence function disappeared, decentralized communities would lose a proven low-infrastructure mechanism for reconstituting practice after institutional rupture, and the historical record of repeated post-catastrophe continuity would not have the same explanatory account. Skeptics (including adherents of the sibling readings) would argue the ritual's other functions — mourning, boundary-maintenance — would sustain continuity on their own, making the survival-competence framing a redundant gloss rather than a load-bearing function; hence the verdict is contested rather than settled in either direction under this reading alone.
% FOUNDING_PROBLEM: The recurring destruction or inaccessibility of centralized religious and political institutions (temple destruction, exile, expulsion) created a need for a continuity mechanism that did not depend on any single fixed institutional location or authority structure.
% FOUNDING_PROBLEM_CORROBORATION: Historians of diaspora religious practice and comparative ritual scholars outside the practicing community attest that decentralized, household-portable ritual forms empirically correlate with community survival through documented episodes of institutional destruction; this corroboration comes from academic observers rather than from the practicing communities who benefit from the reading, though no fully disinterested party exists given the topic's inherent connection to communities that also self-narrate their own resilience.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because under this reading no party is structurally paying a cost for the ritual's persistence — the primary movement is knowledge/competence, not resource transfer, and there is no identified payer class. Suppression is low-moderate (0.22): family and community social pressure to participate exists but nothing resembling coercive enforcement is required for the transmission function to operate. Theater ratio is moderate-low (0.28) and rises gently over the interval, reflecting that as centralized institutional threats recede in the observed record, some of the transmitted competence becomes increasingly commemorative/symbolic rather than functionally rehearsed — a mild drift toward performance without crossing into dominant theater. Accessibility collapse is moderate (0.35): once a household understands the ritual's decentralized, low-infrastructure design, the coordination alternative it embodies (versus dependence on centralized religious/political institutions) becomes clearly visible, though it does not foreclose other readings of the same practice. Resistance is low-moderate (0.3): this reading meets some resistance from adherents of the sibling mourning-centered reading who consider the survival-competence framing a modern reinterpretation rather than the practice's core meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   All three primary stakeholder groups are coded as beneficiaries because, under this reading, the ritual's declared function delivers something of genuine value (a decentralized continuity template) without extracting from an identified payer class. Diaspora communities and household practitioners are near-symmetric-to-beneficiary (moderate power, constrained-to-mobile exit) because they invest effort into maintaining the practice but receive back functional resilience. Future generations sit furthest toward pure beneficiary on the ledger despite powerless/trapped positional atoms, because trapped exit here reflects non-consent rather than extraction — they cannot decline inheritance of the competence, but the reading holds nothing is being taken from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional destruction requiring a non-centralized continuity mechanism) is authored as live rather than dead, which under this reading blocks mandatrophy: the survival-competence function this reading identifies remains exercised precisely because centralized-institution fragility remains a real risk across diaspora history and into the present. If historians established the underlying risk had permanently disappeared (a fully secure, non-fragile centralized institutional guarantee), the founding_problem_status would shift toward dead, and continued transmission of the D5 competence would become a candidate for reclassification toward piton (surviving on inertia rather than function) — but that shift belongs to a different measurement, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_mourning_primacy,
    'Is the survival-competence function (D5) the ritual''s primary encoded purpose, or is it a secondary or emergent effect of a practice whose primary function is mourning and boundary-maintenance (D1/D4)?',
    'Comparative textual and performative analysis across historical periods: does emphasis on adaptive/procedural elements (leadership flexibility, minimal infrastructure requirements) predate or postdate periods of institutional destruction, and does emphasis shift measurably in response to institutional threat versus remaining constant?',
    'If survival-competence framing is shown to be a retrospective reinterpretation rather than an original or persistently encoded function, this reading''s claimed_type and beneficiary structure would need revision toward a more contested or symbolic classification; if it is shown to be load-bearing and historically consistent, the rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_competence_vs_mourning_primacy, conceptual, 'Whether D5 is the ritual''s core function or a secondary/retrospective gloss relative to D1/D4.').

omega_variable(
    kernel_reading_selection_basis,
    'What determines which of the three kernel readings (survival_competence, mourning_practice, hybrid_transformation) a given community or scholar adopts, and is that selection itself value-neutral or does it track institutional interest (e.g., which reading better supports contemporary diaspora communities'' self-narratives of resilience)?',
    'Sociological survey of which communities and scholarly traditions favor which reading, cross-referenced against those communities'' contemporary institutional needs and self-narratives.',
    'If reading selection strongly correlates with which reading flatters the selecting community''s contemporary self-image, that would suggest all three readings (including this one) carry some degree of motivated construction rather than purely descriptive accuracy — though this would not by itself resolve which reading is most textually/historically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether kernel reading selection tracks institutional interest rather than pure textual/historical analysis.').

omega_variable(
    founding_problem_persistence,
    'Does the risk of institutional destruction that originally motivated (on this reading) the decentralized-transmission design remain genuinely live for contemporary diaspora communities, or has it substantially receded due to modern state protections and institutional stability?',
    'Comparative risk assessment of institutional fragility for diaspora religious communities across different contemporary political contexts, tracked over time.',
    'If the founding problem is substantially resolved in most contemporary contexts, the founding_problem_status should shift from live toward contested or dead, which would push this reading''s classification toward piton (functional survival-competence transmission continuing on inertia rather than active need) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the institutional fragility risk motivating decentralized transmission remains live today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(cata_tr_t60, observed).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(cata_tr_t80, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement_basis(cata_be_t60, observed).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(cata_be_t80, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.06).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'what function does Passover commemoration serve relative to historical catastrophe' per the ε-invariance principle. survival_competence_reading (this file) claims rope with low ε (0.18) and a clean beneficiary set with no victims. mourning_practice_reading claims a distinct structure centered on D1/D4 (memorial obligation, boundary-norms) with its own independently authored ε. hybrid_transformation_reading claims both functions operate simultaneously and is authored with its own ε reflecting that combined claim. The three share the kernel_id catastrophe_memory_function but are NOT the same constraint — each has its own stable ε, its own stakeholders, and its own classification, linked via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
